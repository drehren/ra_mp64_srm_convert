mod controller_pack;
mod create_srm;
mod game_pack;
mod retroarch_srm;
mod split_srm;

#[macro_use]
mod logger;

use controller_pack::*;
use create_srm::*;
use split_srm::*;

use clap::Parser;

use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::{Debug, Display};
use std::fs::{self, File};
use std::io::{self, ErrorKind};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

fn pack_number<P: AsRef<Path>>(path: &P) -> Option<usize> {
  let str = path.as_ref().extension()?.to_str()?;
  let last = str.chars().last()?;
  Some(last.to_digit(5)?.checked_sub(1)? as usize)
}

fn change_endianness(buf: &mut [u8]) {
  for i in (0..buf.len()).step_by(4) {
    buf.swap(i + 0, i + 3);
    buf.swap(i + 1, i + 2);
  }
}

struct OutputDir<'out, 'base> {
  out_dir: &'out Option<PathBuf>,
  base: &'base Path,
}
impl<'out, 'base> OutputDir<'out, 'base> {
  fn new(out_dir: &'out Option<PathBuf>, base: &'base Path) -> Self {
    Self { out_dir, base }
  }

  fn output_path<P>(&self, input: &P) -> PathBuf
  where
    P: AsRef<Path> + ?Sized,
  {
    let input = input.as_ref();
    if input.is_absolute() {
      return input.into();
    }
    let file_name = input.file_name().unwrap_or(&OsStr::new(""));
    let mut path: PathBuf = if let Some(out_dir) = self.out_dir {
      out_dir
    } else {
      self.base.parent().unwrap()
    }
    .into();
    path.push(file_name);
    path
  }

  fn from_base<S>(&self, ext: &S) -> PathBuf
  where
    S: AsRef<OsStr> + ?Sized,
  {
    self.output_path(self.base.with_extension(ext).file_name().unwrap())
  }
}

#[derive(PartialEq)]
enum ConvertMode {
  Create(PathBuf),
  Split(PathBuf),
}

impl ConvertMode {
  fn get_path(&self) -> &Path {
    match self {
      ConvertMode::Create(p) | ConvertMode::Split(p) => p,
    }
  }

  fn into_path(self) -> PathBuf {
    match self {
      Self::Create(path) | Self::Split(path) => path,
    }
  }

  fn is_create(&self) -> bool {
    match self {
      ConvertMode::Create(_) => true,
      _ => false,
    }
  }
}

impl Debug for ConvertMode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if f.alternate() {
      return f.write_fmt(format_args!("{}", self.get_path().display()));
    }
    match self {
      Self::Create(path) => f.debug_tuple("Merge").field(path).finish(),
      Self::Split(path) => f.debug_tuple("Split").field(path).finish(),
    }
  }
}

impl Display for ConvertMode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self {
      ConvertMode::Create(path) if !path.exists() => f.write_str("create "),
      ConvertMode::Create(_) => f.write_str("update "),
      ConvertMode::Split(_) => f.write_str("split "),
    }?;
    f.write_fmt(format_args!("{}", self.get_path().display()))
  }
}

#[derive(Debug, Default)]
struct SrmPaths {
  eep: Option<PathBuf>,
  sra: Option<PathBuf>,
  fla: Option<PathBuf>,
  cp: [Option<PathBuf>; 4],
}

impl SrmPaths {
  fn insert_or_update_cp(&mut self, is_mupen: bool, path: PathBuf) -> SavedPath {
    if is_mupen {
      let _ = self.cp.split_first_mut().map(|(_, r)| r.fill(None));
      SavedPath::cp(CPKind::Mupen, self.cp[0].replace(path))
    } else {
      let i = pack_number(&path).map_or(0, |i| i);
      SavedPath::cp((i + 1).into(), self.cp[i].replace(path))
    }
  }

  fn is_empty(&self) -> bool {
    self.eep.is_none()
      && self.fla.is_none()
      && self.sra.is_none()
      && self.cp.iter().all(Option::is_none)
  }

  fn any_is_file(&self) -> bool {
    self.eep.as_ref().map_or(false, |p| p.is_file())
      || self.fla.as_ref().map_or(false, |p| p.is_file())
      || self.sra.as_ref().map_or(false, |p| p.is_file())
      || self.cp[0].as_ref().map_or(false, |p| p.is_file())
      || self.cp[1].as_ref().map_or(false, |p| p.is_file())
      || self.cp[2].as_ref().map_or(false, |p| p.is_file())
      || self.cp[3].as_ref().map_or(false, |p| p.is_file())
  }

  fn get_invalid_paths(&self) -> Vec<&Path> {
    let mut vec = Vec::new();
    if let Some(p) = &self.eep {
      if !p.is_file() {
        vec.push(p.as_path());
      }
    }
    if let Some(p) = &self.fla {
      if !p.is_file() {
        vec.push(p.as_path());
      }
    }
    if let Some(p) = &self.sra {
      if !p.is_file() {
        vec.push(p.as_path());
      }
    }
    for p in &self.cp {
      if let Some(p) = p {
        if !p.is_file() {
          vec.push(p.as_path())
        }
      }
    }
    vec
  }
}

macro_rules! display_some_path {
  ($c:expr, $f:expr, $name:expr, $path:expr) => {
    if let Some(path) = $path {
      if $c > 0 {
        $f.write_str(" and ")?;
      }
      $f.write_fmt(format_args!("{} ({})", $name, path.display()))?;
      $c += 1;
    }
  };
}

impl Display for SrmPaths {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut counter = 0;
    display_some_path!(counter, f, "EEPROM", &self.eep);
    display_some_path!(counter, f, "SRAM", &self.sra);
    display_some_path!(counter, f, "FlashRAM", &self.fla);
    if !f.alternate() {
      for i in 1..=4 {
        display_some_path!(counter, f, format!("Controller Pack {i}"), &self.cp[i - 1]);
      }
    } else {
      if let Some(path) = &self.cp[0] {
        if counter > 0 {
          f.write_str(" and ")?;
        }
        f.write_fmt(format_args!("Controller Pack ({})", path.display()))?;
      }
    }
    Ok(())
  }
}

enum AddOpts {
  ForcedCreate(bool),
  ForcedSplit(bool),
  Automatic(bool),
}

impl AddOpts {
  fn is_mupen(&self) -> bool {
    match self {
      Self::ForcedCreate(m) | Self::ForcedSplit(m) | Self::Automatic(m) => *m,
    }
  }
}

#[derive(Debug, Default)]
struct ConvertArgs {
  mode: Option<ConvertMode>,
  paths: SrmPaths,
}

#[repr(i32)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum CPKind {
  Player1,
  Player2,
  Player3,
  Player4,
  Mupen,
}

impl From<usize> for CPKind {
  fn from(value: usize) -> Self {
    match value {
      1 => Self::Player1,
      2 => Self::Player2,
      3 => Self::Player3,
      4 => Self::Player4,
      _ => Self::Mupen,
    }
  }
}

#[derive(Debug, PartialEq, Eq)]
enum SaveType {
  Eeprom,
  Sram,
  FlashRam,
  ControllerPack(CPKind),
  Srm,
}

impl Display for SaveType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      SaveType::Eeprom => f.write_str("EEPROM"),
      SaveType::Sram => f.write_str("SRAM"),
      SaveType::FlashRam => f.write_str("FlashRAM"),
      SaveType::ControllerPack(CPKind::Mupen) => f.write_str("Mupen Controller Pack"),
      SaveType::ControllerPack(player) => {
        f.write_fmt(format_args!("Controller Pack {}", { *player as i32 + 1 }))
      }
      SaveType::Srm => f.write_str("SRM"),
    }
  }
}

struct SavedPath(SaveType, Option<PathBuf>);
impl SavedPath {
  fn srm(path: Option<PathBuf>) -> Self {
    Self(SaveType::Srm, path)
  }
  fn sram(path: Option<PathBuf>) -> Self {
    Self(SaveType::Sram, path)
  }
  fn eep(path: Option<PathBuf>) -> Self {
    Self(SaveType::Eeprom, path)
  }
  fn fla(path: Option<PathBuf>) -> Self {
    Self(SaveType::FlashRam, path)
  }
  fn cp(kind: CPKind, path: Option<PathBuf>) -> Self {
    Self(SaveType::ControllerPack(kind), path)
  }
}

macro_rules! if_controller_pack {
  ($path:expr, $cp:block, $not_cp:block) => {
    File::open(&$path)
      .and_then(|mut f| ControllerPack::infer_from(&mut f))
      .map(|is_cp| if is_cp $cp else $not_cp)
  };
}

impl ConvertArgs {
  fn set_or_update_mode(&mut self, path: PathBuf, opts: &AddOpts) -> SavedPath {
    SavedPath::srm(
      match opts {
        AddOpts::ForcedCreate(_) => self.mode.replace(ConvertMode::Create(path)),
        AddOpts::ForcedSplit(_) => self.mode.replace(ConvertMode::Split(path)),
        AddOpts::Automatic(_) => match &mut self.mode {
          Some(ConvertMode::Create(value) | ConvertMode::Split(value)) => {
            Some(ConvertMode::Create(std::mem::replace(value, path)))
          }
          None if self.paths.is_empty() => self.mode.replace(ConvertMode::Split(path)),
          None => self.mode.replace(ConvertMode::Create(path)),
        },
      }
      .map(ConvertMode::into_path),
    )
  }

  fn add(&mut self, path: PathBuf, opts: &AddOpts) -> io::Result<SavedPath> {
    if path.exists() {
      self.add_from_data(path, opts)
    } else {
      self.add_from_ext(path, opts)
    }
  }

  fn add_from_data(&mut self, path: PathBuf, opts: &AddOpts) -> io::Result<SavedPath> {
    // we can identify the saves based on their file size & first bytes
    match path.metadata().map(|m| m.len()) {
      // 4Kbit or 16Kbit eeprom
      Ok(0x200 | 0x800) => Ok(SavedPath::eep(self.paths.eep.replace(path))),
      // 256Kbit sram or controller pack
      Ok(0x8000) => if_controller_pack!(
        path,
        { self.paths.insert_or_update_cp(opts.is_mupen(), path) },
        { SavedPath::sram(self.paths.sra.replace(path)) }
      ),
      // 1Mbit flash ram or 4 merged controller packs
      Ok(0x20000) => if_controller_pack!(path, { self.paths.insert_or_update_cp(true, path) }, {
        SavedPath::fla(self.paths.fla.replace(path))
      }),
      // uncompressed retroarch srm save size
      Ok(0x48800) => Ok(self.set_or_update_mode(path, opts)),
      // unknown
      Ok(_) => Err(io::Error::new(ErrorKind::Other, "Unknown file")),
      _ => unreachable!(),
    }
  }

  fn add_from_ext(&mut self, path: PathBuf, opts: &AddOpts) -> io::Result<SavedPath> {
    let Some(extension) = path.extension() else {
      return Err(io::Error::new(ErrorKind::Other, "File without extension"));
    };

    match extension.to_ascii_uppercase().to_str() {
      Some("SRM") => Ok(self.set_or_update_mode(path, opts)),
      Some("SRA") => Ok(SavedPath::sram(self.paths.sra.replace(path))),
      Some("FLA") => Ok(SavedPath::fla(self.paths.fla.replace(path))),
      Some("EEP") => Ok(SavedPath::eep(self.paths.eep.replace(path))),
      Some("MPK" | "MPK1" | "MPK2" | "MPK3" | "MPK4") => {
        Ok(self.paths.insert_or_update_cp(opts.is_mupen(), path))
      }
      _ => Err(io::Error::new(ErrorKind::Other, "Unknown extension")),
    }
  }
}

impl Display for ConvertArgs {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self.mode {
      Some(mode) => f.write_fmt(format_args!(
        "{mode} {} {}",
        if mode.is_create() { "from" } else { "to" },
        self.paths
      )),
      None => f.write_str("No mode"),
    }
  }
}

#[derive(Copy, Clone, Debug, PartialEq, clap::ValueEnum, Default)]
#[repr(u8)]
enum Verbosity {
  Quiet = 0,
  #[default]
  Normal = 1,
  Debug = 2,
}

#[derive(Clone, clap::Args, Debug, Default)]
struct BaseArgs {
  /// If set, the program will overwrite any existing files
  #[arg(long)]
  overwrite: bool,

  /// Is set, any FlashRAM or SRAM data will swap its endianness
  #[arg(long)]
  change_endianness: bool,

  /// If set, the 4 memory pack files will be merged into one
  #[arg(long)]
  merge_mempacks: bool,

  /// Sets the output directory for the created file (or files)
  #[arg(long)]
  output_dir: Option<PathBuf>,
}

/// A simple converter for Retroarch's Mupen64Plus core save files.
#[derive(Parser, Debug, Default)]
#[command(version)]
struct MupenSrmConvert {
  /// Sets the output verbosity
  #[arg(short, long, default_value = "normal")]
  verbosity: Verbosity,

  /// Forces the creation of a SRM file from all the given files
  #[arg(short, long)]
  create_srm: bool,

  /// Forces the split of an existing SRM to all the given files
  #[arg(short, long, conflicts_with = "create_srm")]
  split_srm: bool,

  #[command(flatten)]
  base: BaseArgs,

  /// The input file(s)
  #[arg(name = "FILE", num_args = 1.., required = true)]
  files: Vec<PathBuf>,
}

fn main() -> ExitCode {
  // get the arguments
  let mut args = MupenSrmConvert::parse();

  // init our simple logger
  logger::set_verbosity(args.verbosity);

  if args.split_srm {
    ldbgln!("MODE: Forced SRM split")
  } else if args.create_srm {
    ldbgln!("MODE: Forced SRM creation")
  } else {
    ldbgln!("MODE: Automatic")
  }

  // if there is an output directory, check and create if missing
  if let Some(out_dir) = args.base.output_dir.as_ref() {
    if !out_dir.is_dir() && !out_dir.exists() {
      match fs::create_dir_all(&out_dir) {
        Ok(()) => ldbgln!("Created output directory at: {}", out_dir.display()),
        Err(err) => {
          lerrln!("Could not create output directory: {err}");
          return ExitCode::FAILURE;
        }
      }
    }
  }

  // Get the files out from the arguments
  let files = std::mem::replace(&mut args.files, Vec::new());

  ldbgln!("\n--- Grouping file(s) ---");
  let mut groups = group_files(files, &args);

  // if auto or create, set the srm file name if missing
  if !args.split_srm {
    for (name, value) in &mut groups {
      value.mode.get_or_insert_with(|| {
        ldbgln!("> Group \"{name}\": Will Create SRM");
        ConvertMode::Create(PathBuf::from(name.as_str()).with_extension("srm"))
      });
    }
  }

  ldbgln!("\n--- Validating group(s) ---");
  let invalid_groups = validate(&groups);
  if !invalid_groups.is_empty() {
    ldbgln!("Validation failed");
    for InvalidEntry { name, reason } in invalid_groups {
      match reason {
        InvalidReason::NoMode => lerrln!("Group \"{name}\": No SRM file found"),
        InvalidReason::Create(problem) => match problem {
          Problem::NoInput => lerrln!("Group \"{name}\": Can't create SRM: no input files"),
          Problem::FileDoesNotExist(files) => {
            lerrln!("Group \"{name}\": Can't create SRM: the following file(s) do not exist");
            for f in files {
              lerrln!("  {}", f.display());
            }
          }
          Problem::NotAFile(_) => unreachable!(),
        },
        InvalidReason::Split(problem) => match problem {
          Problem::NoInput => unreachable!(),
          Problem::FileDoesNotExist(files) => {
            lerrln!(
              "Group \"{name}\": Can't split SRM: {} doesn't exist",
              files[0].display()
            )
          }
          Problem::NotAFile(file) => {
            lerrln!(
              "Group \"{name}\": Can't split SRM: {} is not a file",
              file.display()
            )
          }
        },
      }
    }

    return ExitCode::FAILURE;
  }
  ldbgln!("All groups validated");

  ldbgln!("\n--- Start Conversion ---");

  let mut exit_code = ExitCode::SUCCESS;

  // Now work per file name
  for (name, value) in groups {
    let _pad0 = linfln!(>2 "Group \"{name}\":");

    let ConvertArgs { mode, paths } = value;

    // None value modes were removed from the
    let mode = mode.unwrap(); // safe because it was validated

    let _pad1 = linf!(>2"{mode}");
    let mode_str = format!("{mode}");
    if let Err(e) = match mode {
      ConvertMode::Create(output_path) => {
        linf!(" using: ");
        if args.base.merge_mempacks {
          linfln!("{paths:#}")
        } else {
          linfln!("{paths}")
        };
        create_srm(output_path, &args.base, paths)
      }
      ConvertMode::Split(input_path) => {
        if paths.any_is_file() {
          linf!(" into: ");
          if args.base.merge_mempacks {
            linf!("{paths:#}")
          } else {
            linf!("{paths}")
          };
        }
        linfln!();
        split_srm(input_path, &args.base, paths)
      }
    } {
      lerrln!("Could not {mode_str}: {e}");
      exit_code = ExitCode::FAILURE
    }
  }

  exit_code
}

#[derive(Debug)]
struct PathError(PathBuf, io::Error);
impl Display for PathError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let PathError(path, err) = self;
    match err.kind() {
      ErrorKind::NotFound => f.write_fmt(format_args!("file {} not found", path.display())),
      ErrorKind::PermissionDenied => {
        f.write_fmt(format_args!("could not access {}", path.display()))
      }
      ErrorKind::AlreadyExists => {
        f.write_fmt(format_args!("will overwrite existing {}", path.display()))
      }
      ErrorKind::WriteZero => f.write_fmt(format_args!(
        "could not write all data into {}",
        path.display()
      )),
      ErrorKind::UnexpectedEof => f.write_fmt(format_args!(
        "unexpected end of file while reading {}",
        path.display()
      )),
      _ => f.write_fmt(format_args!("{err} with {}", path.display())),
    }
  }
}
impl std::error::Error for PathError {}
type Result = std::result::Result<(), PathError>;

#[derive(Debug, PartialEq, Eq)]
enum Problem<'k> {
  NoInput,
  FileDoesNotExist(Vec<&'k Path>),
  NotAFile(&'k Path),
}

#[derive(Debug, PartialEq, Eq)]
enum InvalidReason<'k> {
  NoMode,
  Create(Problem<'k>),
  Split(Problem<'k>),
}

struct InvalidEntry<'key> {
  name: &'key str,
  reason: InvalidReason<'key>,
}

impl<'k> InvalidEntry<'k> {
  fn new(name: &'k str, reason: InvalidReason<'k>) -> Self {
    Self { name, reason }
  }
}

fn validate<'g>(groups: &'g GroupedFiles) -> Vec<InvalidEntry<'g>> {
  use InvalidReason::*;
  use Problem::*;

  let mut invalid_groups = Vec::new();
  for (name, value) in groups {
    match &value.mode {
      Some(ConvertMode::Create(_)) => {
        if value.paths.is_empty() {
          invalid_groups.push(InvalidEntry::new(name, Create(NoInput)));
        } else if !value.paths.any_is_file() {
          invalid_groups.push({
            let files = value.paths.get_invalid_paths();
            InvalidEntry::new(name, Create(FileDoesNotExist(files)))
          });
        }
      }
      Some(ConvertMode::Split(path)) => {
        if !path.exists() {
          invalid_groups.push(InvalidEntry::new(name, Split(FileDoesNotExist(vec![path]))))
        } else if !path.is_file() {
          invalid_groups.push(InvalidEntry::new(name, Split(NotAFile(path))))
        }
      }
      None => invalid_groups.push(InvalidEntry::new(name, NoMode)),
    };
  }
  invalid_groups
}

struct GroupedFiles(Vec<(String, ConvertArgs)>);

impl IntoIterator for GroupedFiles {
  type Item = (String, ConvertArgs);

  type IntoIter = std::vec::IntoIter<(String, ConvertArgs)>;

  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<'a> IntoIterator for &'a GroupedFiles {
  type Item = &'a (String, ConvertArgs);

  type IntoIter = std::slice::Iter<'a, (String, ConvertArgs)>;

  fn into_iter(self) -> Self::IntoIter {
    self.0.iter()
  }
}

impl<'a> IntoIterator for &'a mut GroupedFiles {
  type Item = &'a mut (String, ConvertArgs);

  type IntoIter = std::slice::IterMut<'a, (String, ConvertArgs)>;

  fn into_iter(self) -> Self::IntoIter {
    self.0.iter_mut()
  }
}

fn group_files(files: Vec<PathBuf>, args: &MupenSrmConvert) -> GroupedFiles {
  let mut order = HashMap::<String, usize>::new();

  let mut values: Vec<(String, ConvertArgs)> = Vec::new();

  // collect files from arguments
  for path in files.into_iter() {
    let mut _pad = ldbgln!(>2 "File {}:", path.display());
    if path.is_dir() {
      lwarnln!("Path {} is a directory", path.display());
      continue;
    }
    ldbg!("Getting name...");
    let name = match path.file_stem().and_then(OsStr::to_str) {
      Some(name) => name,
      None => {
        lwarnln!("Path {} did not name a file", path.display());
        continue;
      }
    };
    ldbgln!(" \"{name}\"");

    let key = if !args.split_srm && !args.create_srm {
      name
    } else {
      values.first().map(|(k, _)| k.as_str()).unwrap_or(name)
    };

    let opts = if args.split_srm {
      AddOpts::ForcedSplit(args.base.merge_mempacks)
    } else if args.create_srm {
      AddOpts::ForcedCreate(args.base.merge_mempacks)
    } else {
      AddOpts::Automatic(args.base.merge_mempacks)
    };

    let convert_args = match order.get(key) {
      Some(&index) => &mut values.get_mut(index).unwrap().1,
      None => {
        ldbg!("  ");
        linfln!("Created group \"{key}\"");
        let key = key.to_string();
        let mut convert_args = ConvertArgs::default();
        if args.create_srm {
          if let Err(err) = convert_args.add(PathBuf::from(format!("{key}.srm")), &opts) {
            lwarnln!("{err}");
            continue;
          }
        }
        order.insert(key.clone(), values.len());
        values.push((key.clone(), convert_args));
        &mut values.last_mut().unwrap().1
      }
    };

    if _pad == 0 {
      _pad = linf!(>2"");
    }

    match convert_args.add(path.clone(), &opts) {
      Ok(SavedPath(save_type, Some(old_path))) => {
        linfln!(
          "{save_type}: replaced {} with {}",
          old_path.display(),
          path.display()
        )
      }
      Ok(SavedPath(save_type, None)) => ldbgln!("{save_type}: added"),
      Err(err) => lwarnln!("{err}"),
    }
  }

  GroupedFiles(values)
}

#[cfg(test)]
mod tests {
  use std::path::Path;

  use crate::{
    group_files, validate, AddOpts, CPKind, ConvertArgs, ConvertMode, GroupedFiles, InvalidReason,
    MupenSrmConvert, Problem, SaveType, SrmPaths,
  };

  type SaveFlag = u8;
  const EEP: SaveFlag = 0x1;
  const FLA: SaveFlag = 0x2;
  const SRA: SaveFlag = 0x4;
  const CP1: SaveFlag = 0x08;
  const CP2: SaveFlag = 0x10;
  const CP3: SaveFlag = 0x20;
  const CP4: SaveFlag = 0x40;
  const CP: SaveFlag = CP1 | CP2 | CP3 | CP4;
  const ALL: SaveFlag = EEP | FLA | SRA | CP;

  fn test_for_none(paths: &SrmPaths, save_flags: SaveFlag) {
    if save_flags & CP1 == CP1 {
      assert_eq!(paths.cp[0], None);
    }
    if save_flags & CP2 == CP2 {
      assert_eq!(paths.cp[1], None);
    }
    if save_flags & CP3 == CP3 {
      assert_eq!(paths.cp[2], None);
    }
    if save_flags & CP4 == CP4 {
      assert_eq!(paths.cp[3], None);
    }
    if save_flags & EEP == EEP {
      assert_eq!(paths.eep, None);
    }
    if save_flags & SRA == SRA {
      assert_eq!(paths.sra, None);
    }
    if save_flags & FLA == FLA {
      assert_eq!(paths.fla, None);
    }
  }

  impl GroupedFiles {
    fn names(&self) -> Vec<&String> {
      self.0.iter().map(|(k, _)| k).collect::<Vec<_>>()
    }
  }

  #[test]
  fn verify_cli() {
    use clap::CommandFactory;
    MupenSrmConvert::command().debug_assert()
  }

  #[test]
  fn verify_automatic_grouping() {
    let args = MupenSrmConvert::default();
    let files = vec![
      "A.srm".into(),
      "B.srm".into(),
      "C.srm".into(),
      "B1.eep".into(),
      "B.mpk1".into(),
      "D.fla".into(),
      "D.srm".into(),
      "folder/D.mpk".into(),
    ];

    assert!(!args.create_srm);
    assert!(!args.split_srm);

    let groups = group_files(files, &args);

    assert_eq!(groups.names(), vec!["A", "B", "C", "B1", "D"]);

    for (key, value) in groups {
      match key.as_str() {
        "A" | "C" => {
          // simple auto-name split
          assert_eq!(
            value.mode,
            Some(ConvertMode::Split(format!("{key}.srm").into()))
          );
          assert!(value.paths.is_empty());
        }
        "B" => {
          // split to named mpk
          assert_eq!(value.mode, Some(ConvertMode::Split("B.srm".into())));
          assert!(!value.paths.is_empty());
          assert_eq!(value.paths.cp[0], Some("B.mpk1".into()));
          test_for_none(&value.paths, ALL & !CP1);
        }
        "B1" => {
          // create from B1.eep, no srm given
          assert_eq!(value.mode, None);
          assert!(!value.paths.is_empty());
          assert_eq!(value.paths.eep, Some("B1.eep".into()));
          test_for_none(&value.paths, ALL & !EEP);
        }
        "D" => {
          // create from D.fla & folder/D.mpk, to D.srm
          assert_eq!(value.mode, Some(ConvertMode::Create("D.srm".into())));
          assert!(!value.paths.is_empty());
          assert_eq!(value.paths.cp[0], Some("folder/D.mpk".into()));
          assert_eq!(value.paths.fla, Some("D.fla".into()));
          test_for_none(&value.paths, ALL & !CP1 & !FLA);
        }
        _ => assert!(false, "unreachable"),
      }
    }
  }

  #[test]
  fn verify_automatic_grouping_replacement() {
    let args = MupenSrmConvert::default();
    let files = vec![
      "A.srm".into(),
      "folder/A.srm".into(),
      "folder2/A.srm".into(),
    ];
    let groups = group_files(files, &args);

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "A");
    assert_eq!(value.mode, Some(ConvertMode::Split("folder2/A.srm".into())));
    assert!(value.paths.is_empty());
  }

  #[test]
  fn verify_create_grouping() {
    let args = MupenSrmConvert {
      create_srm: true,
      ..Default::default()
    };
    let files = vec!["Space.mpk".into(), "folder/extracted.sra".into()];

    let groups = group_files(files, &args);

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "Space");
    assert_eq!(value.mode, Some(ConvertMode::Create("Space.srm".into())));
    assert!(!value.paths.is_empty());
    assert_eq!(value.paths.cp[0], Some("Space.mpk".into()));
    assert_eq!(value.paths.sra, Some("folder/extracted.sra".into()));
    test_for_none(&value.paths, ALL & !CP1 & !SRA);
  }

  #[test]
  fn verify_create_grouping_replacement() {
    let args = MupenSrmConvert {
      create_srm: true,
      ..Default::default()
    };
    let files = vec![
      "initial.mpk".into(),
      "folder/extracted.sra".into(),
      "last.mpk".into(),
      "real.sra".into(),
      "actual.srm".into(),
    ];

    let groups = group_files(files, &args);

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "initial");
    assert_eq!(value.mode, Some(ConvertMode::Create("actual.srm".into())));
    assert!(!value.paths.is_empty());
    assert_eq!(value.paths.sra, Some("real.sra".into()));
    assert_eq!(value.paths.cp[0], Some("last.mpk".into()));
    test_for_none(&value.paths, ALL & !CP1 & !SRA);
  }

  #[test]
  fn verify_split_grouping() {
    let args = MupenSrmConvert {
      split_srm: true,
      ..Default::default()
    };
    let files = vec!["Space.srm".into(), "folder/extracted.sra".into()];

    let groups = group_files(files, &args);

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "Space");
    assert_eq!(value.mode, Some(ConvertMode::Split("Space.srm".into())));
    assert!(!value.paths.is_empty());
    assert_eq!(value.paths.sra, Some("folder/extracted.sra".into()));
    test_for_none(&value.paths, ALL & !SRA);
  }

  #[test]
  fn verify_split_grouping_replacement() {
    let args = MupenSrmConvert {
      split_srm: true,
      ..Default::default()
    };
    let files = vec![
      "this_not_it.srm".into(),
      "initial.mpk".into(),
      "folder/extracted.sra".into(),
      "last.mpk".into(),
      "real.sra".into(),
      "actual.srm".into(),
    ];

    let groups = group_files(files, &args);

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "this_not_it");
    assert_eq!(value.mode, Some(ConvertMode::Split("actual.srm".into())));
    assert!(!value.paths.is_empty());
    assert_eq!(value.paths.sra, Some("real.sra".into()));
    assert_eq!(value.paths.cp[0], Some("last.mpk".into()));
    test_for_none(&value.paths, ALL & !CP1 & !SRA);
  }

  #[test]
  fn verify_convert_args() -> std::io::Result<()> {
    let mut args = ConvertArgs::default();

    assert_eq!(args.mode, None);
    assert!(args.paths.is_empty());

    // test first srm add
    let save_path = args.add("A.srm".into(), &AddOpts::Automatic(false))?;
    assert_eq!(save_path.0, SaveType::Srm);
    assert_eq!(save_path.1, None);

    assert_eq!(args.mode, Some(ConvertMode::Split("A.srm".into())));
    assert!(args.paths.is_empty());

    // test replace srm
    let save_path = args.add("B.srm".into(), &AddOpts::Automatic(false))?;
    assert_eq!(save_path.0, SaveType::Srm);
    assert_eq!(save_path.1, Some("A.srm".into()));

    assert_eq!(args.mode, Some(ConvertMode::Split("B.srm".into())));
    assert!(args.paths.is_empty());

    // test non-srm on empty
    std::mem::take(&mut args);
    assert_eq!(args.mode, None);
    assert!(args.paths.is_empty());

    let save_path = args.add("A.mpk".into(), &AddOpts::Automatic(false))?;
    assert_eq!(save_path.0, SaveType::ControllerPack(CPKind::Player1));
    assert_eq!(save_path.1, None);

    assert_eq!(args.mode, None);
    assert!(!args.paths.is_empty());
    test_for_none(&args.paths, ALL & !CP1);

    // test replace mkp1
    let save_path = args.add("B.mpk1".into(), &AddOpts::Automatic(false))?;
    assert_eq!(save_path.0, SaveType::ControllerPack(CPKind::Player1));
    assert_eq!(save_path.1, Some("A.mpk".into()));

    assert_eq!(args.mode, None);
    assert!(!args.paths.is_empty());
    test_for_none(&args.paths, ALL & !CP1);

    // test add mpk3
    let save_path = args.add("X.mpk3".into(), &AddOpts::Automatic(false))?;
    assert_eq!(save_path.0, SaveType::ControllerPack(CPKind::Player3));
    assert_eq!(save_path.1, None);

    assert_eq!(args.mode, None);
    assert!(!args.paths.is_empty());
    test_for_none(&args.paths, ALL & !CP1 & !CP3);

    // test add srm after file
    let save_path = args.add("A.srm".into(), &AddOpts::Automatic(false))?;
    assert_eq!(save_path.0, SaveType::Srm);
    assert_eq!(save_path.1, None);

    assert_eq!(args.mode, Some(ConvertMode::Create("A.srm".into())));
    assert!(!args.paths.is_empty());
    test_for_none(&args.paths, ALL & !CP1 & !CP3);

    // test replace with mupen mpk
    let save_path = args.add("M.mpk4".into(), &AddOpts::Automatic(true))?;
    assert_eq!(save_path.0, SaveType::ControllerPack(CPKind::Mupen));
    assert_eq!(save_path.1, Some("B.mpk1".into()));

    assert_eq!(args.mode, Some(ConvertMode::Create("A.srm".into())));
    assert!(!args.paths.is_empty());
    test_for_none(&args.paths, ALL & !CP1);

    // reset
    std::mem::take(&mut args);
    assert_eq!(args.mode, None);

    // test forced create/split replacement
    let save_path = args.add("A.srm".into(), &AddOpts::ForcedCreate(false))?;
    assert_eq!(save_path.0, SaveType::Srm);
    assert_eq!(save_path.1, None);

    assert_eq!(args.mode, Some(ConvertMode::Create("A.srm".into())));
    assert!(args.paths.is_empty());

    let save_path = args.add("B.srm".into(), &AddOpts::ForcedSplit(false))?;
    assert_eq!(save_path.0, SaveType::Srm);
    assert_eq!(save_path.1, Some("A.srm".into()));

    assert_eq!(args.mode, Some(ConvertMode::Split("B.srm".into())));
    assert!(args.paths.is_empty());

    let save_path = args.add("A.srm".into(), &AddOpts::ForcedCreate(false))?;
    assert_eq!(save_path.0, SaveType::Srm);
    assert_eq!(save_path.1, Some("B.srm".into()));

    assert_eq!(args.mode, Some(ConvertMode::Create("A.srm".into())));
    assert!(args.paths.is_empty());

    // An auto should only change the path
    let save_path = args.add("B.srm".into(), &AddOpts::Automatic(false))?;
    assert_eq!(save_path.0, SaveType::Srm);
    assert_eq!(save_path.1, Some("A.srm".into()));

    assert_eq!(args.mode, Some(ConvertMode::Create("B.srm".into())));
    assert!(args.paths.is_empty());

    Ok(())
  }

  #[test]
  fn verify_validator() -> std::io::Result<()> {
    let mut groups = GroupedFiles(vec![("A".into(), ConvertArgs::default())]);

    let invalids = validate(&groups);

    assert!(!invalids.is_empty());
    assert_eq!(invalids[0].name, "A");
    assert_eq!(invalids[0].reason, InvalidReason::NoMode);

    groups.0[0]
      .1
      .add("A.srm".into(), &AddOpts::Automatic(false))?;

    let invalids = validate(&groups);
    assert!(!invalids.is_empty());
    assert_eq!(invalids[0].name, "A");
    assert_eq!(
      invalids[0].reason,
      InvalidReason::Split(Problem::FileDoesNotExist(vec![&Path::new("A.srm")]))
    );
    std::mem::take(&mut groups.0[0].1);

    groups.0[0]
      .1
      .add("A.mpk".into(), &AddOpts::Automatic(false))?;
    let invalids = validate(&groups);
    assert!(!invalids.is_empty());
    assert_eq!(invalids[0].name, "A");
    assert_eq!(invalids[0].reason, InvalidReason::NoMode);

    Ok(())
  }
}
