//! # RetroArch Mupen64Plus SRM Converter Lib
//!
//! `ramp64-srm-convert-lib` is the library portion of `ra_mp64_srm_convert` in an attempt to
//! make GUIs out of the same code.

#![deny(missing_docs)]

mod controller_pack;
mod create_srm;
mod game_pack;
mod grouping;
mod retroarch_srm;
mod split_srm;

use std::{
  ffi, fmt, fs, io,
  path::{Path, PathBuf},
};

pub use grouping::{group_saves, validate_groups, InvalidGroup, Problem};

use log::info;

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

/// Provides the path of the error
#[derive(Debug)]
pub struct PathError(PathBuf, io::Error);

impl fmt::Display for PathError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let PathError(path, err) = self;
    match err.kind() {
      io::ErrorKind::NotFound => f.write_fmt(format_args!("file {} not found", path.display())),
      io::ErrorKind::PermissionDenied => {
        f.write_fmt(format_args!("could not access {}", path.display()))
      }
      io::ErrorKind::AlreadyExists => {
        f.write_fmt(format_args!("will overwrite existing {}", path.display()))
      }
      io::ErrorKind::WriteZero => f.write_fmt(format_args!(
        "could not write all data into {}",
        path.display()
      )),
      io::ErrorKind::UnexpectedEof => f.write_fmt(format_args!(
        "unexpected end of file while reading {}",
        path.display()
      )),
      _ => f.write_fmt(format_args!("{err} with {}", path.display())),
    }
  }
}

impl std::error::Error for PathError {}
type Result = std::result::Result<(), PathError>;

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
    let file_name = input.file_name().unwrap_or(&ffi::OsStr::new(""));
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
    S: AsRef<ffi::OsStr> + ?Sized,
  {
    self.output_path(self.base.with_extension(ext).file_name().unwrap())
  }
}

/// The mode to use to convert the SRM file
#[derive(Debug, PartialEq)]
pub enum ConvertMode {
  /// Specify this variant to create an SRM file
  Create(PathBuf),
  /// Specify this variant to split an SRM file
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

impl fmt::Display for ConvertMode {
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
      SavedPath::cp(ControllerPackKind::Mupen, self.cp[0].replace(path))
    } else {
      let i = pack_number(&path).map_or(0, |i| i);
      SavedPath::cp((i + 1).into(), self.cp[i].replace(path))
    }
  }

  pub fn is_empty(&self) -> bool {
    self.eep.is_none()
      && self.fla.is_none()
      && self.sra.is_none()
      && self.cp.iter().all(Option::is_none)
  }

  pub fn any_is_file(&self) -> bool {
    self.eep.as_ref().map_or(false, |p| p.is_file())
      || self.fla.as_ref().map_or(false, |p| p.is_file())
      || self.sra.as_ref().map_or(false, |p| p.is_file())
      || self.cp[0].as_ref().map_or(false, |p| p.is_file())
      || self.cp[1].as_ref().map_or(false, |p| p.is_file())
      || self.cp[2].as_ref().map_or(false, |p| p.is_file())
      || self.cp[3].as_ref().map_or(false, |p| p.is_file())
  }

  pub fn get_invalid_paths(&self) -> Vec<&Path> {
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

impl fmt::Display for SrmPaths {
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

/// Provides the grouping options
#[derive(Debug, Clone, Copy)]
pub enum GroupOpts {
  /// Used to force creation with all files
  ForceCreate(bool),
  /// Used to force split into all files
  ForceSplit(bool),
  /// Used to automatically group based on file type
  Automatic(bool),
}

impl GroupOpts {
  fn is_mupen(&self) -> bool {
    match self {
      Self::ForceCreate(m) | Self::ForceSplit(m) | Self::Automatic(m) => *m,
    }
  }
  fn is_automatic(&self) -> bool {
    match self {
      GroupOpts::Automatic(_) => true,
      _ => false,
    }
  }
  fn is_create(&self) -> bool {
    match self {
      Self::ForceCreate(_) => true,
      _ => false,
    }
  }
}

/// Provides the parameter to apply a conversion
#[derive(Debug, Default)]
pub struct ConvertParams {
  mode: Option<ConvertMode>,
  paths: SrmPaths,
}

/// Represents the kind of controller pack in use
#[repr(i32)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ControllerPackKind {
  /// The controller pack attached to the first player
  Player1,
  /// The controller pack attached to the second player
  Player2,
  /// The controller pack attached to the third player
  Player3,
  /// The controller pack attached to the fourth player
  Player4,
  /// The merged controller pack used by Mupen64 input
  Mupen,
}

impl From<usize> for ControllerPackKind {
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

/// The save types handled by the program
#[derive(Debug, PartialEq, Eq)]
pub enum SaveType {
  /// EEPROM save (4kbit or 16kbit)
  Eeprom,
  /// SRAM (256kbit)
  Sram,
  /// FlashRAM (1Mbit)
  FlashRam,
  /// Controller/Memory Pack (256kbit)
  ControllerPack(ControllerPackKind),
  /// RetroArch Mupen64Plus Save
  Srm,
}

impl fmt::Display for SaveType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      SaveType::Eeprom => f.write_str("EEPROM"),
      SaveType::Sram => f.write_str("SRAM"),
      SaveType::FlashRam => f.write_str("FlashRAM"),
      SaveType::ControllerPack(ControllerPackKind::Mupen) => f.write_str("Mupen Controller Pack"),
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
  fn cp(kind: ControllerPackKind, path: Option<PathBuf>) -> Self {
    Self(SaveType::ControllerPack(kind), path)
  }
}

macro_rules! if_controller_pack {
  ($path:expr, $cp:block, $not_cp:block) => {
    fs::File::open(&$path)
      .and_then(|mut f| controller_pack::ControllerPack::infer_from(&mut f))
      .map(|is_cp| if is_cp $cp else $not_cp)
  };
}

impl ConvertParams {
  fn set_or_update_mode(&mut self, path: PathBuf, opts: GroupOpts) -> SavedPath {
    SavedPath::srm(
      match opts {
        GroupOpts::ForceCreate(_) => self.mode.replace(ConvertMode::Create(path)),
        GroupOpts::ForceSplit(_) => self.mode.replace(ConvertMode::Split(path)),
        GroupOpts::Automatic(_) => match &mut self.mode {
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

  /// Use this function to get or set the [ConvertMode] for this [ConvertParams]
  pub fn get_or_set_mode<F>(&mut self, f: F) -> &mut ConvertMode
  where
    F: FnOnce() -> ConvertMode,
  {
    self.mode.get_or_insert_with(f)
  }

  fn add(&mut self, path: PathBuf, opts: GroupOpts) -> io::Result<SavedPath> {
    if path.exists() {
      self.add_from_data(path, opts)
    } else {
      self.add_from_ext(path, opts)
    }
  }

  fn add_from_data(&mut self, path: PathBuf, opts: GroupOpts) -> io::Result<SavedPath> {
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
      Ok(_) => Err(io::Error::new(io::ErrorKind::Other, "Unknown file")),
      _ => unreachable!(),
    }
  }

  fn add_from_ext(&mut self, path: PathBuf, opts: GroupOpts) -> io::Result<SavedPath> {
    let Some(extension) = path.extension() else {
      return Err(io::Error::new(io::ErrorKind::Other, "File without extension"));
    };

    match extension.to_ascii_uppercase().to_str() {
      Some("SRM") => Ok(self.set_or_update_mode(path, opts)),
      Some("SRA") => Ok(SavedPath::sram(self.paths.sra.replace(path))),
      Some("FLA") => Ok(SavedPath::fla(self.paths.fla.replace(path))),
      Some("EEP") => Ok(SavedPath::eep(self.paths.eep.replace(path))),
      Some("MPK" | "MPK1" | "MPK2" | "MPK3" | "MPK4") => {
        Ok(self.paths.insert_or_update_cp(opts.is_mupen(), path))
      }
      _ => Err(io::Error::new(io::ErrorKind::Other, "Unknown extension")),
    }
  }
}

impl fmt::Display for ConvertParams {
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

/// Basic common arguments for the conversion
#[derive(Clone, clap::Args, Debug, Default)]
pub struct BaseArgs {
  /// If set, the program will overwrite any existing files
  #[arg(long)]
  pub overwrite: bool,

  /// Is set, any FlashRAM or SRAM data will swap its endianness
  #[arg(long)]
  pub change_endianness: bool,

  /// If set, the 4 memory pack files will be merged into one
  #[arg(long)]
  pub merge_mempacks: bool,

  /// Sets the output directory for the created file (or files)
  #[arg(long)]
  pub output_dir: Option<PathBuf>,
}

/// Proceeds to convert based on the parameters
pub fn convert(params: ConvertParams, args: &BaseArgs) -> Result {
  let ConvertParams { mode, paths } = params;

  let mode = mode.expect("the mode should have been validated");
  let mode_str = format!("{mode}");
  match mode {
    ConvertMode::Create(output_path) => {
      if args.merge_mempacks {
        info!("{mode_str} using {paths:#}");
      } else {
        info!("{mode_str} using {paths}");
      }
      create_srm::create_srm(output_path, args, paths)
    }
    ConvertMode::Split(input_path) => {
      if paths.any_is_file() {
        if args.merge_mempacks {
          info!("{mode_str} into: {paths:#}")
        } else {
          info!("{mode_str} into: {paths}")
        };
      }
      split_srm::split_srm(input_path, args, paths)
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::{ControllerPackKind, ConvertMode, ConvertParams, GroupOpts, SaveType, SrmPaths};

  pub(crate) type SaveFlag = u8;
  pub(crate) trait SaveFlagExt {
    const EEP: SaveFlag = 0x1;
    const FLA: SaveFlag = 0x2;
    const SRA: SaveFlag = 0x4;
    const CP1: SaveFlag = 0x08;
    const CP2: SaveFlag = 0x10;
    const CP3: SaveFlag = 0x20;
    const CP4: SaveFlag = 0x40;
    const CP: SaveFlag = Self::CP1 | Self::CP2 | Self::CP3 | Self::CP4;
    const ALL: SaveFlag = Self::EEP | Self::FLA | Self::SRA | Self::CP;
  }
  impl SaveFlagExt for SaveFlag {}

  pub(crate) fn test_for_none(paths: &SrmPaths, save_flags: SaveFlag) {
    if save_flags & SaveFlag::CP1 == SaveFlag::CP1 {
      assert_eq!(paths.cp[0], None);
    }
    if save_flags & SaveFlag::CP2 == SaveFlag::CP2 {
      assert_eq!(paths.cp[1], None);
    }
    if save_flags & SaveFlag::CP3 == SaveFlag::CP3 {
      assert_eq!(paths.cp[2], None);
    }
    if save_flags & SaveFlag::CP4 == SaveFlag::CP4 {
      assert_eq!(paths.cp[3], None);
    }
    if save_flags & SaveFlag::EEP == SaveFlag::EEP {
      assert_eq!(paths.eep, None);
    }
    if save_flags & SaveFlag::SRA == SaveFlag::SRA {
      assert_eq!(paths.sra, None);
    }
    if save_flags & SaveFlag::FLA == SaveFlag::FLA {
      assert_eq!(paths.fla, None);
    }
  }

  #[test]
  fn verify_convert_args() -> std::io::Result<()> {
    let mut args = ConvertParams::default();

    assert_eq!(args.mode, None);
    assert!(args.paths.is_empty());

    // test first srm add
    let save_path = args.add("A.srm".into(), GroupOpts::Automatic(false))?;
    assert_eq!(save_path.0, SaveType::Srm);
    assert_eq!(save_path.1, None);

    assert_eq!(args.mode, Some(ConvertMode::Split("A.srm".into())));
    assert!(args.paths.is_empty());

    // test replace srm
    let save_path = args.add("B.srm".into(), GroupOpts::Automatic(false))?;
    assert_eq!(save_path.0, SaveType::Srm);
    assert_eq!(save_path.1, Some("A.srm".into()));

    assert_eq!(args.mode, Some(ConvertMode::Split("B.srm".into())));
    assert!(args.paths.is_empty());

    // test non-srm on empty
    std::mem::take(&mut args);
    assert_eq!(args.mode, None);
    assert!(args.paths.is_empty());

    let save_path = args.add("A.mpk".into(), GroupOpts::Automatic(false))?;
    assert_eq!(
      save_path.0,
      SaveType::ControllerPack(ControllerPackKind::Player1)
    );
    assert_eq!(save_path.1, None);

    assert_eq!(args.mode, None);
    assert!(!args.paths.is_empty());
    test_for_none(&args.paths, SaveFlag::ALL & !SaveFlag::CP1);

    // test replace mkp1
    let save_path = args.add("B.mpk1".into(), GroupOpts::Automatic(false))?;
    assert_eq!(
      save_path.0,
      SaveType::ControllerPack(ControllerPackKind::Player1)
    );
    assert_eq!(save_path.1, Some("A.mpk".into()));

    assert_eq!(args.mode, None);
    assert!(!args.paths.is_empty());
    test_for_none(&args.paths, SaveFlag::ALL & !SaveFlag::CP1);

    // test add mpk3
    let save_path = args.add("X.mpk3".into(), GroupOpts::Automatic(false))?;
    assert_eq!(
      save_path.0,
      SaveType::ControllerPack(ControllerPackKind::Player3)
    );
    assert_eq!(save_path.1, None);

    assert_eq!(args.mode, None);
    assert!(!args.paths.is_empty());
    test_for_none(&args.paths, SaveFlag::ALL & !SaveFlag::CP1 & !SaveFlag::CP3);

    // test add srm after file
    let save_path = args.add("A.srm".into(), GroupOpts::Automatic(false))?;
    assert_eq!(save_path.0, SaveType::Srm);
    assert_eq!(save_path.1, None);

    assert_eq!(args.mode, Some(ConvertMode::Create("A.srm".into())));
    assert!(!args.paths.is_empty());
    test_for_none(&args.paths, SaveFlag::ALL & !SaveFlag::CP1 & !SaveFlag::CP3);

    // test replace with mupen mpk
    let save_path = args.add("M.mpk4".into(), GroupOpts::Automatic(true))?;
    assert_eq!(
      save_path.0,
      SaveType::ControllerPack(ControllerPackKind::Mupen)
    );
    assert_eq!(save_path.1, Some("B.mpk1".into()));

    assert_eq!(args.mode, Some(ConvertMode::Create("A.srm".into())));
    assert!(!args.paths.is_empty());
    test_for_none(&args.paths, SaveFlag::ALL & !SaveFlag::CP1);

    // reset
    std::mem::take(&mut args);
    assert_eq!(args.mode, None);

    // test forced create/split replacement
    let save_path = args.add("A.srm".into(), GroupOpts::ForceCreate(false))?;
    assert_eq!(save_path.0, SaveType::Srm);
    assert_eq!(save_path.1, None);

    assert_eq!(args.mode, Some(ConvertMode::Create("A.srm".into())));
    assert!(args.paths.is_empty());

    let save_path = args.add("B.srm".into(), GroupOpts::ForceSplit(false))?;
    assert_eq!(save_path.0, SaveType::Srm);
    assert_eq!(save_path.1, Some("A.srm".into()));

    assert_eq!(args.mode, Some(ConvertMode::Split("B.srm".into())));
    assert!(args.paths.is_empty());

    let save_path = args.add("A.srm".into(), GroupOpts::ForceCreate(false))?;
    assert_eq!(save_path.0, SaveType::Srm);
    assert_eq!(save_path.1, Some("B.srm".into()));

    assert_eq!(args.mode, Some(ConvertMode::Create("A.srm".into())));
    assert!(args.paths.is_empty());

    // An auto should only change the path
    let save_path = args.add("B.srm".into(), GroupOpts::Automatic(false))?;
    assert_eq!(save_path.0, SaveType::Srm);
    assert_eq!(save_path.1, Some("A.srm".into()));

    assert_eq!(args.mode, Some(ConvertMode::Create("B.srm".into())));
    assert!(args.paths.is_empty());

    Ok(())
  }
}
