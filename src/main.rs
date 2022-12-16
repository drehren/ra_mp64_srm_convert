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

#[derive(PartialEq)]
enum ConvertMode {
  Create(PathBuf),
  Split(PathBuf),
}

impl ConvertMode {
  fn get_path(&self) -> &PathBuf {
    match self {
      ConvertMode::Create(p) | ConvertMode::Split(p) => p,
    }
  }

  fn is_create(&self) -> bool {
    match self {
      ConvertMode::Create(_) => true,
      _ => false,
    }
  }

  fn display(&self) -> &Self {
    self
  }
}

impl Debug for ConvertMode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if f.alternate() {
      return f.write_fmt(format_args!("{:#?}", self.get_path()));
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
      ConvertMode::Create(path) if !path.exists() => f.write_fmt(format_args!("create {path:#?}")),
      ConvertMode::Create(path) => f.write_fmt(format_args!("update {path:#?}")),
      ConvertMode::Split(path) => f.write_fmt(format_args!("split {path:#?}")),
    }
  }
}

macro_rules! display_some_path {
  ($c:expr, $f:expr, $name:expr, $path:expr) => {
    if let Some(path) = $path {
      if $c > 0 {
        $f.write_str(" and ")?;
      }
      $f.write_fmt(format_args!("{} ({path:#?})", $name))?;
      $c += 1;
    }
  };
}

macro_rules! log_if_replaced {
  ($test:expr, $new:expr) => {{
    if let Some(old) = $test.replace($new) {
      linfln!(
        "Replaced {old:#?} with {:#?}",
        $test.as_ref().unwrap().display() // valid because of replace
      );
    } else {
      ldbgln!()
    }
  }};
}

#[derive(Debug, Default)]
struct SrmPaths {
  eep: Option<PathBuf>,
  sra: Option<PathBuf>,
  fla: Option<PathBuf>,
  cp: [Option<PathBuf>; 4],
}

impl SrmPaths {
  fn insert_cp(&mut self, merged: bool, path: PathBuf) {
    if let Some(i) = match () {
      _ if merged => Some(0usize),
      _ => pack_number(&path).or_else(|| self.cp.iter().position(Option::is_none)),
    } {
      ldbg!("Controller Pack {} ", i + 1);
      log_if_replaced!(self.cp[i], path);
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
}

impl Display for SrmPaths {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut counter = 0;
    display_some_path!(counter, f, "EEPROM", &self.eep);
    display_some_path!(counter, f, "SRAM", &self.sra);
    display_some_path!(counter, f, "FlashRAM", &self.fla);
    for i in 1..=4 {
      display_some_path!(counter, f, format!("Controller Pack {i}"), &self.cp[i - 1]);
    }
    Ok(())
  }
}

macro_rules! if_controller_pack {
  ($path:expr, $cp:block, $not_cp:block) => {
    match ControllerPack::infer_from(&mut File::open(&$path)?) {
      Ok(is_cp) => Ok(if is_cp $cp else $not_cp),
      Err(e) => Err(e),
    }
  };
}

enum AddOpts {
  ForcedCreate(bool),
  ForcedSplit(bool),
  Automatic(bool),
}

impl AddOpts {
  fn merged(&self) -> bool {
    match self {
      AddOpts::ForcedCreate(merged) => *merged,
      AddOpts::ForcedSplit(merged) => *merged,
      AddOpts::Automatic(merged) => *merged,
    }
  }
}

#[derive(Debug, Default)]
struct ConvertArgs {
  mode: Option<ConvertMode>,
  paths: SrmPaths,
}

impl ConvertArgs {
  fn check_mode(&mut self, path: PathBuf, opts: AddOpts) {
    match opts {
      AddOpts::ForcedCreate(_) => {
        log_if_replaced!(self.mode, ConvertMode::Create(path))
      }
      AddOpts::ForcedSplit(_) => {
        log_if_replaced!(self.mode, ConvertMode::Split(path))
      }
      AddOpts::Automatic(_) => match self.mode {
        None if self.paths.is_empty() => {
          self.mode = Some(ConvertMode::Split(path));
          ldbgln!("Will be split");
        }
        None => {
          self.mode = Some(ConvertMode::Create(path));
          ldbgln!("Will be created/updated");
        }
        _ => ldbgln!("Will be skipped"),
      },
    }
  }

  fn add(&mut self, path: PathBuf, opts: AddOpts) -> io::Result<()> {
    let r = if path.exists() {
      self.add_from_data(path, opts)
    } else {
      self.add_from_ext(path, opts)
    };
    r
  }

  fn add_from_data(&mut self, path: PathBuf, opts: AddOpts) -> io::Result<()> {
    // we can identify the saves based on their file size & first bytes
    ldbgln!("Save type (from file size & data): ");
    match path.metadata().map(|m| m.len()) {
      // 4Kbit or 16Kbit eeprom
      Ok(0x200 | 0x800) => Ok({
        ldbg!("EEPROM ");
        log_if_replaced!(self.paths.eep, path);
      }),
      // 256Kbit sram or controller pack
      Ok(0x8000) => if_controller_pack!(
        path,
        {
          ldbg!("MemPack ");
          self.paths.insert_cp(opts.merged(), path)
        },
        {
          ldbg!("SRAM ");
          log_if_replaced!(self.paths.sra, path);
        }
      ),
      // 1Mbit flash ram or 4 merged controller packs
      Ok(0x20000) => if_controller_pack!(
        path,
        {
          ldbg!("MemPack (Mupen64) ");
          self.paths.insert_cp(opts.merged(), path)
        },
        {
          ldbg!("FlashRAM ");
          log_if_replaced!(self.paths.fla, path);
        }
      ),
      // retroarch srm save size
      Ok(0x48800) => Ok({
        ldbg!("SRM (RA Mupen64Plus-Next) ");
        self.check_mode(path, opts);
      }),
      Ok(_) => Err(io::Error::new(ErrorKind::Other, "Unknown file")),
      _ => unreachable!(),
    }
  }

  fn add_from_ext(&mut self, path: PathBuf, opts: AddOpts) -> io::Result<()> {
    ldbg!("Save type (from extension): ");
    match path
      .extension()
      .map(OsStr::to_ascii_uppercase)
      .as_ref()
      .and_then(|s| s.to_str())
    {
      Some("SRM") => Ok({
        ldbg!("SRM ");
        self.check_mode(path, opts);
      }),
      Some("SRA") => Ok({
        ldbg!("SRAM ");
        log_if_replaced!(self.paths.sra, path);
      }),
      Some("FLA") => Ok({
        ldbg!("FlashRAM ");
        log_if_replaced!(self.paths.fla, path);
      }),
      Some("EEP") => Ok({
        ldbg!("EEPROM ");
        log_if_replaced!(self.paths.eep, path);
      }),
      Some("MPK" | "MPK1" | "MPK2" | "MPK3" | "MPK4") => {
        ldbg!("MemPack ");
        Ok(self.paths.insert_cp(opts.merged(), path))
      }
      None => Err(io::Error::new(ErrorKind::Other, "File without extension")),
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

#[derive(Copy, Clone, Debug, PartialEq, clap::ValueEnum)]
#[repr(u8)]
enum Verbosity {
  Quiet = 0,
  Normal = 1,
  Debug = 2,
}

#[derive(Clone, clap::Args, Debug)]
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
#[derive(Parser, Debug)]
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
        Ok(()) => ldbgln!("Created output directory at: {:#?}", out_dir.display()),
        Err(err) => {
          lerrln!("Could not create output directory: {err}");
          return ExitCode::FAILURE;
        }
      }
    }
  }

  let (mut groups, order) = group_files(&mut args);

  if !validate(order, &mut groups, &args) {
    return ExitCode::FAILURE;
  }
  ldbgln!("\n--- Start Conversion ---");

  // Now work per file name
  for (name, value) in groups {
    let _pad0 = linfln!(>2 "Group \"{name}\":");

    let ConvertArgs { mode, paths } = value;

    // None value modes were removed from the
    let mode = mode.unwrap(); // safe because it was validated

    let _pad1 = linfln!(>2"{mode}, using: {paths}");
    if let Err(e) = match mode {
      ConvertMode::Create(output_path) => {
        let mut args = args.base.clone();
        if let Some(output_dir) = args.output_dir.as_mut() {
          // is safe to unwrap because the path has been validated
          output_dir.push(output_path.file_name().unwrap());
        } else {
          args.output_dir = Some(output_path)
        }
        create_srm(args, paths)
      }
      ConvertMode::Split(input_path) => split_srm(input_path, &args.base, paths),
    } {
      lerrln!("{e}");
    }
  }

  ExitCode::SUCCESS
}

fn validate(
  order: Vec<String>,
  groups: &mut HashMap<String, ConvertArgs>,
  args: &MupenSrmConvert,
) -> bool {
  ldbgln!("\n--- Validating group(s) ---");
  // remove invalid groups
  for name in order.into_iter() {
    // is safe to unwrap because the key was inserted when the group was created
    let value = groups.get_mut(&name).unwrap();

    if !args.split_srm {
      // if auto or create, set the srm file name if missing
      value.mode.get_or_insert_with(|| {
        ldbgln!("> Group \"{name}\": Will Create SRM");
        ConvertMode::Create(PathBuf::from(&name).with_extension("srm"))
      });
    }

    let keep = match &value.mode {
      Some(ConvertMode::Create(_)) => {
        if value.paths.is_empty() {
          lerrln!("Group \"{name}\": Can't create SRM: no input file(s)");
          false
        } else if !value.paths.any_is_file() {
          lerrln!("Group \"{name}\": Can't create SRM: input file(s) do not exist");
          ldbgln!("  Input files: {}", value.paths);
          false
        } else {
          true
        }
      }
      Some(ConvertMode::Split(path)) => {
        if !path.exists() {
          lerrln!("Group \"{name}\":Can't split SRM: srm file doesn't exist")
        } else if !path.is_file() {
          lerrln!("Group \"{name}\":Can't split SRM: srm path is not a file")
        }
        path.is_file()
      }
      None => {
        lerrln!("Group \"{name}\": No SRM file found");
        false
      }
    };
    if !keep {
      drop(value);
      groups.remove_entry(&name);
    } else {
      ldbgln!("Group \"{name}\": OK");
    }
  }
  !groups.is_empty()
}

fn group_files(args: &mut MupenSrmConvert) -> (HashMap<String, ConvertArgs>, Vec<String>) {
  let mut map = HashMap::<String, ConvertArgs>::new();
  ldbgln!("\n--- Grouping file(s) ---");

  let mut order = Vec::new();

  // collect files from arguments
  let files = std::mem::replace(&mut args.files, Vec::new());

  for path in files.into_iter() {
    let mut _pad = ldbgln!(>2 "File {path:#?}:");
    if path.is_dir() {
      lwarnln!("Path {path:#?} is a directory");
      continue;
    }
    ldbg!("Getting name...");
    let name = match path.file_stem().and_then(OsStr::to_str) {
      Some(name) => name,
      None => {
        lwarnln!("Path {path:#?} did not name a file");
        continue;
      }
    };
    ldbgln!(" \"{name}\"");

    let key = if !args.split_srm && !args.create_srm {
      name.to_string()
    } else {
      if map.len() == 0 {
        name.to_string()
      } else {
        // it is safe because we have at least one key
        map.keys().peekable().peek().unwrap().to_string()
      }
    };

    let opts = if args.split_srm {
      AddOpts::ForcedSplit(args.base.merge_mempacks)
    } else if args.create_srm {
      AddOpts::ForcedCreate(args.base.merge_mempacks)
    } else {
      AddOpts::Automatic(args.base.merge_mempacks)
    };

    let conversion_args = map.entry(key).or_insert_with_key(|key| {
      ldbg!("  ");
      linfln!("Created group \"{key}\"");
      order.push(key.clone());
      ConvertArgs::default()
    });

    if _pad == 0 {
      _pad = linf!(>2"");
    }

    if let Err(err) = conversion_args.add(path, opts) {
      lwarnln!("{err}");
    }
  }
  (map, order)
}

#[cfg(test)]
mod tests {
  use crate::MupenSrmConvert;

  #[test]
  fn verify_cli() {
    use clap::CommandFactory;
    MupenSrmConvert::command().debug_assert()
  }
}
