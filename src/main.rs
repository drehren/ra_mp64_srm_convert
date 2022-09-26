mod game_pack;
mod retroarch_srm;

mod controller_pack;
use controller_pack::*;

mod create_srm;
use create_srm::*;

mod split_srm;
use split_srm::*;

#[macro_use]
mod logger;
use logger::*;

use clap::Parser;

use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::Debug;
use std::fs::OpenOptions;
use std::io::{self, ErrorKind};
use std::path::{Path, PathBuf};

fn get_pack_number<P: AsRef<Path>>(path: &P) -> Option<usize> {
  let str = path.as_ref().parent()?.to_str()?;
  let last = str.chars().last()?;
  Some(last.to_digit(5)?.checked_sub(1)? as usize)
}

#[derive(Debug, Default, Clone)]
enum SingleMulti {
  #[default]
  Empty,
  Single(PathBuf),
  Multiple(Vec<Option<PathBuf>>),
}
impl SingleMulti {
  fn take(&mut self) -> SingleMulti {
    std::mem::replace(self, SingleMulti::Empty)
  }
  fn single(self) -> Option<PathBuf> {
    match self {
      SingleMulti::Empty => None,
      SingleMulti::Multiple(v) => v[0].clone(),
      SingleMulti::Single(p) => Some(p),
    }
  }
}
#[derive(Debug)]
enum ConvertMode {
  Merge(Option<PathBuf>),
  Split(PathBuf),
}
#[derive(Debug, Default)]
pub struct ConvertArgs {
  eep_path: Option<PathBuf>,
  sra_path: Option<PathBuf>,
  fla_path: Option<PathBuf>,
  mpk_path: SingleMulti,
}

impl ConvertArgs {
  fn insert(&mut self, save: SaveFile) {
    match save.save_type {
      SaveType::EEP => self.eep_path = Some(save.path),
      SaveType::FLA => self.fla_path = Some(save.path),
      SaveType::SRA => self.sra_path = Some(save.path),
      SaveType::MPK(CpSaveType::Split(None)) => {}
      SaveType::MPK(CpSaveType::Split(Some(idx))) => {
        let mut v = match self.mpk_path.take() {
          SingleMulti::Multiple(v) => v,
          _ => Vec::new(),
        };
        if idx > v.len() {
          v.resize(idx + 1, None);
        }
        v.insert(idx, Some(save.path));
        self.mpk_path = SingleMulti::Multiple(v)
      }
      SaveType::MPK(CpSaveType::Mp64) => self.mpk_path = SingleMulti::Single(save.path),
      _ => {}
    }
  }
}

#[derive(Debug)]
struct ConvertModeArgs {
  mode: ConvertMode,
  args: ConvertArgs,
}
impl ConvertModeArgs {
  fn new(save: SaveFile) -> Self {
    match save.save_type {
      SaveType::SRM => Self {
        mode: ConvertMode::Split(save.path),
        args: Default::default(),
      },
      _ => {
        let mut me = Self {
          mode: ConvertMode::Merge(None),
          args: Default::default(),
        };
        me.insert(save);
        me
      }
    }
  }
  fn insert(&mut self, save: SaveFile) {
    match save.save_type {
      SaveType::SRM => {
        if let ConvertMode::Merge(None) = self.mode {
          self.mode = ConvertMode::Merge(Some(save.path))
        }
      }
      _ => self.args.insert(save),
    }
  }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum CpSaveType {
  Mp64,
  Split(Option<usize>),
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum SaveType {
  UNSUPPORTED,
  SRM,
  FLA,
  EEP,
  SRA,
  MPK(CpSaveType),
}

impl SaveType {
  fn infer_from_path<P: AsRef<Path>>(path: &P) -> SaveType {
    // filename extension detection
    let path = path.as_ref();
    match &path.extension().unwrap().to_ascii_uppercase().to_str() {
      Some("SRM") => SaveType::SRM,
      Some("SRA") => SaveType::SRA,
      Some("FLA") => SaveType::FLA,
      Some("EEP") => SaveType::EEP,
      Some("MPK") => SaveType::MPK(CpSaveType::Split(get_pack_number(&path))),
      _ => SaveType::UNSUPPORTED,
    }
  }

  fn infer_from_data<P: AsRef<Path>>(path: &P) -> std::io::Result<SaveType> {
    // we can identify the saves based on their file size & first bytes
    // we can also check if the saves are byteswapped or not
    let path = path.as_ref();

    if path.file_name().is_none() {
      return Ok(SaveType::UNSUPPORTED);
    }

    let load_check = |not_cp, mp| -> io::Result<SaveType> {
      let mut file = std::fs::File::open(path)?;
      match ControllerPack::infer_from(&mut file) {
        Ok(check) if check == Check::NotAControllerPack => Ok(not_cp),
        Ok(_) => Ok(SaveType::MPK(mp)),
        Err(_) => Ok(SaveType::UNSUPPORTED),
      }
    };

    match path.metadata().map(|m| m.len()) {
      Ok(0x800) => Ok(SaveType::EEP),
      Ok(0x8000) => load_check(SaveType::SRA, CpSaveType::Split(get_pack_number(&path))),
      Ok(0x20000) => load_check(SaveType::FLA, CpSaveType::Mp64),
      Ok(0x48800) => Ok(SaveType::SRM),
      Ok(_) => Ok(SaveType::UNSUPPORTED),
      _ => unreachable!(),
    }
  }
}

#[derive(Clone)]
pub struct SaveFile {
  path: PathBuf,
  save_type: SaveType,
}

impl TryFrom<PathBuf> for SaveFile {
  type Error = io::Error;

  fn try_from(path: PathBuf) -> Result<Self, Self::Error> {
    if (path.exists() && !path.is_file()) || (!path.exists() && !path.extension().is_none()) {
      Err(io::Error::new(
        ErrorKind::Other,
        format!("Impossible to determine the save type from {:#?}", path),
      ))
    } else if !path.exists() {
      Ok(Self {
        save_type: SaveType::infer_from_path(&path),
        path,
      })
    } else {
      Ok(Self {
        save_type: SaveType::infer_from_data(&path)?,
        path,
      })
    }
  }
}

impl SaveFile {
  fn simple_name(&self) -> String {
    if let Some(file_name) = self.path.file_stem() {
      let mut name = file_name.to_string_lossy().to_string();
      if name.ends_with(|c: char| c.is_digit(5)) {
        name.pop();
      }
      name
    } else {
      "".to_owned()
    }
  }
}

#[derive(Parser)]
#[clap(verbatim_doc_comment, version)]
/// A simple converter for Retroarch's Mupen64 core save files.
///
/// It (tries) to detect the save file and does one of the following:
/// - .eep|.fla|*.mpk|.sav: group based on the file names and creates and .srm file.
/// - .srm                : extract save data and creates .eep, .fla, .*.mpk, .sav file(s).
struct MupenSrmConvert {
  /// Sets the verbose output (1 time for error logs, 2 times for debug log)
  #[clap(short, long, action = clap::ArgAction::Count)]
  verbose: u8,

  /// Logs to the specified file
  #[clap(long, conflicts_with = "quiet", parse(from_os_str))]
  log_file: Option<PathBuf>,

  /// Set this to suppress all output
  #[clap(short, long, conflicts_with = "verbose")]
  quiet: bool,

  /// If set, the program can overwrite an existing filesystem files
  #[clap(long)]
  overwrite: bool,

  /// Specify the output directory for the created file (or files)
  #[clap(long, parse(from_os_str))]
  output_dir: Option<PathBuf>,

  /// If set, the 4 memory pack files will be merged into one
  #[clap(long)]
  merge_mempacks: bool,

  /// The input file(s).
  /// It can be *.srm (to extract), or *.sra, *.fla, *.eep or *.mpk (to create, based on file name)
  #[clap(parse(from_os_str), min_values = 1, required = true)]
  files: Vec<PathBuf>,
}

fn main() -> io::Result<()> {
  let mut args = MupenSrmConvert::parse();

  let logger_builder = match args.verbose {
    1 => LoggerBuilder::error(),
    2 => LoggerBuilder::debug(),
    _ => LoggerBuilder::info(),
  };
  let mut logger = match args.log_file {
    Some(path) => OpenOptions::new()
      .create(true)
      .append(true)
      .open(path)
      .map_or_else(
        |_| logger_builder.create(std::io::sink()),
        |f| logger_builder.create(f),
      ),
    None => logger_builder.create(std::io::stdout()),
  };

  // if there is out dir, check!
  if let Some(out_dir) = args.output_dir.as_mut() {
    out_dir.canonicalize()?;
    std::fs::create_dir_all(&out_dir)?;
    log!(d: logger, "Created output dir {:#?}", out_dir);
  }

  // collect same-filename paths
  let mut map = HashMap::<String, ConvertModeArgs>::new();
  for path in args.files.into_iter() {
    log!(d: logger, "Checking {:#?}", &path);
    let save = match SaveFile::try_from(path) {
      Ok(save_file) if save_file.save_type == SaveType::UNSUPPORTED => {
        log!(logger, "Save file {:#?} not supported", save_file.path);
        continue;
      }
      Err(err) => {
        log!(logger, "Error while getting save type: {err}");
        continue;
      }
      Ok(save_file) => save_file,
    };
    log!(d: logger, "File is a {:#?}", &save.save_type);

    // get the files vector and put the data
    map
      .entry(save.simple_name())
      .or_insert_with(|| ConvertModeArgs::new(save.clone()))
      .insert(save);
  }

  // check that files where collected
  if map.len() == 0 {
    return Err(io::Error::new(ErrorKind::Other, "Invalid input file(s)"));
  }

  // Now work per file name
  for (name, value) in map {
    let ConvertModeArgs {
      mode,
      args: conv_args,
    } = value;
    log!(d: logger, "Convert {name} with {:?}", &conv_args);
    if let Err(e) = match mode {
      ConvertMode::Merge(output_file) => {
        let mut output_path =
          output_file.map_or_else(|| PathBuf::from(name).with_extension("srm"), |p| p);
        if let Some(mut output_dir) = args.output_dir.clone() {
          output_dir.push(output_path.file_name().unwrap());
          output_path = output_dir;
        }
        create_srm(&mut logger, args.overwrite, conv_args, output_path)
      }
      ConvertMode::Split(input_path) => split_srm(
        &mut logger,
        args.overwrite,
        args.output_dir.clone(),
        args.merge_mempacks,
        input_path,
        conv_args,
      ),
    } {
      log!(e: logger, "{e}");
    }
  }

  Ok(())
}
