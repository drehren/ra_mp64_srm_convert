mod game_pack;
use game_pack::*;

mod controller_pack;
use controller_pack::*;

use clap::Parser;
use std::{
  collections::{HashMap, VecDeque},
  ffi::{OsStr, OsString},
  fs::{File, OpenOptions},
  io::{self, ErrorKind, Read, Write},
  ops::{Deref, DerefMut},
  path::{Path, PathBuf},
  str::FromStr,
};

/// Loads `B` with enough data from `R`
pub fn load<B, R>(into: &mut B, from: &mut R) -> io::Result<()>
where
  R: Read,
  B: DerefMut<Target = [u8]>,
{
  from.read_exact(into)
}
/// Stores `B` bytes to `R`
pub fn store<B, W>(data: &B, into: &mut W) -> io::Result<()>
where
  W: Write,
  B: Deref<Target = [u8]>,
{
  into.write_all(&data)
}

pub struct RetroArchSrm {
  eeprom: Eeprom,
  mempack: [Mempack; 4],
  sram: Sram,
  flashram: FlashRam,
}

impl RetroArchSrm {
  fn new() -> Self {
    Self {
      eeprom: Eeprom::new(),
      mempack: [
        Mempack::new(),
        Mempack::new(),
        Mempack::new(),
        Mempack::new(),
      ],
      sram: Sram::new(),
      flashram: FlashRam::new(),
    }
  }

  fn init(&mut self) {
    // Initialize the mempacks
    for mp in &mut self.mempack {
      mp.init()
    }
  }

  fn load(&mut self, file: &mut File) -> io::Result<()> {
    load(&mut self.eeprom, file)?;
    for i in 0..4 {
      load(&mut self.mempack[i], file)?;
      // let mut data = [0; 0x8000];
      // file.read_exact(data.as_mut())?;
      // self.mempack[i] = data.into();
    }
    load(&mut self.sram, file)?;
    load(&mut self.flashram, file)
  }

  fn store(&self, file: &mut File) -> io::Result<()> {
    store(&self.eeprom, file)?;
    for mp in &self.mempack {
      // mp.save(file)?;
      store(mp, file)?;
    }
    store(&self.sram, file)?;
    store(&self.flashram, file)
  }
}

#[derive(PartialEq)]
enum SaveType {
  UNSUPPORTED,
  SRM,
  FLA,
  EEP,
  SRA,
  MPK,
}
impl FromStr for SaveType {
  type Err = &'static str;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s.to_uppercase().as_str() {
      "SRM" => Ok(SaveType::SRM),
      "FLA" => Ok(SaveType::FLA),
      "EEP" => Ok(SaveType::EEP),
      "MPK" => Ok(SaveType::MPK),
      "SRA" => Ok(SaveType::SRA),
      _ => Err("Unexpected save type"),
    }
  }
}
impl From<&OsStr> for SaveType {
  fn from(s: &OsStr) -> Self {
    match s.to_ascii_uppercase().to_str() {
      Some("SRM") => SaveType::SRM,
      Some("FLA") => SaveType::FLA,
      Some("EEP") => SaveType::EEP,
      Some("MPK") => SaveType::MPK,
      Some("SRA") => SaveType::SRA,
      _ => SaveType::UNSUPPORTED,
    }
  }
}

#[derive(Default)]
struct ConvertArgs<'a> {
  overwrite: bool,
  out_dir: Option<&'a PathBuf>,
  srm_file: Option<&'a PathBuf>,
  eep_file: Option<&'a PathBuf>,
  mpk_files: [Option<&'a PathBuf>; 4],
  sra_file: Option<&'a PathBuf>,
  fla_file: Option<&'a PathBuf>,
}

fn output_file(in_path: &Path, out_dir: &Option<&PathBuf>) -> PathBuf {
  out_dir.map_or_else(
    || in_path.to_owned(),
    |dir| {
      let mut out_dir = dir.to_path_buf();
      out_dir.push(in_path.file_name().unwrap());
      out_dir
    },
  )
}

fn to_srm<'a>(args: ConvertArgs<'a>) -> io::Result<()> {
  // here we should get the files to put into the srm
  let mut srm = Box::new(RetroArchSrm::new());
  srm.init();

  let mut load_opts = OpenOptions::new();
  load_opts.read(true);

  // setup now the save options in case the srm file exists, which we should update
  let mut save_opts = OpenOptions::new();
  save_opts
    .create(args.overwrite)
    .create_new(!args.overwrite)
    .write(true);

  // If the srm file exists, its update mode!
  if let Some(srm_file) = &args.srm_file {
    if srm_file.is_file() {
      srm.load(&mut load_opts.open(srm_file)?)?;
      save_opts.create(true).create_new(false);
    }
  }

  if let Some(path) = args.eep_file {
    load(&mut srm.eeprom, &mut load_opts.open(path)?)?;
  }
  for (i, mp) in args.mpk_files.iter().enumerate() {
    if let Some(path) = mp {
      load(&mut srm.mempack[i], &mut load_opts.open(path)?)?;
    }
  }
  if let Some(path) = args.sra_file {
    load(&mut srm.sram, &mut load_opts.open(path)?)?;
  }
  if let Some(path) = args.fla_file {
    load(&mut srm.flashram, &mut load_opts.open(path)?)?;
  }

  let mut srm_file = save_opts.open(output_file(args.srm_file.unwrap(), &args.out_dir))?;
  srm.store(&mut srm_file)
}

fn from_srm<'a>(args: ConvertArgs<'a>) -> io::Result<()> {
  let input = args.srm_file.as_ref().unwrap();

  let mut srm = Box::from(RetroArchSrm::new());
  {
    let mut file = OpenOptions::new().read(true).open(input)?;
    srm.load(&mut file)?;
  }

  let mut open_opts = OpenOptions::new();
  open_opts
    .create(args.overwrite)
    .create_new(!args.overwrite)
    .write(true);
  let mut existing_open = OpenOptions::new();
  existing_open.create(true).write(true);

  if !srm.eeprom.is_empty() {
    let mut file = args.eep_file.map_or_else(
      || open_opts.open(output_file(&input.with_extension("eep"), &args.out_dir)),
      |f| existing_open.open(output_file(f, &args.out_dir)),
    )?;
    store(&srm.eeprom, &mut file)?;
  }
  for (i, mp) in srm.mempack.iter().enumerate() {
    if mp.is_empty() {
      continue;
    }
    let mut file = args.mpk_files[i].map_or_else(
      || {
        let mut file_name = input.file_stem().unwrap().to_owned();
        file_name.push((i + 1).to_string());
        file_name.push(".mpk");
        open_opts.open(output_file(&input.with_file_name(file_name), &args.out_dir))
      },
      |f| existing_open.open(output_file(f, &args.out_dir)),
    )?;
    store(mp, &mut file)?;
  }
  if !srm.sram.is_empty() {
    let mut file = args.sra_file.map_or_else(
      || open_opts.open(output_file(&input.with_extension("sra"), &args.out_dir)),
      |f| existing_open.open(output_file(f, &args.out_dir)),
    )?;
    store(&srm.sram, &mut file)?;
  }
  if !srm.flashram.is_empty() {
    let mut file = args.fla_file.map_or_else(
      || open_opts.open(output_file(&input.with_extension("fla"), &args.out_dir)),
      |f| existing_open.open(output_file(f, &args.out_dir)),
    )?;
    store(&srm.flashram, &mut file)?;
  }
  Ok(())
}

#[derive(Parser)]
#[clap(verbatim_doc_comment)]
/// A simple converter for Retroarch's Mupen64 core save files.
///
/// It detects the save (based on its extension) and does the following:
/// - .eep|.fla|.mpk*|.sav: groups based on the file names and creates and .srm file.
/// - .srm                : extracts save data and creates .eep, .fla, .mpk*, .sav file(s).
struct MupenSrmConvert {
  /// If set, the program can overwrite an existing filesystem files
  #[clap(long)]
  overwrite: bool,

  /// Specify the output directory for the created file (or files)
  #[clap(long, parse(from_os_str))]
  output_dir: Option<PathBuf>,

  /// The input file(s).
  /// It can be *.srm (to extract), or *.sra, *.fla, *.eep or *.mpk (to create, based on file name)
  #[clap(parse(from_os_str), min_values = 1, required = true)]
  files: Vec<PathBuf>,
}

fn run() -> io::Result<Vec<(String, std::io::Error)>> {
  let args = MupenSrmConvert::parse();
  let mut map = HashMap::<OsString, VecDeque<(PathBuf, SaveType)>>::new();

  // if there is out dir, check!
  if let Some(out_dir) = args.output_dir.as_ref() {
    if out_dir.exists() && !out_dir.is_dir() {
      return Err(std::io::Error::new(
        ErrorKind::Other,
        "Output directory path is not a directory!",
      ));
    }
    if !out_dir.exists() {
      std::fs::create_dir_all(out_dir)?;
    }
  }

  for file in args.files.into_iter() {
    if !file.exists() || !file.is_file() {
      continue;
    }

    let save_type = if let Some(ext) = file.extension() {
      Into::<SaveType>::into(ext)
    } else {
      continue;
    };
    if save_type == SaveType::UNSUPPORTED {
      continue;
    }

    // get the vector
    let vector = file
      .file_stem()
      .map(|name| map.entry(name.into()).or_default());

    if let Some(v) = vector {
      v.push_back((file, save_type));
    }
  }

  if map.len() == 0 {
    return Err(std::io::Error::new(
      ErrorKind::Other,
      "Invalid input file(s)",
    ));
  }

  let mut file_errs = vec![];

  let work_groups = map.len();

  // Now work per file name
  for files in map.into_values() {
    let (path, save_type) = files.front().unwrap();

    let mut args = ConvertArgs {
      overwrite: args.overwrite,
      ..ConvertArgs::default()
    };

    let mut mpk_idx = 0;
    for (file, sav_type) in &files {
      match sav_type {
        SaveType::EEP => args.eep_file = Some(file),
        SaveType::FLA => args.fla_file = Some(file),
        SaveType::MPK => {
          args.mpk_files[mpk_idx] = Some(file);
          mpk_idx += 1;
        }
        SaveType::SRA => args.sra_file = Some(file),
        SaveType::SRM => args.srm_file = Some(file),
        _ => unreachable!(),
      }
    }

    match match save_type {
      SaveType::SRM => from_srm(args),
      _ => {
        let srm_path = path.with_extension("srm");
        if args.srm_file.is_none() {
          args.srm_file = Some(&srm_path);
        }
        to_srm(args)
      }
    } {
      Ok(_) => {}
      Err(error) => file_errs.push((
        path.file_name().unwrap().to_str().unwrap().to_owned(),
        error,
      )),
    }
  }

  if work_groups == file_errs.len() {
    let mut msg = Vec::with_capacity(file_errs.len() + 1);
    msg.push("Could not process any file!".to_owned());
    for err in file_errs.into_iter() {
      msg.push(format!("-  While working with '{}': {}", err.0, err.1));
    }
    Err(std::io::Error::new(ErrorKind::Other, msg.join("\n")))
  } else {
    Ok(file_errs)
  }
}

fn main() {
  match run() {
    Ok(file_errs) => {
      if !file_errs.is_empty() {
        println!("ERROR: Some errors found while working");
      }
      for err in file_errs.into_iter() {
        println!("ERROR: while working with '{}': {}", err.0, err.1)
      }
    }
    Err(error) => {
      write!(std::io::stderr(), "ERROR: {}", error).unwrap();
    }
  }
}
