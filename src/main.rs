use std::{
  collections::{HashMap, VecDeque},
  ffi::{OsStr, OsString},
  fs::{File, OpenOptions},
  io::Read,
  io::{ErrorKind, Write},
  path::{Path, PathBuf},
  str::FromStr,
};
use structopt::StructOpt;

struct Eeprom {
  data: [u8; 0x800],
}
impl Eeprom {
  fn is_empty(&self) -> bool {
    self.data.iter().rposition(|b| *b != 0xff) == None
  }

  fn new() -> Self {
    Self {
      data: [0xff; 0x800],
    }
  }

  fn save(&self, file: &mut File) -> std::io::Result<()> {
    file.write_all(&self.data)
  }
}

struct Mempack {
  data: [u8; 0x8000],
}
impl Mempack {
  const PAGE_SIZE: usize = 256;
  const PAGE_0: [u8; Mempack::PAGE_SIZE] = [
    /* Label area */
    0x81, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
    /* Main ID area */
    0xff, 0xff, 0xff, 0xff, 0x05, 0x1a, 0x5f, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01, 0xff, 0x66, 0x25, 0x99, 0xcd,
    /* Unused */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    /* ID area backup #1 */
    0xff, 0xff, 0xff, 0xff, 0x05, 0x1a, 0x5f, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01, 0xff, 0x66, 0x25, 0x99, 0xcd,
    /* ID area backup #2 */
    0xff, 0xff, 0xff, 0xff, 0x05, 0x1a, 0x5f, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01, 0xff, 0x66, 0x25, 0x99, 0xcd,
    /* Unused */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    /* ID area backup #3 */
    0xff, 0xff, 0xff, 0xff, 0x05, 0x1a, 0x5f, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01, 0xff, 0x66, 0x25, 0x99, 0xcd,
    /* Unused */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  ];

  fn is_empty(&self) -> bool {
    self.data[..Mempack::PAGE_SIZE] == Self::PAGE_0
  }

  fn save(&self, file: &mut File) -> std::io::Result<()> {
    file.write_all(&self.data)
  }

  fn new() -> Self {
    let mut data = [0u8; 0x8000];
    let mut pos = Mempack::PAGE_SIZE;

    // fill page 0
    data[..pos].copy_from_slice(&Self::PAGE_0);

    // fill inode page 1 + checksum
    const START_PAGE: usize = 5;
    for b in &mut data[pos..pos + 2 * START_PAGE] {
      *b = 0;
    }
    pos += 2 * START_PAGE;
    for b in data[pos + 1..2 * Mempack::PAGE_SIZE].iter_mut().step_by(2) {
      *b = 0x03;
    }

    // page 2 identical to page 1
    data.copy_within(
      Mempack::PAGE_SIZE..2 * Mempack::PAGE_SIZE,
      2 * Mempack::PAGE_SIZE,
    );

    Self { data }
  }
}

struct Sram {
  data: [u8; 0x8000],
}
impl Sram {
  fn is_empty(&self) -> bool {
    self.data.iter().rposition(|b| *b != 0xff) == None
  }

  fn new() -> Self {
    Self {
      data: [0xff; 0x8000],
    }
  }

  fn save(&self, file: &mut File) -> std::io::Result<()> {
    file.write_all(&self.data)
  }
}

struct FlashRam {
  data: [u8; 0x20000],
}
impl FlashRam {
  fn is_empty(&self) -> bool {
    self.data.iter().rposition(|b| *b != 0xff) == None
  }

  fn new() -> Self {
    Self {
      data: [0xff; 0x20000],
    }
  }

  fn save(&self, file: &mut File) -> std::io::Result<()> {
    file.write_all(&self.data)
  }
}

struct Srm {
  eeprom: Eeprom,
  mempack: [Mempack; 4],
  sram: Sram,
  flashram: FlashRam,
}
impl Srm {
  fn load(&mut self, file: &mut File) -> std::io::Result<()> {
    file.read(&mut self.eeprom.data)?;
    for mp in &mut self.mempack {
      file.read(&mut mp.data)?;
    }
    file.read(&mut self.sram.data)?;
    file.read(&mut self.flashram.data).map(|_| ())
  }

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

  fn save(&self, file: &mut File) -> std::io::Result<()> {
    self.eeprom.save(file)?;
    for mp in &self.mempack {
      mp.save(file)?;
    }
    self.sram.save(file)?;
    self.flashram.save(file)
  }
}

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

fn to_srm<'a>(args: ConvertArgs<'a>) -> std::io::Result<()> {
  // here we should get the files to put into the srm
  let mut srm = Box::new(Srm::new());

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
    load_opts.open(path)?.read(&mut srm.eeprom.data)?;
  }
  for (i, mp) in args.mpk_files.iter().enumerate() {
    if let Some(path) = mp {
      load_opts.open(path)?.read(&mut srm.mempack[i].data)?;
    }
  }
  if let Some(path) = args.sra_file {
    load_opts.open(path)?.read(&mut srm.sram.data)?;
  }
  if let Some(path) = args.fla_file {
    load_opts.open(path)?.read(&mut srm.flashram.data)?;
  }

  let mut srm_file = save_opts.open(output_file(args.srm_file.unwrap(), &args.out_dir))?;
  srm.save(&mut srm_file)
}

fn from_srm<'a>(args: ConvertArgs<'a>) -> std::io::Result<()> {
  let input = args.srm_file.as_ref().unwrap();

  let mut srm = Box::from(Srm::new());
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
    srm.eeprom.save(&mut file)?;
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
    mp.save(&mut file)?;
  }
  if !srm.sram.is_empty() {
    let mut file = args.sra_file.map_or_else(
      || open_opts.open(output_file(&input.with_extension("sra"), &args.out_dir)),
      |f| existing_open.open(output_file(f, &args.out_dir)),
    )?;
    srm.sram.save(&mut file)?;
  }
  if !srm.flashram.is_empty() {
    let mut file = args.fla_file.map_or_else(
      || open_opts.open(output_file(&input.with_extension("fla"), &args.out_dir)),
      |f| existing_open.open(output_file(f, &args.out_dir)),
    )?;
    srm.flashram.save(&mut file)?;
  }
  Ok(())
}

#[derive(StructOpt)]
/// A simple converter for Retroarch's Mupen64 core save files.
/// It detects the input file and "converts" it into an *.srm, or extracts from it into the other files.
struct MupenSrmConvert {
  #[structopt(long)]
  /// If set, the program can overwrite an existing filesystem files
  overwrite: bool,

  #[structopt(long, parse(from_os_str))]
  /// Specify the output directory for the created file (or files)
  output_dir: Option<PathBuf>,

  #[structopt(parse(from_os_str), min_values = 1, required = true)]
  /// The input file(s).
  /// It can be *.srm (to extract), or *.sra, *.fla, *.eep or *.mpk (to create, based on file name)
  files: Vec<PathBuf>,
}

fn run() -> std::io::Result<Vec<(String, std::io::Error)>> {
  let args = MupenSrmConvert::from_args();
  let mut map = HashMap::<OsString, VecDeque<(PathBuf, SaveType)>>::new();

  // if there is out dir, check!
  if let Some(out_dir) = args.output_dir.as_ref() {
    if out_dir.exists() && !out_dir.is_dir() {
      return Err(std::io::Error::new(
        ErrorKind::Other,
        "Ouput directory path is not a directory!",
      ));
    }
    if !out_dir.exists() {
      std::fs::create_dir_all(out_dir)?;
    }
  }

  for file in args.files.into_iter() {
    if file.exists() && !file.is_file() {
      continue;
    }

    let save_type: SaveType = if let Some(ext) = file.extension() {
      ext.into()
    } else {
      continue;
    };
    match save_type {
      SaveType::UNSUPPORTED => continue,
      _ => {}
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
    for (file, stype) in &files {
      match stype {
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
        let srmp = path.with_extension("srm");
        if args.srm_file.is_none() {
          args.srm_file = Some(&srmp);
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
        println!("WARN: Some errors found while working");
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
