use assert_cmd::{assert::Assert, Command};
use assert_fs::{fixture::ChildPath, prelude::*, TempDir};
use predicates::prelude::*;
use std::error::Error;
use std::fmt::Display;
use std::io::Cursor;
use std::path::{Path, PathBuf};
use zip::ZipArchive;

pub type TestResult<T> = Result<T, Box<dyn Error>>;

#[derive(Debug, PartialEq, Eq)]
pub enum Endianness {
  Big,
  Little,
}
impl Endianness {
  fn name(&self) -> &str {
    match self {
      Endianness::Big => "be",
      Endianness::Little => "le",
    }
  }
}
impl Display for Endianness {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(self.name())
  }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Player {
  P1,
  P2,
  P3,
  P4,
}

#[derive(Debug, PartialEq, Eq)]
pub enum EepromSize {
  _4Kbit,
  _16Kbit,
}

#[derive(Debug, PartialEq, Eq)]
pub enum SaveType {
  FlashRam(Endianness),
  MupenControllerPack,
  ControllerPack(Player),
  Sram(Endianness),
  Eeprom(EepromSize),
  Srm,
}
impl SaveType {
  pub fn name(&self) -> &str {
    match self {
      SaveType::FlashRam(endianness) => match endianness {
        Endianness::Big => "fla_be",
        Endianness::Little => "fla_le",
      },
      SaveType::Sram(endianness) => match endianness {
        Endianness::Big => "sra_be",
        Endianness::Little => "sra_le",
      },
      SaveType::MupenControllerPack => "mpk",
      SaveType::ControllerPack(n) => match n {
        Player::P1 => "mpk1",
        Player::P2 => "mpk2",
        Player::P3 => "mpk3",
        Player::P4 => "mpk4",
      },
      SaveType::Eeprom(size) => match size {
        EepromSize::_16Kbit => "eep_16k",
        EepromSize::_4Kbit => "eep_4k",
      },
      SaveType::Srm => "srm",
    }
  }

  pub fn is_big_endian(&self) -> bool {
    matches!(
      self,
      Self::FlashRam(Endianness::Big) | Self::Sram(Endianness::Big)
    )
  }
}
impl Display for SaveType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(self.name())
  }
}
impl AsRef<Path> for SaveType {
  fn as_ref(&self) -> &Path {
    Path::new(match self {
      SaveType::FlashRam(endianness) => match endianness {
        Endianness::Big => "fla/be",
        Endianness::Little => "fla/le",
      },
      SaveType::Sram(endianness) => match endianness {
        Endianness::Big => "sra/be",
        Endianness::Little => "sra/le",
      },
      _ => self.name(),
    })
  }
}

pub struct DataPrepare {
  tmp: assert_fs::TempDir,
}
impl DataPrepare {
  pub fn new() -> TestResult<Self> {
    let tmp = TempDir::new()?;
    let data_zip_bytes = include_bytes!("test_data.zip");
    let mut zip = ZipArchive::new(Cursor::new(data_zip_bytes))?;

    zip.extract(tmp.path())?;

    let data_path = tmp.child("test_data");
    data_path.assert(predicate::path::is_dir());

    tmp.child("out").create_dir_all()?;

    Ok(Self { tmp })
  }

  pub fn out_dir(&self) -> ChildPath {
    self.tmp.child("out")
  }

  pub fn test_data_dir(&self) -> ChildPath {
    self.tmp.child("test_data")
  }

  pub fn get_saves<P>(&self, child_path: &P) -> TestResult<Vec<PathBuf>>
  where
    P: AsRef<Path> + ?Sized,
  {
    let save_path = self.test_data_dir().child(child_path);

    save_path.assert(predicate::path::is_dir());

    let mut result = Vec::with_capacity(2);

    let dir_read = save_path.read_dir()?;
    for entry in dir_read.flatten() {
      if entry.file_type()?.is_file() {
        result.push(entry.path())
      }
    }

    Ok(result)
  }
}

#[derive(Debug, Default)]
pub enum CmdMode {
  Create,
  Split,
  #[default]
  Auto,
}

#[derive(Debug, Default)]
pub struct CmdOptions {
  mode: CmdMode,
  overwrite: bool,
  change_endianness: bool,
  merge_mempacks: bool,
  out_dir: Option<PathBuf>,
  srm: Option<PathBuf>,
}

impl CmdOptions {
  pub fn new(mode: CmdMode) -> Self {
    Self {
      mode,
      ..Default::default()
    }
  }

  pub fn set_from_save_type(mut self, save_type: &SaveType) -> Self {
    self.change_endianness = save_type.is_big_endian();
    self.merge_mempacks = save_type == &SaveType::MupenControllerPack;
    self
  }

  pub fn set_merge_mempacks(self, merge_mempacks: bool) -> Self {
    Self {
      merge_mempacks,
      ..self
    }
  }

  pub fn set_overwrite(mut self, overwrite: bool) -> Self {
    self.overwrite = overwrite;
    self
  }

  pub fn set_out_dir<P>(mut self, out_dir: P) -> Self
  where
    P: AsRef<Path>,
  {
    self.out_dir = Some(out_dir.as_ref().to_owned());
    self
  }

  pub fn set_srm(mut self, srm: Option<PathBuf>) -> Self {
    self.srm = srm;
    self
  }

  pub fn apply(self, cmd: &mut Command) {
    match self.mode {
      CmdMode::Create => cmd.arg("-c"),
      CmdMode::Split => cmd.arg("-s"),
      CmdMode::Auto => cmd,
    };
    if self.change_endianness {
      cmd.arg("--change-endianness");
    }
    if self.merge_mempacks {
      cmd.arg("--merge-mempacks");
    }
    if self.overwrite {
      cmd.arg("--overwrite");
    }
    if let Some(out_dir) = self.out_dir {
      cmd.arg("--output-dir").arg(out_dir);
    }
    if let Some(srm) = self.srm {
      cmd.arg(srm);
    }
  }
}

pub fn run<P>(
  exe_name: &str,
  input_files: &Vec<PathBuf>,
  cwd: P,
  opts: CmdOptions,
) -> TestResult<Assert>
where
  P: AsRef<Path>,
{
  let mut cmd = Command::cargo_bin(exe_name)?;

  opts.apply(cmd.current_dir(cwd));

  cmd.args(input_files);
  cmd.args(["-v", "debug"]);

  Ok(cmd.assert())
}

#[derive(Debug)]
pub struct NoInputFilesError(SaveType);

impl Error for NoInputFilesError {}

impl Display for NoInputFilesError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "No input files where found for save type: {}",
      self.0
    ))
  }
}

pub fn run_split_test(exe_name: &str, data: &DataPrepare, save_type: SaveType) -> TestResult<()> {
  let input_files = data.get_saves(&save_type)?;
  if input_files.is_empty() {
    return Err(Box::new(NoInputFilesError(save_type)));
  }

  let name = input_files[0]
    .file_name()
    .map(|n| Path::new(n).with_extension("srm"))
    .expect("file should have a name");

  let create_run = run(
    exe_name,
    &input_files,
    data.test_data_dir(),
    CmdOptions::new(CmdMode::Create)
      .set_from_save_type(&save_type)
      .set_out_dir(data.out_dir())
      .set_srm(Some(name.clone())),
  )?;

  create_run.success();

  // assert that the file is there
  let out_file = data.out_dir().child(&name);
  out_file.assert(predicate::path::is_file());

  // get the output names
  let output_saves = input_files
    .iter()
    .map(|p| p.file_name().unwrap().into())
    .collect::<Vec<PathBuf>>();

  // an srm should exist now... if we were to split this one, its contents should match the input
  let split_run = run(
    exe_name,
    &output_saves,
    data.test_data_dir(),
    CmdOptions::new(CmdMode::Split)
      .set_srm(Some(out_file.to_path_buf()))
      .set_out_dir(data.out_dir())
      .set_from_save_type(&save_type),
  )?;

  split_run.success();

  // assert that the file is out there...
  for out_save in &output_saves {
    data
      .out_dir()
      .child(out_save)
      .assert(predicate::path::is_file());
  }

  // now compare input and output
  for (input, output) in input_files.iter().zip(&output_saves) {
    data
      .out_dir()
      .child(output)
      .assert(predicate::path::eq_file(input));
    assert_eq!(output.file_name(), input.file_name());
  }

  Ok(())
}

#[derive(Debug)]
pub struct NonMupenMpkFileError(PathBuf);

impl From<PathBuf> for NonMupenMpkFileError {
  fn from(value: PathBuf) -> Self {
    Self(value)
  }
}

impl Display for NonMupenMpkFileError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "{} is not a Mupen64 Mempack file",
      self.0.display()
    ))
  }
}
impl Error for NonMupenMpkFileError {}

#[derive(Debug)]
pub struct SrmWithoutOutputError;
impl Display for SrmWithoutOutputError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str("srm split did not output any file")
  }
}
impl Error for SrmWithoutOutputError {}
