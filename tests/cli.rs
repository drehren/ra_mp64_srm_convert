use assert_cmd::{assert::Assert, prelude::*};
use assert_fs::{fixture::ChildPath, prelude::*, TempDir};
use predicates::prelude::*;
use std::{
  collections::HashMap,
  error::Error,
  ffi::OsString,
  fmt::Display,
  io::Cursor,
  path::{Path, PathBuf},
  process::Command,
};
use zip::ZipArchive;

type TestResult<T> = Result<T, Box<dyn Error>>;

#[test]
fn auto_missing_srm_file() -> TestResult<()> {
  let mut cmd = Command::cargo_bin("ra_mp64_srm_convert")?;
  cmd
    .arg("missing.srm")
    .assert()
    .failure()
    .stderr(predicate::str::contains("srm file doesn't exist"));
  Ok(())
}

#[test]
fn split_missing_srm_file() -> TestResult<()> {
  let mut cmd = Command::cargo_bin("ra_mp64_srm_convert")?;
  cmd
    .arg("-s")
    .arg("missing.srm")
    .assert()
    .failure()
    .stderr(predicate::str::contains("srm file doesn't exist"));
  Ok(())
}

#[test]
fn create_srm_no_input() -> TestResult<()> {
  let mut cmd = Command::cargo_bin("ra_mp64_srm_convert")?;
  cmd
    .arg("-c")
    .arg("new.srm")
    .assert()
    .failure()
    .stderr(predicate::str::contains("no input file(s)"));
  Ok(())
}

#[derive(Debug, PartialEq, Eq)]
enum Endianness {
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
enum SaveType {
  FlashRam(Endianness),
  MupenControllerPack,
  ControllerPack(usize),
  Sram(Endianness),
  Eeprom(usize),
  Srm,
}
impl SaveType {
  fn name(&self) -> &str {
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
        4 => "mpk4",
        3 => "mpk3",
        2 => "mpk2",
        _ => "mpk1",
      },
      SaveType::Eeprom(size) => match size {
        16 => "eep_16k",
        _ => "eep_4k",
      },
      SaveType::Srm => "srm",
    }
  }

  fn is_big_endian(&self) -> bool {
    match self {
      Self::FlashRam(Endianness::Big) | Self::Sram(Endianness::Big) => true,
      _ => false,
    }
  }
}
impl Display for SaveType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(self.name())
  }
}
impl AsRef<Path> for SaveType {
  fn as_ref(&self) -> &Path {
    &Path::new(match self {
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

struct DataPrepare {
  tmp: assert_fs::TempDir,
}
impl DataPrepare {
  fn new() -> TestResult<Self> {
    let tmp = TempDir::new()?;
    let data_zip_bytes = include_bytes!("test_data.zip");
    let mut zip = ZipArchive::new(Cursor::new(data_zip_bytes))?;

    zip.extract(tmp.path())?;

    let data_path = tmp.child("test_data");
    data_path.assert(predicate::path::is_dir());

    tmp.child("out").create_dir_all()?;

    Ok(Self { tmp })
  }

  fn out_dir(&self) -> ChildPath {
    self.tmp.child("out")
  }

  fn test_data_dir(&self) -> ChildPath {
    self.tmp.child("test_data")
  }

  fn get_saves(&self, save_type: &SaveType) -> TestResult<Vec<PathBuf>> {
    let save_path = self.test_data_dir().child(save_type);

    save_path.assert(predicate::path::is_dir());

    let mut result = Vec::with_capacity(2);

    let dir_read = save_path.read_dir()?;
    for entry in dir_read {
      if let Ok(entry) = entry {
        if entry.file_type()?.is_file() {
          result.push(entry.path())
        }
      }
    }

    Ok(result)
  }
}

#[derive(Debug, Default)]
enum CmdMode {
  Create,
  Split,
  #[default]
  Auto,
}

#[derive(Debug, Default)]
struct CmdOptions {
  mode: CmdMode,
  change_endianness: bool,
  merge_mempacks: bool,
  out_dir: PathBuf,
  srm: Option<PathBuf>,
}

impl CmdOptions {
  fn new(mode: CmdMode) -> Self {
    Self {
      mode,
      ..Default::default()
    }
  }

  fn update_from_save_type(mut self, save_type: &SaveType) -> Self {
    self.change_endianness = save_type.is_big_endian();
    self.merge_mempacks = save_type == &SaveType::MupenControllerPack;
    self
  }

  fn set_out_dir(mut self, data: &DataPrepare) -> Self {
    self.out_dir = data.out_dir().to_path_buf();
    self
  }

  fn set_srm(mut self, srm: Option<PathBuf>) -> Self {
    self.srm = srm;
    self
  }

  fn apply(mut self, cmd: &mut Command) {
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
    cmd
      .arg("--output-dir")
      .arg(std::mem::replace(&mut self.out_dir, PathBuf::new()));

    if let Some(srm) = self.srm {
      cmd.arg(srm);
    }
  }
}

fn run(input_files: &Vec<PathBuf>, data: &DataPrepare, opts: CmdOptions) -> TestResult<Assert> {
  let mut cmd = Command::cargo_bin("ra_mp64_srm_convert")?;

  opts.apply(cmd.current_dir(data.test_data_dir()));

  cmd.args(input_files);

  Ok(cmd.assert())
}

#[derive(Debug)]
struct NoInputFilesError(SaveType);

impl Error for NoInputFilesError {}

impl Display for NoInputFilesError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "No input files where found for save type: {}",
      self.0
    ))
  }
}

fn create_split_test(data: &DataPrepare, save_type: SaveType) -> TestResult<()> {
  let input_files = data.get_saves(&save_type)?;
  if input_files.is_empty() {
    return Err(Box::new(NoInputFilesError(save_type)));
  }

  let name = input_files[0]
    .file_stem()
    .map(|n| Path::new(n).with_extension("srm"))
    .expect("file should have a name");

  let create_run = run(
    &input_files,
    data,
    CmdOptions::new(CmdMode::Create)
      .update_from_save_type(&save_type)
      .set_out_dir(data)
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
    &output_saves,
    data,
    CmdOptions::new(CmdMode::Split)
      .set_srm(Some(out_file.to_path_buf()))
      .set_out_dir(data)
      .update_from_save_type(&save_type),
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
  }

  Ok(())
}

#[test]
fn create_from_sra_be_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  create_split_test(&data, SaveType::Sram(Endianness::Little))
}

#[test]
fn create_from_sra_le_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  create_split_test(&data, SaveType::Sram(Endianness::Little))
}

#[test]
fn create_from_fla_be_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  create_split_test(&data, SaveType::FlashRam(Endianness::Big))
}

#[test]
fn create_from_fla_le_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  create_split_test(&data, SaveType::FlashRam(Endianness::Little))
}

#[test]
fn create_from_eep_4k_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  create_split_test(&data, SaveType::Eeprom(4))
}

#[test]
fn create_from_eep_16k_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  create_split_test(&data, SaveType::Eeprom(16))
}

#[test]
fn create_from_mupen_mpk_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  create_split_test(&data, SaveType::MupenControllerPack)
}

#[test]
fn create_from_mpk1_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  create_split_test(&data, SaveType::ControllerPack(1))
}

#[test]
fn create_from_mpk2_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  create_split_test(&data, SaveType::ControllerPack(2))
}

#[test]
fn create_from_mpk3_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  create_split_test(&data, SaveType::ControllerPack(3))
}

#[test]
fn create_from_mpk4_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  create_split_test(&data, SaveType::ControllerPack(4))
}

#[derive(Debug)]
struct SrmWithoutOutputError;
impl Display for SrmWithoutOutputError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str("srm split did not output any file")
  }
}
impl Error for SrmWithoutOutputError {}

#[test]
fn split_srm_test() -> TestResult<()> {
  let data = DataPrepare::new()?;

  let srm_files = data.get_saves(&SaveType::Srm)?;

  let split_run = run(
    &srm_files,
    &data,
    CmdOptions::new(CmdMode::Auto).set_out_dir(&data),
  )?;

  split_run.success();

  // now check the files in the output directory...

  let mut map = HashMap::<OsString, Vec<PathBuf>>::new();

  for entry in data.out_dir().read_dir()? {
    let entry = entry?;
    if entry.file_type()?.is_file() {
      let path = entry.path();
      let Some(name) = path.file_stem() else {
        continue;
      };
      let vec = map.entry(name.to_os_string()).or_default();
      vec.push(entry.path());
    }
  }

  assert!(map.len() >= srm_files.len());

  for srm in srm_files {
    let name = srm.file_stem().expect("srm without a name").to_os_string();
    let Some(files) = map.get(&name) else {
      return Err(Box::new(SrmWithoutOutputError{}));
    };

    let create_run = run(
      files,
      &data,
      CmdOptions::new(CmdMode::Create).set_out_dir(&data),
    )?;

    create_run.success();

    data
      .out_dir()
      .child({
        let mut p = PathBuf::from(name);
        p.set_extension("srm");
        p
      })
      .assert(predicate::path::eq_file(srm));
  }

  Ok(())
}
