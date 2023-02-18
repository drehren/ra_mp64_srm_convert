use assert_cmd::{assert::Assert, prelude::*};
use assert_fs::{fixture::ChildPath, prelude::*, TempDir};
use predicates::prelude::*;
use std::{
  collections::HashMap,
  error::Error,
  ffi::OsString,
  fmt::Display,
  fs,
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
    .stderr(predicate::str::contains("missing.srm does not exist"));
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
    .stderr(predicate::str::contains("missing.srm does not exist"));
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
    .stderr(predicate::str::contains("no input files"));
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
enum Player {
  P1,
  P2,
  P3,
  P4,
}

#[derive(Debug, PartialEq, Eq)]
enum EepromSize {
  _4Kbit,
  _16Kbit,
}

#[derive(Debug, PartialEq, Eq)]
enum SaveType {
  FlashRam(Endianness),
  MupenControllerPack,
  ControllerPack(Player),
  Sram(Endianness),
  Eeprom(EepromSize),
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
        &Player::P1 => "mpk1",
        &Player::P2 => "mpk2",
        &Player::P3 => "mpk3",
        &Player::P4 => "mpk4",
      },
      SaveType::Eeprom(size) => match size {
        &EepromSize::_16Kbit => "eep_16k",
        &EepromSize::_4Kbit => "eep_4k",
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

  fn get_saves<P>(&self, child_path: &P) -> TestResult<Vec<PathBuf>>
  where
    P: AsRef<Path> + ?Sized,
  {
    let save_path = self.test_data_dir().child(child_path);

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
  overwrite: bool,
  change_endianness: bool,
  merge_mempacks: bool,
  out_dir: Option<PathBuf>,
  srm: Option<PathBuf>,
}

impl CmdOptions {
  fn new(mode: CmdMode) -> Self {
    Self {
      mode,
      ..Default::default()
    }
  }

  fn set_from_save_type(mut self, save_type: &SaveType) -> Self {
    self.change_endianness = save_type.is_big_endian();
    self.merge_mempacks = save_type == &SaveType::MupenControllerPack;
    self
  }

  fn set_merge_mempacks(self, merge_mempacks: bool) -> Self {
    Self {
      merge_mempacks,
      ..self
    }
  }

  fn set_overwrite(mut self, overwrite: bool) -> Self {
    self.overwrite = overwrite;
    self
  }

  fn set_out_dir<P>(mut self, out_dir: P) -> Self
  where
    P: AsRef<Path>,
  {
    self.out_dir = Some(out_dir.as_ref().to_owned());
    self
  }

  fn set_srm(mut self, srm: Option<PathBuf>) -> Self {
    self.srm = srm;
    self
  }

  fn apply(self, cmd: &mut Command) {
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

fn run<P>(input_files: &Vec<PathBuf>, cwd: P, opts: CmdOptions) -> TestResult<Assert>
where
  P: AsRef<Path>,
{
  let mut cmd = Command::cargo_bin("ra_mp64_srm_convert")?;

  opts.apply(cmd.current_dir(cwd));

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

fn run_split_test(data: &DataPrepare, save_type: SaveType) -> TestResult<()> {
  let input_files = data.get_saves(&save_type)?;
  if input_files.is_empty() {
    return Err(Box::new(NoInputFilesError(save_type)));
  }

  let name = input_files[0]
    .file_name()
    .map(|n| Path::new(n).with_extension("srm"))
    .expect("file should have a name");

  let create_run = run(
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

#[test]
fn create_from_sra_be_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_split_test(&data, SaveType::Sram(Endianness::Little))
}

#[test]
fn create_from_sra_le_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_split_test(&data, SaveType::Sram(Endianness::Little))
}

#[test]
fn create_from_fla_be_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_split_test(&data, SaveType::FlashRam(Endianness::Big))
}

#[test]
fn create_from_fla_le_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_split_test(&data, SaveType::FlashRam(Endianness::Little))
}

#[test]
fn create_from_eep_4k_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_split_test(&data, SaveType::Eeprom(EepromSize::_4Kbit))
}

#[test]
fn create_from_eep_16k_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_split_test(&data, SaveType::Eeprom(EepromSize::_16Kbit))
}

#[test]
fn create_from_mupen_mpk_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_split_test(&data, SaveType::MupenControllerPack)
}

#[test]
fn create_from_mpk1_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_split_test(&data, SaveType::ControllerPack(Player::P1))
}

#[test]
fn create_from_mpk2_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_split_test(&data, SaveType::ControllerPack(Player::P2))
}

#[test]
fn create_from_mpk3_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_split_test(&data, SaveType::ControllerPack(Player::P3))
}

#[test]
fn create_from_mpk4_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_split_test(&data, SaveType::ControllerPack(Player::P4))
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
    data.test_data_dir(),
    CmdOptions::new(CmdMode::Auto).set_out_dir(data.out_dir()),
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
      data.test_data_dir(),
      CmdOptions::new(CmdMode::Create).set_out_dir(data.out_dir()),
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

#[derive(Debug)]
struct NonMupenMpkFileError(PathBuf);
impl Display for NonMupenMpkFileError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "{} is not a Mupen64 Mempack file",
      self.0.display()
    ))
  }
}
impl Error for NonMupenMpkFileError {}

#[test]
fn verify_split_srm_mupen_mempack() -> TestResult<()> {
  let data = DataPrepare::new()?;

  let srm_files = data.get_saves(&SaveType::Srm)?;

  let split_run = run(
    &srm_files,
    data.test_data_dir(),
    CmdOptions::new(CmdMode::Auto)
      .set_merge_mempacks(true)
      .set_out_dir(data.out_dir()),
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
      // we cannot have non 128K mpkX files
      if let Some("mpk" | "mpk1" | "mpk2" | "mpk3" | "mpk4") = path
        .extension()
        .expect("File should have an extension")
        .to_str()
      {
        if fs::metadata(&path)?.len() != 0x20000 {
          return Err(Box::new(NonMupenMpkFileError(path)));
        }
      }
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
      data.test_data_dir(),
      CmdOptions::new(CmdMode::Create).set_out_dir(data.out_dir()),
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

#[test]
fn verify_split_srm_mupen_mempack_2() -> TestResult<()> {
  let data = DataPrepare::new()?;

  let split_run = run(
    &vec!["A.srm".into(), "F.mpk3".into()],
    data.test_data_dir().child("auto"),
    CmdOptions::new(CmdMode::Split)
      .set_merge_mempacks(true)
      .set_out_dir(data.out_dir()),
  )?;

  split_run
    .success()
    .stdout(predicate::str::contains("Could not set controller pack as a Mupen file").not());

  let out_dir = data.out_dir();

  out_dir.child("A.eep").assert(predicate::path::is_file());
  out_dir.child("A.sra").assert(predicate::path::is_file());
  out_dir.child("A.fla").assert(predicate::path::is_file());
  out_dir.child("F.mpk3").assert(predicate::path::is_file());
  out_dir.child("A.mpk").assert(predicate::path::missing());
  out_dir.child("A.mpk1").assert(predicate::path::missing());
  out_dir.child("A.mpk2").assert(predicate::path::missing());
  out_dir.child("A.mpk3").assert(predicate::path::missing());
  out_dir.child("A.mpk4").assert(predicate::path::missing());

  Ok(())
}

#[test]
fn verify_group_by_filename() -> TestResult<()> {
  let data = DataPrepare::new()?;

  let in_saves = data.out_dir().child("auto");
  in_saves.create_dir_all()?;
  in_saves.copy_from(
    {
      let source = data.test_data_dir().child("auto");
      source.assert(predicate::path::is_dir());
      source
    },
    &["*"],
  )?;

  // now build the file paths
  let mut input_saves = Vec::with_capacity(9);
  for name in [
    "A.srm", "B.mpk", "B.eep", "C.fla", "C.srm", "D.srm", "D.sra", "F.mpk1", "F.mpk3",
  ] {
    input_saves.push(name.into());
  }

  let group_run = run(
    &input_saves,
    data.out_dir().child("auto"),
    CmdOptions::new(CmdMode::Auto),
  )?;

  group_run.failure().stderr(
    predicate::str::contains("will overwrite existing C.srm")
      .and(predicate::str::contains("will overwrite existing D.sra")),
  );

  let group_run = run(
    &input_saves,
    data.out_dir().child("auto"),
    CmdOptions::new(CmdMode::Auto).set_overwrite(true),
  )?;

  group_run.success();

  Ok(())
}

#[test]
fn verify_create_srm() -> TestResult<()> {
  let data = DataPrepare::new()?;

  let in_saves = data.out_dir().child("auto");
  in_saves.create_dir_all()?;
  in_saves.copy_from(
    {
      let source = data.test_data_dir().child("auto");
      source.assert(predicate::path::is_dir());
      source
    },
    &["*"],
  )?;

  let mut input_files = Vec::with_capacity(9);
  for name in [
    "A.srm", "B.mpk", "B.eep", "C.fla", "C.srm", "D.srm", "D.sra", "F.mpk1", "F.mpk3",
  ] {
    input_files.push(name.into());
  }

  let create1_run = run(
    &input_files,
    in_saves,
    CmdOptions::new(CmdMode::Create).set_out_dir(data.out_dir().child("auto_out")),
  )?;
  create1_run.success();

  data
    .out_dir()
    .child("auto_out")
    .assert(predicate::path::is_dir());

  data
    .out_dir()
    .child("auto_out/D.srm")
    .assert(predicate::path::is_file());

  Ok(())
}

#[test]
fn verify_split_srm() -> TestResult<()> {
  let data = DataPrepare::new()?;

  let in_saves = data.out_dir().child("auto");
  in_saves.create_dir_all()?;
  in_saves.copy_from(
    {
      let source = data.test_data_dir().child("auto");
      source.assert(predicate::path::is_dir());
      source
    },
    &["*"],
  )?;

  let mut input_files = Vec::with_capacity(9);
  for name in [
    "A.srm", "B.mpk", "B.eep", "C.fla", "C.srm", "D.srm", "D.fla", "F.mpk1", "F.mpk3",
  ] {
    input_files.push(name.into());
  }

  let out_dir = data.out_dir().child("auto_out");

  let create1_run = run(
    &input_files,
    in_saves,
    CmdOptions::new(CmdMode::Split).set_out_dir(&out_dir),
  )?;
  create1_run.success();

  out_dir.assert(predicate::path::is_dir());

  let read_dir = out_dir.read_dir()?;
  assert_eq!(read_dir.count(), 7);

  for name in [
    "D.sra", "B.eep", "D.fla", "F.mpk1", "D.mpk2", "F.mpk3", "D.mpk4",
  ] {
    out_dir.child(name).assert(predicate::path::is_file());
  }

  Ok(())
}

#[test]
fn verify_tl_dr_create_1() -> TestResult<()> {
  let data = DataPrepare::new()?;

  let in_saves = data.out_dir().child("auto");
  in_saves.create_dir_all()?;
  in_saves.copy_from(
    {
      let source = data.test_data_dir().child("auto");
      source.assert(predicate::path::is_dir());
      source
    },
    &["*"],
  )?;

  let mut input_files = Vec::with_capacity(9);
  for name in ["B.mpk", "B.eep", "C.fla", "F.mpk3"] {
    input_files.push(name.into());
  }

  let out_dir = data.out_dir().child("auto_out");

  let create1_run = run(
    &input_files,
    in_saves,
    CmdOptions::new(CmdMode::Create).set_out_dir(&out_dir),
  )?;
  create1_run.success();

  out_dir.child("B.srm").assert(predicate::path::is_file());

  Ok(())
}

#[test]
fn verify_tl_dr_create_2() -> TestResult<()> {
  let data = DataPrepare::new()?;

  let in_saves = data.out_dir().child("auto");
  in_saves.create_dir_all()?;
  in_saves.copy_from(
    {
      let source = data.test_data_dir().child("auto");
      source.assert(predicate::path::is_dir());
      source
    },
    &["*"],
  )?;

  let mut input_files = Vec::with_capacity(9);
  for name in ["B.mpk", "B.eep", "C.fla", "F.mpk3", "C.srm"] {
    input_files.push(name.into());
  }

  let out_dir = data.out_dir().child("auto_out");

  let create1_run = run(
    &input_files,
    in_saves,
    CmdOptions::new(CmdMode::Create).set_out_dir(&out_dir),
  )?;
  create1_run.success();

  out_dir.child("C.srm").assert(predicate::path::is_file());

  Ok(())
}

#[test]
fn verify_tl_dr_split_1() -> TestResult<()> {
  let data = DataPrepare::new()?;

  let in_saves = data.out_dir().child("auto");
  in_saves.create_dir_all()?;
  in_saves.copy_from(
    {
      let source = data.test_data_dir().child("auto");
      source.assert(predicate::path::is_dir());
      source
    },
    &["*"],
  )?;

  let input_files = vec!["A.srm".into()];

  let out_dir = data.out_dir().child("auto_out");

  let create1_run = run(
    &input_files,
    in_saves,
    CmdOptions::new(CmdMode::Split).set_out_dir(&out_dir),
  )?;
  create1_run.success();

  out_dir.child("A.eep").assert(predicate::path::is_file());
  out_dir.child("A.sra").assert(predicate::path::is_file());
  out_dir.child("A.fla").assert(predicate::path::is_file());
  out_dir.child("A.mpk1").assert(predicate::path::is_file());
  out_dir.child("A.mpk2").assert(predicate::path::is_file());
  out_dir.child("A.mpk3").assert(predicate::path::is_file());
  out_dir.child("A.mpk4").assert(predicate::path::is_file());

  Ok(())
}

#[test]
fn verify_tl_dr_split_2() -> TestResult<()> {
  let data = DataPrepare::new()?;

  let in_saves = data.out_dir().child("auto");
  in_saves.create_dir_all()?;
  in_saves.copy_from(
    {
      let source = data.test_data_dir().child("auto");
      source.assert(predicate::path::is_dir());
      source
    },
    &["*"],
  )?;

  let mut input_files = Vec::with_capacity(9);
  for name in ["B.mpk", "B.eep", "C.fla", "F.mpk3", "A.srm"] {
    input_files.push(name.into());
  }

  let out_dir = data.out_dir().child("auto_out");

  let create1_run = run(
    &input_files,
    in_saves,
    CmdOptions::new(CmdMode::Split).set_out_dir(&out_dir),
  )?;
  create1_run.success();

  out_dir.child("B.eep").assert(predicate::path::is_file());
  out_dir.child("A.sra").assert(predicate::path::is_file());
  out_dir.child("C.fla").assert(predicate::path::is_file());
  out_dir.child("B.mpk").assert(predicate::path::is_file());
  out_dir.child("A.mpk2").assert(predicate::path::is_file());
  out_dir.child("F.mpk3").assert(predicate::path::is_file());
  out_dir.child("A.mpk4").assert(predicate::path::is_file());

  Ok(())
}
