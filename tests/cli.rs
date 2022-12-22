use assert_cmd::prelude::*;
use assert_fs::{fixture::ChildPath, prelude::*, TempDir};
use predicates::prelude::*;
use std::{
  error::Error,
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
  //Srm,
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
      //SaveType::Srm => "srm",
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

fn run_test(data: &DataPrepare, save_type: SaveType) -> TestResult<()> {
  let name = format!("{save_type}.srm");

  let mut cmd = Command::cargo_bin("ra_mp64_srm_convert")?;
  let input_saves = data.get_saves(&save_type)?;

  if save_type.is_big_endian() {
    cmd.arg("--change-endianness");
  }

  if save_type == SaveType::MupenControllerPack {
    cmd.arg("--merge-mempacks");
  }

  cmd
    .current_dir(data.test_data_dir())
    .arg("-c")
    .arg(&name)
    .args(&input_saves)
    .args([PathBuf::from("--output-dir"), data.out_dir().to_path_buf()])
    .assert()
    .success();

  // assert that the file is there
  data
    .out_dir()
    .child(&name)
    .assert(predicate::path::is_file());

  // get the output names
  let output_saves = input_saves
    .iter()
    .map(|p| p.file_name().unwrap().into())
    .collect::<Vec<PathBuf>>();

  // an srm should exist now... if we were to split this one, its contents should match the input
  let mut cmd = Command::cargo_bin("ra_mp64_srm_convert")?;

  if save_type.is_big_endian() {
    cmd.arg("--change-endianness");
  }

  if save_type == SaveType::MupenControllerPack {
    cmd.arg("--merge-mempacks");
  }

  cmd
    .current_dir(data.test_data_dir())
    .arg("-s")
    .arg(data.out_dir().child(&name).to_path_buf())
    .args(&output_saves)
    .args([PathBuf::from("--output-dir"), data.out_dir().to_path_buf()])
    .assert()
    .success();

  // assert that the file is out there...
  for out_save in &output_saves {
    data
      .out_dir()
      .child(out_save)
      .assert(predicate::path::is_file());
  }

  // now compare input and output
  for (input, output) in input_saves.iter().zip(&output_saves) {
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
  run_test(&data, SaveType::Sram(Endianness::Little))
}

#[test]
fn create_from_sra_le_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_test(&data, SaveType::Sram(Endianness::Little))
}

#[test]
fn create_from_fla_be_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_test(&data, SaveType::FlashRam(Endianness::Big))
}

#[test]
fn create_from_fla_le_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_test(&data, SaveType::FlashRam(Endianness::Little))
}

#[test]
fn create_from_eep_4k_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_test(&data, SaveType::Eeprom(4))
}

#[test]
fn create_from_eep_16k_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_test(&data, SaveType::Eeprom(16))
}

#[test]
fn create_from_mupen_mpk_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_test(&data, SaveType::MupenControllerPack)
}

#[test]
fn create_from_mpk1_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_test(&data, SaveType::ControllerPack(1))
}

#[test]
fn create_from_mpk2_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_test(&data, SaveType::ControllerPack(2))
}

#[test]
fn create_from_mpk3_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_test(&data, SaveType::ControllerPack(3))
}

#[test]
fn create_from_mpk4_test() -> TestResult<()> {
  let data = DataPrepare::new()?;
  run_test(&data, SaveType::ControllerPack(4))
}

// fn split_test(data: &DataPrepare, save_type: SaveType) -> TestResult<()> {
//   todo!()
// }

// #[test]
// fn split_srm_eep_mpk_test() -> TestResult<()> {
//   let data = DataPrepare::new()?;
//   split_test(&data, SaveType::Srm)?;
//   Ok(())
// }
