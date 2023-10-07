use assert_cmd::{assert::Assert, prelude::*};
use assert_fs::prelude::{PathAssert, PathChild, PathCopy, PathCreateDir};
use predicates::prelude::*;
use std::collections::BTreeMap;
use std::ffi::OsString;
use std::fs::{self};
use std::path::{Path, PathBuf};
use std::process::Command;
use tests_common::*;

fn run_split_test(data: &DataPrepare, save_type: SaveType) -> TestResult<()> {
  tests_common::run_split_test("ra_mp64_srm_convert", data, save_type)
}

fn run(input_files: &Vec<PathBuf>, cwd: impl AsRef<Path>, opts: CmdOptions) -> TestResult<Assert> {
  tests_common::run("ra_mp64_srm_convert", input_files, cwd, opts)
}

#[test]
fn auto_missing_srm_file() -> TestResult<()> {
  let mut cmd = Command::cargo_bin("ra_mp64_srm_convert")?;
  cmd
    .args(&["-v", "debug"])
    .arg("missing.srm")
    .assert()
    .failure()
    .stderr(predicate::str::contains("\"missing\": no input files"));
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
    .stderr(predicate::str::contains("does not exist"));
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

  let mut map = BTreeMap::<OsString, Vec<PathBuf>>::new();

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
      return Err(Box::new(SrmWithoutOutputError {}));
    };

    let create_run = run(
      files,
      data.test_data_dir(),
      CmdOptions::new(CmdMode::Create).set_out_dir(data.out_dir()),
    )?;

    create_run.success();

    if let Ok(len) = srm.metadata().map(|m| m.len()) {
      if len < 0x48800 {
        let data = unrzip(&srm)?;
        std::fs::write(&srm, &data)?;
      }
    }

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

  let mut map = BTreeMap::<OsString, Vec<PathBuf>>::new();

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
          return Err(Box::new(NonMupenMpkFileError::from(path)));
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
      return Err(Box::new(SrmWithoutOutputError {}));
    };

    let create_run = run(
      files,
      data.test_data_dir(),
      CmdOptions::new(CmdMode::Create).set_out_dir(data.out_dir()),
    )?;

    create_run.success();

    if let Ok(len) = srm.metadata().map(|m| m.len()) {
      if len < 0x48800 {
        let data = unrzip(&srm)?;
        std::fs::write(&srm, &data)?;
      }
    }

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
  out_dir.child("A.mpk").assert(predicate::path::is_file());
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
    predicate::str::contains("will overwrite \"C.srm\"")
      .and(predicate::str::contains("will overwrite \"D.sra\"")),
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
    "A.sra", "A.eep", "A.fla", "A.mpk1", "A.mpk2", "A.mpk3", "A.mpk4",
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
  let out = create1_run
    .success()
    .stdout(
      predicate::str::is_match(r#"Use (?:[^ ]+) for SRM file name"#)
        .unwrap()
        .not(),
    )
    .get_output()
    .stdout
    .clone();

  let out_str = String::from_utf8(out).unwrap();
  if out_str.is_empty() {
    panic!();
  };

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

  out_dir.child("A.eep").assert(predicate::path::is_file());
  out_dir.child("A.sra").assert(predicate::path::is_file());
  out_dir.child("A.fla").assert(predicate::path::is_file());
  out_dir.child("A.mpk1").assert(predicate::path::is_file());
  out_dir.child("A.mpk2").assert(predicate::path::is_file());
  out_dir.child("A.mpk3").assert(predicate::path::is_file());
  out_dir.child("A.mpk4").assert(predicate::path::is_file());

  Ok(())
}
