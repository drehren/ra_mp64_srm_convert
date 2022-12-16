use assert_cmd::prelude::*;
use assert_fs::{prelude::*, TempDir};
use predicates::prelude::*;
use std::{
  error::Error,
  ffi::{OsStr, OsString},
  io::Cursor,
  path::PathBuf,
  process::Command,
};

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

