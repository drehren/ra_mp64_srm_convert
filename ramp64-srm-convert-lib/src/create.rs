//! Create a SRM file from save/controller pack files

use crate::io::ReadExt;
use crate::path::{BatteryPath, BatteryType, ControllerPackPaths};
use crate::utils::word_swap;
use crate::{Converter, UserParams};

use std::io::prelude::*;

/// Parameters to create a SRM file
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Params {
  battery: Option<BatteryPath>,
  controller_pack: Option<ControllerPackPaths>,
  out_dir: Option<std::path::PathBuf>,
  name: Option<String>,
}

impl Params {
  /// Sets the battery save file
  pub fn set_battery(self, battery: BatteryPath) -> Self {
    Self {
      battery: Some(battery),
      ..self
    }
  }

  /// Sets the controller pack file
  pub fn set_controller_pack(mut self, controller_pack: ControllerPackPaths) -> Self {
    match &mut self.controller_pack {
      Some(cp) => cp.replace_from(controller_pack),
      self_cp => self_cp.replace(controller_pack),
    };
    self
  }

  /// Sets the output directory
  pub fn set_out_dir<P>(self, out_dir: Option<P>) -> Self
  where
    P: Into<std::path::PathBuf>,
  {
    Params {
      battery: self.battery,
      out_dir: out_dir.map(|v| v.into()),
      controller_pack: self.controller_pack,
      name: self.name,
    }
  }

  /// Sets the name of the file
  pub fn set_name<S>(self, name: Option<S>) -> Self
  where
    S: Into<String>,
  {
    Params {
      battery: self.battery,
      out_dir: self.out_dir,
      controller_pack: self.controller_pack,
      name: name.map(|v| v.into()),
    }
  }

  /// Gets an mut instance
  pub fn as_mut(&mut self) -> ParamsMut<'_> {
    ParamsMut(self)
  }
}

/// Safely mutates a [Params] reference
pub struct ParamsMut<'params>(&'params mut Params);

impl<'params> ParamsMut<'params> {
  /// Sets the controller pack
  pub fn set_controller_pack(
    &mut self,
    controller_pack: ControllerPackPaths,
  ) -> Option<ControllerPackPaths> {
    match &mut self.0.controller_pack {
      Some(self_cp) => self_cp.replace_from(controller_pack),
      self_cp => self_cp.replace(controller_pack),
    }
  }

  /// Sets the battery save
  pub fn set_battery(&mut self, battery: BatteryPath) -> Option<BatteryPath> {
    self.0.battery.replace(battery)
  }

  /// Sets the output directory
  pub fn set_out_dir<P>(&mut self, out_dir: Option<P>) -> &mut Self
  where
    P: Into<std::path::PathBuf>,
  {
    self.0.out_dir = out_dir.map(|v| v.into());
    self
  }

  /// Sets the name
  pub fn set_name<S>(&mut self, name: Option<S>) -> &mut Self
  where
    S: Into<String>,
  {
    self.0.name = name.map(|v| v.into());
    self
  }
}

/// Enumerates validation values
#[derive(Debug)]
pub enum Validation {
  /// Validation succeeded
  Ok,
  /// No battery or controller pack file
  NoInputFiles,
  /// Battery file validation error
  BatteryError(crate::io::Error),
  /// Controller pack validation error
  ControllerPackError(crate::io::Error),
  /// Output directory is not a directory
  OutputDirIsNotADir,
  /// Additional validation Error
  And(Box<Validation>, Box<Validation>),
}

impl Validation {
  /// Checks if the validation succeeded
  pub fn is_ok(&self) -> bool {
    matches!(self, Validation::Ok)
  }
}

impl std::fmt::Display for Validation {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Validation::Ok => f.write_str("OK"),
      Validation::NoInputFiles => f.write_str("no input files"),
      Validation::BatteryError(err) => {
        f.write_fmt(format_args!("failed battery file check: {err}"))
      }
      Validation::ControllerPackError(err) => {
        f.write_fmt(format_args!("failed controller pack check {err}"))
      }
      Validation::OutputDirIsNotADir => f.write_str("output directory is not a directory"),
      Validation::And(lhs, rhs) => {
        lhs.as_ref().fmt(f)?;
        f.write_str("; and ")?;
        rhs.as_ref().fmt(f)
      }
    }
  }
}

impl std::ops::AddAssign for Validation {
  fn add_assign(&mut self, rhs: Self) {
    if matches!(self, Validation::Ok) {
      *self = rhs;
    } else if !matches!(rhs, Validation::Ok) {
      *self = Validation::And(
        Box::new(std::mem::replace(self, Validation::Ok)),
        Box::new(rhs),
      );
    }
  }
}

impl From<Validation> for bool {
  fn from(value: Validation) -> Self {
    matches!(value, Validation::Ok)
  }
}

impl Converter for Params {
  type Error = super::Error;

  type Validation = Validation;

  #[must_use]
  fn validate(&self) -> Self::Validation {
    let mut validation = Validation::Ok;

    let mut no_battery = true;
    if let Some(battery) = &self.battery {
      if battery.exists() {
        validation += battery
          .validate_type()
          .map(|_| Validation::Ok)
          .unwrap_or_else(Validation::BatteryError);
        no_battery = false;
      }
    }
    let mut no_cp = true;
    if let Some(cp) = &self.controller_pack {
      if cp.exists() {
        no_cp = false;
        validation += if cp.is_valid_size() {
          Validation::Ok
        } else {
          Validation::ControllerPackError(crate::io::Error::InvalidSize)
        }
      }
    }

    if no_battery && no_cp {
      validation += Validation::NoInputFiles;
    }

    if !self.out_dir.as_ref().map_or(true, |d| d.is_dir()) {
      validation += Validation::OutputDirIsNotADir
    }

    validation
  }

  fn convert(self, user_params: &UserParams) -> Result<(), Self::Error> {
    let Self {
      battery: battery_file,
      controller_pack,
      out_dir,
      name,
    } = self;

    let mut srm_data = super::SrmBuf::new();

    let out_path = {
      let main_path = battery_file
        .as_deref()
        .or(controller_pack.as_deref())
        .unwrap()
        .to_owned();

      let name = name.map_or_else(
        || main_path.file_name().map(std::path::PathBuf::from).unwrap(),
        std::path::PathBuf::from,
      );

      out_dir
        .map_or_else(|| main_path.with_file_name(&name), |o| o.join(&name))
        .with_extension("srm")
    };

    if out_path.is_file() {
      // ignore any error here... we only do as best effort
      let _ = std::fs::File::open(&out_path).and_then(|mut file| file.read_up_to(&mut srm_data));
    }

    if let Some(battery_save) = battery_file {
      let data_buf = match battery_save.battery_type() {
        BatteryType::Eeprom => srm_data.eeprom_mut(),
        BatteryType::Sram => srm_data.sram_mut(),
        BatteryType::FlashRam => srm_data.flashram_mut(),
      };

      std::fs::File::open(&battery_save)
        .and_then(|mut file| file.read_up_to(data_buf))
        .map_err(|e| super::Error(battery_save.into(), e))?;
    }

    if let Some(controller_pack) = controller_pack {
      if controller_pack.is_mupen() {
        std::fs::File::open(&controller_pack)
          .and_then(|mut file| file.read_up_to(srm_data.full_controller_pack_mut()))
          .map_err(|e| super::Error(controller_pack.into(), e))?;
      } else {
        let mut packs = srm_data.controller_pack_iter_mut().collect::<Vec<_>>();
        for (i, path) in controller_pack.into_indexed_paths() {
          std::fs::File::open(&path)
            .and_then(|mut file| file.read_up_to(packs[i]))
            .map_err(|e| super::Error(path, e))?;
        }
      }
    }

    if user_params.swap_bytes {
      word_swap(srm_data.eeprom_mut());
      word_swap(srm_data.flashram_mut());
    }

    std::fs::OpenOptions::new()
      .create(user_params.overwrite)
      .create_new(!user_params.overwrite)
      .write(true)
      .open(&out_path)
      .and_then(|mut file| file.write_all(&srm_data))
      .map_err(|e| super::Error(out_path, e))
  }
}

#[cfg(test)]
mod tests {
  use super::{Params, Validation};
  use crate::path::{battery::to_battery, controller_pack::to_controller_pack};
  use crate::Converter;

  use assert_fs::{prelude::PathChild, TempDir};

  use std::io::Write;

  type TestResult = Result<(), Box<dyn std::error::Error>>;

  #[test]
  fn test_validation_no_input() {
    let params = Params::default()
      .set_battery(to_battery("save.eep".into()).expect("New path always OK"))
      .set_controller_pack(to_controller_pack("cp.mpk1".into()).expect("New path always OK"));

    assert!(matches!(params.validate(), Validation::NoInputFiles));
  }

  #[test]
  fn test_validation_battery_input() -> TestResult {
    let file_creator = FileCreator::new()?;
    let eep_path = file_creator.from_name_and_size("save.eep", 0x500)?;

    let params = Params::default()
      .set_battery(to_battery(eep_path).expect("eep correct size"))
      .set_controller_pack(to_controller_pack("cp.mpk1".into()).expect("New path always OK"));

    assert!(params.validate().is_ok());
    Ok(())
  }

  #[test]
  fn test_validation_bad_battery() -> TestResult {
    let file_creator = FileCreator::new()?;
    let bad_sra_path = file_creator.from_name_and_size("bad.sra", 0x8000)?;
    let sra = to_battery(bad_sra_path).expect("New path always OK");

    // now make it bad
    std::fs::File::create(&sra).and_then(|f| f.set_len(0x20012))?;

    let params = Params::default().set_battery(sra);
    assert!(matches!(
      params.validate(),
      Validation::BatteryError(crate::io::Error::InvalidSize)
    ));
    Ok(())
  }

  #[test]
  fn test_validation_controller_pack_input() -> TestResult {
    let file_creator = FileCreator::new()?;
    let cp_path = file_creator.from_name("cp.mpk1", |mut f| {
      let mut buf = Box::new([0; 0x8000]);
      crate::path::controller_pack::init(buf.as_mut());
      f.write_all(buf.as_ref())
    })?;

    let params = Params::default()
      .set_battery(to_battery("save.eep".into()).expect("New path always OK"))
      .set_controller_pack(to_controller_pack(cp_path).expect("mpk1 correct size"));

    assert!(params.validate().is_ok());
    Ok(())
  }

  struct FileCreator {
    tmp_dir: TempDir,
  }

  impl FileCreator {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
      Ok(Self {
        tmp_dir: TempDir::new()?,
      })
    }

    fn from_name_and_size(
      &self,
      name: impl AsRef<std::path::Path>,
      size: u64,
    ) -> Result<std::path::PathBuf, Box<dyn std::error::Error>> {
      let path = self.tmp_dir.child(name);
      std::fs::File::create(&path).and_then(|f| f.set_len(size))?;
      Ok(path.to_path_buf())
    }

    fn from_name(
      &self,
      name: impl AsRef<std::path::Path>,
      init: impl FnOnce(std::fs::File) -> std::io::Result<()>,
    ) -> Result<std::path::PathBuf, Box<dyn std::error::Error>> {
      let path = self.tmp_dir.child(name);
      std::fs::File::create(&path).and_then(init)?;
      Ok(path.to_path_buf())
    }
  }
}
