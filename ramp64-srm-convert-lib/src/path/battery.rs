//! Battery Path Types

use crate::io::Error;

#[derive(Clone, Debug, PartialEq)]
/// Represents a battery save path
pub struct BatteryPath {
  battery_type: BatteryType,
  path: std::path::PathBuf,
}

impl BatteryPath {
  fn new(path: impl Into<std::path::PathBuf>, battery_type: BatteryType) -> Self {
    Self {
      battery_type,
      path: path.into(),
    }
  }

  /// Gets the [BatteryType].
  pub fn battery_type(&self) -> BatteryType {
    self.battery_type
  }

  pub(crate) fn validate_type(&self) -> crate::io::Result<()> {
    let metadata = self.metadata()?;
    match self.battery_type {
      BatteryType::Eeprom => (0x1..=0x800).contains(&metadata.len()),
      BatteryType::Sram => (0x801..=0x8000).contains(&metadata.len()),
      BatteryType::FlashRam => (0x8001..=0x20000).contains(&metadata.len()),
    }
    .then_some(())
    .ok_or(crate::io::Error::InvalidSize)
  }
}

impl AsRef<std::path::Path> for BatteryPath {
  fn as_ref(&self) -> &std::path::Path {
    &self.path
  }
}

impl std::ops::Deref for BatteryPath {
  type Target = std::path::Path;

  fn deref(&self) -> &Self::Target {
    &self.path
  }
}

impl From<BatteryPath> for std::path::PathBuf {
  fn from(value: BatteryPath) -> Self {
    value.path
  }
}

/// The type of battery
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BatteryType {
  /// An EEPROM battery
  Eeprom,
  /// A SRAM battery
  Sram,
  /// A FLASHRAM battery
  FlashRam,
}

impl std::fmt::Display for BatteryType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      BatteryType::Eeprom => f.write_str("EEPROM"),
      BatteryType::Sram => f.write_str("SRAM"),
      BatteryType::FlashRam => f.write_str("FlashRAM"),
    }
  }
}

/// Tries to create a [BatteryPath], consuming the path if it succeeds.
///
/// # Examples
///
/// ```
/// use ramp64_srm_convert_lib::{to_battery, path::battery::BatteryType};
/// # use assert_fs::{TempDir, prelude::*};
///
/// // create a EEPROM from path extension
/// let eeprom = to_battery("data.eep".into());
/// assert!(eeprom.is_ok());
/// assert_eq!(eeprom.unwrap().battery_type(), BatteryType::Eeprom);
///
/// // create SRAM from existing file
/// let sra_path = std::path::PathBuf::from("data_sra");
/// # let tmp_dir = TempDir::new()?;
/// # let sra_path = tmp_dir.child(sra_path).to_path_buf();
/// # std::fs::File::create(&sra_path).and_then(|f| f.set_len(0x8000))?;
///
/// let sra = to_battery(sra_path);
/// assert!(sra.is_ok());
/// assert_eq!(sra.unwrap().battery_type(), BatteryType::Sram);
///
/// // will not consume path if file exists and does not match a battery
/// let mut bad_path = std::path::PathBuf::from("too_much_data");
/// # let bad_path = tmp_dir.child(bad_path).to_path_buf();
/// # std::fs::File::create(&bad_path).and_then(|f| f.set_len(0x48800))?;
/// let bad = to_battery(bad_path);
/// assert!(bad.is_err());
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
pub fn to_battery(path: std::path::PathBuf) -> Result<BatteryPath, (std::path::PathBuf, Error)> {
  match path.metadata() {
    Ok(metadata) => match metadata.len() {
      (0x1..=0x800) => Ok(BatteryPath::new(path, BatteryType::Eeprom)),
      (0x801..=0x8000) => Ok(BatteryPath::new(path, BatteryType::Sram)),
      (0x8001..=0x20000) => Ok(BatteryPath::new(path, BatteryType::FlashRam)),
      _ if metadata.is_dir() => Err((path, Error::PathIsDirectory)),
      _ => Err((path, Error::InvalidSize)),
    },
    Err(err) => {
      if err.kind() == std::io::ErrorKind::NotFound {
        let ext = path.extension().map(|s| s.to_ascii_uppercase());
        match ext.as_ref().and_then(|s| s.to_str()) {
          Some("EEP") => Ok(BatteryPath::new(path, BatteryType::Eeprom)),
          Some("SRA") => Ok(BatteryPath::new(path, BatteryType::Sram)),
          Some("FLA") => Ok(BatteryPath::new(path, BatteryType::FlashRam)),
          _ => Err((path, Error::InvalidExtension)),
        }
      } else {
        Err((path, err.into()))
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::{to_battery, BatteryType};

  use assert_fs::{prelude::*, TempDir};

  use std::path::Path;

  #[test]
  fn test_controller_pack_validity_checks() -> Result<(), Box<dyn std::error::Error>> {
    // create from path extension
    for (battery_type, ext) in [
      (BatteryType::Eeprom, "eep"),
      (BatteryType::Sram, "sra"),
      (BatteryType::FlashRam, "fla"),
    ] {
      let path = Path::new("data").with_extension(ext);
      let battery = to_battery(path).expect("non-existent path always good");
      assert_eq!(battery.battery_type(), battery_type);
    }

    let tmp_dir = TempDir::new()?;
    // create from existing file size
    for (index, (battery_type, len)) in [
      (BatteryType::Eeprom, 0x800),
      (BatteryType::Sram, 0x8000),
      (BatteryType::FlashRam, 0x20000),
    ]
    .into_iter()
    .enumerate()
    {
      let path = tmp_dir.child(format!("data_{index}")).to_path_buf();
      std::fs::File::create(&path).and_then(|f| f.set_len(len))?;

      let battery = to_battery(path).expect("existing file has required size");
      assert_eq!(battery.battery_type(), battery_type);
      battery.validate_type()?; // only when file exist
    }

    // will not consume path if file exists and does not match a battery
    let bad_path = tmp_dir.child("too_much_data").to_path_buf();
    std::fs::File::create(&bad_path).and_then(|f| f.set_len(0x48800))?;

    let bad = to_battery(bad_path);
    assert!(bad.is_err());

    Ok(())
  }
}
