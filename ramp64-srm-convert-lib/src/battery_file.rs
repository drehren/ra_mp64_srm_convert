use std::{
  fmt, fs,
  hash::Hash,
  ops::Deref,
  path::{Path, PathBuf},
  result,
};

use either::Either;

use crate::{is_rzip_file, ConvertibleFile, ConvertibleType, CreateError};

impl<P> ConvertibleFile<P>
where
  P: AsRef<Path>,
{
  /// Checks if this [ConvertibleFile] is a [BatteryFile]
  pub fn is_battery(&self) -> bool {
    matches!(self.part_type, ConvertibleType::Battery { .. })
  }

  /// Gets the [BatteryFile]
  pub fn try_into_battery(self) -> Either<BatteryFile, Self> {
    if !self.is_battery() {
      return Either::Right(self);
    }
    let (size, extension) = match self.part_type {
      ConvertibleType::Battery { size, extension } => (size, extension),
      _ => unreachable!(),
    };

    match (size, extension.as_deref()) {
      (Some(0x1..=0x800), _) | (_, Some("EEP")) => Either::Left(BatteryFile {
        save_type: BatteryType::Eeprom,
        path: self.path.as_ref().into(),
      }),
      (Some(0x801..=0x8000), _) | (_, Some("SRA")) => Either::Left(BatteryFile {
        save_type: BatteryType::Sram,
        path: self.path.as_ref().into(),
      }),
      (Some(0x8001..=0x20000), _) | (_, Some("FLA")) => Either::Left(BatteryFile {
        save_type: BatteryType::FlashRam,
        path: self.path.as_ref().into(),
      }),
      _ => unreachable!(),
    }
  }
}

/// Defines an specific save file
#[derive(Debug, Clone, PartialEq)]
pub struct BatteryFile {
  save_type: BatteryType,
  path: PathBuf,
}

impl BatteryFile {
  /// Tries to create an [BatteryFile] from the specified path and type.
  pub fn try_new(path: impl AsRef<Path>, save_type: BatteryType) -> Result<Self, CreateError> {
    use BatteryType::*;

    if !path.as_ref().exists() {
      return Ok(Self {
        path: path.as_ref().to_path_buf(),
        save_type,
      });
    }
    if !path.as_ref().is_file() {
      return Err(CreateError::IsADir);
    }

    // Now try check.. don't like reading the actual file but there is nothing I can do

    fs::File::open(&path)
      .map_err(CreateError::Other)
      .and_then(|mut f| {
        let file_len = f.metadata()?.len();

        if file_len == 0 {
          return Err(CreateError::FileEmpty);
        } else if file_len >= 8 && is_rzip_file(&mut f)? {
          // first check that the e
          return Err(CreateError::IsASrm);
        }

        let is_expected_len = match save_type {
          Eeprom => (0x1..=0x800).contains(&file_len),
          Sram => (0x801..=0x8000).contains(&file_len),
          FlashRam => (0x8001..=0x20000).contains(&file_len),
        };

        if is_expected_len {
          Ok(Self {
            path: path.as_ref().to_path_buf(),
            save_type,
          })
        } else {
          match file_len {
            0x1..=0x800 => Err(CreateError::AnotherType(Eeprom)),
            0x801..=0x8000 => Err(CreateError::AnotherType(Sram)),
            0x8001..=0x20000 => Err(CreateError::AnotherType(FlashRam)),
            _ => Err(CreateError::FileTooLarge),
          }
        }
      })
  }

  /// Gets the [BatteryType] of this [BatteryFile].
  pub fn battery_type(&self) -> BatteryType {
    self.save_type
  }

  pub(crate) fn from_file_len<P: AsRef<Path>>(path: P) -> result::Result<Self, CreateError> {
    fs::File::open(&path)
      .map_err(|e| e.into())
      .and_then(|mut file| {
        let file_len = file.metadata()?.len();

        if file_len == 0 {
          return Err(CreateError::FileEmpty);
        } else if file_len > 8 && is_rzip_file(&mut file)? {
          return Err(CreateError::IsASrm);
        }

        match file_len {
          // 4Kbit or 16Kbit eeprom
          0x1..=0x800 => Ok(BatteryType::Eeprom),
          // 256Kbit sram or controller pack
          0x801..=0x8000 => Ok(BatteryType::Sram),
          // 1Mbit flash ram or 4 merged controller packs
          0x8001..=0x20000 => Ok(BatteryType::FlashRam),
          // uncompressed retroarch srm save size
          0x48800 => Err(CreateError::IsASrm),
          // unknown
          _ => Err(CreateError::FileTooLarge),
        }
      })
      .map(|save_type| Self {
        save_type,
        path: path.as_ref().to_path_buf(),
      })
  }

  pub(crate) fn from_extension<P: AsRef<Path>>(path: P) -> result::Result<Self, CreateError> {
    use BatteryType::*;
    path
      .as_ref()
      .extension()
      .ok_or_else(|| CreateError::NoExtension)
      .and_then(|ext| match ext.to_ascii_uppercase().to_str() {
        Some("SRA") => Ok(Sram),
        Some("FLA") => Ok(FlashRam),
        Some("EEP") => Ok(Eeprom),
        Some("SRM") => Err(CreateError::IsASrm),
        _ => Err(CreateError::UnknownExtension),
      })
      .map(|save_type| Self {
        save_type,
        path: path.as_ref().to_path_buf(),
      })
  }
}

impl TryFrom<&str> for BatteryFile {
  type Error = CreateError;

  fn try_from(value: &str) -> result::Result<Self, Self::Error> {
    Self::from_extension(Path::new(value))
  }
}

impl TryFrom<PathBuf> for BatteryFile {
  type Error = CreateError;

  fn try_from(path: PathBuf) -> result::Result<Self, CreateError> {
    if path.exists() {
      Self::from_file_len(&path)
    } else {
      Self::from_extension(path)
    }
  }
}

impl Deref for BatteryFile {
  type Target = Path;

  fn deref(&self) -> &Self::Target {
    &self.path
  }
}

impl AsRef<Path> for BatteryFile {
  fn as_ref(&self) -> &Path {
    &self.path
  }
}

impl From<BatteryFile> for PathBuf {
  fn from(value: BatteryFile) -> Self {
    value.path
  }
}

/// The save types handled by the program
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum BatteryType {
  /// EEPROM (4kbit / 16kbit)
  Eeprom,
  /// SRAM (256kbit)
  Sram,
  /// FlashRAM (1Mbit)
  FlashRam,
}

impl BatteryType {
  /// Gets the [BatteryType] preferred extension
  pub fn extension(&self) -> &'static str {
    match self {
      Self::Eeprom => "eep",
      Self::Sram => "sra",
      Self::FlashRam => "fla",
    }
  }
}

impl fmt::Display for BatteryType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      BatteryType::Eeprom => f.write_str("EEPROM"),
      BatteryType::Sram => f.write_str("SRAM"),
      BatteryType::FlashRam => f.write_str("FlashRAM"),
    }
  }
}

#[cfg(test)]
mod tests {
  use std::{
    collections::HashMap,
    fs,
    io::Write,
    path::{Path, PathBuf},
  };

  use assert_fs::prelude::*;

  use super::{BatteryFile, BatteryType, CreateError};

  #[test]
  fn verify_save_file_try_from_str() {
    let eep: BatteryFile = "simple.eep".try_into().unwrap();
    assert_eq!(
      eep,
      BatteryFile {
        save_type: BatteryType::Eeprom,
        path: "simple.eep".into()
      }
    );

    let sra: BatteryFile = "simple.sra".try_into().unwrap();
    assert_eq!(
      sra,
      BatteryFile {
        save_type: BatteryType::Sram,
        path: "simple.sra".into()
      }
    );

    let fla: BatteryFile = "simple.fla".try_into().unwrap();
    assert_eq!(
      fla,
      BatteryFile {
        save_type: BatteryType::FlashRam,
        path: "simple.fla".into()
      }
    );
  }

  #[test]
  fn verify_save_file_try_from_path() {
    let eep: BatteryFile = PathBuf::from("simple.eep").try_into().unwrap();
    assert_eq!(
      eep,
      BatteryFile {
        save_type: BatteryType::Eeprom,
        path: "simple.eep".into()
      }
    );

    let sra: BatteryFile = PathBuf::from("simple.sra").try_into().unwrap();
    assert_eq!(
      sra,
      BatteryFile {
        save_type: BatteryType::Sram,
        path: "simple.sra".into()
      }
    );

    let fla: BatteryFile = PathBuf::from("simple.fla").try_into().unwrap();
    assert_eq!(
      fla,
      BatteryFile {
        save_type: BatteryType::FlashRam,
        path: "simple.fla".into()
      }
    );
  }

  #[test]
  fn verify_eep_file_try_from() {
    let tmp_dir = assert_fs::TempDir::new().expect("tmp dir should be created");

    let file_4k = tmp_dir.child("save_4k_eep_no_ext");
    std::fs::File::create(&file_4k)
      .and_then(|mut f| f.write_all(b"10101").and_then(|_| f.set_len(512)))
      .expect("4k file should have been written");

    file_4k.assert(predicates::path::is_file());

    let eep: BatteryFile = file_4k.to_path_buf().try_into().unwrap();
    assert_eq!(
      eep,
      BatteryFile {
        save_type: BatteryType::Eeprom,
        path: file_4k.to_path_buf()
      }
    );

    let file_16k = tmp_dir.child("save_16k_eep_no_ext");
    std::fs::File::create(&file_16k)
      .and_then(|mut f| f.write_all(b"010101").and_then(|_| f.set_len(2048)))
      .expect("16k file should have been written");

    let eep: BatteryFile = file_16k.to_path_buf().try_into().unwrap();
    assert_eq!(
      eep,
      BatteryFile {
        save_type: BatteryType::Eeprom,
        path: file_16k.to_path_buf()
      }
    );
  }

  #[test]
  fn verify_sra_file_try_from() {
    let tmp_dir = assert_fs::TempDir::new().expect("temp dir not created");

    let file_path = tmp_dir.child("srm_file_no_ext");
    fs::File::create(&file_path)
      .and_then(|mut f| f.write_all(b"1x4as").and_then(|_| f.set_len(0x8000)))
      .expect("could not create/write file");

    let sra: BatteryFile = file_path.to_path_buf().try_into().unwrap();
    assert_eq!(
      sra,
      BatteryFile {
        save_type: BatteryType::Sram,
        path: file_path.to_path_buf()
      }
    )
  }

  #[test]
  fn verify_fla_file_try_from() {
    let tmp_dir = assert_fs::TempDir::new().expect("temp dir not created");

    let file_path = tmp_dir.child("fla_file_no_ext");
    fs::File::create(&file_path)
      .and_then(|mut f| f.write_all(b"hello word").and_then(|_| f.set_len(0x20000)))
      .expect("could not create/write file");

    let fla: BatteryFile = file_path.to_path_buf().try_into().unwrap();
    assert_eq!(
      fla,
      BatteryFile {
        save_type: BatteryType::FlashRam,
        path: file_path.to_path_buf(),
      }
    )
  }

  impl PartialEq for CreateError {
    fn eq(&self, other: &Self) -> bool {
      match (self, other) {
        (Self::AnotherType(l0), Self::AnotherType(r0)) => l0 == r0,
        _ => core::mem::discriminant(self) == core::mem::discriminant(other),
      }
    }
  }

  #[test]
  fn verify_try_new() {
    let tmp_dir = assert_fs::TempDir::new().expect("temp dir not created");

    let eep_path = tmp_dir.child("eep_save");
    fs::File::create(&eep_path)
      .and_then(|f| f.set_len(0x600))
      .expect("could not create eep");

    let sram_path = tmp_dir.child("sram_save");
    fs::File::create(&sram_path)
      .and_then(|f| f.set_len(0x1500))
      .expect("could not create sram");

    let flash_path = tmp_dir.child("flash_save");
    fs::File::create(&flash_path)
      .and_then(|f| f.set_len(0x10600))
      .expect("could not create flash");

    let srm_file = tmp_dir.child("srm_file.srm");
    fs::File::create(&srm_file)
      .and_then(|mut f| f.write_all(b"#RZIPv1#").and_then(|_| f.set_len(0x500)))
      .expect("could not create srm file");

    let big_file = tmp_dir.child("big_file");
    fs::File::create(&big_file)
      .and_then(|f| f.set_len(0x20001))
      .expect("could not create big file");

    let types = [
      BatteryType::Eeprom,
      BatteryType::Sram,
      BatteryType::FlashRam,
    ];

    let type_file = HashMap::<BatteryType, &Path>::from_iter(
      [
        (BatteryType::Eeprom, eep_path.path()),
        (BatteryType::Sram, sram_path.path()),
        (BatteryType::FlashRam, flash_path.path()),
      ]
      .into_iter(),
    );

    for save_type in &types {
      // creation with non existent files
      assert_eq!(
        BatteryFile::try_new("file", *save_type),
        Ok(BatteryFile {
          save_type: *save_type,
          path: "file".into()
        })
      );

      // assert creation with existing files
      assert_eq!(
        BatteryFile::try_new(type_file[save_type], *save_type),
        Ok(BatteryFile {
          save_type: *save_type,
          path: type_file[save_type].to_path_buf()
        })
      );

      // assert creating other for other existing files
      assert_eq!(
        BatteryFile::try_new(&srm_file, *save_type),
        Err(CreateError::IsASrm)
      );

      assert_eq!(
        BatteryFile::try_new(tmp_dir.path(), *save_type),
        Err(CreateError::IsADir)
      );

      for another_type in &types {
        if save_type != another_type {
          assert_eq!(
            BatteryFile::try_new(type_file[another_type], *save_type),
            Err(CreateError::AnotherType(*another_type)),
            "while parsing {}",
            type_file[another_type].display()
          );
        }
      }

      assert_eq!(
        BatteryFile::try_new(&big_file, *save_type),
        Err(CreateError::FileTooLarge)
      )
    }
  }
}
