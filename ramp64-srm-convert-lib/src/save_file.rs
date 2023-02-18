use std::{
  error, ffi, fmt, fs, io,
  ops::Deref,
  path::{Path, PathBuf},
  result,
};

use crate::controller_pack::ControllerPack;

/// Defines an specific save file
#[derive(Debug, Clone, PartialEq)]
pub struct SaveFile {
  save_type: SaveType,
  file: PathBuf,
}

impl SaveFile {
  /// Gets the [SaveType] of this [SaveFile].
  pub fn save_type(&self) -> SaveType {
    self.save_type
  }

  /// Attempts to change the type of the current [SaveFile].
  ///
  /// ### Compatible changes:
  /// - If file does not exist: All <-> All
  /// - If file does exists: Only player Controller Packs can be changed
  pub fn try_change_type(
    self,
    new_save_type: impl Into<SaveType>,
  ) -> result::Result<Self, ConvertTypeError> {
    let Self { file, save_type } = self;
    let new_save_type = new_save_type.into();
    if matches!(save_type, SaveType::ControllerPack(_)) {
      if file.exists() {
        if new_save_type == ControllerPackKind::Mupen.into() && save_type == new_save_type {
          Ok(Self { file, save_type })
        } else if new_save_type != ControllerPackKind::Mupen.into()
          && save_type != ControllerPackKind::Mupen.into()
        {
          Ok(Self {
            file,
            save_type: new_save_type,
          })
        } else {
          Err(ConvertTypeError::ExistingOkPlayerToMupen)
        }
      } else {
        Ok(Self {
          file,
          save_type: new_save_type,
        })
      }
    } else {
      if !file.exists() {
        Ok(Self {
          file,
          save_type: new_save_type,
        })
      } else {
        Err(ConvertTypeError::FileExistsNoCp)
      }
    }
  }

  pub(crate) fn from_file_len<P: AsRef<Path>>(path: P) -> result::Result<Self, SaveFileInferError> {
    fs::File::open(&path)
      .map_err(|e| e.into())
      .and_then(|mut file| {
        match file.metadata().unwrap().len() {
          // 4Kbit or 16Kbit eeprom
          0x200 | 0x800 => Ok(SaveType::Eeprom),
          // 256Kbit sram or controller pack
          0x8000 => ControllerPack::infer_from(&mut file)
            .map(|cp| {
              if cp {
                SaveType::ControllerPack(path.as_ref().extension().into())
              } else {
                SaveType::Sram
              }
            })
            .map_err(|e| e.into()),
          // 1Mbit flash ram or 4 merged controller packs
          0x20000 => ControllerPack::infer_from(&mut file)
            .map(|cp| {
              if cp {
                SaveType::ControllerPack(ControllerPackKind::Mupen)
              } else {
                SaveType::FlashRam
              }
            })
            .map_err(|e| e.into()),
          // uncompressed retroarch srm save size
          0x48800 => Err(SaveFileInferError::IsAnSrmFile),
          // unknown
          _ => Err(SaveFileInferError::UnknownFile),
        }
      })
      .map(|save_type| Self {
        save_type,
        file: path.as_ref().to_path_buf(),
      })
  }

  pub(crate) fn from_extension<P: AsRef<Path>>(
    path: P,
  ) -> result::Result<Self, SaveFileInferError> {
    path
      .as_ref()
      .extension()
      .ok_or_else(|| SaveFileInferError::NoExtension)
      .and_then(|ext| match ext.to_ascii_uppercase().to_str() {
        Some("SRA") => Ok(SaveType::Sram),
        Some("FLA") => Ok(SaveType::FlashRam),
        Some("EEP") => Ok(SaveType::Eeprom),
        Some("MPK" | "MPK1") => Ok(SaveType::ControllerPack(ControllerPackKind::Player1)),
        Some("MPK2") => Ok(SaveType::ControllerPack(ControllerPackKind::Player2)),
        Some("MPK3") => Ok(SaveType::ControllerPack(ControllerPackKind::Player3)),
        Some("MPK4") => Ok(SaveType::ControllerPack(ControllerPackKind::Player4)),
        Some("SRM") => Err(SaveFileInferError::IsAnSrmFile),
        _ => Err(SaveFileInferError::UnknownFile),
      })
      .map(|save_type| Self {
        save_type,
        file: path.as_ref().to_path_buf(),
      })
  }
}

impl TryFrom<&str> for SaveFile {
  type Error = SaveFileInferError;

  fn try_from(value: &str) -> result::Result<Self, Self::Error> {
    Self::from_extension(Path::new(value))
  }
}

impl TryFrom<PathBuf> for SaveFile {
  type Error = SaveFileInferError;

  fn try_from(path: PathBuf) -> result::Result<Self, SaveFileInferError> {
    if path.exists() {
      Self::from_file_len(&path)
    } else {
      Self::from_extension(path)
    }
  }
}
impl Deref for SaveFile {
  type Target = Path;

  fn deref(&self) -> &Self::Target {
    &self.file
  }
}
impl AsRef<Path> for SaveFile {
  fn as_ref(&self) -> &Path {
    &self.file
  }
}
impl From<SaveFile> for PathBuf {
  fn from(value: SaveFile) -> Self {
    value.file
  }
}

/// Defines a [SaveFile] inference error
#[derive(Debug)]
pub enum SaveFileInferError {
  /// The specified file is an SRM file
  IsAnSrmFile,
  /// The specified file is unknown
  UnknownFile,
  /// The specified path did not contain a file extension
  NoExtension,
  /// An Io error
  IoError(io::Error),
}
impl From<io::Error> for SaveFileInferError {
  fn from(value: io::Error) -> Self {
    Self::IoError(value)
  }
}
impl fmt::Display for SaveFileInferError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::IsAnSrmFile => f.write_str("is an SRM save"),
      Self::UnknownFile => f.write_str("unknown save file"),
      Self::NoExtension => f.write_str("path did not have a known extension"),
      Self::IoError(err) => f.write_fmt(format_args!("{err}")),
    }
  }
}
impl error::Error for SaveFileInferError {}

#[derive(Debug, PartialEq, Eq)]
pub enum ConvertTypeError {
  ExistingOkPlayerToMupen,
  FileExistsNoCp,
}

impl fmt::Display for ConvertTypeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::ExistingOkPlayerToMupen => f.write_str("player to/from mupen and file exists"),
      Self::FileExistsNoCp => f.write_str("cannot change if file exists and not a controller pack"),
    }
  }
}

impl error::Error for ConvertTypeError {}

/// The save types handled by the program
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SaveType {
  /// EEPROM save (4kbit or 16kbit)
  Eeprom,
  /// SRAM (256kbit)
  Sram,
  /// FlashRAM (1Mbit)
  FlashRam,
  /// Controller/Memory Pack (256kbit)
  ControllerPack(ControllerPackKind),
}

impl SaveType {
  /// Gets the extensions of the specified [SaveType]
  pub fn extension(&self) -> &'static str {
    match self {
      Self::Eeprom => &"eep",
      Self::Sram => &"sra",
      Self::FlashRam => &"fla",
      Self::ControllerPack(ControllerPackKind::Mupen) => &"mpk",
      Self::ControllerPack(ControllerPackKind::Player1) => &"mpk1",
      Self::ControllerPack(ControllerPackKind::Player2) => &"mpk2",
      Self::ControllerPack(ControllerPackKind::Player3) => &"mpk3",
      Self::ControllerPack(ControllerPackKind::Player4) => &"mpk4",
    }
  }

  // Validates that the file is of the specified [SaveType]
  //pub fn is_file_of_type(&self, file: Path) -> result::Result {}
}

impl fmt::Display for SaveType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      SaveType::Eeprom => f.write_str("EEPROM"),
      SaveType::Sram => f.write_str("SRAM"),
      SaveType::FlashRam => f.write_str("FlashRAM"),
      SaveType::ControllerPack(ControllerPackKind::Mupen) => f.write_str("Mupen Controller Pack"),
      SaveType::ControllerPack(player) => {
        f.write_fmt(format_args!("Controller Pack {}", { *player as i32 }))
      }
    }
  }
}

/// Represents the controller pack
#[repr(usize)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ControllerPackKind {
  /// The merged controller pack used by Mupen64 input
  Mupen,
  /// The controller pack attached to the first player
  Player1,
  /// The controller pack attached to the second player
  Player2,
  /// The controller pack attached to the third player
  Player3,
  /// The controller pack attached to the fourth player
  Player4,
}

impl From<usize> for ControllerPackKind {
  fn from(value: usize) -> Self {
    match value {
      1 => Self::Player1,
      2 => Self::Player2,
      3 => Self::Player3,
      4 => Self::Player4,
      _ => Self::Mupen,
    }
  }
}
impl From<ControllerPackKind> for usize {
  fn from(value: ControllerPackKind) -> Self {
    (value as usize).saturating_sub(1)
  }
}
impl From<ControllerPackKind> for SaveType {
  fn from(value: ControllerPackKind) -> Self {
    Self::ControllerPack(value)
  }
}
impl From<Option<&ffi::OsStr>> for ControllerPackKind {
  fn from(value: Option<&ffi::OsStr>) -> Self {
    value.map_or(Self::Player1, |ext| {
      match ext.to_ascii_uppercase().to_str() {
        Some("MPK2") => Self::Player2,
        Some("MPK3") => Self::Player3,
        Some("MPK4") => Self::Player4,
        _ => Self::Player1,
      }
    })
  }
}

#[cfg(test)]
mod tests {
  use std::{fs, io::Write, path::PathBuf};

  use assert_fs::prelude::{PathAssert, PathChild};

  use crate::controller_pack::{ControllerPack, ControllerPackInitializer};

  use super::{ControllerPackKind, SaveFile, SaveType};

  #[test]
  fn verify_save_file_try_from_str() {
    let eep: SaveFile = "simple.eep".try_into().unwrap();
    assert_eq!(
      eep,
      SaveFile {
        save_type: SaveType::Eeprom,
        file: "simple.eep".into()
      }
    );

    let sra: SaveFile = "simple.sra".try_into().unwrap();
    assert_eq!(
      sra,
      SaveFile {
        save_type: SaveType::Sram,
        file: "simple.sra".into()
      }
    );

    let fla: SaveFile = "simple.fla".try_into().unwrap();
    assert_eq!(
      fla,
      SaveFile {
        save_type: SaveType::FlashRam,
        file: "simple.fla".into()
      }
    );

    let mpk: SaveFile = "simple.mpk".try_into().unwrap();
    assert_eq!(
      mpk,
      SaveFile {
        save_type: SaveType::ControllerPack(ControllerPackKind::Player1),
        file: "simple.mpk".into()
      }
    );

    let mpk1: SaveFile = "simple.mpk1".try_into().unwrap();
    assert_eq!(
      mpk1,
      SaveFile {
        save_type: SaveType::ControllerPack(ControllerPackKind::Player1),
        file: "simple.mpk1".into()
      }
    );

    let mpk2: SaveFile = "simple.mpk2".try_into().unwrap();
    assert_eq!(
      mpk2,
      SaveFile {
        save_type: SaveType::ControllerPack(ControllerPackKind::Player2),
        file: "simple.mpk2".into()
      }
    );

    let mpk3: SaveFile = "simple.mpk3".try_into().unwrap();
    assert_eq!(
      mpk3,
      SaveFile {
        save_type: SaveType::ControllerPack(ControllerPackKind::Player3),
        file: "simple.mpk3".into()
      }
    );

    let mpk4: SaveFile = "simple.mpk4".try_into().unwrap();
    assert_eq!(
      mpk4,
      SaveFile {
        save_type: SaveType::ControllerPack(ControllerPackKind::Player4),
        file: "simple.mpk4".into()
      }
    );
  }

  #[test]
  fn verify_save_file_try_from_path() {
    let eep: SaveFile = PathBuf::from("simple.eep").try_into().unwrap();
    assert_eq!(
      eep,
      SaveFile {
        save_type: SaveType::Eeprom,
        file: "simple.eep".into()
      }
    );

    let sra: SaveFile = PathBuf::from("simple.sra").try_into().unwrap();
    assert_eq!(
      sra,
      SaveFile {
        save_type: SaveType::Sram,
        file: "simple.sra".into()
      }
    );

    let fla: SaveFile = PathBuf::from("simple.fla").try_into().unwrap();
    assert_eq!(
      fla,
      SaveFile {
        save_type: SaveType::FlashRam,
        file: "simple.fla".into()
      }
    );

    let mpk: SaveFile = PathBuf::from("simple.mpk").try_into().unwrap();
    assert_eq!(
      mpk,
      SaveFile {
        save_type: SaveType::ControllerPack(ControllerPackKind::Player1),
        file: "simple.mpk".into()
      }
    );

    let mpk1: SaveFile = PathBuf::from("simple.mpk1").try_into().unwrap();
    assert_eq!(
      mpk1,
      SaveFile {
        save_type: SaveType::ControllerPack(ControllerPackKind::Player1),
        file: "simple.mpk1".into()
      }
    );

    let mpk2: SaveFile = PathBuf::from("simple.mpk2").try_into().unwrap();
    assert_eq!(
      mpk2,
      SaveFile {
        save_type: SaveType::ControllerPack(ControllerPackKind::Player2),
        file: "simple.mpk2".into()
      }
    );

    let mpk3: SaveFile = PathBuf::from("simple.mpk3").try_into().unwrap();
    assert_eq!(
      mpk3,
      SaveFile {
        save_type: SaveType::ControllerPack(ControllerPackKind::Player3),
        file: "simple.mpk3".into()
      }
    );

    let mpk4: SaveFile = PathBuf::from("simple.mpk4").try_into().unwrap();
    assert_eq!(
      mpk4,
      SaveFile {
        save_type: SaveType::ControllerPack(ControllerPackKind::Player4),
        file: "simple.mpk4".into()
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

    let eep: SaveFile = file_4k.to_path_buf().try_into().unwrap();
    assert_eq!(
      eep,
      SaveFile {
        save_type: SaveType::Eeprom,
        file: file_4k.to_path_buf()
      }
    );

    let file_16k = tmp_dir.child("save_16k_eep_no_ext");
    std::fs::File::create(&file_16k)
      .and_then(|mut f| f.write_all(b"010101").and_then(|_| f.set_len(2048)))
      .expect("16k file should have been written");

    let eep: SaveFile = file_16k.to_path_buf().try_into().unwrap();
    assert_eq!(
      eep,
      SaveFile {
        save_type: SaveType::Eeprom,
        file: file_16k.to_path_buf()
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

    let sra: SaveFile = file_path.to_path_buf().try_into().unwrap();
    assert_eq!(
      sra,
      SaveFile {
        save_type: SaveType::Sram,
        file: file_path.to_path_buf()
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

    let fla: SaveFile = file_path.to_path_buf().try_into().unwrap();
    assert_eq!(
      fla,
      SaveFile {
        save_type: SaveType::FlashRam,
        file: file_path.to_path_buf(),
      }
    )
  }

  #[test]
  fn verify_controller_pack_file_try_from() {
    let tmp_dir = assert_fs::TempDir::new().expect("temp dir not created");

    let file_path = tmp_dir.child("mpk_file_no_ext");
    let mut cp_data = Box::new(ControllerPack::default());
    ControllerPackInitializer::new().init(&mut cp_data);
    fs::File::create(&file_path)
      .and_then(|mut f| f.write_all(cp_data.as_ref().as_ref()))
      .expect("could not create/write file");

    let cp1: SaveFile = file_path.to_path_buf().try_into().unwrap();
    assert_eq!(
      cp1,
      SaveFile {
        save_type: ControllerPackKind::Player1.into(),
        file: file_path.to_path_buf()
      }
    );

    for i in 1..=4 {
      let file_path = tmp_dir.child(format!("mpk_file.mpk{i}"));
      fs::File::create(&file_path)
        .and_then(|mut f| f.write_all(cp_data.as_ref().as_ref()))
        .expect("could not create/write file");

      let cp1: SaveFile = file_path.to_path_buf().try_into().unwrap();
      assert_eq!(
        cp1,
        SaveFile {
          save_type: SaveType::ControllerPack(i.into()),
          file: file_path.to_path_buf()
        }
      );
    }

    // make first controller pack a mupen file and check
    fs::File::create(&file_path)
      .map(|mut f| {
        for _ in 0..4 {
          f.write_all(cp_data.as_ref().as_ref()).unwrap()
        }
      })
      .expect("could not create/write file");

    let mcp: SaveFile = file_path.to_path_buf().try_into().unwrap();
    assert_eq!(
      mcp,
      SaveFile {
        save_type: ControllerPackKind::Mupen.into(),
        file: file_path.to_path_buf()
      }
    );
  }
}
