use std::{
  fmt, fs,
  io::{Read, Seek},
  ops::Deref,
  path::{Path, PathBuf},
  result,
};

use crate::{
  controller_pack::{self, ControllerPack},
  CreateError,
};

/// Defines an specific save file
#[derive(Debug, Clone, PartialEq)]
pub struct SaveFile {
  save_type: SaveType,
  file: PathBuf,
}

impl SaveFile {
  /// Tries to create an [SaveFile] from the specified path and type.
  pub fn try_new(
    path: impl AsRef<std::path::Path>,
    save_type: SaveType,
  ) -> Result<Self, CreateError> {
    use ControllerPackSlot::*;
    use SaveType::*;

    if !path.as_ref().exists() {
      return Ok(Self {
        file: path.as_ref().to_path_buf(),
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
        } else if file_len >= 8 {
          // first check that the existing file is not a compressed srm
          let mut buf = [0u8; 8];
          f.read_exact(&mut buf)?;
          if &buf == b"#RZIPv1#" {
            return Err(CreateError::IsASrm);
          }
          f.rewind()?;
        }

        let is_file_cp = file_len == 0x8000
          || file_len == 0x20000
            && controller_pack::ControllerPack::infer_from(&mut f).map_err(CreateError::Other)?;

        let player = is_file_cp.then(|| User::from(path.as_ref().extension()));

        let is_expected_len = match save_type {
          Eeprom => (0x1..=0x800).contains(&file_len),
          Sram => !is_file_cp && (0x801..=0x8000).contains(&file_len),
          FlashRam => !is_file_cp && (0x8001..=0x20000).contains(&file_len),
          ControllerPack(Mupen) => is_file_cp && file_len == 0x20000,
          ControllerPack(Player(user)) => {
            is_file_cp && file_len == 0x8000 && user == player.unwrap()
          }
        };

        if is_expected_len {
          Ok(Self {
            file: path.as_ref().to_path_buf(),
            save_type,
          })
        } else {
          match file_len {
            0x1..=0x800 => Err(CreateError::AnotherType(Eeprom)),
            0x801..=0x8000 => Err(CreateError::AnotherType(if is_file_cp {
              ControllerPack(Player(player.unwrap()))
            } else {
              Sram
            })),
            0x8001..=0x20000 => Err(CreateError::AnotherType(if is_file_cp {
              ControllerPack(Mupen)
            } else {
              FlashRam
            })),
            _ => Err(CreateError::FileTooLarge),
          }
        }
      })
  }

  /// Gets the [SaveType] of this [SaveFile].
  pub fn save_type(&self) -> SaveType {
    self.save_type
  }

  pub(crate) fn from_file_len<P: AsRef<Path>>(path: P) -> result::Result<Self, CreateError> {
    fs::File::open(&path)
      .map_err(|e| e.into())
      .and_then(|mut file| {
        let file_len = file.metadata()?.len();

        if file_len == 0 {
          return Err(CreateError::FileEmpty);
        } else if file_len > 8 {
          // check rzip_stream magic number, first 8 bytes
          // (https://github.com/libretro/RetroArch/blob/master/libretro-common/streams/rzip_stream.c)
          let mut magic = [0u8; 8];
          file.read_exact(&mut magic)?;
          if &magic == b"#RZIPv1#" {
            return Err(CreateError::IsASrm);
          }
          file.rewind()?;
        }

        match file_len {
          // 4Kbit or 16Kbit eeprom
          0x1..=0x800 => Ok(SaveType::Eeprom),
          // 256Kbit sram or controller pack
          0x801..=0x7FFF => Ok(SaveType::Sram),
          0x8000 => ControllerPack::infer_from(&mut file)
            .map(|cp| {
              if cp {
                // check extension...
                SaveType::ControllerPack(ControllerPackSlot::from(path.as_ref().extension()))
              } else {
                SaveType::Sram
              }
            })
            .map_err(|e| e.into()),
          // 1Mbit flash ram or 4 merged controller packs
          0x8001..=0x1FFFF => Ok(SaveType::FlashRam),
          0x20000 => ControllerPack::infer_from(&mut file)
            .map(|cp| {
              if cp {
                SaveType::ControllerPack(ControllerPackSlot::Mupen)
              } else {
                SaveType::FlashRam
              }
            })
            .map_err(|e| e.into()),
          // uncompressed retroarch srm save size
          0x48800 => Err(CreateError::IsASrm),
          // unknown
          _ => Err(CreateError::FileTooLarge),
        }
      })
      .map(|save_type| Self {
        save_type,
        file: path.as_ref().to_path_buf(),
      })
  }

  pub(crate) fn from_extension<P: AsRef<Path>>(path: P) -> result::Result<Self, CreateError> {
    use ControllerPackSlot as Slot;
    use SaveType::*;
    path
      .as_ref()
      .extension()
      .ok_or_else(|| CreateError::NoExtension)
      .and_then(|ext| match ext.to_ascii_uppercase().to_str() {
        Some("SRA") => Ok(Sram),
        Some("FLA") => Ok(FlashRam),
        Some("EEP") => Ok(Eeprom),
        Some("MPK") => Ok(ControllerPack(Slot::Player(User::Any))),
        Some("MPK1") => Ok(ControllerPack(Slot::Player(User::Used(By::Player1)))),
        Some("MPK2") => Ok(ControllerPack(Slot::Player(User::Used(By::Player2)))),
        Some("MPK3") => Ok(ControllerPack(Slot::Player(User::Used(By::Player3)))),
        Some("MPK4") => Ok(ControllerPack(Slot::Player(User::Used(By::Player4)))),
        Some("SRM") => Err(CreateError::IsASrm),
        _ => Err(CreateError::UnknownExtension),
      })
      .map(|save_type| Self {
        save_type,
        file: path.as_ref().to_path_buf(),
      })
  }
}

impl TryFrom<&str> for SaveFile {
  type Error = CreateError;

  fn try_from(value: &str) -> result::Result<Self, Self::Error> {
    Self::from_extension(Path::new(value))
  }
}

impl TryFrom<PathBuf> for SaveFile {
  type Error = CreateError;

  fn try_from(path: PathBuf) -> result::Result<Self, CreateError> {
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

/// The save types handled by the program
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum SaveType {
  /// EEPROM (4kbit / 16kbit)
  Eeprom,
  /// SRAM (256kbit)
  Sram,
  /// FlashRAM (1Mbit)
  FlashRam,
  /// Controller/Memory Pack (256kbit)
  ControllerPack(ControllerPackSlot),
}

impl SaveType {
  /// Gets this [SaveType]s preferred extension
  pub fn extension(&self) -> &'static str {
    use ControllerPackSlot as Slot;

    match self {
      Self::Eeprom => "eep",
      Self::Sram => "sra",
      Self::FlashRam => "fla",
      Self::ControllerPack(Slot::Mupen | Slot::Player(User::Any)) => "mpk",
      Self::ControllerPack(Slot::Player(User::Used(By::Player1))) => "mpk1",
      Self::ControllerPack(Slot::Player(User::Used(By::Player2))) => "mpk2",
      Self::ControllerPack(Slot::Player(User::Used(By::Player3))) => "mpk3",
      Self::ControllerPack(Slot::Player(User::Used(By::Player4))) => "mpk4",
    }
  }
}

impl fmt::Display for SaveType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      SaveType::Eeprom => f.write_str("EEPROM"),
      SaveType::Sram => f.write_str("SRAM"),
      SaveType::FlashRam => f.write_str("FlashRAM"),
      SaveType::ControllerPack(ControllerPackSlot::Mupen) => f.write_str("Mupen Controller Pack"),
      SaveType::ControllerPack(ControllerPackSlot::Player(User::Any)) => {
        f.write_fmt(format_args!("Player Controller Pack"))
      }
      SaveType::ControllerPack(ControllerPackSlot::Player(User::Used(by))) => {
        f.write_fmt(format_args!("Controller Pack {}", by.index() + 1))
      }
    }
  }
}

/// Represents the controller pack
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ControllerPackSlot {
  /// The merged controller pack used by Mupen64 input
  Mupen,
  /// A controller pack that may be attached to any player
  Player(User),
}

impl From<usize> for ControllerPackSlot {
  fn from(value: usize) -> Self {
    match value {
      1 => Self::Player(User::Used(By::Player1)),
      2 => Self::Player(User::Used(By::Player2)),
      3 => Self::Player(User::Used(By::Player3)),
      4 => Self::Player(User::Used(By::Player4)),
      _ => Self::Mupen,
    }
  }
}

impl From<Option<&std::ffi::OsStr>> for ControllerPackSlot {
  fn from(value: Option<&std::ffi::OsStr>) -> Self {
    let Some(ext) = value.map(|v| v.to_ascii_uppercase()) else {
      return ControllerPackSlot::Player(User::Any);
    };
    match ext.to_str() {
      Some("MPK1") => By::Player1.into(),
      Some("MPK2") => By::Player2.into(),
      Some("MPK3") => By::Player3.into(),
      Some("MPK4") => By::Player4.into(),
      _ => User::Any.into(),
    }
  }
}

impl From<ControllerPackSlot> for SaveType {
  fn from(value: ControllerPackSlot) -> Self {
    Self::ControllerPack(value)
  }
}

/// Represents the use of a controller pack
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum User {
  /// A controller pack
  Any,
  /// A specific user controller pack
  Used(By),
}

impl From<User> for ControllerPackSlot {
  fn from(value: User) -> Self {
    ControllerPackSlot::Player(value)
  }
}

impl From<User> for SaveType {
  fn from(value: User) -> Self {
    SaveType::ControllerPack(value.into())
  }
}

impl From<Option<&std::ffi::OsStr>> for User {
  fn from(value: Option<&std::ffi::OsStr>) -> Self {
    let Some(value) = value else {
      return User::Any
    };
    match value.to_ascii_uppercase().to_str() {
      Some("MPK1") => User::Used(By::Player1),
      Some("MPK2") => User::Used(By::Player2),
      Some("MPK3") => User::Used(By::Player3),
      Some("MPK4") => User::Used(By::Player4),
      _ => User::Any,
    }
  }
}

/// Represents a controller pack user
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum By {
  /// Player 1 controller pack
  Player1 = 1,
  /// Player 2 controller pack
  Player2 = 2,
  /// Player 3 controller pack
  Player3 = 3,
  /// Player 4 controller pack
  Player4 = 4,
}

impl By {
  pub(crate) fn index(&self) -> usize {
    match self {
      By::Player1 => 0,
      By::Player2 => 1,
      By::Player3 => 2,
      By::Player4 => 3,
    }
  }
}

impl From<By> for ControllerPackSlot {
  fn from(value: By) -> Self {
    ControllerPackSlot::Player(User::Used(value))
  }
}

impl From<By> for SaveType {
  fn from(value: By) -> Self {
    SaveType::ControllerPack(value.into())
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

  use super::{
    controller_pack::ControllerPack, By, ControllerPackSlot, CreateError, SaveFile, SaveType, User,
  };

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
        save_type: User::Any.into(),
        file: "simple.mpk".into()
      }
    );

    let mpk1: SaveFile = "simple.mpk1".try_into().unwrap();
    assert_eq!(
      mpk1,
      SaveFile {
        save_type: By::Player1.into(),
        file: "simple.mpk1".into()
      }
    );

    let mpk2: SaveFile = "simple.mpk2".try_into().unwrap();
    assert_eq!(
      mpk2,
      SaveFile {
        save_type: By::Player2.into(),
        file: "simple.mpk2".into()
      }
    );

    let mpk3: SaveFile = "simple.mpk3".try_into().unwrap();
    assert_eq!(
      mpk3,
      SaveFile {
        save_type: By::Player3.into(),
        file: "simple.mpk3".into()
      }
    );

    let mpk4: SaveFile = "simple.mpk4".try_into().unwrap();
    assert_eq!(
      mpk4,
      SaveFile {
        save_type: By::Player4.into(),
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
        save_type: User::Any.into(),
        file: "simple.mpk".into()
      }
    );

    let mpk1: SaveFile = PathBuf::from("simple.mpk1").try_into().unwrap();
    assert_eq!(
      mpk1,
      SaveFile {
        save_type: By::Player1.into(),
        file: "simple.mpk1".into()
      }
    );

    let mpk2: SaveFile = PathBuf::from("simple.mpk2").try_into().unwrap();
    assert_eq!(
      mpk2,
      SaveFile {
        save_type: By::Player2.into(),
        file: "simple.mpk2".into()
      }
    );

    let mpk3: SaveFile = PathBuf::from("simple.mpk3").try_into().unwrap();
    assert_eq!(
      mpk3,
      SaveFile {
        save_type: By::Player3.into(),
        file: "simple.mpk3".into()
      }
    );

    let mpk4: SaveFile = PathBuf::from("simple.mpk4").try_into().unwrap();
    assert_eq!(
      mpk4,
      SaveFile {
        save_type: By::Player4.into(),
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
    let cp_data = Box::new(ControllerPack::new());
    fs::File::create(&file_path)
      .and_then(|mut f| f.write_all(cp_data.as_ref().as_ref()))
      .expect("could not create/write file");

    let cp1: SaveFile = file_path.to_path_buf().try_into().unwrap();
    assert_eq!(
      cp1,
      SaveFile {
        save_type: User::Any.into(),
        file: file_path.to_path_buf()
      }
    );

    for i in 1..=4 {
      let file_path = tmp_dir.child(format!("mpk_file.mpk{i}"));
      fs::File::create(&file_path)
        .and_then(|mut f| f.write_all(cp_data.as_ref().as_ref()))
        .expect("could not create/write file");

      let cp: SaveFile = file_path.to_path_buf().try_into().unwrap();
      assert_eq!(
        cp,
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
        save_type: ControllerPackSlot::Mupen.into(),
        file: file_path.to_path_buf()
      }
    );
  }

  impl PartialEq for CreateError {
    fn eq(&self, other: &Self) -> bool {
      match (self, other) {
        (Self::AnotherType(l0), Self::AnotherType(r0)) => l0 == r0,
        //(Self::Other(l0), Self::Other(r0)) => l0 == r0,
        _ => core::mem::discriminant(self) == core::mem::discriminant(other),
      }
    }
  }

  #[test]
  fn verify_try_new() {
    let tmp_dir = assert_fs::TempDir::new().expect("temp dir not created");

    let any_cp_path = tmp_dir.child("player_cp");
    let cp_data = Box::new(ControllerPack::new());
    fs::File::create(&any_cp_path)
      .and_then(|mut f| f.write_all(cp_data.as_ref().as_ref()))
      .expect("could not create/write player cp");

    let p1_cp_path = any_cp_path.with_extension("mpk1");
    fs::copy(&any_cp_path, &p1_cp_path).expect("copy p1");
    let p2_cp_path = any_cp_path.with_extension("mpk2");
    fs::copy(&any_cp_path, &p2_cp_path).expect("copy p2");
    let p3_cp_path = any_cp_path.with_extension("mpk3");
    fs::copy(&any_cp_path, &p3_cp_path).expect("copy p3");
    let p4_cp_path = any_cp_path.with_extension("mpk4");
    fs::copy(&any_cp_path, &p4_cp_path).expect("copy p4");

    let mupen_cp_path = tmp_dir.child(format!("mupen_cp"));
    fs::File::create(&mupen_cp_path)
      .map(|mut f| {
        for _ in 0..4 {
          f.write_all(cp_data.as_ref().as_ref()).unwrap()
        }
      })
      .expect("could not create/write mupen cp");

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
      SaveType::Eeprom,
      SaveType::Sram,
      SaveType::FlashRam,
      SaveType::ControllerPack(ControllerPackSlot::Mupen),
      SaveType::ControllerPack(ControllerPackSlot::Player(User::Any)),
      SaveType::ControllerPack(ControllerPackSlot::Player(User::Used(By::Player1))),
      SaveType::ControllerPack(ControllerPackSlot::Player(User::Used(By::Player2))),
      SaveType::ControllerPack(ControllerPackSlot::Player(User::Used(By::Player3))),
      SaveType::ControllerPack(ControllerPackSlot::Player(User::Used(By::Player4))),
    ];

    let type_file = HashMap::<SaveType, &Path>::from_iter(
      [
        (SaveType::Eeprom, eep_path.path()),
        (SaveType::Sram, sram_path.path()),
        (SaveType::FlashRam, flash_path.path()),
        (ControllerPackSlot::Mupen.into(), mupen_cp_path.path()),
        (User::Any.into(), any_cp_path.path()),
        (By::Player1.into(), p1_cp_path.as_path()),
        (By::Player2.into(), p2_cp_path.as_path()),
        (By::Player3.into(), p3_cp_path.as_path()),
        (By::Player4.into(), p4_cp_path.as_path()),
      ]
      .into_iter(),
    );

    for save_type in &types {
      // creation with non existent files
      assert_eq!(
        SaveFile::try_new("file", *save_type),
        Ok(SaveFile {
          save_type: *save_type,
          file: "file".into()
        })
      );

      // assert creation with existing files
      assert_eq!(
        SaveFile::try_new(type_file[save_type], *save_type),
        Ok(SaveFile {
          save_type: *save_type,
          file: type_file[save_type].to_path_buf()
        })
      );

      // assert creating other for other existing files
      assert_eq!(
        SaveFile::try_new(&srm_file, *save_type),
        Err(CreateError::IsASrm)
      );

      assert_eq!(
        SaveFile::try_new(tmp_dir.path(), *save_type),
        Err(CreateError::IsADir)
      );

      for another_type in &types {
        if save_type != another_type {
          assert_eq!(
            SaveFile::try_new(type_file[another_type], *save_type),
            Err(CreateError::AnotherType(*another_type)),
            "while parsing {}",
            type_file[another_type].display()
          );
        }
      }

      assert_eq!(
        SaveFile::try_new(&big_file, *save_type),
        Err(CreateError::FileTooLarge)
      )
    }
  }
}
