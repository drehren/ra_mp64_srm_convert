use crate::{ConvertibleFile, ConvertibleType};
use either::Either;
use std::{
  num::NonZeroU8,
  path::{Path, PathBuf},
};

impl<P> ConvertibleFile<P>
where
  P: AsRef<Path>,
{
  /// Checks if this [ConvertibleFile] is a [MupenPackFile]
  pub fn is_mupen_pack(&self) -> bool {
    matches!(
      self.part_type,
      ConvertibleType::ControllerPack {
        size: None | Some(0x20000),
      }
    )
  }

  /// Gets the [MupenPackFile] from this [ConvertibleFile]
  pub fn into_mupen_pack(self) -> Either<MupenPackFile, Self> {
    if !self.is_mupen_pack() {
      return Either::Right(self);
    }
    let pack_size = match self.part_type {
      ConvertibleType::ControllerPack { size } => size,
      _ => unreachable!(),
    };
    match pack_size {
      None | Some(0x20000) => Either::Left(MupenPackFile::from(self.path.as_ref().to_owned())),
      _ => unreachable!(),
    }
  }

  /// Checks if this [ConvertibleFile] is a [ControllerPackFile]
  pub fn is_player_pack(&self) -> bool {
    matches!(
      self.part_type,
      ConvertibleType::ControllerPack {
        size: None | Some(0x8000)
      }
    )
  }

  /// Gets the [ControllerPackFile] from this [ConvertibleFile]
  pub fn into_player_pack(self) -> Either<ControllerPackFile, Self> {
    if !self.is_player_pack() {
      return Either::Right(self);
    }
    let pack_size = match self.part_type {
      ConvertibleType::ControllerPack { size } => size,
      _ => unreachable!(),
    };
    match pack_size {
      None | Some(0x8000) => Either::Left(ControllerPackFile::from(self.path.as_ref().to_owned())),
      _ => unreachable!(),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
/// Represents a Mupen controller pack file
pub struct MupenPackFile(PathBuf);

impl MupenPackFile {
  /// Take ownership of the contained [PathBuf]
  pub fn into_path_buf(self) -> PathBuf {
    self.0
  }
}

impl AsRef<Path> for MupenPackFile {
  fn as_ref(&self) -> &Path {
    &self.0
  }
}

impl<T> From<T> for MupenPackFile
where
  T: Into<PathBuf>,
{
  fn from(value: T) -> Self {
    Self(value.into())
  }
}

#[derive(Debug, PartialEq, Clone)]
/// Represents a single controller pack
pub struct ControllerPackFile(PathBuf, Option<Player>);

impl ControllerPackFile {
  /// Returns the player for this controller pack, if set
  pub fn player(&self) -> Option<Player> {
    self.1
  }

  /// Takes ownership of the path.
  pub fn into_path_buf(self) -> PathBuf {
    self.0
  }
}

impl AsRef<Path> for ControllerPackFile {
  fn as_ref(&self) -> &Path {
    &self.0
  }
}

impl<T> From<T> for ControllerPackFile
where
  T: Into<PathBuf>,
{
  fn from(value: T) -> Self {
    let value: PathBuf = value.into();
    let player = value
      .extension()
      .and_then(|o| o.to_str().and_then(|s| s.as_bytes().iter().last()))
      .map(|b| Player::from((*b - b'0') as usize));
    Self(value, player)
  }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
/// Represents a player
pub enum Player {
  /// The first player
  P1,
  /// The second player
  P2,
  /// The third player
  P3,
  /// The fourth player
  P4,
}

impl Player {
  pub(crate) fn index(&self) -> usize {
    match self {
      Self::P1 => 0,
      Self::P2 => 1,
      Self::P3 => 2,
      Self::P4 => 3,
    }
  }
}

impl From<NonZeroU8> for Player {
  fn from(value: NonZeroU8) -> Self {
    match u8::from(value) {
      4 => Self::P4,
      3 => Self::P3,
      2 => Self::P2,
      _ => Self::P1,
    }
  }
}

impl From<usize> for Player {
  fn from(value: usize) -> Self {
    match value {
      4 => Self::P4,
      3 => Self::P3,
      2 => Self::P2,
      _ => Self::P1,
    }
  }
}

pub(crate) trait PackFile {
  fn file_size() -> u64;
}

impl PackFile for MupenPackFile {
  fn file_size() -> u64 {
    0x20000
  }
}

impl PackFile for ControllerPackFile {
  fn file_size() -> u64 {
    0x8000
  }
}

#[cfg(test)]
mod tests {
  use super::{ControllerPackFile, MupenPackFile};
  use crate::controller_pack::ControllerPack;
  use assert_fs::prelude::*;
  use std::{io::prelude::*, path::PathBuf};

  #[test]
  fn verify_controller_pack_file_from_str() {
    let names = [
      "simple.mpk",
      "simple.mpk1",
      "simple.mpk2",
      "simple.mpk3",
      "simple.mpk4",
    ];

    for name in names {
      let cp = ControllerPackFile::from(&name);
      assert_eq!(cp.0, PathBuf::from(name))
    }
  }

  #[test]
  fn verify_controller_pack_file_from_path() {
    let paths = [
      PathBuf::from("simple.mpk"),
      PathBuf::from("simple.mpk1"),
      PathBuf::from("simple.mpk2"),
      PathBuf::from("simple.mpk3"),
      PathBuf::from("simple.mpk4"),
    ];

    for path in paths {
      let cp = ControllerPackFile::from(&path);
      assert_eq!(&cp.0, &path);
    }
  }

  #[test]
  fn verify_controller_pack_file_try_from() {
    let tmp_dir = assert_fs::TempDir::new().expect("temp dir not created");

    let file_path = tmp_dir.child("mpk_file_no_ext");
    let cp_data = Box::new(ControllerPack::new());
    std::fs::File::create(&file_path)
      .and_then(|mut f| f.write_all(cp_data.as_ref().as_ref()))
      .expect("could not create/write file");

    let cp = ControllerPackFile(file_path.to_path_buf(), None);
    assert_eq!(&cp.0, file_path.as_ref());

    for i in 1..=4 {
      let file_path = tmp_dir.child(format!("mpk_file.mpk{i}"));
      std::fs::File::create(&file_path)
        .and_then(|mut f| f.write_all(cp_data.as_ref().as_ref()))
        .expect("could not create/write file");

      let cp_x = ControllerPackFile(file_path.to_path_buf(), Some(i.into()));
      assert_eq!(&cp_x.0, file_path.as_ref());
    }

    // make first controller pack a mupen file and check
    let mupen_path = tmp_dir.child("mupen_mpk");
    std::fs::File::create(&mupen_path)
      .and_then(|mut f| {
        Ok(for _ in 0..4 {
          f.write_all(cp_data.as_ref().as_ref())?;
        })
      })
      .expect("could not create/write file");

    let mupen_cp = MupenPackFile(mupen_path.to_path_buf());
    assert_eq!(mupen_cp.0, mupen_path.as_ref());
  }
}
