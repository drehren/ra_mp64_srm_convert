//! # RetroArch Mupen64Plus SRM Converter Lib
//!
//! `ramp64-srm-convert-lib` is the library portion of `ra_mp64_srm_convert` in an attempt to
//! make GUIs out of the same code.

#![deny(missing_docs)]

mod controller_pack;
mod convert_params;
mod create_srm;
mod game_pack;
mod grouping;
mod retroarch_srm;
mod split_srm;

use std::{
  error, ffi, fmt, fs, io,
  ops::Deref,
  path::{Path, PathBuf},
  result,
};

use controller_pack::ControllerPack;
pub use convert_params::{convert, ConvertMode, ConvertParams, Problem};
pub use grouping::{group_saves, validate_groups, GroupedSaves, Grouping, InvalidGroup};

fn word_byte_swap(buf: &mut [u8]) {
  for i in (0..buf.len()).step_by(4) {
    buf.swap(i, i + 3);
    buf.swap(i + 1, i + 2);
  }
}

/// Provides the path to which the error originated from
#[derive(Debug)]
pub struct PathError(PathBuf, io::Error);

impl fmt::Display for PathError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let PathError(path, err) = self;
    match err.kind() {
      io::ErrorKind::NotFound => f.write_fmt(format_args!("file {} not found", path.display())),
      io::ErrorKind::PermissionDenied => {
        f.write_fmt(format_args!("could not access {}", path.display()))
      }
      io::ErrorKind::AlreadyExists => {
        f.write_fmt(format_args!("will overwrite existing {}", path.display()))
      }
      io::ErrorKind::WriteZero => f.write_fmt(format_args!(
        "could not write all data into {}",
        path.display()
      )),
      io::ErrorKind::UnexpectedEof => f.write_fmt(format_args!(
        "unexpected end of file while reading {}",
        path.display()
      )),
      _ => f.write_fmt(format_args!("{err} ({})", path.display())),
    }
  }
}

impl std::error::Error for PathError {}
type Result = std::result::Result<(), PathError>;

struct OutputDir<'out, 'base> {
  out_dir: &'out Option<PathBuf>,
  base: &'base Path,
}
impl<'out, 'base> OutputDir<'out, 'base> {
  fn new(out_dir: &'out Option<PathBuf>, base: &'base Path) -> Self {
    Self { out_dir, base }
  }

  fn to_out_dir<P>(&self, input: &P) -> PathBuf
  where
    P: AsRef<Path> + ?Sized,
  {
    let input = input.as_ref();
    if input.is_absolute() {
      return input.into();
    }
    let file_name = input.file_name().unwrap_or(ffi::OsStr::new(""));
    match self.out_dir {
      Some(path) => path.as_path(),
      None => self.base.parent().unwrap(),
    }
    .join(file_name)
  }

  fn base_with_extension<S>(&self, ext: &S) -> PathBuf
  where
    S: AsRef<ffi::OsStr> + ?Sized,
  {
    self.to_out_dir(self.base.with_extension(ext).file_name().unwrap())
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
  /// Gets the extension of the specified [SaveType]
  pub fn extension(&self) -> &str {
    match self {
      Self::Eeprom => "eep",
      Self::Sram => "sra",
      Self::FlashRam => "fla",
      Self::ControllerPack(ControllerPackKind::Mupen) => "mpk",
      Self::ControllerPack(ControllerPackKind::Player1) => "mpk1",
      Self::ControllerPack(ControllerPackKind::Player2) => "mpk2",
      Self::ControllerPack(ControllerPackKind::Player3) => "mpk3",
      Self::ControllerPack(ControllerPackKind::Player4) => "mpk4",
    }
  }
}

impl fmt::Display for SaveType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      SaveType::Eeprom => f.write_str("EEPROM"),
      SaveType::Sram => f.write_str("SRAM"),
      SaveType::FlashRam => f.write_str("FlashRAM"),
      SaveType::ControllerPack(ControllerPackKind::Mupen) => f.write_str("Mupen Controller Pack"),
      SaveType::ControllerPack(player) => {
        f.write_fmt(format_args!("Controller Pack {}", { *player as i32 + 1 }))
      }
    }
  }
}

/// Specifies a typed SRM file
#[derive(Debug, PartialEq, Clone)]
pub struct SrmFile(PathBuf);
impl SrmFile {
  /// Gets the extension of an [SrmFile]
  pub fn extension(&self) -> &str {
    "srm"
  }

  fn from_name<S: AsRef<str>>(name: S) -> Self {
    Self(Path::new(name.as_ref()).with_extension("srm"))
  }

  fn from_file_len<P: AsRef<Path>>(path: P) -> result::Result<Self, SrmFileInferError> {
    fs::File::open(path.as_ref())
      .map_err(SrmFileInferError::Other)
      .and_then(|file| {
        if file.metadata().unwrap().len() == 0x48800 {
          Ok(Self(path.as_ref().to_path_buf()))
        } else {
          Err(SrmFileInferError::InvalidSize)
        }
      })
  }

  fn from_extension<P: AsRef<Path>>(path: P) -> result::Result<Self, SrmFileInferError> {
    let path = path.as_ref();
    path
      .extension()
      .ok_or(SrmFileInferError::NoExtension)
      .and_then(|ext| {
        if ext.to_ascii_uppercase().to_str() == Some("SRM") {
          Ok(Self(path.to_path_buf()))
        } else {
          Err(SrmFileInferError::InvalidExtension)
        }
      })
  }
}
impl Deref for SrmFile {
  type Target = Path;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}
impl AsRef<Path> for SrmFile {
  fn as_ref(&self) -> &Path {
    &self.0
  }
}
impl From<SrmFile> for PathBuf {
  fn from(value: SrmFile) -> Self {
    value.0
  }
}
impl TryFrom<PathBuf> for SrmFile {
  type Error = SrmFileInferError;

  fn try_from(path: PathBuf) -> result::Result<Self, Self::Error> {
    if path.exists() {
      Self::from_file_len(&path)
    } else {
      Self::from_extension(path)
    }
  }
}
impl From<&str> for SrmFile {
  fn from(name: &str) -> Self {
    Self::from_name(name)
  }
}

/// Specifies an [SrmFile] inference error
#[derive(Debug)]
pub enum SrmFileInferError {
  /// The specified file was not of the expected size
  InvalidSize,
  /// The specified file did not contain the expected extension
  InvalidExtension,
  /// The specified path did not have an extension
  NoExtension,
  /// An Io error
  Other(io::Error),
}
impl fmt::Display for SrmFileInferError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::InvalidSize => f.write_str("invalid file size"),
      Self::InvalidExtension => f.write_str("has not an SRM extension"),
      Self::NoExtension => f.write_str("path did not have an extension"),
      Self::Other(err) => f.write_fmt(format_args!("{err}")),
    }
  }
}
impl error::Error for SrmFileInferError {}

/// Defines an specific save file
#[derive(Debug, Clone, PartialEq)]
pub struct SaveFile {
  save_type: SaveType,
  file: PathBuf,
}

impl SaveFile {
  /// Gets the [SaveType] of this [SaveFile]
  pub fn save_type(&self) -> SaveType {
    self.save_type
  }

  pub(crate) fn is_controller_pack(&self) -> bool {
    matches!(self.save_type, SaveType::ControllerPack(_))
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

/// Basic common arguments for the conversion
#[derive(Clone, clap::Args, Debug, Default)]
pub struct BaseArgs {
  /// If set, the program will overwrite any existing files
  #[arg(long)]
  pub overwrite: bool,

  /// Is set, any FlashRAM or SRAM data will swap its endianness
  #[arg(long)]
  pub change_endianness: bool,

  /// If set, the 4 memory pack files will be merged into one
  #[arg(long)]
  pub merge_mempacks: bool,

  /// Sets the output directory for the created file (or files)
  #[arg(long)]
  pub output_dir: Option<PathBuf>,
}

#[cfg(test)]
mod tests {
  use std::{
    fs,
    io::Write,
    path::{Path, PathBuf},
  };

  use assert_fs::prelude::{PathAssert, PathChild};

  use crate::{
    controller_pack::{ControllerPack, ControllerPackInitializer},
    retroarch_srm::RetroArchSrm,
    word_byte_swap, ControllerPackKind, OutputDir, SaveFile, SaveType, SrmFile,
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

  #[test]
  fn verify_srm_file_try_into() {
    let srm: SrmFile = "file".into();
    assert_eq!(srm, SrmFile("file.srm".into()));

    let srm: SrmFile = "file.srm".into();
    assert_eq!(srm, SrmFile("file.srm".into()));

    let tmp_dir = assert_fs::TempDir::new().expect("tmp dir not created");

    let srm_path = tmp_dir.child("file.srm");

    let srm: SrmFile = srm_path.to_path_buf().try_into().unwrap();
    assert_eq!(srm, SrmFile(tmp_dir.join("file.srm")));

    // remove srm extension
    let srm_path = srm_path.with_extension("");

    // put srm data...
    fs::File::create(&srm_path)
      .and_then(|mut f| {
        let srm_data = Box::new(RetroArchSrm::new_init());
        f.write_all(srm_data.as_ref().as_ref())
      })
      .expect("could not create/write srm file");

    let srm: SrmFile = srm_path.try_into().expect("could not infer file");
    assert_eq!(srm, SrmFile(tmp_dir.join("file")));
  }

  #[test]
  fn verify_change_endianness() {
    let mut bytes = [1, 2, 3, 4];
    word_byte_swap(&mut bytes);
    assert_eq!(bytes, [4, 3, 2, 1]);
  }

  #[test]
  fn verify_output_dir_without_out_dir() {
    let out_dir = None;
    let base = Path::new("base");
    let output = OutputDir::new(&out_dir, &base);

    assert_eq!(output.base_with_extension("ext"), PathBuf::from("base.ext"));
    assert_eq!(
      output.to_out_dir("input/data.file"),
      PathBuf::from("data.file")
    );
    // absolute paths are not changed
    if cfg!(windows) {
      assert_eq!(
        output.to_out_dir("c:/input/data.file"),
        PathBuf::from("c:/input/data.file")
      );
    } else {
      assert_eq!(
        output.to_out_dir("/input/data.file"),
        PathBuf::from("/input/data.file")
      );
    }

    let out_dir = None;
    let base = Path::new("relative/base/file");
    let output = OutputDir::new(&out_dir, &base);
    assert_eq!(
      output.base_with_extension("ext"),
      PathBuf::from("relative/base/file.ext")
    );
    assert_eq!(
      output.to_out_dir("input/data.file"),
      PathBuf::from("relative/base/data.file")
    );
    // absolute paths are not changed
    if cfg!(windows) {
      assert_eq!(
        output.to_out_dir("c:/input/data.file"),
        PathBuf::from("c:/input/data.file")
      );
    } else {
      assert_eq!(
        output.to_out_dir("/input/data.file"),
        PathBuf::from("/input/data.file")
      );
    }

    let out_dir = Some("user/output/dir".into());
    let base = Path::new("something/base_file.data");
    let output = OutputDir::new(&out_dir, &base);
    assert_eq!(
      output.base_with_extension("ext"),
      PathBuf::from("user/output/dir/base_file.ext")
    );
    assert_eq!(
      output.to_out_dir("input/data.file"),
      PathBuf::from("user/output/dir/data.file")
    );
    // absolute paths are not changed
    if cfg!(windows) {
      assert_eq!(
        output.to_out_dir("c:/input/data.file"),
        PathBuf::from("c:/input/data.file")
      );
    } else {
      assert_eq!(
        output.to_out_dir("/input/data.file"),
        PathBuf::from("/input/data.file")
      );
    }
  }
}
