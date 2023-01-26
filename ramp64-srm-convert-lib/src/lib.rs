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
pub use convert_params::{ConvertMode, ConvertParams};
pub use grouping::{group_saves, validate_groups, GroupedSaves, Grouping, InvalidGroup, Problem};

use log::info;

fn change_endianness(buf: &mut [u8]) {
  for i in (0..buf.len()).step_by(4) {
    buf.swap(i + 0, i + 3);
    buf.swap(i + 1, i + 2);
  }
}

/// Provides the path of the error
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
      _ => f.write_fmt(format_args!("{err} with {}", path.display())),
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

  fn output_path<P>(&self, input: &P) -> PathBuf
  where
    P: AsRef<Path> + ?Sized,
  {
    let input = input.as_ref();
    if input.is_absolute() {
      return input.into();
    }
    let file_name = input.file_name().unwrap_or(&ffi::OsStr::new(""));
    let mut path: PathBuf = if let Some(out_dir) = self.out_dir {
      out_dir
    } else {
      self.base.parent().unwrap()
    }
    .into();
    path.push(file_name);
    path
  }

  fn from_base<S>(&self, ext: &S) -> PathBuf
  where
    S: AsRef<ffi::OsStr> + ?Sized,
  {
    self.output_path(self.base.with_extension(ext).file_name().unwrap())
  }
}

/// Represents the kind of controller pack in use
#[repr(i32)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ControllerPackKind {
  /// The controller pack attached to the first player
  Player1,
  /// The controller pack attached to the second player
  Player2,
  /// The controller pack attached to the third player
  Player3,
  /// The controller pack attached to the fourth player
  Player4,
  /// The merged controller pack used by Mupen64 input
  Mupen,
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
    if value == ControllerPackKind::Mupen {
      return 0;
    }
    value as usize
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
#[derive(Debug, PartialEq, Eq, Clone)]
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

/// Specifies a typed SRM file
#[derive(Debug, PartialEq, Clone)]
pub struct SrmFile(PathBuf);
impl SrmFile {
  fn from_name<S: AsRef<str>>(name: S) -> Self {
    Self(Path::new(name.as_ref()).with_extension("srm"))
  }
  fn from_file_len<P: AsRef<Path>>(path: P) -> result::Result<Self, SrmFileInferError> {
    fs::File::open(path.as_ref())
      .or_else(|e| Err(SrmFileInferError::Other(e)))
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
      Self::from_file_len(&path).or_else(|_| Self::from_extension(path))
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

/// Defines an specific save file
#[derive(Debug, Clone, PartialEq)]
pub struct SaveFile {
  save_type: SaveType,
  file: PathBuf,
}

impl SaveFile {
  pub(crate) fn is_controller_pack(&self) -> bool {
    match self.save_type {
      SaveType::ControllerPack(_) => true,
      _ => false,
    }
  }

  pub(crate) fn from_file_len<P: AsRef<Path>>(path: P) -> result::Result<Self, SaveFileInferError> {
    fs::File::open(path.as_ref())
      .or_else(|e| Err(e.into()))
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
            .or_else(|e| Err(e.into())),
          // 1Mbit flash ram or 4 merged controller packs
          0x20000 => ControllerPack::infer_from(&mut file)
            .map(|cp| {
              if cp {
                SaveType::ControllerPack(ControllerPackKind::Mupen)
              } else {
                SaveType::FlashRam
              }
            })
            .or_else(|e| Err(e.into())),
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

impl TryFrom<PathBuf> for SaveFile {
  type Error = SaveFileInferError;

  fn try_from(path: PathBuf) -> result::Result<Self, SaveFileInferError> {
    if path.exists() {
      Self::from_file_len(&path).or_else(|_| Self::from_extension(path))
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

/// Converts to/from SRM files based on the parameters
pub fn convert(params: ConvertParams, args: &BaseArgs) -> Result {
  let mode_str = format!("{params}");
  let ConvertParams { mode, file, paths } = params;
  match mode {
    ConvertMode::Create => {
      if args.merge_mempacks {
        info!("{mode_str} using {paths:#}");
      } else {
        info!("{mode_str} using {paths}");
      }
      create_srm::create_srm(file, args, paths)
    }
    ConvertMode::Split => {
      if paths.any_is_file() {
        if args.merge_mempacks {
          info!("{mode_str} into: {paths:#}")
        } else {
          info!("{mode_str} into: {paths}")
        };
      }
      split_srm::split_srm(file, args, paths)
    }
  }
}

#[cfg(test)]
mod tests {

  #[test]
  fn verify_save_file_try_from() {}
}
