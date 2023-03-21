//! # RetroArch Mupen64Plus SRM Converter Lib
//!
//! `ramp64-srm-convert-lib` is the library portion of `ra_mp64_srm_convert` in an attempt to
//! make GUIs out of the same code.

#![deny(missing_docs)]

mod battery_file;
mod controller_pack;
mod controller_pack_file;
mod convert_params;
mod create_srm;
mod game_pack;
mod retroarch_srm;
mod split_srm;
mod srm_file;

/// Provides the v2 API
pub mod v2;

use std::{
  ffi::{self},
  fmt,
  io::{self, Read, Seek},
  path::{Path, PathBuf},
};

pub use battery_file::{BatteryFile, BatteryType};
pub use controller_pack_file::{ControllerPackFile, MupenPackFile, Player};
pub use convert_params::{convert, ConvertMode, ConvertParams, Problem};
pub use srm_file::SrmFile;

fn word_byte_swap(buf: &mut [u8]) {
  for i in (0..buf.len()).step_by(4) {
    buf.swap(i, i + 3);
    buf.swap(i + 1, i + 2);
  }
}

fn is_rzip_file<F>(file: &mut F) -> std::io::Result<bool>
where
  F: Read + Seek,
{
  // (https://github.com/libretro/RetroArch/blob/master/libretro-common/streams/rzip_stream.c)

  let mut magic = [0u8; 8];
  file.read_up_to(&mut magic)?;
  file.rewind()?;
  Ok(&magic == b"#RZIPv1#")
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
        f.write_fmt(format_args!("will overwrite \"{}\"", path.display()))
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

#[derive(Debug)]
enum ConvertibleType {
  Battery {
    size: Option<u64>,
    extension: Option<String>,
  },
  ControllerPack {
    size: Option<u64>,
  },
  Srm,
}

#[derive(Debug)]
/// Represents a convertible file
pub struct ConvertibleFile<P>
where
  P: AsRef<Path>,
{
  part_type: ConvertibleType,
  path: P,
}

impl<P> ConvertibleFile<P>
where
  P: AsRef<Path>,
{
  fn srm(path: P) -> Self {
    Self {
      part_type: ConvertibleType::Srm,
      path,
    }
  }

  fn battery(size: Option<u64>, extension: Option<String>, path: P) -> Self {
    Self {
      part_type: ConvertibleType::Battery { size, extension },
      path,
    }
  }

  fn controller_pack(size: Option<u64>, path: P) -> Self {
    Self {
      part_type: ConvertibleType::ControllerPack { size },
      path,
    }
  }
}

#[derive(Debug)]
/// Holds the possible errors when trying to check if a path holds a [ConvertibleFile]
pub enum CheckConvertibleError {
  /// The specified new file path did not contain an extension
  NewFileNoExtension,
  /// The specified new file path did not have a known extension
  NewFileUnknownExtension,
  /// The specified existing path is not a file
  PathNotAFile,
  /// The specified existing file cannot be converted
  NonConvertible,
  /// An IO error
  Other(std::io::Error),
}

impl From<std::io::Error> for CheckConvertibleError {
  fn from(value: std::io::Error) -> Self {
    Self::Other(value)
  }
}

impl fmt::Display for CheckConvertibleError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::NewFileNoExtension => f.write_str("new path without extension"),
      Self::NewFileUnknownExtension => f.write_str("new path has an unknown extension"),
      Self::PathNotAFile => f.write_str("existing path is not a file"),
      Self::NonConvertible => f.write_str("existing file cannot be converted"),
      Self::Other(err) => err.fmt(f),
    }
  }
}

impl std::error::Error for CheckConvertibleError {}

type ConvertibleResult<P> = std::result::Result<ConvertibleFile<P>, CheckConvertibleError>;

/// Determines if the file is a [ConvertibleFile].
pub fn try_into_convertible_file<P>(path: P) -> ConvertibleResult<P>
where
  P: AsRef<Path>,
{
  if !path.as_ref().exists() {
    match path.as_ref().extension() {
      Some(ext) => match ext.to_ascii_uppercase().to_str() {
        Some("SRM") => Ok(ConvertibleFile::srm(path)),
        ext @ Some("EEP" | "SRA" | "FLA") => {
          Ok(ConvertibleFile::battery(None, ext.map(String::from), path))
        }
        Some("MPK" | "MPK1" | "MPK2" | "MPK3" | "MPK4") => {
          Ok(ConvertibleFile::controller_pack(None, path))
        }
        _ => Err(CheckConvertibleError::NewFileUnknownExtension),
      },
      None => Err(CheckConvertibleError::NewFileNoExtension),
    }
  } else if path.as_ref().is_file() {
    read_into_convertible_file(path)
  } else {
    Err(CheckConvertibleError::PathNotAFile)
  }
}

fn read_into_convertible_file<P>(path: P) -> ConvertibleResult<P>
where
  P: AsRef<Path>,
{
  // ugh.. now by file size and maybe other stuff
  let metadata = std::fs::metadata(&path)?;
  if metadata.len() < 8 {
    return Ok(ConvertibleFile::battery(
      Some(metadata.len()),
      Some("eep".to_string()),
      path,
    ));
  }

  {
    // for file close
    let mut file = std::fs::File::open(&path)?;
    if is_rzip_file(&mut file)? {
      return Ok(ConvertibleFile::srm(path));
    }
    if controller_pack::ControllerPack::infer_from(&mut file)? {
      return Ok(ConvertibleFile::controller_pack(Some(metadata.len()), path));
    }
  }

  match metadata.len() {
    0x1..=0x20000 => Ok(ConvertibleFile::battery(Some(metadata.len()), None, path)),
    0x48800 => Ok(ConvertibleFile::srm(path)),
    _ => Err(CheckConvertibleError::NonConvertible),
  }
}

trait ReadExt
where
  Self: Read,
{
  /// Tries to fill the buffer if there is enough data, otherwise returns all what was read
  fn read_up_to(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
    let max_read = buf.len();
    let mut bytes_read = 0;
    while bytes_read < max_read {
      match self.read(&mut buf[bytes_read..max_read]) {
        Ok(0) => break,
        Ok(bytes) => bytes_read += bytes,
        Err(err) if err.kind() == io::ErrorKind::Interrupted => continue,
        Err(err) => return Err(err),
      }
    }
    Ok(bytes_read)
  }
}

impl<T> ReadExt for T where Self: Read {}

/// An error from the construction of a [SaveFile] or [crate::SrmFile] from an existing file
#[derive(Debug)]
pub enum CreateError {
  /// Specified path is a directory
  IsADir,
  /// Specified file is empty
  FileEmpty,
  /// Specified file size does not match any existing save
  FileTooLarge,
  /// Specified file does not have a extension
  NoExtension,
  /// Specified file does have an unknown extension
  UnknownExtension,
  /// Only when matching a [SaveFile]: specified file is an [crate::SrmFile]
  IsASrm,
  /// Specified file is not the passed type
  AnotherType(BatteryType),
  /// Only when matching a [crate::SrmFile]: specified file is not of the expected size
  InvalidSize,
  /// An IO error
  Other(io::Error),
}

impl From<io::Error> for CreateError {
  fn from(value: io::Error) -> Self {
    Self::Other(value)
  }
}

impl fmt::Display for CreateError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::IsADir => f.write_str("path was a folder"),
      Self::FileEmpty => f.write_str("file is empty"),
      Self::FileTooLarge => f.write_str("file is to large for a save"),
      Self::AnotherType(other_type) => {
        f.write_fmt(format_args!("file actual type is {other_type}"))
      }
      Self::IsASrm => f.write_str("file is an SRM"),
      Self::Other(err) => f.write_fmt(format_args!("an IO error happened: {err}")),
      Self::NoExtension => f.write_str("file does not have an extension"),
      Self::UnknownExtension => f.write_str("file has an unknown extension"),
      Self::InvalidSize => f.write_str("file size was not expected"),
    }
  }
}

impl std::error::Error for CreateError {}

/// Provides a way to get the expected output file based on a base file and an optional output
/// directory.
pub struct OutputDir<'out, 'base> {
  out_dir: &'out Option<PathBuf>,
  base: &'base Path,
}
impl<'out, 'base> OutputDir<'out, 'base> {
  /// Returns a new [OutputDir].
  pub fn new(out_dir: &'out Option<PathBuf>, base: &'base Path) -> Self {
    Self { out_dir, base }
  }

  /// Uses the specified `input` path to build the expected output path.
  ///
  /// # Remarks
  ///
  /// If the `input` path is absolute, it will not be changed.
  pub fn to_out_dir<P>(&self, input: &P) -> PathBuf
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

  /// Creates a new path from `base` by changing its extension and considering the possible output
  /// directory.
  pub fn base_with_extension<S>(&self, ext: &S) -> PathBuf
  where
    S: AsRef<ffi::OsStr> + ?Sized,
  {
    self.to_out_dir(self.base.with_extension(ext).file_name().unwrap())
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

#[cfg(test)]
mod tests {
  use std::path::{Path, PathBuf};

  use crate::{word_byte_swap, OutputDir};

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

  use std::io::{Cursor, Read};

  use crate::ReadExt;

  #[test]
  fn test_read_ext() {
    let data = b"Hello World!";

    let mut buf = [0u8; 15];

    let mut cursor = Cursor::new(data);
    cursor.read_exact(&mut buf).expect_err("buffer is larger");

    cursor.set_position(0);

    let len = cursor.read_up_to(&mut buf).expect("idk");

    assert_eq!(buf[..len], data[..]);
  }
}
