//! # RetroArch Mupen64Plus SRM Converter Lib
//!
//! `ramp64-srm-convert-lib` is the library portion of `ra_mp64_srm_convert` in an attempt to
//! make GUIs out of the same code.

#![deny(missing_docs)]

mod controller_pack;
mod convert_params;
mod create_srm;
mod game_pack;
mod retroarch_srm;
mod save_file;
mod split_srm;
mod srm_file;

use std::{
  ffi, fmt, io,
  path::{Path, PathBuf},
};

pub use convert_params::{convert, ConvertMode, ConvertParams, Problem};
pub use save_file::{By, ControllerPackSlot, SaveFile, SaveFileInferError, SaveType, User};
pub use srm_file::{SrmFile, SrmFileInferError};

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
}
