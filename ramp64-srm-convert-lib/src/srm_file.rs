use std::{
  error, fmt, fs, io,
  ops::Deref,
  path::{Path, PathBuf},
  result,
};

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

#[cfg(test)]
mod tests {
  use std::{fs, io::Write};

  use assert_fs::prelude::PathChild;

  use crate::retroarch_srm::RetroArchSrm;

  use super::SrmFile;

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
}
