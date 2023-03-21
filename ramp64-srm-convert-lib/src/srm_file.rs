use std::{
  fs,
  ops::Deref,
  path::{Path, PathBuf},
  result,
};

use either::Either;

use crate::{ConvertibleFile, ConvertibleType, CreateError};

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

  fn from_file_len<P: AsRef<Path>>(path: P) -> result::Result<Self, CreateError> {
    fs::File::open(path.as_ref())
      .map_err(CreateError::Other)
      .and_then(|file| {
        if file.metadata().unwrap().len() == 0x48800 {
          Ok(Self(path.as_ref().to_path_buf()))
        } else {
          Err(CreateError::InvalidSize)
        }
      })
  }

  fn from_extension<P: AsRef<Path>>(path: P) -> result::Result<Self, CreateError> {
    let path = path.as_ref();
    path
      .extension()
      .ok_or(CreateError::NoExtension)
      .and_then(|ext| {
        if ext.to_ascii_uppercase().to_str() == Some("SRM") {
          Ok(Self(path.to_path_buf()))
        } else {
          Err(CreateError::UnknownExtension)
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
  type Error = CreateError;

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

impl<P> ConvertibleFile<P>
where
  P: AsRef<Path>,
{
  /// Checks if this [ConvertibleFile] is a [SrmFile]
  pub fn is_srm(&self) -> bool {
    matches!(self.part_type, ConvertibleType::Srm)
  }

  /// Gets the [SrmFile] of this value if it can become one
  pub fn try_into_srm(self) -> Either<SrmFile, Self> {
    if self.is_srm() {
      Either::Left(SrmFile(self.path.as_ref().to_path_buf()))
    } else {
      Either::Right(self)
    }
  }
}

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
        let srm_data = Box::new(RetroArchSrm::new());
        f.write_all(srm_data.as_ref().as_ref())
      })
      .expect("could not create/write srm file");

    let srm: SrmFile = srm_path.try_into().expect("could not infer file");
    assert_eq!(srm, SrmFile(tmp_dir.join("file")));
  }
}
