//! Split a SRM file into its components

use super::utils::word_swap;
use super::{Converter, IsEmpty, UserParams};

use crate::io::ReadExt;

use std::io::prelude::*;

#[derive(Clone, Debug, PartialEq)]
/// Parameters to split a SRM file
pub struct Params {
  srm_file: std::path::PathBuf,
  output_mupen: bool,
  out_dir: Option<std::path::PathBuf>,
  name: Option<String>,
}

impl Params {
  /// Creates a new [Params] value
  pub fn new<S>(srm_file: S) -> Self
  where
    S: Into<std::path::PathBuf>,
  {
    Self {
      srm_file: srm_file.into(),
      output_mupen: false,
      out_dir: None,
      name: None,
    }
  }

  /// Sets to output a mupen pack file instead of each controller pack
  pub fn set_output_mupen_pack(self, output_mupen: bool) -> Self {
    Self {
      output_mupen,
      ..self
    }
  }

  /// Sets the output directory
  pub fn set_out_dir<P>(self, out_dir: Option<P>) -> Self
  where
    P: Into<std::path::PathBuf>,
  {
    Self {
      out_dir: out_dir.map(|v| v.into()),
      ..self
    }
  }

  /// Sets the name of the file
  pub fn set_name<S>(self, name: Option<S>) -> Self
  where
    S: Into<String>,
  {
    Self {
      name: name.map(|v| v.into()),
      ..self
    }
  }

  /// Gets mutable access to this [Params].
  pub fn as_mut(&mut self) -> ParamsMut<'_> {
    ParamsMut(self)
  }
}

/// Mutable access to a [Params] value.
pub struct ParamsMut<'p>(&'p mut Params);

impl<'p> ParamsMut<'p> {
  /// Sets the SRM path.
  pub fn set_srm_file(&'p mut self, path: impl Into<std::path::PathBuf>) {
    self.0.srm_file = path.into();
  }
}

/// Enumerates the different validation results
#[derive(Debug)]
pub enum Validation {
  /// Successful validation
  Ok,
  /// SRM File Not Found
  FileNotFound,
  /// Invalid SRM File Size
  InvalidSrmSize,
  /// Output Directory is Not a Directory
  OutputDirIsNotDir,
  /// SRM File is Empty
  SrmEmpty,
}

impl Validation {
  /// Checks if the validation succeeded
  pub fn is_ok(&self) -> bool {
    matches!(self, Validation::Ok)
  }
}

impl std::fmt::Display for Validation {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Validation::Ok => f.write_str("OK"),
      Validation::FileNotFound => f.write_str("SRM file does not exist"),
      Validation::InvalidSrmSize => f.write_str("invalid SRM file size"),
      Validation::OutputDirIsNotDir => f.write_str("output directory is not a directory"),
      Validation::SrmEmpty => f.write_str("SRM file has no data"),
    }
  }
}

impl std::ops::BitOr for Validation {
  type Output = Self;

  fn bitor(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (Validation::Ok, rhs) => rhs,
      (lhs, _) => lhs,
    }
  }
}

impl From<Validation> for bool {
  fn from(value: Validation) -> Self {
    matches!(value, Validation::Ok)
  }
}

impl Converter for Params {
  type Error = super::Error;

  type Validation = Validation;

  fn validate(&self) -> Self::Validation {
    self
      .srm_file
      .metadata()
      .map_or(Validation::FileNotFound, |m| {
        if m.is_file() && m.len() == 0x48800 {
          let mut srm_data = super::SrmBuf::new();
          std::fs::File::open(&self.srm_file)
            .and_then(|mut f| f.read_up_to(&mut srm_data))
            .map_or(Validation::FileNotFound, |_| {
              if srm_data.has_save_data() {
                Validation::Ok
              } else {
                Validation::SrmEmpty
              }
            })
        } else {
          Validation::InvalidSrmSize
        }
      })
      | self.out_dir.as_ref().map_or(Validation::Ok, |o| {
        if o.is_dir() {
          Validation::Ok
        } else {
          Validation::OutputDirIsNotDir
        }
      })
  }

  fn convert(self, user_params: &UserParams) -> Result<(), Self::Error> {
    let Self {
      srm_file,
      output_mupen,
      out_dir,
      name,
    } = self;

    // get the name
    let name = name.map_or_else(
      || srm_file.file_name().map(std::path::PathBuf::from).unwrap(),
      std::path::PathBuf::from,
    );

    // build the base output path
    let base_path = out_dir.map_or_else(|| srm_file.clone(), |o| o.join(name));

    // load the srm data
    let mut srm_data = super::SrmBuf::new();
    std::fs::File::open(&srm_file)
      .and_then(|mut file| file.read_up_to(&mut srm_data))
      .map_err(|e| super::Error(srm_file, e))?;

    let file_writer = FileWriter::new(&base_path, user_params);

    if !srm_data.eeprom().is_empty() {
      if user_params.swap_bytes {
        word_swap(srm_data.eeprom_mut());
      }

      let buf = if srm_data.eeprom().is_4k() {
        srm_data.eeprom().as_4k()
      } else {
        srm_data.eeprom()
      };

      file_writer.create_file(buf.as_ref(), "eep")?;
    }

    if !srm_data.sram().is_empty() {
      file_writer.create_file(srm_data.sram(), "sra")?;
    }

    if !srm_data.flashram().is_empty() {
      if user_params.swap_bytes {
        word_swap(srm_data.flashram_mut());
      }
      file_writer.create_file(srm_data.flashram(), "fla")?;
    }

    if srm_data.controller_pack_iter().any(|cp| !cp.is_empty()) {
      if output_mupen {
        file_writer.create_file(srm_data.full_controller_pack(), "mpk")?;
      } else {
        for (i, cp) in srm_data.controller_pack_iter().enumerate() {
          if !cp.is_empty() {
            file_writer.create_file(cp.as_ref(), format!("mpk{}", i + 1))?;
          }
        }
      }
    }

    Ok(())
  }
}

struct FileWriter<'p> {
  base_path: &'p std::path::Path,
  create_opts: std::fs::OpenOptions,
}

impl<'p> FileWriter<'p> {
  fn new<'b: 'p>(base_path: &'b std::path::Path, user_params: &UserParams) -> Self {
    let mut create_opts = std::fs::OpenOptions::new();
    create_opts
      .create(user_params.overwrite)
      .create_new(!user_params.overwrite)
      .write(true);
    Self {
      base_path,
      create_opts,
    }
  }

  fn create_file<E: AsRef<std::ffi::OsStr>>(
    &self,
    buf: impl AsRef<[u8]>,
    ext: E,
  ) -> Result<(), super::Error> {
    let path = self.base_path.with_extension(ext);
    self
      .create_opts
      .open(&path)
      .and_then(|mut file| file.write_all(buf.as_ref()))
      .map_err(|e| super::Error(path, e))
  }
}

/// Checks the given path to determine if it can be used as a SRM file
pub fn can_be_srm<P>(path: P) -> Result<P, (P, crate::io::Error)>
where
  P: AsRef<std::path::Path>,
{
  match path.as_ref().metadata() {
    Ok(metadata) => {
      if metadata.is_file() && metadata.len() == 0x48800 {
        Ok(path)
      } else {
        Err(if metadata.is_dir() {
          (path, crate::io::Error::PathIsDirectory)
        } else {
          (path, crate::io::Error::InvalidSize)
        })
      }
    }
    Err(e) => {
      if e.kind() == std::io::ErrorKind::NotFound {
        if path
          .as_ref()
          .extension()
          .map_or(false, |ext| (ext.to_ascii_uppercase() == "SRM"))
        {
          Ok(path)
        } else {
          Err((path, crate::io::Error::InvalidExtension))
        }
      } else {
        Err((path, e.into()))
      }
    }
  }
}
