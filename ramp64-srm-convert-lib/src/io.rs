//! Types to handle common IO related tasks, and avoid multiple times a file

#[derive(Debug)]
/// Defines special error cases when dealing with save files
pub enum Error {
  /// The file size in invalid for the specified type
  InvalidSize,
  /// The file has an invalid or empty extension
  InvalidExtension,
  /// The specified path is a directory
  PathIsDirectory,
  /// An unspecified IO error happened
  Other(std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

impl From<std::io::Error> for Error {
  fn from(error: std::io::Error) -> Self {
    Self::Other(error)
  }
}

impl std::fmt::Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::InvalidSize => f.write_str("invalid file size"),
      Self::InvalidExtension => f.write_str("invalid or empty extension"),
      Self::PathIsDirectory => f.write_str("path is not a file"),
      Self::Other(io_err) => f.write_fmt(format_args!("unexpected: {io_err}")),
    }
  }
}

impl std::error::Error for Error {}

pub(crate) trait ReadExt
where
  Self: std::io::Read,
{
  /// Tries to fill the buffer if there is enough data, otherwise returns all what was read
  fn read_up_to(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
    let max_read = buf.len();
    let mut bytes_read = 0;
    while bytes_read < max_read {
      match self.read(&mut buf[bytes_read..max_read]) {
        Ok(0) => break,
        Ok(bytes) => bytes_read += bytes,
        Err(err) if err.kind() == std::io::ErrorKind::Interrupted => continue,
        Err(err) => return Err(err),
      }
    }
    Ok(bytes_read)
  }
}

impl<T> ReadExt for T where Self: std::io::Read {}

#[cfg(test)]
mod tests {
  use super::ReadExt;

  use std::io::{Cursor, Read};

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
