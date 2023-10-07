use std::{
  array::TryFromSliceError,
  fmt::Display,
  io::{Read, Seek},
  path::Path,
};

use flate2::Decompress;
use log::debug;

use crate::io::ReadExt;

struct Header {
  chunk_size: u32,
}

#[derive(Debug)]
pub struct RZipInfo {
  is_compressed: bool,
  real_len: u64,
}

impl RZipInfo {
  pub fn new<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
    let mut data = [0u8; 20];
    let len = {
      let mut file = std::fs::File::open(path)?;
      file.read_exact(&mut data)?;
      file.metadata().unwrap().len()
    };

    let is_compressed = b"#RZIPv\x01#" == &data[0..8];
    let real_len = if is_compressed {
      u64::from_le_bytes(data[12..20].try_into().unwrap())
    } else {
      len
    };

    Ok(Self {
      is_compressed,
      real_len,
    })
  }
}

impl RZipInfo {
  pub fn is_compressed(&self) -> bool {
    self.is_compressed
  }

  pub fn uncompressed_len(&self) -> u64 {
    self.real_len
  }
}

impl Display for RZipInfo {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "RZipInfo; -compressed: {}; -uncompressed_len: {}",
      self.is_compressed, self.real_len
    ))
  }
}

pub struct RZip {
  header: Header,
  file: std::fs::File,
  chunk: Option<Vec<u8>>,
}

impl RZip {
  pub fn open<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
    let file = std::fs::File::open(path)?;
    file.try_into()
  }
}

impl Read for RZip {
  fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
    if self.chunk.is_none() {
      let file_pos = self.file.stream_position()?;
      debug!("RZip: file position {file_pos}");
      if file_pos < 20 {
        self.file.seek(std::io::SeekFrom::Start(20))?;
      }
      // fetch next chunk

      let mut buf = [0; 4];
      self.file.read_exact(&mut buf)?;
      let chunk_size = u32::from_le_bytes(buf);
      debug!("RZip: reading chunk size {chunk_size}");
      let mut chunk = vec![0u8; chunk_size as usize];
      debug!("RZip: new chunk len {}", chunk.len());
      self.file.read_up_to(&mut chunk)?;

      let mut data = Vec::with_capacity(self.header.chunk_size as usize);

      let mut decompressor = Decompress::new(true);
      decompressor
        .decompress_vec(&chunk[..], &mut data, flate2::FlushDecompress::Sync)
        .map_err(|err| std::io::Error::new(std::io::ErrorKind::Other, err))?;

      debug!("RZip: decoded {} bytes of data", data.len());
      self.chunk = Some(data);
    }

    let data = self.chunk.as_mut().unwrap();

    // select the minimum between the buffer and the rest of the data
    let n = buf.len().min(data.len());

    debug!("RZip: reading {n} bytes of chunk data");

    // get the rest of the data into the chunk and swap it to have
    // the chunk data in chunk and the rest in data
    let mut chunk = data.split_off(n);
    std::mem::swap(data, &mut chunk);

    buf[..n].copy_from_slice(&chunk[..n]);

    // if there is no more data, then remove the chunk
    if data.is_empty() {
      self.chunk = None
    }

    Ok(n)
  }
}

impl TryFrom<std::fs::File> for RZip {
  type Error = std::io::Error;
  fn try_from(mut file: std::fs::File) -> Result<Self, Self::Error> {
    file.rewind()?;

    let mut data = [0; 20];

    file.read_exact(&mut data)?;

    // Read header
    // id : [#][R][Z][I][P][v][file format version][#]
    if b"#RZIPv"[..] != data[0..6] {
      return Err(RZipOpenError::NotRZip.into());
    }
    if data[6] != 1 {
      return Err(RZipOpenError::UnknownVersion.into());
    }
    if data[7] != b'#' {
      return Err(RZipOpenError::BadHeader.into());
    }

    // SAFETY: the following unwrap is safe because we read exactly 20 bytes before
    let chunk_size = u32::from_le_bytes(data[8..12].try_into().unwrap());

    Ok(Self {
      header: Header { chunk_size },
      file,
      chunk: None,
    })
  }
}

#[derive(Debug)]
pub enum RZipOpenError {
  NotRZip,
  UnknownVersion,
  BadHeader,
  InvalidRZipHeader,
}

impl From<RZipOpenError> for std::io::Error {
  fn from(value: RZipOpenError) -> Self {
    Self::new(std::io::ErrorKind::Other, value)
  }
}

impl Display for RZipOpenError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      RZipOpenError::NotRZip => f.write_str("not an rzip file"),
      RZipOpenError::UnknownVersion => f.write_str("unknown rzip version"),
      RZipOpenError::BadHeader => f.write_str("bad rzip header"),
      RZipOpenError::InvalidRZipHeader => f.write_str("invalid rzip header"),
    }
  }
}

impl std::error::Error for RZipOpenError {}

impl From<TryFromSliceError> for RZipOpenError {
  fn from(_: TryFromSliceError) -> Self {
    Self::InvalidRZipHeader
  }
}

#[cfg(test)]
mod tests {
  #[test]
  fn verify_rzip_file() {}
}
