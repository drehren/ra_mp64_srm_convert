//! Controller Pack File Type

use crate::io::Error;

use std::path::PathBuf;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Kind {
  Mupen(std::path::PathBuf),
  ControllerPack {
    first: usize,
    packs: [Option<std::path::PathBuf>; 4],
  },
}

#[derive(Clone, Debug, PartialEq)]
/// Represents a Mupen pack path or the four controller packs paths.
pub struct ControllerPackPaths(Kind);

impl ControllerPackPaths {
  pub(crate) fn is_valid_size(&self) -> bool {
    match &self.0 {
      Kind::Mupen(mp) => check_file_size(mp, 0x20000),
      Kind::ControllerPack { packs, .. } => packs
        .iter()
        .filter_map(Option::as_ref)
        .all(|p| check_file_size(p, 0x8000)),
    }
  }

  pub(crate) fn replace_from(&mut self, other: ControllerPackPaths) -> Option<ControllerPackPaths> {
    match (&mut self.0, other.0) {
      (_, other @ Kind::Mupen(_)) | (Kind::Mupen(_), other @ Kind::ControllerPack { .. }) => {
        Some(std::mem::replace(self, Self(other)))
      }
      (
        Kind::ControllerPack { first, packs },
        Kind::ControllerPack {
          first: other_first,
          packs: mut other_packs,
        },
      ) => {
        let mut replaced = [None, None, None, None];
        let mut did_replace = false;

        for i in 0..4 {
          if other_packs[i].is_some() {
            did_replace |= packs[i].is_some() && packs[i] != other_packs[i];
            replaced[i] = std::mem::replace(&mut packs[i], other_packs[i].take());
          }
        }

        did_replace.then_some(Self(Kind::ControllerPack {
          first: std::mem::replace(first, other_first),
          packs: replaced,
        }))
      }
    }
  }

  /// Returns true if this [ControllerPackPaths] is a Mupen pack
  pub fn is_mupen(&self) -> bool {
    matches!(self.0, Kind::Mupen(_))
  }

  /// Gets the paths from this [`ControllerPackPaths`]
  pub fn into_indexed_paths(self) -> Vec<(usize, std::path::PathBuf)> {
    match self.0 {
      Kind::Mupen(mp) => vec![(0, mp)],
      Kind::ControllerPack { packs, .. } => packs
        .into_iter()
        .enumerate()
        .filter_map(|(i, p)| p.map(|p| (i, p)))
        .collect::<Vec<_>>(),
    }
  }

  /// Gets the path of the pack that initialized this [`ControllerPackPaths`]
  pub fn first_path(&self) -> &std::path::Path {
    match &self.0 {
      Kind::Mupen(mp) => mp,
      Kind::ControllerPack { first, packs } => packs[*first].as_ref().unwrap(),
    }
  }
}

impl std::fmt::Display for ControllerPackPaths {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self.0 {
      Kind::Mupen(_) => f.write_str("Mupen Controller Pack"),
      Kind::ControllerPack { .. } => f.write_str("Controller Pack"),
    }
  }
}

impl From<ControllerPackPaths> for std::path::PathBuf {
  fn from(value: ControllerPackPaths) -> Self {
    match value.0 {
      Kind::Mupen(mp) => mp,
      Kind::ControllerPack { first, mut packs } => packs[first].take().unwrap(),
    }
  }
}

fn check_file_size(path: &std::path::Path, expected_len: u64) -> bool {
  let Ok(result) = std::fs::metadata(path).map(|m| m.len() == expected_len) else {
    return false;
  };
  result
}

fn index_from(path: Option<&std::path::PathBuf>) -> Option<usize> {
  path
    .and_then(|p| p.extension())
    .map(|e| e.to_ascii_uppercase())
    .and_then(|e| {
      ["MPK", "MPK1", "MPK2", "MPK3", "MPK4"]
        .iter()
        .position(|&cp| e == cp)
        .map(|i| i.saturating_sub(1))
    })
}

/// Tries to create a [ControllerPackPaths], consuming the path if it succeeds.
///
/// # Examples
/// ```
/// use ramp64_srm_convert_lib::to_controller_pack;
/// # use assert_fs::{TempDir, prelude::*};
/// # use std::io::Write;
///
/// // create a mupen pack from an non-existent path
/// let mupen = to_controller_pack("MP.mpk".into());
/// assert!(mupen.is_ok());
/// assert!(mupen.unwrap().is_mupen());
///
/// // create paths containing player 1 pack path
/// let player = to_controller_pack("P1.mpk1".into());
/// assert!(player.is_ok());
/// assert!(!player.unwrap().is_mupen());
///
/// // if path is a file, but no controller pack, path is not consumed
/// let bad_cp_path = std::path::PathBuf::from("bad.mpk");
/// # let tmp_dir = TempDir::new()?;
/// # let bad_cp_path = tmp_dir.child(bad_cp_path).to_path_buf();
/// std::fs::File::create(&bad_cp_path).and_then(|f| f.set_len(523))?;
///
/// let no_cp = to_controller_pack(bad_cp_path);
/// assert!(no_cp.is_err());
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
pub fn to_controller_pack(path: PathBuf) -> Result<ControllerPackPaths, (PathBuf, Error)> {
  match path.metadata() {
    Ok(metadata) => match metadata.len() {
      0x8000 if is_controller_pack(std::fs::File::open(&path)) => Ok({
        let first = index_from(Some(&path)).unwrap_or(0);
        let mut packs = [None, None, None, None];
        packs[first] = Some(path);
        ControllerPackPaths(Kind::ControllerPack { first, packs })
      }),
      0x20000 if is_controller_pack(std::fs::File::open(&path)) => {
        Ok(ControllerPackPaths(Kind::Mupen(path)))
      }
      _ => Err((path, crate::io::Error::InvalidSize)),
    },
    Err(err) => {
      if err.kind() == std::io::ErrorKind::NotFound {
        // check if the extension is correct
        if let Some(extension) = path.extension() {
          let ext = extension.to_ascii_uppercase();
          if let Some(first) = ["MPK1", "MPK2", "MPK3", "MPK4"]
            .iter()
            .position(|cp| *cp == ext)
          {
            let mut packs = [None, None, None, None];
            packs[first] = Some(path);
            Ok(ControllerPackPaths(Kind::ControllerPack { first, packs }))
          } else if "MPK" == ext {
            Ok(ControllerPackPaths(Kind::Mupen(path)))
          } else {
            Err((path, crate::io::Error::InvalidSize))
          }
        } else {
          Ok(ControllerPackPaths(Kind::ControllerPack {
            first: 0,
            packs: [Some(path), None, None, None],
          }))
        }
      } else {
        Err((path, crate::io::Error::InvalidSize))
      }
    }
  }
}

fn is_controller_pack<F, E>(file: Result<F, E>) -> bool
where
  F: std::io::Read,
  E: From<std::io::Error>,
{
  let mut buf: [u8; 256] = [0u8; 256];

  match file.and_then(|mut f| Ok(f.read_exact(&mut buf)?)) {
    Ok(_) => {}
    Err(_) => return false,
  }

  for range in [32..64, 96..128, 128..160, 192..224] {
    let block = &buf[range];

    // checksum1
    let sum1 = checksum1(block);
    if sum1 != block[28..30] {
      return false;
    }

    let sum2 = checksum2(&block[28..30]);
    if sum2 != block[30..32] {
      return false;
    }
  }

  true
}

fn checksum1(buf: &[u8]) -> [u8; 2] {
  let mut sum1 = 0u16;
  for half_word in buf[0..24].chunks(2) {
    sum1 = sum1.wrapping_add(u16::from_be_bytes(half_word.try_into().unwrap()));
  }
  sum1
    .wrapping_add(u16::from_be_bytes([buf[24], buf[25]]))
    .wrapping_add(u16::from_be_bytes([buf[26], buf[27]]))
    .to_be_bytes()
}

fn checksum2(buf: &[u8]) -> [u8; 2] {
  u16::from_be_bytes([0xff, 0xf2])
    .wrapping_sub(u16::from_be_bytes(buf.try_into().unwrap()))
    .to_be_bytes()
}

pub(crate) fn is_empty(mut buf: &[u8]) -> bool {
  const FREE_SPACE: u16 = u16::from_be_bytes([0, 3]);
  buf = &buf[256..512];
  // check the index table, if all unallocated there is no prob
  for v in buf[10..].chunks(2) {
    let val = u16::from_be_bytes(v.try_into().unwrap());
    if val != FREE_SPACE {
      return false;
    }
  }
  true
}

pub(crate) fn init(buf: &mut [u8]) {
  const MUPEN64_SERIAL: [u8; 24] = [
    0xff, 0xff, 0xff, 0xff, 0x05, 0x1a, 0x5f, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  ];

  buf.fill(0);
  buf[0] = 0x81;
  for (i, b) in buf[0..32].iter_mut().enumerate().skip(1) {
    *b = i as u8;
  }
  buf[32..56].copy_from_slice(&MUPEN64_SERIAL);
  buf[56] = 0xff;
  buf[57] = 0xff;
  buf[58] = 0x01;
  buf[59] = 0xff;
  let s1 = checksum1(&buf[32..64]);
  buf[60..62].copy_from_slice(&s1);
  let s2 = checksum2(&buf[60..62]);
  buf[62..64].copy_from_slice(&s2);

  buf.copy_within(32..64, 96);
  buf.copy_within(32..64, 128);
  buf.copy_within(32..64, 192);

  let table = &mut buf[256..512];
  table[0] = 0;
  table[1] = 113;
  for (i, b) in table[10..].iter_mut().enumerate() {
    *b = ((i % 2) * 3) as u8;
  }

  buf.copy_within(256..512, 512);
}

#[cfg(test)]
mod tests {
  use super::{index_from, init, is_empty, to_controller_pack, ControllerPackPaths, Kind};

  macro_rules! build_pack {
    (0) => {
      [Some(_), None, None, None]
    };
    (1) => {
      [None, Some(_), None, None]
    };
    (2) => {
      [None, None, Some(_), None]
    };
    (3) => {
      [None, None, None, Some(_)]
    };
  }

  macro_rules! assert_cp {
    ($cp:expr, $index:tt) => {
      assert!(matches!(
        $cp.0,
        Kind::ControllerPack {
          first: $index,
          packs: build_pack!($index)
        }
      ));
    };
  }

  macro_rules! expect {
      ($v:expr $(,$fmt:tt)*) => {{
        match $v {
          Ok(x) => x,
          Err(_) => panic!($($fmt)*),
        }
      }};
  }

  #[test]
  fn test_no_empty_controller_pack() {
    let mut cp1 = expect!(to_controller_pack("A.mpk1".into()));
    assert_cp!(cp1, 0);
    assert!(!cp1.is_mupen());

    let cp2 = expect!(to_controller_pack("A.mpk2".into()), "controller pack 2");
    assert_cp!(cp2, 1);
    assert!(!cp2.is_mupen());
    assert_eq!(cp1.replace_from(cp2), None);
    assert!(matches!(
      cp1.0,
      Kind::ControllerPack {
        first: 1,
        packs: [Some(_), Some(_), None, None]
      }
    ));

    let cp3 = expect!(to_controller_pack("A.mpk3".into()), "controller pack 3");
    assert_cp!(cp3, 2);
    assert!(!cp3.is_mupen());
    assert_eq!(cp1.replace_from(cp3), None);
    assert!(matches!(
      cp1.0,
      Kind::ControllerPack {
        first: 2,
        packs: [Some(_), Some(_), Some(_), None]
      }
    ));

    let cp4 = expect!(to_controller_pack("A.mpk4".into()), "controller pack 4");
    assert_cp!(cp4, 3);
    assert!(!cp4.is_mupen());
    assert_eq!(cp1.replace_from(cp4), None);
    assert!(matches!(
      cp1.0,
      Kind::ControllerPack {
        first: 3,
        packs: [Some(_), Some(_), Some(_), Some(_)]
      }
    ));

    let mp = expect!(to_controller_pack("A.mpk".into()), "mupen pack name");
    assert!(matches!(mp.0, Kind::Mupen(_)));
    assert!(mp.is_mupen());
    assert_eq!(
      cp1.replace_from(mp),
      Some(ControllerPackPaths(Kind::ControllerPack {
        first: 3,
        packs: [
          Some("A.mpk1".into()),
          Some("A.mpk2".into()),
          Some("A.mpk3".into()),
          Some("A.mpk4".into())
        ]
      }))
    )
  }

  #[test]
  fn test_index_from() {
    assert_eq!(index_from(None), None);
    assert_eq!(index_from(Some(&"".into())), None);
    assert_eq!(index_from(Some(&"abc1".into())), None);
    assert_eq!(index_from(Some(&"mP1".into())), None);
    assert_eq!(index_from(Some(&"mpk".into())), None);
    assert_eq!(index_from(Some(&".mpk1".into())), None);
    assert_eq!(index_from(Some(&"F.mPK1".into())), Some(0));
    assert_eq!(index_from(Some(&"B_b/sa2.mpk".into())), Some(0));
    assert_eq!(index_from(Some(&"FB1.MpK2".into())), Some(1));
    assert_eq!(index_from(Some(&"F.mpk4".into())), Some(3));
  }

  #[test]
  fn check_into_indexed_paths() {
    let mp = ControllerPackPaths(Kind::Mupen("A.mpk".into()));
    let mp_vec = mp.into_indexed_paths();
    assert!(!mp_vec.is_empty());
    assert_eq!(mp_vec.len(), 1);
    assert_eq!(mp_vec[0], (0, "A.mpk".into()));

    let cp = ControllerPackPaths(Kind::ControllerPack {
      first: 1,
      packs: [None, Some("A.mpk2".into()), None, Some("A.mpk4".into())],
    });
    let cp_vec = cp.into_indexed_paths();
    assert!(!cp_vec.is_empty());
    assert_eq!(cp_vec.len(), 2);
    assert_eq!(cp_vec, vec![(1, "A.mpk2".into()), (3, "A.mpk4".into())]);
  }

  #[test]
  fn test_controller_pack_data_functions() {
    let mut cp_data = [0u8; 0x8000];
    init(&mut cp_data);
    assert!(is_empty(&cp_data));
    assert_eq!(&cp_data[512..768], &cp_data[256..512]);
    assert_eq!(&cp_data[60..62], &super::checksum1(&cp_data[32..64]));
    assert_eq!(&cp_data[62..64], &super::checksum2(&cp_data[60..62]));
  }
}
