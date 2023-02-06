use std::array::from_fn;
use std::convert::TryInto;
use std::fmt::Debug;
use std::io::Read;
use std::mem::size_of;
use std::slice;

#[repr(C)]
#[derive(Clone, Copy)]
pub(crate) struct ControllerPack {
  id_sector: IdSector,
  index_table: IndexTable,
  index_table_bkp: IndexTable,
  note_table: NoteTable,
  pages: [Page; 123],
}

impl Default for ControllerPack {
  fn default() -> Self {
    Self {
      id_sector: IdSector::default(),
      index_table: IndexTable::default(),
      index_table_bkp: IndexTable::default(),
      note_table: NoteTable::default(),
      pages: [Page::default(); 123],
    }
  }
}

impl ControllerPack {
  pub(crate) fn is_empty(&self) -> bool {
    const FREE_SPACE: u16 = u16::from_be_bytes([0, 3]);
    // check the index table, if all unallocated there is no prob
    for v in self.index_table.inodes.chunks(2) {
      let val = u16::from_be_bytes(v.try_into().unwrap());
      if val != FREE_SPACE {
        return false;
      }
    }
    true
  }

  pub(crate) fn infer_from<R: Read>(data: &mut R) -> std::io::Result<bool> {
    let mut id_sector = IdSector::default();
    data.read_exact(id_sector.as_mut())?;

    // now test the checksums
    Ok(id_sector.check())
  }
}

impl AsRef<[u8]> for ControllerPack {
  fn as_ref(&self) -> &[u8] {
    const _: () = assert!(std::mem::align_of::<ControllerPack>() == 1);
    let ptr = self as *const _ as *const _;
    unsafe { slice::from_raw_parts(ptr, 0x8000) }
  }
}

impl AsMut<[u8]> for ControllerPack {
  fn as_mut<'a>(&'a mut self) -> &'a mut [u8] {
    let ptr = self as *mut _ as *mut _;
    unsafe { slice::from_raw_parts_mut::<'a>(ptr, 0x8000) }
  }
}

#[repr(C)]
#[derive(Clone, Copy, Default)]
struct IdSector {
  label: [u8; 32],
  id_block: IdBlock,
  unused1: [u8; 32],
  id_block_bk1: IdBlock,
  id_block_bk2: IdBlock,
  unused2: [u8; 32],
  id_block_bk3: IdBlock,
  unused3: [u8; 32],
}

impl IdSector {
  fn check(&self) -> bool {
    self.id_block.check()
      || self.id_block_bk1.check()
      || self.id_block_bk2.check()
      || self.id_block_bk3.check()
  }
}

impl AsMut<[u8]> for IdSector {
  fn as_mut<'a>(&'a mut self) -> &'a mut [u8] {
    const _: () = assert!(std::mem::align_of::<IdSector>() == 1);
    let ptr = self as *mut _ as *mut _;
    unsafe { slice::from_raw_parts_mut::<'a>(ptr, size_of::<IdSector>()) }
  }
}

#[repr(C)]
#[derive(PartialEq, Clone, Copy, Default)]
struct IdBlock {
  serial: [u8; 24],
  unused1: u8,
  dev_id: u8,
  bank_size: u8,
  unused2: u8,
  checksum1: [u8; 2],
  checksum2: [u8; 2],
}

impl IdBlock {
  fn check(&self) -> bool {
    // The first checksum is a 16-bit big endian word computed by
    // summing the first fourteen 16-bit words in the structure
    // check against the checksum
    let sum1 = self.calculate_checksum1();
    let sum2 = self.calculate_checksum2();
    sum1 == self.checksum1 && sum2 == self.checksum2
  }

  fn calculate_checksum1(&self) -> [u8; 2] {
    let mut sum = 0u16;
    for data in self.serial.chunks(2) {
      sum = sum.wrapping_add(u16::from_be_bytes(data.try_into().unwrap()));
    }
    sum = sum.wrapping_add(u16::from_be_bytes([self.unused1, self.dev_id]));
    sum = sum.wrapping_add(u16::from_be_bytes([self.bank_size, self.unused2]));
    sum.to_be_bytes()
  }

  fn calculate_checksum2(&self) -> [u8; 2] {
    u16::from_be_bytes([0xff, 0xf2])
      .wrapping_sub(u16::from_be_bytes(self.checksum1))
      .to_be_bytes()
  }
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct IndexTable {
  unused1: u8,
  checksum: u8,
  unused2: [u8; 8],
  inodes: [u8; 246],
}
impl Default for IndexTable {
  fn default() -> Self {
    Self {
      unused1: 0,
      checksum: 0,
      unused2: [0; 8],
      inodes: [0; 246],
    }
  }
}

#[repr(C)]
#[derive(Clone, Copy)]
struct NoteTable([u8; 16 * 32]); // notes * note_size
impl Default for NoteTable {
  fn default() -> Self {
    Self([0; 16 * 32])
  }
}

#[repr(C)]
#[derive(Clone, Copy)]
struct Page([u8; 256]);
impl Default for Page {
  fn default() -> Self {
    Self([0; 256])
  }
}

pub(crate) struct ControllerPackInitializer;
impl ControllerPackInitializer {
  pub(crate) fn new() -> Self {
    Self {}
  }
}

const MUPEN64_SERIAL: [u8; 24] = [
  0xff, 0xff, 0xff, 0xff, 0x05, 0x1a, 0x5f, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
];

impl ControllerPackInitializer {
  pub(crate) fn init(&mut self, controller_pack: &mut ControllerPack) {
    self.init_id_sector(&mut controller_pack.id_sector);
    self.init_index_table(&mut controller_pack.index_table);
    controller_pack.index_table_bkp = controller_pack.index_table;
  }

  fn init_id_sector(&mut self, id_sector: &mut IdSector) {
    id_sector.label = from_fn(|i| i as u8);
    id_sector.label[0] = 0x81;
    self.init_id_block(&mut id_sector.id_block);
    id_sector.id_block_bk1 = id_sector.id_block;
    id_sector.id_block_bk2 = id_sector.id_block;
    id_sector.id_block_bk3 = id_sector.id_block;
  }

  fn id_block_serial(&mut self, serial: &mut [u8; 24]) {
    *serial = MUPEN64_SERIAL;
  }

  fn init_id_block(&mut self, id_block: &mut IdBlock) {
    self.id_block_serial(&mut id_block.serial);

    id_block.unused1 = 0xff;
    id_block.dev_id = 0xff;
    id_block.bank_size = 1;
    id_block.unused2 = 0xff;

    id_block.checksum1 = id_block.calculate_checksum1();
    id_block.checksum2 = id_block.calculate_checksum2();
  }

  fn init_index_table(&mut self, table: &mut IndexTable) {
    table.unused1 = 0;
    table.checksum = 113; // 3 * 246 / 2 u8 wrapped
    table.unused2.fill(0);
    table.inodes = from_fn(|i| ((i % 2) * 3) as u8);
  }
}

#[cfg(test)]
mod tests {
  use super::{ControllerPack, ControllerPackInitializer};

  #[test]
  fn init_pack() {
    let mut cp = ControllerPack::default();

    assert!(!cp.is_empty());

    let mut pack_init = ControllerPackInitializer::new();

    pack_init.init(&mut cp);

    assert!(cp.is_empty());
    assert_eq!(cp.index_table_bkp, cp.index_table);
    assert_eq!(
      cp.id_sector.id_block.checksum1,
      cp.id_sector.id_block.calculate_checksum1()
    );
    assert_eq!(
      cp.id_sector.id_block.checksum2,
      cp.id_sector.id_block.calculate_checksum2()
    );
  }
}
