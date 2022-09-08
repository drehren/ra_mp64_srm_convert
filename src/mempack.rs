#![allow(dead_code)]

use std::{
  convert::TryInto,
  num::Wrapping,
  ops::{Deref, DerefMut},
  slice,
};

#[repr(C)]
pub struct Mempack {
  id_sector: IdSector,
  index_table: IndexTable,
  index_table_bkp: IndexTable,
  note_table: NoteTable,
  pages: [Page; 123],
}

impl Mempack {
  pub fn new() -> Self {
    Self {
      id_sector: IdSector::new(),
      index_table: IndexTable::new(),
      index_table_bkp: IndexTable::new(),
      note_table: NoteTable::new(),
      pages: [Page::new(); 123],
    }
  }

  pub fn is_empty(&self) -> bool {
    const FREE_SPACE: u16 = u16::from_be_bytes([0, 3]);
    // check the index table, if all unallocated there is no prob
    for v in self.index_table.inodes.windows(2) {
      let val = u16::from_be_bytes(v.try_into().unwrap());
      if val != FREE_SPACE {
        return false;
      }
    }
    true
  }

  pub fn init(&mut self) {
    self.init_with_serial(&IdSector::A_DEFAULT_SERIAL);
  }

  pub fn init_with_serial(&mut self, serial: &[u8; 24]) {
    let id_sector = &mut self.id_sector;
    id_sector.init_with_serial(&serial);

    let idx_table = &mut self.index_table;
    idx_table.init();
    self.index_table_bkp = self.index_table;
  }
}

impl Deref for Mempack {
  type Target = [u8];
  fn deref(&self) -> &Self::Target {
    const _: () = assert!(std::mem::align_of::<Mempack>() == 1);
    let ptr = self as *const _ as *const _;
    unsafe { slice::from_raw_parts(ptr, 0x8000) }
  }
}

impl DerefMut for Mempack {
  fn deref_mut<'a>(&'a mut self) -> &'a mut Self::Target {
    let ptr = self as *mut _ as *mut _;
    unsafe { slice::from_raw_parts_mut::<'a>(ptr, 0x8000) }
  }
}

#[derive(Clone, Copy)]
pub struct Page([u8; 256]);
impl Page {
  fn new() -> Self {
    Self([0; 256])
  }
}

#[derive(Clone, Copy)]
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
  fn new() -> Self {
    Self {
      serial: [0; 24],
      unused1: 0,
      dev_id: 0,
      bank_size: 0,
      unused2: 0,
      checksum1: [0; 2],
      checksum2: [0; 2],
    }
  }

  fn init_with_serial(&mut self, serial: &[u8; 24]) {
    self.serial.copy_from_slice(serial);
    self.unused1 = 0xff;
    self.dev_id = 0xff;
    self.bank_size = 1;
    self.unused2 = 0xff;

    let mut sum = Wrapping(0u16);
    for data in self.serial.windows(2) {
      sum += Wrapping(u16::from_be_bytes(data.try_into().unwrap()));
    }
    sum += Wrapping(u16::from_be_bytes([self.unused1, self.dev_id]));
    sum += Wrapping(u16::from_be_bytes([self.bank_size, self.unused2]));

    self.checksum1.copy_from_slice(&sum.0.to_be_bytes());
    sum = Wrapping(u16::from_be(0xF2FF)) - sum;
    self.checksum2.copy_from_slice(&sum.0.to_be_bytes());
  }
}

#[derive(Clone, Copy)]
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
  const A_DEFAULT_SERIAL: [u8; 24] = [
    0xff, 0xff, 0xff, 0xff, 0x05, 0x1a, 0x5f, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  ];

  fn new() -> Self {
    Self {
      label: [0; 32],
      id_block: IdBlock::new(),
      unused1: [0; 32],
      id_block_bk1: IdBlock::new(),
      id_block_bk2: IdBlock::new(),
      unused2: [0; 32],
      id_block_bk3: IdBlock::new(),
      unused3: [0; 32],
    }
  }

  fn init_with_serial(&mut self, serial: &[u8; 24]) {
    self.label.copy_from_slice(&[
      0x81, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
      0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
      0x1e, 0x1f,
    ]);
    self.id_block.init_with_serial(serial);
    self.id_block_bk1 = self.id_block;
    self.id_block_bk2 = self.id_block;
    self.id_block_bk3 = self.id_block;
  }
}

#[derive(Clone, Copy)]
struct IndexTable {
  unused1: u8,
  checksum: u8,
  unused2: [u8; 8],
  inodes: [u8; 246],
}

impl IndexTable {
  fn new() -> Self {
    Self {
      unused1: 0,
      checksum: 0,
      unused2: [0; 8],
      inodes: [0; 246],
    }
  }

  fn init(&mut self) {
    self.unused1 = 0;
    self.checksum = 0;
    self.unused2.fill(0);
    self.inodes.fill(3);
    self.inodes.iter_mut().step_by(2).for_each(|v| *v = 0);
    self.update_checksum();
  }

  fn update_checksum(&mut self) {
    let mut sum = Wrapping(0u8);
    for val in &self.inodes {
      sum += Wrapping(*val);
    }
    self.checksum = sum.0;
  }
}

struct NoteTable([u8; 16 * 32]); // notes * note_size
impl NoteTable {
  fn new() -> Self {
    Self([0u8; 16 * 32])
  }
}
