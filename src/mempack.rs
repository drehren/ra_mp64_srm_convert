use std::{
  convert::{TryFrom, TryInto},
  fs::File,
  io::Write,
  num::Wrapping,
};

pub const PAGE_SIZE: usize = 256;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Mempack {
  id_sector: IdSector,
  index_table: IndexTable,
  index_table_bkp: IndexTable,
  note_table: NoteTable,
  pages: [Page; 123],
}

impl From<[u8; 0x8000]> for Mempack {
  fn from(data: [u8; 0x8000]) -> Self {
    // Just use this data as backend, since it is what we require
    let me = unsafe { std::ptr::read_volatile(&data as *const _ as *const Mempack) };
    std::mem::forget(data);
    me
  }
}

impl Mempack {
  pub fn is_empty(&self) -> bool {
    const FREE_SPACE: u16 = u16::from_be_bytes([0, 3]);
    // check by checking index table, if all unallocated there is no prob
    for v in self.index_table.inodes.windows(2) {
      let val = u16::from_be_bytes(v.try_into().unwrap());
      if val != FREE_SPACE {
        return false;
      }
    }
    true
  }

  pub fn save(&self, file: &mut File) -> std::io::Result<()> {
    self.id_sector.save(file)?;
    self.index_table.save(file)?;
    self.index_table_bkp.save(file)?;
    self.note_table.save(file)?;
    for p in &self.pages {
      file.write_all(&p.data)?;
    }
    Ok(())
  }

  pub fn new() -> Self {
    Self {
      id_sector: IdSector::new(),
      index_table: IndexTable::new(),
      index_table_bkp: IndexTable::new(),
      note_table: NoteTable::new(),
      pages: [Page::new(); 123],
    }
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

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Page {
  data: [u8; PAGE_SIZE],
}

impl Page {
  fn new() -> Self {
    Self {
      data: [0; PAGE_SIZE],
    }
  }
}

impl From<[u8; PAGE_SIZE]> for Page {
  fn from(data: [u8; PAGE_SIZE]) -> Self {
    Self { data }
  }
}

#[repr(C)]
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

  fn save(&self, file: &mut File) -> std::io::Result<()> {
    file.write_all(&self.serial)?;
    file.write_all(&[self.unused1, self.dev_id, self.bank_size, self.unused2])?;
    file.write_all(&self.checksum1)?;
    file.write_all(&self.checksum2)
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

impl<'a> TryFrom<&'a [u8]> for IdBlock {
  type Error = <[u8; 32] as TryFrom<&'a [u8]>>::Error;

  fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
    let data: &[u8; 32] = value.try_into()?;
    Ok(Self {
      serial: data[..24].try_into()?,
      unused1: data[24],
      dev_id: data[25],
      bank_size: data[26],
      unused2: data[27],
      checksum1: data[28..30].try_into()?,
      checksum2: data[30..32].try_into()?,
    })
  }
}

#[repr(C)]
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

  fn save(&self, file: &mut File) -> std::io::Result<()> {
    file.write_all(&self.label)?;
    self.id_block.save(file)?;
    file.write_all(&self.unused1)?;
    self.id_block_bk1.save(file)?;
    self.id_block_bk2.save(file)?;
    file.write_all(&self.unused2)?;
    self.id_block_bk3.save(file)?;
    file.write_all(&self.unused3)
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

impl<'a> TryFrom<&'a [u8]> for IdSector {
  type Error = <[u8; 32] as TryFrom<&'a [u8]>>::Error;

  fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
    Ok(Self {
      label: value[0..32].try_into()?,
      id_block: value[32..64].try_into()?,
      unused1: value[64..96].try_into()?,
      id_block_bk1: value[96..128].try_into()?,
      id_block_bk2: value[128..160].try_into()?,
      unused2: value[160..192].try_into()?,
      id_block_bk3: value[192..224].try_into()?,
      unused3: value[224..256].try_into()?,
    })
  }
}

#[repr(C)]
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

  fn save(&self, file: &mut File) -> std::io::Result<()> {
    file.write_all(&[self.unused1, self.checksum])?;
    file.write_all(&self.unused2)?;
    file.write_all(&self.inodes)
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

impl<'a> TryFrom<&'a [u8]> for IndexTable {
  type Error = <[u8; 32] as TryFrom<&'a [u8]>>::Error;

  fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
    let first_vals: &[u8; 2] = value[..1].try_into()?;
    Ok(Self {
      unused1: first_vals[0],
      checksum: first_vals[1],
      unused2: value[2..10].try_into()?,
      inodes: value[10..256].try_into()?,
    })
  }
}

#[repr(C)]
#[derive(Clone, Copy)]
struct NoteEntry {
  game_code: [u8; 4],
  publ_code: [u8; 2],
  start_pag: [u8; 2],
  status: u8,
  reserved: u8,
  data_sum: [u8; 2],
  file_ext: [u8; 4],
  file_name: [u8; 16],
}

impl NoteEntry {
  fn new() -> Self {
    Self {
      game_code: [0; 4],
      publ_code: [0; 2],
      start_pag: [0; 2],
      status: 0,
      reserved: 0,
      data_sum: [0; 2],
      file_ext: [0; 4],
      file_name: [0; 16],
    }
  }

  fn save(&self, file: &mut File) -> std::io::Result<()> {
    file.write_all(&self.game_code)?;
    file.write_all(&self.publ_code)?;
    file.write_all(&self.start_pag)?;
    file.write_all(&[self.status, self.reserved])?;
    file.write_all(&self.data_sum)?;
    file.write_all(&self.file_ext)?;
    file.write_all(&self.file_name)
  }
}

impl<'a> TryFrom<&'a [u8]> for NoteEntry {
  type Error = <[u8; 32] as TryFrom<&'a [u8]>>::Error;

  fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
    let data: &[u8; 32] = value.try_into()?;
    Ok(Self {
      game_code: data[0..4].try_into().unwrap(),
      publ_code: data[4..6].try_into().unwrap(),
      start_pag: data[6..8].try_into().unwrap(),
      status: data[8],
      reserved: data[9],
      data_sum: data[10..12].try_into().unwrap(),
      file_ext: data[12..16].try_into().unwrap(),
      file_name: data[16..32].try_into().unwrap(),
    })
  }
}

#[repr(C)]
#[derive(Clone, Copy)]
struct NoteTable {
  entries: [NoteEntry; 16],
}

impl NoteTable {
  fn new() -> Self {
    Self {
      entries: [NoteEntry::new(); 16],
    }
  }

  fn save(&self, file: &mut File) -> std::io::Result<()> {
    for entry in &self.entries {
      entry.save(file)?;
    }
    Ok(())
  }
}

impl<'a> TryFrom<&'a [u8]> for NoteTable {
  type Error = <[u8; 32] as TryFrom<&'a [u8]>>::Error;

  fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
    let value: &[u8; PAGE_SIZE * 2] = value.try_into()?;
    let mut windowed = value.windows(32);
    Ok(Self {
      entries: [
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
        windowed.next().unwrap().try_into()?,
      ],
    })
  }
}
