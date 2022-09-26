use crate::controller_pack::ControllerPack;
use crate::game_pack::{Eeprom, FlashRam, Sram};

#[repr(C)]
pub struct RetroArchSrm {
  pub eeprom: Eeprom,
  pub mempack: [ControllerPack; 4],
  pub sram: Sram,
  pub flashram: FlashRam,
}

impl AsRef<[u8]> for RetroArchSrm {
  fn as_ref(&self) -> &[u8] {
    const _: () = assert!(std::mem::align_of::<RetroArchSrm>() == 1);
    let ptr = self as *const _ as *const _;
    unsafe { std::slice::from_raw_parts(ptr, std::mem::size_of::<RetroArchSrm>()) }
  }
}
impl AsMut<[u8]> for RetroArchSrm {
  fn as_mut(&mut self) -> &mut [u8] {
    const _: () = assert!(std::mem::align_of::<RetroArchSrm>() == 1);
    let ptr = self as *mut _ as *mut _;
    unsafe { std::slice::from_raw_parts_mut(ptr, std::mem::size_of::<RetroArchSrm>()) }
  }
}

impl RetroArchSrm {
  pub fn new() -> Self {
    Self {
      eeprom: Eeprom::new(),
      mempack: [ControllerPack::new(); 4],
      sram: Sram::new(),
      flashram: FlashRam::new(),
    }
  }

  /// Initialize the mempacks
  pub fn init(&mut self) {
    for mp in &mut self.mempack {
      mp.init()
    }
  }

  pub fn all_controller_packs<'a>(&'a self) -> &'a [u8] {
    let ptr = self.mempack.as_ptr() as *const _;
    unsafe { std::slice::from_raw_parts(ptr, 0x8000 * 4) }
  }

  pub fn all_controller_packs_mut<'a>(&'a mut self) -> &'a mut [u8] {
    let ptr = self.mempack.as_mut_ptr() as *mut _;
    unsafe { std::slice::from_raw_parts_mut(ptr, 0x8000 * 4) }
  }
}
