use rand::Rng;

use crate::controller_pack::{ControllerPack, ControllerPackInitializer};
use crate::game_pack::{Eeprom, FlashRam, Sram};

#[repr(C)]
#[derive(Default)]
pub(crate) struct RetroArchSrm {
  pub eeprom: Eeprom,
  pub controller_pack: [ControllerPack; 4],
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
  pub fn new_init<R: Rng>(rng: R) -> Self {
    let mut me = Self::default();

    let mut pack_init = ControllerPackInitializer::from(rng);
    for pack in &mut me.controller_pack {
      pack_init.init(pack);
    }

    me
  }
}
