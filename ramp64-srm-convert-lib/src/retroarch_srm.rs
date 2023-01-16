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
  fn as_ref<'a>(&'a self) -> &'a [u8] {
    const _: () = assert!(std::mem::align_of::<RetroArchSrm>() == 1);
    let ptr = self as *const _ as *const _;
    unsafe { std::slice::from_raw_parts(ptr, std::mem::size_of::<RetroArchSrm>()) }
  }
}
impl AsMut<[u8]> for RetroArchSrm {
  fn as_mut<'a>(&'a mut self) -> &'a mut [u8] {
    const _: () = assert!(std::mem::align_of::<RetroArchSrm>() == 1);
    let ptr = self as *mut _ as *mut _;
    unsafe { std::slice::from_raw_parts_mut(ptr, std::mem::size_of::<RetroArchSrm>()) }
  }
}

impl RetroArchSrm {
  pub fn new_init() -> Self {
    let mut me = Self::default();

    let mut pack_init = ControllerPackInitializer::new();
    for pack in &mut me.controller_pack {
      pack_init.init(pack);
    }

    me
  }
}

#[cfg(test)]
mod tests {
  use super::RetroArchSrm;

  #[test]
  fn srm_data() {
    let srm = RetroArchSrm::default();

    let srm_data = srm.as_ref().to_vec();

    let cp_chain = srm.controller_pack[0]
      .as_ref()
      .iter()
      .chain(srm.controller_pack[1].as_ref().iter())
      .chain(srm.controller_pack[2].as_ref().iter())
      .chain(srm.controller_pack[3].as_ref().iter());

    let fields_data: Vec<_> = srm
      .eeprom
      .as_ref()
      .iter()
      .chain(cp_chain)
      .chain(srm.sram.as_ref())
      .chain(srm.flashram.as_ref())
      .map(|b| *b)
      .collect();

    assert_eq!(srm_data, fields_data)
  }

  #[test]
  fn srm_init() {
    let srm = RetroArchSrm::new_init();

    assert!(srm.eeprom.is_empty());
    assert!(srm.sram.is_empty());
    assert!(srm.flashram.is_empty());
    for cp in &srm.controller_pack {
      assert!(cp.is_empty());
    }
  }
}
