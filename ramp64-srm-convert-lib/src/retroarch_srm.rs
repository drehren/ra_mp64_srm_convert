use crate::controller_pack::ControllerPack;
use crate::game_pack::{Eeprom, FlashRam, Sram};

#[repr(C)]
pub(crate) struct RetroArchSrm {
  pub eeprom: Eeprom,
  pub controller_pack: [ControllerPack; 4],
  pub sram: Sram,
  pub flashram: FlashRam,
}

impl RetroArchSrm {
  pub(crate) fn new() -> Self {
    Self {
      eeprom: Eeprom::default(),
      controller_pack: [ControllerPack::new(); 4],
      sram: Sram::default(),
      flashram: FlashRam::default(),
    }
  }
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

#[cfg(test)]
mod tests {
  use super::RetroArchSrm;

  #[test]
  fn srm_data() {
    let srm = RetroArchSrm::new();

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
    let srm = RetroArchSrm::new();

    assert!(srm.eeprom.is_empty());
    assert!(srm.sram.is_empty());
    assert!(srm.flashram.is_empty());
    for cp in &srm.controller_pack {
      assert!(cp.is_empty());
    }
  }
}
