macro_rules! add_battery {
  ($name:ident,$size:literal,$init:literal,$ext:literal) => {
    pub(crate) struct $name([u8; $size]);
    impl $name {
      pub(crate) fn is_empty(&self) -> bool {
        self.0.iter().rposition(|b| *b != $init) == None
      }
    }
    impl AsRef<[u8]> for $name {
      fn as_ref<'a>(&'a self) -> &'a [u8] {
        &self.0
      }
    }
    impl AsMut<[u8]> for $name {
      fn as_mut<'a>(&'a mut self) -> &'a mut [u8] {
        &mut self.0
      }
    }
    impl Default for $name {
      fn default() -> Self {
        Self([$init; $size])
      }
    }
  };
}

add_battery!(Eeprom, 0x800, 0x00, "eep");
add_battery!(Sram, 0x8000, 0x00, "sra");
add_battery!(FlashRam, 0x20000, 0xff, "fla");
