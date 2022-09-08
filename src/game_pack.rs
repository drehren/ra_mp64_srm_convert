use std::ops::{Deref, DerefMut};

macro_rules! add_battery {
  ($name:ident,$size:literal,$init:literal) => {
    pub struct $name([u8; $size]);
    impl $name {
      pub fn new() -> Self {
        Self([$init; $size])
      }
      pub fn is_empty(&self) -> bool {
        self.0.iter().rposition(|b| *b != $init) == None
      }
    }
    impl Deref for $name {
      type Target = [u8];
      fn deref<'a>(&'a self) -> &'a [u8] {
        &self.0
      }
    }
    impl DerefMut for $name {
      fn deref_mut<'a>(&'a mut self) -> &'a mut [u8] {
        &mut self.0
      }
    }
  };
}

add_battery!(Eeprom, 0x800, 0xff);
add_battery!(Sram, 0x8000, 0xff);
add_battery!(FlashRam, 0x20000, 0xff);
