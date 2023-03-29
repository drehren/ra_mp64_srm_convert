pub mod create;
pub mod io;
pub mod path;
pub mod split;

pub(crate) mod utils;

// re-export
pub use path::battery::{to_battery, BatteryPath};
pub use path::controller_pack::{to_controller_pack, ControllerPackPaths};
pub use split::can_be_srm;

/// Provides a unified way to process conversion parameters
pub trait Converter {
  /// The conversion error type
  type Error;

  /// The validation type
  type Validation: std::fmt::Display;

  #[must_use]
  /// Validates the parameters
  fn validate(&self) -> Self::Validation;

  /// Proceeds to apply the conversion
  fn convert(self, user_params: &UserParams) -> Result<(), Self::Error>;
}

#[derive(Debug, Default, Clone, Copy)]
/// Holds the user parameters for the conversion
pub struct UserParams {
  /// If ```true```, files will be overwritten
  pub overwrite: bool,
  /// If ```true```, EEP & FLASHRAM data will be word-swapped
  pub swap_bytes: bool,
}

#[derive(Debug)]
/// Represents the collection of an error and the path that produced it
pub struct Error(std::path::PathBuf, std::io::Error);

impl Error {
  /// Returns the corresponding [std::io::ErrorKind] for this error.
  pub fn kind(&self) -> std::io::ErrorKind {
    self.1.kind()
  }

  /// Returns the [std::path::Path] that was involved in this error.
  pub fn path(&self) -> &std::path::Path {
    &self.0
  }
}

impl std::fmt::Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!("\"{}\": {}", self.0.display(), self.1))
  }
}

impl std::error::Error for Error {}

struct SrmBuf {
  data: Box<[u8; 0x48800]>,
}

impl SrmBuf {
  fn new() -> Self {
    let mut data = Box::new([0xff; 0x48800]);
    path::controller_pack::init(&mut data[0x800..0x8800]);
    data.copy_within(0x800..0x8800, 0x8800);
    data.copy_within(0x800..0x8800, 0x10800);
    data.copy_within(0x800..0x8800, 0x18800);
    Self { data }
  }

  fn has_save_data(&self) -> bool {
    !self.eeprom().is_empty()
      || !self.sram().is_empty()
      || !self.flashram().is_empty()
      || self.controller_pack_iter().any(|cp| !cp.is_empty())
  }

  fn eeprom(&self) -> Eeprom<'_> {
    Eeprom(&self.data[..0x800])
  }
  fn eeprom_mut(&mut self) -> &mut [u8] {
    &mut self.data[..0x800]
  }

  fn controller_pack_iter(&self) -> impl Iterator<Item = ControllerPack> {
    self.data[0x800..0x20800].chunks(0x8000).map(ControllerPack)
  }

  fn controller_pack_iter_mut(&mut self) -> std::slice::ChunksMut<u8> {
    self.data[0x800..0x20800].chunks_mut(0x8000)
  }

  fn full_controller_pack(&self) -> ControllerPack {
    ControllerPack(&self.data[0x800..0x20800])
  }
  fn full_controller_pack_mut(&mut self) -> &mut [u8] {
    &mut self.data[0x800..0x20800]
  }
  fn sram(&self) -> Sram<'_> {
    Sram(&self.data[0x20800..0x28800])
  }
  fn sram_mut(&mut self) -> &mut [u8] {
    &mut self.data[0x20800..0x28800]
  }

  fn flashram(&self) -> FlashRam<'_> {
    FlashRam(&self.data[0x28800..0x48800])
  }
  fn flashram_mut(&mut self) -> &mut [u8] {
    &mut self.data[0x28800..0x48800]
  }
}

impl std::ops::Deref for SrmBuf {
  type Target = [u8];

  fn deref(&self) -> &Self::Target {
    self.data.deref()
  }
}

impl std::ops::DerefMut for SrmBuf {
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.data.deref_mut()
  }
}

macro_rules! srm_internal_data {
  ($name:ident, $is_empty:expr) => {
    struct $name<'srm>(&'srm [u8]);
    impl<'srm> IsEmpty for $name<'srm> {
      fn is_empty(&self) -> bool {
        $is_empty(self)
      }
    }
    impl<'srm> AsRef<[u8]> for $name<'srm> {
      fn as_ref(&self) -> &[u8] {
        self.0
      }
    }
  };

  ($name:ident) => {
    srm_internal_data!($name, |x: &$name| x
      .0
      .iter()
      .rposition(|b| *b != 0xff)
      .is_none());
  };
}

srm_internal_data!(Eeprom);
srm_internal_data!(FlashRam);
srm_internal_data!(Sram);
srm_internal_data!(ControllerPack, |me: &ControllerPack| {
  path::controller_pack::is_empty(me.0)
});

impl<'srm> Eeprom<'srm> {
  pub(crate) fn is_4k(&self) -> bool {
    self.0[0x200..].iter().all(|b| b == &0xff)
  }

  pub(crate) fn as_4k(&self) -> Self {
    Eeprom(&self.0[..0x200])
  }
}

trait IsEmpty {
  fn is_empty(&self) -> bool;
}
