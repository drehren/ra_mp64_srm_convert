use std::{fmt, path::Path};

use crate::{ControllerPackKind, Problem, SaveFile, SaveType, SrmFile};

macro_rules! display_some_path {
  ($c:expr, $f:expr, $name:expr, $path:expr) => {
    if let Some(path) = $path {
      if $c > 0 {
        $f.write_str(" and ")?;
      }
      $f.write_fmt(format_args!("{} ({})", $name, path.display()))?;
      $c += 1;
    }
  };
}

/// Provides the parameter to apply a conversion
#[derive(Debug)]
pub struct ConvertParams {
  pub(crate) mode: ConvertMode,
  pub(crate) file: SrmFile,
  pub(crate) paths: SrmPaths,
}

impl ConvertParams {
  /// Creates a new [`ConvertParams`]
  pub(crate) fn new(mode: ConvertMode, file: SrmFile, paths: SrmPaths) -> Self {
    Self { mode, file, paths }
  }

  /// Returns the path for the EEPROM save
  pub fn eeprom(&self) -> &Option<SaveFile> {
    &self.paths.eep
  }
  /// Returns the path for the SRAM save
  pub fn sram(&self) -> &Option<SaveFile> {
    &self.paths.sra
  }
  /// Returns the path for the FlashRAM save
  pub fn flash_ram(&self) -> &Option<SaveFile> {
    &self.paths.fla
  }
  /// Return the path of the specified Controller Pack
  pub fn controller_pack(&self, kind: ControllerPackKind) -> &Option<SaveFile> {
    let index: usize = kind.into();
    &self.paths.cp[index]
  }

  /// Returns the path to the SRM file
  pub fn srm_file(&self) -> &Path {
    &self.file
  }

  /// Gets the mode for these conversion parameters
  pub fn mode(&self) -> &ConvertMode {
    &self.mode
  }

  /// Replaces the current [`ConvertMode`] with the given one
  pub fn replace_mode(&mut self, mode: ConvertMode) -> ConvertMode {
    std::mem::replace(&mut self.mode, mode)
  }

  /// Replaces the current [`SrmFile`] with the given one
  pub fn replace_srm_file(&mut self, srm_file: SrmFile) -> SrmFile {
    std::mem::replace(&mut self.file, srm_file)
  }

  /// Sets or replaces an existing [`SaveFile`]
  pub fn set_or_replace_file(&mut self, save_file: SaveFile) -> Option<SaveFile> {
    self.paths.set(save_file)
  }

  /// Unset the path for the specified save type
  pub fn unset_file(&mut self, save_type: SaveType) -> Option<SaveFile> {
    self.paths.unset(save_type)
  }

  pub(crate) fn validate(&self) -> Option<Problem<'_>> {
    match self.mode {
      ConvertMode::Create => {
        if self.paths.is_empty() {
          Some(Problem::NoInput)
        } else if !self.paths.any_is_file() {
          Some(Problem::FileDoesNotExist(self.paths.get_invalid_paths()))
        } else {
          None
        }
      }
      ConvertMode::Split => {
        if !self.file.exists() {
          Some(Problem::FileDoesNotExist(vec![self.file.as_ref()]))
        } else if !self.file.is_file() {
          Some(Problem::NotAFile)
        } else {
          None
        }
      }
    }
  }
}

impl fmt::Display for ConvertParams {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mode = match self.mode {
      ConvertMode::Create if self.file.exists() => "Update",
      ConvertMode::Create => "Create",
      ConvertMode::Split => "Split",
    };

    f.write_fmt(format_args!(
      "{mode} {} {}",
      if self.mode == ConvertMode::Create {
        "from"
      } else {
        "to"
      },
      self.paths
    ))
  }
}

/// The mode to use to convert the SRM file
#[derive(Default, Debug, PartialEq)]
pub enum ConvertMode {
  /// Specify this variant to create an SRM file
  #[default]
  Create,
  /// Specify this variant to split an SRM file
  Split,
}

#[derive(Debug, PartialEq)]
pub(crate) struct SrmPaths {
  pub(crate) eep: Option<SaveFile>,
  pub(crate) sra: Option<SaveFile>,
  pub(crate) fla: Option<SaveFile>,
  pub(crate) cp: [Option<SaveFile>; 4],
}

impl Default for SrmPaths {
  fn default() -> Self {
    Self {
      eep: Default::default(),
      sra: Default::default(),
      fla: Default::default(),
      cp: Default::default(),
    }
  }
}

impl SrmPaths {
  pub(crate) fn is_empty(&self) -> bool {
    let SrmPaths { eep, fla, sra, cp } = self;
    [eep, fla, sra].iter().all(|&p| p.is_none()) && cp.iter().all(Option::is_none)
  }

  pub(crate) fn any_is_file(&self) -> bool {
    let SrmPaths { eep, sra, fla, cp } = self;
    cp.iter()
      .map(Option::as_ref)
      .chain([eep.as_ref(), fla.as_ref(), sra.as_ref()])
      .any(|o| o.map_or(false, |p| p.is_file()))
  }

  pub(crate) fn set(&mut self, save_file: SaveFile) -> Option<SaveFile> {
    use ControllerPackKind::*;
    match save_file.save_type {
      crate::SaveType::Eeprom => &mut self.eep,
      crate::SaveType::Sram => &mut self.sra,
      crate::SaveType::FlashRam => &mut self.fla,
      crate::SaveType::ControllerPack(kind @ Mupen | kind @ Player1) => {
        if kind == Mupen {
          self.cp = [std::mem::take(&mut self.cp[0]), None, None, None];
        }
        &mut self.cp[0]
      }
      crate::SaveType::ControllerPack(Player2) => &mut self.cp[1],
      crate::SaveType::ControllerPack(Player3) => &mut self.cp[2],
      crate::SaveType::ControllerPack(Player4) => &mut self.cp[3],
    }
    .replace(save_file)
  }

  pub(crate) fn unset(&mut self, save_type: SaveType) -> Option<SaveFile> {
    use ControllerPackKind::*;
    match save_type {
      crate::SaveType::Eeprom => &mut self.eep,
      crate::SaveType::Sram => &mut self.sra,
      crate::SaveType::FlashRam => &mut self.fla,
      crate::SaveType::ControllerPack(Mupen | Player1) => &mut self.cp[0],
      crate::SaveType::ControllerPack(Player2) => &mut self.cp[1],
      crate::SaveType::ControllerPack(Player3) => &mut self.cp[2],
      crate::SaveType::ControllerPack(Player4) => &mut self.cp[3],
    }
    .take()
  }

  pub(crate) fn get_invalid_paths(&self) -> Vec<&Path> {
    let SrmPaths { eep, fla, sra, cp } = self;
    [eep, fla, sra, &cp[0], &cp[1], &cp[2], &cp[3]]
      .iter()
      .filter_map(|&f| f.as_ref().map(|p| p.file.as_path()))
      .filter(|p| !p.exists())
      .collect::<Vec<_>>()
  }
}

impl fmt::Display for SrmPaths {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut counter = 0;
    display_some_path!(counter, f, "EEPROM", &self.eep);
    display_some_path!(counter, f, "SRAM", &self.sra);
    display_some_path!(counter, f, "FlashRAM", &self.fla);
    if !f.alternate() {
      for i in 1..=4 {
        display_some_path!(counter, f, format!("Controller Pack {i}"), &self.cp[i - 1]);
      }
    } else {
      if let Some(path) = &self.cp[0] {
        if counter > 0 {
          f.write_str(" and ")?;
        }
        f.write_fmt(format_args!("Controller Pack ({})", path.display()))?;
      }
    }
    Ok(())
  }
}

#[cfg(test)]
pub(super) mod tests {
  use crate::SaveType;

  use super::*;

  pub(crate) type SaveFlag = u8;
  pub(crate) trait SaveFlagExt {
    const EEP: SaveFlag = 0x1;
    const FLA: SaveFlag = 0x2;
    const SRA: SaveFlag = 0x4;
    const CP1: SaveFlag = 0x08;
    const CP2: SaveFlag = 0x10;
    const CP3: SaveFlag = 0x20;
    const CP4: SaveFlag = 0x40;
    const CP: SaveFlag = Self::CP1 | Self::CP2 | Self::CP3 | Self::CP4;
    const ALL: SaveFlag = Self::EEP | Self::FLA | Self::SRA | Self::CP;
  }
  impl SaveFlagExt for SaveFlag {}

  pub(crate) fn test_for_none(paths: &SrmPaths, save_flags: SaveFlag) {
    if save_flags & SaveFlag::CP1 == SaveFlag::CP1 {
      assert_eq!(paths.cp[0], None);
    }
    if save_flags & SaveFlag::CP2 == SaveFlag::CP2 {
      assert_eq!(paths.cp[1], None);
    }
    if save_flags & SaveFlag::CP3 == SaveFlag::CP3 {
      assert_eq!(paths.cp[2], None);
    }
    if save_flags & SaveFlag::CP4 == SaveFlag::CP4 {
      assert_eq!(paths.cp[3], None);
    }
    if save_flags & SaveFlag::EEP == SaveFlag::EEP {
      assert_eq!(paths.eep, None);
    }
    if save_flags & SaveFlag::SRA == SaveFlag::SRA {
      assert_eq!(paths.sra, None);
    }
    if save_flags & SaveFlag::FLA == SaveFlag::FLA {
      assert_eq!(paths.fla, None);
    }
  }

  #[test]
  fn verify_srm_paths() {
    let paths = SrmPaths::default();

    assert!(paths.is_empty());
    assert!(!paths.any_is_file());
    assert_eq!(paths.get_invalid_paths(), Vec::<&Path>::new());
  }

  #[test]
  fn verify_srm_paths_set_mpk() {
    let mut paths = SrmPaths::default();

    let mpk: SaveFile = "A.mpk".try_into().expect("Save name is valid");
    assert_eq!(paths.set(mpk.clone()), None);

    assert!(!paths.is_empty());
    test_for_none(&paths, SaveFlag::ALL & !SaveFlag::CP1);

    // test replace mkp1
    let mpk1: SaveFile = "B.mpk1".try_into().expect("Save name is valid");
    assert_eq!(paths.set(mpk1.clone()), Some(mpk));

    assert!(!paths.is_empty());
    test_for_none(&paths, SaveFlag::ALL & !SaveFlag::CP1);

    // test add mpk3
    let mpk3: SaveFile = "X.mpk3".try_into().expect("Save name is valid");
    assert_eq!(paths.set(mpk3.clone()), None);

    assert!(!paths.is_empty());
    test_for_none(&paths, SaveFlag::ALL & !SaveFlag::CP1 & !SaveFlag::CP3);

    // test replace with mupen mpk
    let mut mpk: SaveFile = "M.mpk4".try_into().expect("Save name is valid");
    mpk.save_type = SaveType::ControllerPack(ControllerPackKind::Mupen);
    assert_eq!(paths.set(mpk), Some(mpk1));

    assert!(!paths.is_empty());
    test_for_none(&paths, SaveFlag::ALL & !SaveFlag::CP1);
  }

  #[test]
  fn verify_convert_params_new() {
    let params = ConvertParams::new(
      ConvertMode::Create,
      SrmFile::from_name("file.srm"),
      SrmPaths::default(),
    );

    assert_eq!(params.mode, ConvertMode::Create);
    assert_eq!(params.file, SrmFile::from_name("file"));
    assert_eq!(params.paths, SrmPaths::default());
  }

  #[test]
  fn verify_convert_params_set_eep() {
    let mut params = ConvertParams::new(
      ConvertMode::Create,
      SrmFile::from_name("file.srm"),
      SrmPaths::default(),
    );

    assert!(params.paths.is_empty());

    let eep: SaveFile = "save.eep".try_into().expect("Save name is ok");
    assert_eq!(params.set_or_replace_file(eep.clone()), None);
    assert_eq!(params.eeprom(), &Some(eep.clone()));

    assert!(!params.paths.is_empty());

    let eep_new: SaveFile = "sav2.eep".try_into().expect("Save name is ok");
    assert_eq!(params.set_or_replace_file(eep_new.clone()), Some(eep));
    assert_eq!(params.eeprom(), &Some(eep_new));

    assert!(!params.paths.is_empty());
  }

  #[test]
  fn verify_convert_params_set_sram() {
    let mut params = ConvertParams::new(
      ConvertMode::Create,
      SrmFile::from_name("file.srm"),
      SrmPaths::default(),
    );

    assert!(params.paths.is_empty());

    let sra: SaveFile = "save.sra".try_into().expect("Save name is ok");
    assert_eq!(params.set_or_replace_file(sra.clone()), None);
    assert_eq!(params.sram(), &Some(sra.clone()));

    assert!(!params.paths.is_empty());

    let sra_new: SaveFile = "sav2.sra".try_into().expect("Save name is ok");
    assert_eq!(params.set_or_replace_file(sra_new.clone()), Some(sra));
    assert_eq!(params.sram(), &Some(sra_new));

    assert!(!params.paths.is_empty());
  }

  #[test]
  fn verify_convert_params_set_flash_ram() {
    let mut params = ConvertParams::new(
      ConvertMode::Create,
      SrmFile::from_name("file.srm"),
      SrmPaths::default(),
    );

    assert!(params.paths.is_empty());

    let fla: SaveFile = "save.fla".try_into().expect("Save name is ok");
    assert_eq!(params.set_or_replace_file(fla.clone()), None);
    assert_eq!(params.flash_ram(), &Some(fla.clone()));

    assert!(!params.paths.is_empty());

    let fla_new: SaveFile = "sav2.fla".try_into().expect("Save name is ok");
    assert_eq!(params.set_or_replace_file(fla_new.clone()), Some(fla));
    assert_eq!(params.flash_ram(), &Some(fla_new));

    assert!(!params.paths.is_empty());
  }

  #[test]
  fn verify_convert_params_set_mpk() {
    use ControllerPackKind::*;

    let mut params = ConvertParams::new(
      ConvertMode::Create,
      SrmFile::from_name("file.srm"),
      SrmPaths::default(),
    );

    assert!(params.paths.is_empty());

    let cp: SaveFile = "save.mpk1".try_into().expect("Save name is ok");
    assert_eq!(params.set_or_replace_file(cp.clone()), None);
    assert_eq!(params.controller_pack(Player1), &Some(cp.clone()));

    assert!(!params.paths.is_empty());

    let cp_new: SaveFile = "save.mpk2".try_into().expect("Save name is ok");
    assert_eq!(params.set_or_replace_file(cp_new.clone()), None);
    assert_eq!(params.controller_pack(Player2), &Some(cp_new));

    let cp_new: SaveFile = "save.mpk3".try_into().expect("Save name is ok");
    assert_eq!(params.set_or_replace_file(cp_new.clone()), None);
    assert_eq!(params.controller_pack(Player3), &Some(cp_new));

    let cp_new: SaveFile = "save.mpk4".try_into().expect("Save name is ok");
    assert_eq!(params.set_or_replace_file(cp_new.clone()), None);
    assert_eq!(params.controller_pack(Player4), &Some(cp_new));

    // test replace by mupen
    let mut cp_new: SaveFile = "save.mpk".try_into().expect("Save name is ok");
    cp_new.save_type = ControllerPackKind::Mupen.into();
    assert_eq!(params.set_or_replace_file(cp_new.clone()), Some(cp));
    assert_eq!(params.controller_pack(Mupen), &Some(cp_new));

    test_for_none(&params.paths, SaveFlag::ALL & !SaveFlag::CP1);
  }

  #[test]
  fn verify_convert_params() {
    let mut params = ConvertParams::new(
      ConvertMode::Create,
      SrmFile::from_name("file.srm"),
      SrmPaths::default(),
    );

    assert_eq!(params.mode, ConvertMode::Create);
    assert_eq!(params.file, "file".into());
    assert!(params.paths.is_empty());

    assert_eq!(params.replace_mode(ConvertMode::Split), ConvertMode::Create);
    assert_eq!(params.replace_srm_file("new".into()), "file".into());
    assert!(params.paths.is_empty());

    let eep: SaveFile = "save.eep".try_into().expect("File name is ok");
    params.set_or_replace_file(eep.clone());

    assert!(!params.paths.is_empty());

    assert_eq!(params.unset_file(SaveType::Eeprom), Some(eep));

    assert!(params.paths.is_empty());
  }

  #[test]
  fn verify_convert_params_valid() {
    let mut params = ConvertParams::new(ConvertMode::Split, "file".into(), SrmPaths::default());

    assert_eq!(
      params.validate(),
      Some(Problem::FileDoesNotExist(vec![Path::new("file.srm")]))
    );

    params.replace_mode(ConvertMode::Create);

    assert_eq!(params.validate(), Some(Problem::NoInput));
  }
}