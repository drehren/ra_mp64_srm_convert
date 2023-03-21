use std::{fmt, path::Path};

use log::info;

use crate::{
  controller_pack_file::{ControllerPackFile, MupenPackFile, Player},
  create_srm::create_srm,
  split_srm::split_srm,
  BaseArgs, BatteryFile, BatteryType, SrmFile,
};

/// Does the srm file conversion, depending on the given [params](ConvertParams) and [args](BaseArgs).
///
/// This function will also use the [info!] macro to provide information regarding the conversion
/// that will be done.
///
/// # Examples
/// ```
/// use ramp64_srm_convert_lib::*;
/// # use assert_fs::{TempDir, prelude::*};
/// # use std::io::Write;
///
/// # let tmp_dir = TempDir::new()?;
/// // Create the default args
/// let args = BaseArgs::default();
///
/// // Setup the file name
/// let file: SrmFile = "file.srm".into();
/// # let file: SrmFile = tmp_dir.child("file.srm").to_path_buf().try_into()?;
/// // Check that the file does not exist
/// assert!(!file.exists());
///
/// // Create the convert parameters
/// let mut create_params = ConvertParams::new(ConvertMode::Create, file.clone());
///
/// // Setup an input file
/// let eep: BatteryFile = "file.eep".try_into()?;
/// // ...
/// # let eep: BatteryFile = tmp_dir.child("file.eep").to_path_buf().try_into()?;
/// # std::fs::File::create(&eep).and_then(|mut f| f.write_all(b"101").and_then(|_| f.set_len(512)))?;
/// assert!(eep.exists());
///
/// create_params.change_battery(Some(eep));
///
/// // Apply the conversion
/// convert(create_params, &args)?;
///
/// // Now check the file exists
/// assert!(file.exists());
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
pub fn convert(params: ConvertParams, args: &BaseArgs) -> crate::Result {
  let mode_str = format!("{params}");
  let ConvertParams { mode, file, paths } = params;
  match mode {
    ConvertMode::Create => {
      if args.merge_mempacks {
        info!("{mode_str} using {paths:#}");
      } else {
        info!("{mode_str} using {paths}");
      }
      create_srm(file, args, paths)
    }
    ConvertMode::Split => {
      if paths.any_is_file() {
        if args.merge_mempacks {
          info!("{mode_str} into: {paths:#}")
        } else {
          info!("{mode_str} into: {paths}")
        };
      }
      split_srm(file, args, paths)
    }
  }
}

/// Provides the parameters to apply a conversion
#[derive(Debug, Clone)]
pub struct ConvertParams {
  mode: ConvertMode,
  file: SrmFile,
  paths: SrmPaths,
}

impl ConvertParams {
  /// Creates conversion parameters with the specified convert [`mode`]
  /// and srm [file](`SrmFile`).
  ///
  /// # Examples
  /// ```
  /// use ramp64_srm_convert_lib::{ConvertParams, ConvertMode};
  ///
  /// // Will split File.srm if it exists
  /// let split_params = ConvertParams::new(ConvertMode::Split, "File.srm".into());
  ///
  /// // Will create New.srm, if inputs are given
  /// let create_params = ConvertParams::new(ConvertMode::Create, "New.srm".into());
  /// ```
  /// [`mode`]: enum.ConvertMode.html
  pub fn new(mode: ConvertMode, file: SrmFile) -> Self {
    Self {
      mode,
      file,
      paths: SrmPaths::new(),
    }
  }

  /// Returns a parameters that will split te given [`SrmFile`].
  pub fn split(file: SrmFile) -> Self {
    Self {
      mode: ConvertMode::Split,
      file,
      paths: SrmPaths::new(),
    }
  }

  /// Returns the parameters that will create/update the given [`SrmFile`].
  pub fn create(file: SrmFile) -> Self {
    Self {
      mode: ConvertMode::Create,
      file,
      paths: SrmPaths::new(),
    }
  }

  /// Gets the conversion [mode](ConvertMode) these parameters would apply.
  ///
  /// # Examples
  ///
  /// ```
  /// use ramp64_srm_convert_lib::{ConvertParams, ConvertMode};
  ///
  /// let split_params = ConvertParams::new(ConvertMode::Split, "File.srm".into());
  ///
  /// assert_eq!(split_params.mode(), ConvertMode::Split);
  /// ```
  pub fn mode(&self) -> ConvertMode {
    self.mode
  }

  /// Returns the [srm file](SrmFile) to which the conversion would apply to.
  ///
  /// # Examples
  ///
  /// ```
  /// use ramp64_srm_convert_lib::*;
  ///
  /// let params = ConvertParams::new(ConvertMode::Create, "SrmFile".into());
  ///
  /// assert_eq!(params.srm_file(), &"SrmFile.srm".into());
  /// ```
  pub fn srm_file(&self) -> &SrmFile {
    &self.file
  }

  /// Returns the current battery file, if set
  pub fn battery_file(&self) -> Option<&BatteryFile> {
    self.paths.battery()
  }

  /// Gets the [file path](BatteryFile) for the given [BatteryType]
  ///
  /// # Examples
  ///
  /// ```
  /// use ramp64_srm_convert_lib::*;
  ///
  /// let params = ConvertParams::new(ConvertMode::Create, "File".into());
  ///
  /// let eep_file = params.save_file(BatteryType::Eeprom);   // Gets the EEPROM file path
  /// let sra_file = params.save_file(BatteryType::Sram);     // Gets the SRAM file path
  /// let fla_file = params.save_file(BatteryType::FlashRam); // Gets the FlashRam file path
  /// ```
  #[inline]
  pub fn save_file(&self, save_type: impl Into<BatteryType>) -> Option<&Path> {
    self.paths.battery().and_then(|b| {
      if b.battery_type() == save_type.into() {
        Some(b.as_ref())
      } else {
        None
      }
    })
  }

  ///
  pub fn player_pack_file(&self, player: Player) -> Option<&Path> {
    self
      .paths
      .player_controller_packs()
      .and_then(|packs| packs[player.index()])
  }

  ///
  pub fn get_mupen_pack_file(&self) -> Option<&Path> {
    self.paths.mupen_controller_pack()
  }

  /// TODO!
  pub fn set_player_pack(&mut self, pack: ControllerPackFile) -> Option<ReplacedPack> {
    self.paths.set_controller_pack(pack.into()).into()
  }

  ///
  pub fn set_mupen_pack_file(&mut self, mupen_pack: MupenPackFile) -> Option<ReplacedPack> {
    self.paths.set_controller_pack(mupen_pack.into()).into()
  }

  /// Replaces the current [`ConvertMode`] with the given one, returning the old one.
  ///
  /// # Examples
  ///
  /// ```
  /// use ramp64_srm_convert_lib::{ConvertParams, ConvertMode};
  ///
  /// let mut params = ConvertParams::new(ConvertMode::Split, "File.srm".into());
  /// assert_eq!(params.mode(), ConvertMode::Split);
  ///
  /// // Replace returns the old mode
  /// assert_eq!(params.change_mode(ConvertMode::Create), ConvertMode::Split);
  ///
  /// // Check that the replace was effective
  /// assert_eq!(params.mode(), ConvertMode::Create);
  /// ```
  pub fn change_mode(&mut self, mode: ConvertMode) -> ConvertMode {
    std::mem::replace(&mut self.mode, mode)
  }

  /// Replaces the current [`SrmFile`] with the given one, returning the old one.
  ///
  /// # Examples
  /// ```
  /// use ramp64_srm_convert_lib::{ConvertParams, ConvertMode};
  ///
  /// let mut params = ConvertParams::new(ConvertMode::Split, "old".into());
  ///
  /// assert_eq!(params.replace_srm_file("new".into()), "old".into());
  /// ```
  pub fn replace_srm_file(&mut self, srm_file: SrmFile) -> SrmFile {
    std::mem::replace(&mut self.file, srm_file)
  }

  /// Sets or replaces the [BatteryFile], returning the old one, if any.
  pub fn change_battery(&mut self, battery_file: Option<BatteryFile>) -> Option<BatteryFile> {
    self.paths.set_battery(battery_file)
  }

  /// Validates the current parameters, returning an existing possible problem
  pub fn validate(&self) -> Option<Problem<'_>> {
    match self.mode {
      ConvertMode::Create => {
        if self.paths.is_empty() {
          Some(Problem::NoInput)
        } else {
          let invalid_paths = self.paths.get_invalid_paths();
          (!invalid_paths.is_empty()).then_some(Problem::FileDoesNotExist(invalid_paths))
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

#[derive(Debug, PartialEq, Clone)]
pub enum ReplacedPack {
  Mupen(MupenPackFile),
  Player(Vec<(Player, ControllerPackFile)>),
}

impl From<SrmControllerPackPath> for Option<ReplacedPack> {
  fn from(value: SrmControllerPackPath) -> Self {
    match value {
      SrmControllerPackPath::Empty => None,
      SrmControllerPackPath::Mupen(pack) => pack.map(ReplacedPack::Mupen),
      SrmControllerPackPath::Player(packs) => {
        let replaced = packs
          .into_iter()
          .enumerate()
          .filter_map(|(i, p)| p.map(|p| (Player::from(i + 1), p)))
          .collect::<Vec<_>>();
        (!replaced.is_empty()).then_some(ReplacedPack::Player(replaced))
      }
    }
  }
}

/// Specifies the way to convert an SRM file
#[derive(Clone, Copy, Default, Debug, PartialEq)]
#[repr(u8)]
pub enum ConvertMode {
  /// Specify this variant to create/overwrite an SRM file
  #[default]
  Create = 0,
  /// Specify this variant to split an existing SRM file
  Split = 1,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct SrmPaths {
  battery: Option<BatteryFile>,
  controller_pack: SrmControllerPackPath,
}

impl SrmPaths {
  fn new() -> Self {
    Self {
      battery: None,
      controller_pack: SrmControllerPackPath::Empty,
    }
  }

  pub(crate) fn is_empty(&self) -> bool {
    self.battery.is_none() && self.controller_pack.is_empty()
  }

  pub(crate) fn battery(&self) -> Option<&BatteryFile> {
    self.battery.as_ref()
  }

  pub(crate) fn set_battery(&mut self, battery_file: Option<BatteryFile>) -> Option<BatteryFile> {
    std::mem::replace(&mut self.battery, battery_file)
  }

  pub(crate) fn set_controller_pack(
    &mut self,
    value: SrmControllerPackPath,
  ) -> SrmControllerPackPath {
    match (&mut self.controller_pack, value) {
      (SrmControllerPackPath::Player(cps), SrmControllerPackPath::Player(new_cps)) => {
        let mut changed = false;
        let mut replaced = [None, None, None, None];
        for (i, cp) in new_cps.into_iter().enumerate() {
          if let Some(cp) = cp {
            changed = true;
            replaced[i] = cps[i].replace(cp);
          }
        }
        if changed {
          SrmControllerPackPath::Player(replaced)
        } else {
          SrmControllerPackPath::Empty
        }
      }
      (_, value) => std::mem::replace(&mut self.controller_pack, value),
    }
  }

  fn get_paths_vec(&self) -> Vec<Option<&Path>> {
    match &self.controller_pack {
      SrmControllerPackPath::Empty => vec![],
      SrmControllerPackPath::Mupen(cp) => {
        vec![
          self.battery.as_ref().map(|f| f.as_ref()),
          cp.as_ref().map(|p| p.as_ref()),
        ]
      }
      SrmControllerPackPath::Player(cps) => vec![
        self.battery.as_ref().map(|f| f.as_ref()),
        cps[0].as_ref().map(|f| f.as_ref()),
        cps[1].as_ref().map(|f| f.as_ref()),
        cps[2].as_ref().map(|f| f.as_ref()),
        cps[3].as_ref().map(|f| f.as_ref()),
      ],
    }
  }

  fn get_typed_vec(&self) -> Vec<Option<(SrmType, &Path)>> {
    use SrmType::*;
    match &self.controller_pack {
      SrmControllerPackPath::Empty => vec![],
      SrmControllerPackPath::Mupen(cp) => vec![
        self
          .battery
          .as_ref()
          .map(|f| (f.battery_type().into(), f.as_ref())),
        cp.as_ref().map(|p| (Mupen, p.as_ref())),
      ],
      SrmControllerPackPath::Player(cps) => vec![
        self
          .battery
          .as_ref()
          .map(|f| (f.battery_type().into(), f.as_ref())),
        cps[0].as_ref().map(|f| (Cp1, f.as_ref())),
        cps[1].as_ref().map(|f| (Cp2, f.as_ref())),
        cps[2].as_ref().map(|f| (Cp3, f.as_ref())),
        cps[3].as_ref().map(|f| (Cp4, f.as_ref())),
      ],
    }
  }

  pub(crate) fn any_is_file(&self) -> bool {
    self
      .get_paths_vec()
      .iter()
      .any(|p| p.map_or(true, |f| f.is_file()))
  }

  pub(crate) fn get_invalid_paths(&self) -> Vec<&Path> {
    self
      .get_paths_vec()
      .iter()
      .filter_map(|p| p.as_ref().copied())
      .filter(|p| !p.exists())
      .collect::<Vec<_>>()
  }

  pub(crate) fn mupen_controller_pack(&self) -> Option<&Path> {
    match &self.controller_pack {
      SrmControllerPackPath::Mupen(path) => path.as_ref().map(|m| m.as_ref()),
      _ => None,
    }
  }

  pub(crate) fn player_controller_packs(&self) -> Option<[Option<&Path>; 4]> {
    match &self.controller_pack {
      SrmControllerPackPath::Empty | SrmControllerPackPath::Mupen(_) => None,
      SrmControllerPackPath::Player(cps) => Some([
        cps[0].as_ref().map(|p| p.as_ref()),
        cps[1].as_ref().map(|p| p.as_ref()),
        cps[2].as_ref().map(|p| p.as_ref()),
        cps[3].as_ref().map(|p| p.as_ref()),
      ]),
    }
  }
}

impl fmt::Display for SrmPaths {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for (i, path) in self.get_typed_vec().into_iter().enumerate() {
      if let Some((srm_type, path)) = path {
        if i > 0 {
          f.write_str(" and ")?;
        }
        f.write_fmt(format_args!("{} ({})", srm_type, path.display()))?;
      }
    }
    Ok(())
  }
}

enum SrmType {
  Eeprom,
  Sram,
  FlashRam,
  Mupen,
  Cp1,
  Cp2,
  Cp3,
  Cp4,
}

impl From<BatteryType> for SrmType {
  fn from(value: BatteryType) -> Self {
    match value {
      BatteryType::Eeprom => Self::Eeprom,
      BatteryType::Sram => Self::Sram,
      BatteryType::FlashRam => Self::FlashRam,
    }
  }
}

impl fmt::Display for SrmType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::Eeprom => "EEPROM",
      Self::Sram => "SRAM",
      Self::FlashRam => "FlashRAM",
      Self::Mupen => "Mupen Controller Pack",
      Self::Cp1 => "Controller Pack 1",
      Self::Cp2 => "Controller Pack 2",
      Self::Cp3 => "Controller Pack 3",
      Self::Cp4 => "Controller Pack 4",
    })
  }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum SrmControllerPackPath {
  Empty,
  Mupen(Option<MupenPackFile>),
  Player([Option<ControllerPackFile>; 4]),
}

impl SrmControllerPackPath {
  fn is_empty(&self) -> bool {
    match self {
      Self::Empty => true,
      Self::Mupen(cp) => cp.is_none(),
      Self::Player(cps) => cps.iter().all(Option::is_none),
    }
  }
}

impl From<[Option<ControllerPackFile>; 4]> for SrmControllerPackPath {
  fn from(value: [Option<ControllerPackFile>; 4]) -> Self {
    Self::Player(value)
  }
}

impl From<Option<ControllerPackFile>> for SrmControllerPackPath {
  fn from(value: Option<ControllerPackFile>) -> Self {
    match value {
      Some(pack) => match pack.player() {
        Some(Player::P1) => SrmControllerPackPath::Player([Some(pack), None, None, None]),
        Some(Player::P2) => SrmControllerPackPath::Player([None, Some(pack), None, None]),
        Some(Player::P3) => SrmControllerPackPath::Player([None, None, Some(pack), None]),
        Some(Player::P4) => SrmControllerPackPath::Player([None, None, None, Some(pack)]),
        None => SrmControllerPackPath::Player([Some(pack), None, None, None]),
      },
      None => SrmControllerPackPath::Player([None, None, None, None]),
    }
  }
}

impl From<ControllerPackFile> for SrmControllerPackPath {
  fn from(pack: ControllerPackFile) -> Self {
    let mut cps = [None, None, None, None];
    match pack.player() {
      Some(player) => cps[player.index()] = Some(pack),
      None => cps[0] = Some(pack),
    }
    Self::Player(cps)
  }
}

impl From<Option<MupenPackFile>> for SrmControllerPackPath {
  fn from(value: Option<MupenPackFile>) -> Self {
    Self::Mupen(value)
  }
}

impl From<MupenPackFile> for SrmControllerPackPath {
  fn from(value: MupenPackFile) -> Self {
    Self::Mupen(Some(value))
  }
}

/// The validation problem a group had
#[derive(Debug, PartialEq, Eq)]
pub enum Problem<'g> {
  /// Missing input save files
  NoInput,
  /// Files do not exist
  FileDoesNotExist(Vec<&'g Path>),
  /// Path is not a file
  NotAFile,
}

#[cfg(test)]
pub(super) mod tests {
  use crate::BatteryType;

  use super::*;

  #[test]
  fn verify_srm_paths() {
    let paths = SrmPaths::new();

    assert!(paths.is_empty());
    assert!(!paths.any_is_file());
    assert_eq!(paths.get_invalid_paths(), Vec::<&Path>::new());
  }

  #[test]
  fn verify_srm_paths_set_mpk() {
    let mut paths = SrmPaths::new();

    let cp: ControllerPackFile = "A.mpk".into();
    assert_eq!(
      paths.set_controller_pack(cp.clone().into()),
      SrmControllerPackPath::Empty
    );
    assert!(!matches!(
      paths.controller_pack,
      SrmControllerPackPath::Mupen(_)
    ));

    assert!(!paths.is_empty());
    assert_eq!(
      paths.controller_pack,
      SrmControllerPackPath::Player([Some("A.mpk".into()), None, None, None])
    );

    // test replace mkp1
    let cp1: ControllerPackFile = "B.mpk1".into();
    assert_eq!(
      paths.set_controller_pack(cp1.clone().into()),
      SrmControllerPackPath::Player([Some(cp), None, None, None])
    );

    assert!(!paths.is_empty());
    assert_eq!(
      paths.controller_pack,
      SrmControllerPackPath::Player([Some("B.mpk1".into()), None, None, None])
    );

    // test add mpk3
    let cp3: ControllerPackFile = "X.mpk3".into();
    assert_eq!(
      paths.set_controller_pack(cp3.clone().into()),
      SrmControllerPackPath::Player([None, None, None, None])
    );

    assert!(!paths.is_empty());
    assert_eq!(
      paths.controller_pack,
      SrmControllerPackPath::Player([Some("B.mpk1".into()), None, Some("X.mpk3".into()), None])
    );

    // test replace with mupen mpk
    let mpk: MupenPackFile = "M.mpk4".into();
    assert_eq!(
      paths.set_controller_pack(mpk.clone().into()),
      SrmControllerPackPath::Player([Some("B.mpk1".into()), None, Some("X.mpk3".into()), None])
    );

    assert!(!paths.is_empty());
    assert!(matches!(
      paths.controller_pack,
      SrmControllerPackPath::Mupen(_)
    ));
    assert_eq!(
      paths.controller_pack,
      SrmControllerPackPath::Mupen(Some("M.mpk4".into()))
    )
  }

  #[test]
  fn verify_convert_params_new() {
    let params = ConvertParams::new(ConvertMode::Create, "file.srm".into());

    assert_eq!(params.mode, ConvertMode::Create);
    assert_eq!(params.file, "file".into());
    assert_eq!(params.paths, SrmPaths::new());
  }

  #[test]
  fn verify_convert_params_set_eep() {
    let mut params = ConvertParams::new(ConvertMode::Create, "file.srm".into());

    assert!(params.paths.is_empty());

    let eep: BatteryFile = "save.eep".try_into().expect("Save name is ok");
    assert_eq!(params.change_battery(Some(eep.clone())), None);
    assert_eq!(
      params.save_file(BatteryType::Eeprom),
      Some(Path::new("save.eep"))
    );

    assert!(!params.paths.is_empty());

    let eep_new: BatteryFile = "sav2.eep".try_into().expect("Save name is ok");
    assert_eq!(params.change_battery(Some(eep_new.clone())), Some(eep));
    assert_eq!(
      params.save_file(BatteryType::Eeprom),
      Some(Path::new("sav2.eep"))
    );

    assert!(!params.paths.is_empty());
  }

  #[test]
  fn verify_convert_params_set_sram() {
    let mut params = ConvertParams::new(ConvertMode::Create, "file.srm".into());

    assert!(params.paths.is_empty());

    let sra: BatteryFile = "save.sra".try_into().expect("Save name is ok");
    assert_eq!(params.change_battery(Some(sra.clone())), None);
    assert_eq!(
      params.save_file(BatteryType::Sram),
      Some(Path::new("save.sra"))
    );

    assert!(!params.paths.is_empty());

    let sra_new: BatteryFile = "sav2.sra".try_into().expect("Save name is ok");
    assert_eq!(params.change_battery(Some(sra_new.clone())), Some(sra));
    assert_eq!(
      params.save_file(BatteryType::Sram),
      Some(Path::new("sav2.sra"))
    );

    assert!(!params.paths.is_empty());
  }

  #[test]
  fn verify_convert_params_set_flash_ram() {
    let mut params = ConvertParams::new(ConvertMode::Create, "file.srm".into());

    assert!(params.paths.is_empty());

    let fla: BatteryFile = "save.fla".try_into().expect("Save name is ok");
    assert_eq!(params.change_battery(Some(fla.clone())), None);
    assert_eq!(
      params.save_file(BatteryType::FlashRam),
      Some(Path::new("save.fla"))
    );

    assert!(!params.paths.is_empty());

    let fla_new: BatteryFile = "sav2.fla".try_into().expect("Save name is ok");
    assert_eq!(params.change_battery(Some(fla_new.clone())), Some(fla));
    assert_eq!(
      params.save_file(BatteryType::FlashRam),
      Some(Path::new("sav2.fla"))
    );

    assert!(!params.paths.is_empty());
  }

  #[test]
  fn verify_convert_params_set_mpk() {
    use Player::*;

    let mut params = ConvertParams::new(ConvertMode::Create, "file.srm".into());

    assert!(params.paths.is_empty());

    let cp1: ControllerPackFile = "save.mpk1".into();
    assert_eq!(params.set_player_pack(cp1.clone()), None);
    assert_eq!(params.player_pack_file(P1), Some(Path::new("save.mpk1")));

    assert!(!params.paths.is_empty());

    let cp2: ControllerPackFile = "save.mpk2".into();
    assert_eq!(params.set_player_pack(cp2.clone()), None);
    assert_eq!(params.player_pack_file(P2), Some(Path::new("save.mpk2")));

    let cp3: ControllerPackFile = "save.mpk3".try_into().expect("Save name is ok");
    assert_eq!(params.set_player_pack(cp3.clone()), None);
    assert_eq!(params.player_pack_file(P3), Some(Path::new("save.mpk3")));

    let cp4: ControllerPackFile = "save.mpk4".try_into().expect("Save name is ok");
    assert_eq!(params.set_player_pack(cp4.clone()), None);
    assert_eq!(params.player_pack_file(P4), Some(Path::new("save.mpk4")));

    // test replace by mupen
    let cp_new: MupenPackFile = "save.mpk".into();
    assert_eq!(
      params.set_mupen_pack_file(cp_new.clone()),
      Some(ReplacedPack::Player(vec![
        (P1, cp1),
        (P2, cp2),
        (P3, cp3),
        (P4, cp4)
      ]))
    );
    assert_eq!(params.get_mupen_pack_file(), Some(Path::new("save.mpk")));
  }

  #[test]
  fn verify_convert_params() {
    let mut params = ConvertParams::new(ConvertMode::Create, "file.srm".into());

    assert_eq!(params.mode, ConvertMode::Create);
    assert_eq!(params.file, "file".into());
    assert!(params.paths.is_empty());

    assert_eq!(params.change_mode(ConvertMode::Split), ConvertMode::Create);
    assert_eq!(params.replace_srm_file("new".into()), "file".into());
    assert!(params.paths.is_empty());

    let eep: BatteryFile = "save.eep".try_into().expect("File name is ok");
    params.change_battery(Some(eep.clone()));

    assert!(!params.paths.is_empty());

    assert_eq!(params.change_battery(None), Some(eep));

    assert!(params.paths.is_empty());
  }

  #[test]
  fn verify_convert_params_valid() {
    let mut params = ConvertParams::new(ConvertMode::Split, "file".into());

    assert_eq!(
      params.validate(),
      Some(Problem::FileDoesNotExist(vec![Path::new("file.srm")]))
    );

    params.change_mode(ConvertMode::Create);

    assert_eq!(params.validate(), Some(Problem::NoInput));
  }
}
