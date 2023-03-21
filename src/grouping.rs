use std::collections::HashMap;
use std::ffi;
use std::ops::{Deref, DerefMut};
use std::path::PathBuf;

use log::{debug, info, warn};

use ramp64_srm_convert_lib::{
  try_into_convertible_file, BatteryFile, ControllerPackFile, ConvertMode, ConvertParams,
  MupenPackFile, Problem, SrmFile,
};

#[derive(Debug)]
pub(crate) struct InvalidGroup<'g> {
  key: &'g str,
  mode: ConvertMode,
  file: &'g SrmFile,
  problem: Problem<'g>,
}

impl<'g> InvalidGroup<'g> {
  fn new(key: &'g str, mode: ConvertMode, file: &'g SrmFile, problem: Problem<'g>) -> Self {
    Self {
      key,
      mode,
      file,
      problem,
    }
  }

  /// Gets the invalid group name
  pub fn group_name(&self) -> &str {
    self.key
  }
  /// Gets the problem
  pub fn problem(&self) -> &Problem<'_> {
    &self.problem
  }
  /// Gets the mode the problem arose from
  pub fn mode(&self) -> ConvertMode {
    self.mode
  }

  /// Gets the [SrmFile]
  pub fn srm_file(&self) -> &SrmFile {
    self.file
  }
}

pub(crate) fn validate_groups(groups: &GroupedSaves) -> Vec<InvalidGroup<'_>> {
  let mut invalid_groups = Vec::new();
  for (key, value) in groups {
    if let Some(problem) = value.validate() {
      invalid_groups.push(InvalidGroup::new(
        key,
        value.mode(),
        value.srm_file(),
        problem,
      ))
    }
  }
  invalid_groups
}

/// Contains the groups of save conversion parameters, keyed by a group name
#[derive(Debug, Default)]
pub struct GroupedSaves(Vec<(String, ConvertParams)>);

impl Deref for GroupedSaves {
  type Target = [(String, ConvertParams)];

  fn deref(&self) -> &Self::Target {
    self.0.deref()
  }
}

impl DerefMut for GroupedSaves {
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl IntoIterator for GroupedSaves {
  type Item = (String, ConvertParams);

  type IntoIter = std::vec::IntoIter<(String, ConvertParams)>;

  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<'a> IntoIterator for &'a GroupedSaves {
  type Item = &'a (String, ConvertParams);

  type IntoIter = std::slice::Iter<'a, (String, ConvertParams)>;

  fn into_iter(self) -> Self::IntoIter {
    self.0.iter()
  }
}

impl<'a> IntoIterator for &'a mut GroupedSaves {
  type Item = &'a mut (String, ConvertParams);

  type IntoIter = std::slice::IterMut<'a, (String, ConvertParams)>;

  fn into_iter(self) -> Self::IntoIter {
    self.0.iter_mut()
  }
}

enum ValidFile {
  Empty,
  Srm(SrmFile),
  Save(BatteryFile),
  ControllerPack(ControllerPackFile),
  MupenPack(MupenPackFile),
}

impl ValidFile {
  fn display(&self) -> std::path::Display<'_> {
    match self {
      ValidFile::Empty => unreachable!(),
      ValidFile::Srm(f) => f.as_ref(),
      ValidFile::Save(f) => f.as_ref(),
      ValidFile::ControllerPack(f) => f.as_ref(),
      ValidFile::MupenPack(f) => f.as_ref(),
    }
    .display()
  }

  fn srm_file(&mut self) -> Option<SrmFile> {
    match std::mem::replace(self, ValidFile::Empty) {
      ValidFile::Srm(file) => Some(file),
      value => {
        *self = value;
        None
      }
    }
  }

  fn battery_file(&mut self) -> Option<BatteryFile> {
    match std::mem::replace(self, ValidFile::Empty) {
      ValidFile::Save(file) => Some(file),
      value => {
        *self = value;
        None
      }
    }
  }

  fn controller_pack(&mut self) -> Option<ControllerPackFile> {
    match std::mem::replace(self, ValidFile::Empty) {
      ValidFile::ControllerPack(file) => Some(file),
      value => {
        *self = value;
        None
      }
    }
  }

  fn mupen_pack(&mut self) -> Option<MupenPackFile> {
    match std::mem::replace(self, ValidFile::Empty) {
      ValidFile::MupenPack(file) => Some(file),
      value => {
        *self = value;
        None
      }
    }
  }

  fn cp(&mut self) -> Cp {
    let cp = self.controller_pack();
    let mp = self.mupen_pack();
    if cp.is_some() {
      cp.into()
    } else {
      mp.into()
    }
  }
}

#[derive(Default)]
enum Cp {
  #[default]
  Empty,
  Mupen(MupenPackFile),
  Cp(Vec<ControllerPackFile>),
}

impl Cp {
  fn put_cp(&mut self, cp: ControllerPackFile) {
    match self {
      Cp::Empty | Cp::Mupen(_) => *self = Cp::Cp(vec![cp]),
      Cp::Cp(cps) => cps.push(cp),
    }
  }

  fn set_mp(&mut self, mp: MupenPackFile) {
    *self = Cp::Mupen(mp);
  }
}

impl From<Option<ControllerPackFile>> for Cp {
  fn from(value: Option<ControllerPackFile>) -> Self {
    value.map_or(Cp::Empty, |f| Cp::Cp(vec![f]))
  }
}

impl From<Option<MupenPackFile>> for Cp {
  fn from(value: Option<MupenPackFile>) -> Self {
    value.map_or(Cp::Empty, Cp::Mupen)
  }
}

#[derive(Default)]
struct GroupData {
  key: String,
  mode: Option<ConvertMode>,
  srm_file: Option<SrmFile>,
  battery_file: Option<BatteryFile>,
  controller_pack: Cp,
}

pub(crate) fn group_saves(files: Vec<PathBuf>, opts: Grouping) -> GroupedSaves {
  let mut order = HashMap::<String, usize>::new();

  let mut values: Vec<GroupData> = Vec::new();

  // collect files from arguments
  for path in files.into_iter() {
    debug!("File {}:", path.display());
    if path.is_dir() {
      warn!("Path {} is a directory", path.display());
      continue;
    }

    let Some(name) = path.file_stem().and_then(ffi::OsStr::to_str).map(|s| s.to_string()) else {
      warn!("Path {} did not name a file", path.display());
      continue;
    };

    let key = if opts.mode == GroupMode::Automatic {
      debug!("Group name is: \"{name}\"");
      name
    } else {
      values
        .first()
        .map(|GroupData { key, .. }| key.clone())
        .unwrap_or(name)
    };

    let convertible_file = match try_into_convertible_file(&path) {
      Ok(convertible_file) => convertible_file,
      Err(err) => {
        warn!("Not a valid file: {err}");
        continue;
      }
    };

    // determine if this is an SrmFile or an SaveFile
    let mut valid_file = match () {
      _ if convertible_file.is_player_pack() => convertible_file
        .into_player_pack()
        .map_left(ValidFile::ControllerPack),
      _ if convertible_file.is_mupen_pack() => convertible_file
        .into_mupen_pack()
        .map_left(ValidFile::MupenPack),
      _ if convertible_file.is_battery() => convertible_file
        .try_into_battery()
        .map_left(ValidFile::Save),
      _ if convertible_file.is_srm() => convertible_file.try_into_srm().map_left(ValidFile::Srm),
      _ => unreachable!(),
    }
    .unwrap_left();

    if matches!(valid_file, ValidFile::Empty) {
      panic!("ValidFile should not be empty here");
    }

    match order.get(&key) {
      Some(&index) => {
        let file_str = format!("{}", valid_file.display());
        match valid_file {
          ValidFile::Srm(srm) => match values[index].srm_file.replace(srm) {
            Some(old) => warn!("{key} SRM: replaced {} with {file_str}", old.display()),
            None => debug!("{key} SRM: set"),
          },
          ValidFile::Save(save) => {
            let batt_type = save.battery_type();
            match values[index].battery_file.replace(save) {
              Some(old) => warn!(
                "{key} Battery: {} ({}) replaced with {file_str}",
                old.battery_type(),
                old.display()
              ),
              None => debug!("{key} Battery: set {batt_type}"),
            }
          }
          ValidFile::ControllerPack(cp) => values[index].controller_pack.put_cp(cp),
          ValidFile::MupenPack(mp) => values[index].controller_pack.set_mp(mp),
          ValidFile::Empty => unreachable!(),
        }
      }
      None => {
        info!("Created new group: \"{key}\"");
        let group_data = GroupData {
          key: key.clone(),
          mode: opts.mode.get_convert_mode(&valid_file),
          srm_file: valid_file.srm_file(),
          battery_file: valid_file.battery_file(),
          controller_pack: valid_file.cp(),
        };

        order.insert(key, values.len());
        values.push(group_data);
      }
    }
  }

  let values = values
    .into_iter()
    .map(|data| {
      let GroupData {
        key,
        mode,
        srm_file,
        battery_file,
        controller_pack,
      } = data;

      let mut params = ConvertParams::new(
        mode.unwrap_or_else(|| {
          debug!("> Group \"{key}\": Will Create SRM");
          ConvertMode::Create
        }),
        srm_file.unwrap_or_else(|| {
          debug!("> Group \"{key}\": Default SRM");
          key.as_str().into()
        }),
      );

      params.change_battery(battery_file);

      match controller_pack {
        Cp::Mupen(mp) => {
          params.set_mupen_pack_file(mp);
        }
        Cp::Cp(cps) => {
          for cp in cps {
            params.set_player_pack(cp);
          }
        }
        _ => {}
      }

      (key, params)
    })
    .collect::<Vec<_>>();

  GroupedSaves(values)
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum GroupMode {
  Create,
  Split,
  Automatic,
}

impl GroupMode {
  fn get_convert_mode(&self, valid_file: &ValidFile) -> Option<ConvertMode> {
    match self {
      GroupMode::Create => Some(ConvertMode::Create),
      GroupMode::Split => Some(ConvertMode::Split),
      GroupMode::Automatic => match valid_file {
        ValidFile::Srm(_) => Some(ConvertMode::Split),
        _ => None,
      },
    }
  }
}

/// Provides the grouping options
#[derive(Debug, Clone, Copy)]
pub struct Grouping {
  mode: GroupMode,
}
impl Grouping {
  fn new(mode: GroupMode) -> Self {
    Self { mode }
  }
  pub(crate) fn automatic() -> Self {
    Self::new(GroupMode::Automatic)
  }
  pub(crate) fn force_create() -> Self {
    Self::new(GroupMode::Create)
  }
  pub(crate) fn force_split() -> Self {
    Self::new(GroupMode::Split)
  }
}

#[cfg(test)]
mod tests {
  use std::{fs, path::Path};

  use assert_fs::{prelude::PathChild, TempDir};

  use super::{group_saves, validate_groups, GroupedSaves, Grouping, Problem};

  use ramp64_srm_convert_lib::{BatteryType, ConvertMode, ConvertParams, Player, SrmFile};

  use BatteryType::*;
  use Player::*;

  impl GroupedSaves {
    fn names(&self) -> Vec<&String> {
      self.0.iter().map(|(k, ..)| k).collect::<Vec<_>>()
    }
  }

  const ALL_TYPES: &[BatteryType; 3] = &[Eeprom, Sram, FlashRam];

  #[inline]
  fn check_all_empty_saves_but(
    params: &ConvertParams,
    battery_skip: &[BatteryType],
    cp_skip: &[Player],
    mupen_skip: bool,
  ) {
    for save_type in ALL_TYPES.iter().filter(|t| !battery_skip.contains(t)) {
      assert!(
        matches!(params.save_file(*save_type), None),
        "{save_type} file should have been empty"
      )
    }
    for player in [P1, P2, P3, P4].iter().filter(|p| !cp_skip.contains(p)) {
      assert!(
        params.player_pack_file(*player).is_none(),
        "{player:?} pack should have been empty"
      )
    }
    if !mupen_skip {
      assert!(
        params.get_mupen_pack_file().is_none(),
        "mupen pack should have been empty"
      )
    }
  }

  #[test]
  fn verify_automatic_grouping() {
    let files = vec![
      "A.srm".into(),
      "B.srm".into(),
      "C.srm".into(),
      "B1.eep".into(),
      "B.mpk1".into(),
      "D.fla".into(),
      "D.srm".into(),
      "folder/D.mpk".into(),
    ];

    let groups = group_saves(files, Grouping::automatic());

    assert_eq!(groups.names(), vec!["A", "B", "C", "B1", "D"]);

    for (key, value) in groups {
      let key = key.as_str();
      match key {
        "A" | "C" => {
          // simple auto-name split
          assert_eq!(value.mode(), ConvertMode::Split);
          assert_eq!(value.srm_file(), &key.into());
          check_all_empty_saves_but(&value, &[], &[], false);
        }
        "B" => {
          // split to named mpk
          assert_eq!(value.mode(), ConvertMode::Split);
          assert_eq!(value.srm_file(), &key.into());
          assert_eq!(value.player_pack_file(P1), Some(Path::new("B.mpk1")));
          check_all_empty_saves_but(&value, &[], &[P1], false);
        }
        "B1" => {
          // create from B1.eep, no srm given
          assert_eq!(value.mode(), ConvertMode::Create);
          assert_eq!(value.srm_file(), &key.into());
          assert_eq!(value.save_file(Eeprom), Some(Path::new("B1.eep")));
          check_all_empty_saves_but(&value, &[Eeprom], &[], false);
        }
        "D" => {
          // create from D.fla & folder/D.mpk, to D.srm
          assert_eq!(value.mode(), ConvertMode::Create);
          assert_eq!(value.srm_file(), &key.into());
          assert_eq!(value.player_pack_file(P1), Some(Path::new("folder/D.mpk")));
          assert_eq!(value.save_file(FlashRam), Some(Path::new("D.fla")));
          check_all_empty_saves_but(&value, &[FlashRam], &[P1], false);
        }
        _ => assert!(false, "unreachable"),
      }
    }
  }

  #[test]
  fn verify_automatic_grouping_replacement() {
    let files = vec![
      "A.srm".into(),
      "folder/A.srm".into(),
      "folder2/A.srm".into(),
    ];
    let groups = group_saves(files, Grouping::automatic());

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "A");
    assert_eq!(value.mode(), ConvertMode::Split);
    assert_eq!(value.srm_file(), &"folder2/A".into());
    check_all_empty_saves_but(value, &[], &[], false);
  }

  #[test]
  fn verify_create_grouping() {
    let files = vec!["Space.mpk".into(), "folder/extracted.sra".into()];

    let groups = group_saves(files, Grouping::force_create());

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "Space");
    assert_eq!(value.mode(), ConvertMode::Create);
    assert_eq!(value.srm_file(), &"Space".into());
    assert_eq!(value.player_pack_file(P1), Some(Path::new("Space.mpk")));
    assert_eq!(
      value.save_file(Sram),
      Some(Path::new("folder/extracted.sra"))
    );
    check_all_empty_saves_but(&value, &[Sram], &[P1], false);
  }

  #[test]
  fn verify_create_grouping_replacement() {
    let files = vec![
      "initial.mpk".into(),
      "folder/extracted.sra".into(),
      "last.mpk".into(),
      "real.sra".into(),
      "actual.srm".into(),
    ];

    let groups = group_saves(files, Grouping::force_create());

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "initial");
    assert_eq!(value.mode(), ConvertMode::Create);
    assert_eq!(value.srm_file(), &"actual".into());
    assert_eq!(value.save_file(Sram), Some(Path::new("real.sra")));
    assert_eq!(value.player_pack_file(P1), Some(Path::new("last.mpk")));
    check_all_empty_saves_but(&value, &[Sram], &[P1], false);
  }

  #[test]
  fn verify_split_grouping() {
    let files = vec!["Space.srm".into(), "folder/extracted.sra".into()];

    let groups = group_saves(files, Grouping::force_split());

    assert_eq!(groups.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "Space");
    assert_eq!(value.mode(), ConvertMode::Split);
    assert_eq!(value.srm_file(), &"Space".into());
    assert_eq!(
      value.save_file(Sram),
      Some(Path::new("folder/extracted.sra"))
    );
    check_all_empty_saves_but(&value, &[Sram], &[], false);
  }

  #[test]
  fn verify_split_grouping_mupen() {
    let input = vec!["A.srm".into(), "F.mpk3".into()];

    let group = group_saves(input, Grouping::force_split());

    assert_eq!(group.len(), 1);

    let (name, params) = group.first().unwrap();

    assert_eq!(name, "A");
    assert_eq!(params.mode(), ConvertMode::Split);
    assert_eq!(params.srm_file(), &"A".into());
    assert_eq!(params.get_mupen_pack_file(), Some(Path::new("F.mpk3")));
    check_all_empty_saves_but(&params, &[], &[], true);
  }

  #[test]
  fn verify_split_grouping_mupen_existing() {
    let data = TempDir::new().expect("tmp dir created");
    let a_path = data.child("A.srm").to_path_buf();
    let f_path = data.child("F.mpk3").to_path_buf();

    {
      let file = fs::File::create(&a_path).expect("file should not exist");
      file
        .set_len(0x48800)
        .expect("file should have changed size");
    }

    let input = vec![a_path.clone(), f_path.clone()];

    let group = group_saves(input, Grouping::force_split());

    assert_eq!(group.len(), 1);

    let (name, params) = group.first().unwrap();

    assert_eq!(name, "A");
    assert_eq!(params.mode(), ConvertMode::Split);
    assert_eq!(
      params.srm_file(),
      &SrmFile::try_from(a_path).expect("file is ok")
    );
    assert_eq!(params.get_mupen_pack_file(), Some(f_path.as_path()));
    check_all_empty_saves_but(&params, &[], &[], true);
  }

  #[test]
  fn verify_split_grouping_replacement() {
    let files = vec![
      "this_not_it.srm".into(),
      "initial.mpk".into(),
      "folder/extracted.sra".into(),
      "last.mpk".into(),
      "real.sra".into(),
      "actual.srm".into(),
    ];

    let groups = group_saves(files, Grouping::force_split());

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "this_not_it");
    assert_eq!(value.mode(), ConvertMode::Split);
    assert_eq!(value.srm_file(), &"actual".into());
    assert_eq!(value.save_file(Sram), Some(Path::new("real.sra")));
    assert_eq!(value.player_pack_file(P1), Some(Path::new("last.mpk")));
    check_all_empty_saves_but(&value, &[Sram], &[P1], false);
  }

  #[test]
  fn verify_validator() {
    let groups = GroupedSaves(vec![
      (
        "A".into(),
        ConvertParams::new(ConvertMode::Split, "A".into()),
      ),
      (
        "B".into(),
        ConvertParams::new(ConvertMode::Create, "B".into()),
      ),
    ]);

    let invalids = validate_groups(&groups);
    assert!(!invalids.is_empty());
    assert_eq!(invalids.len(), 2);

    assert_eq!(invalids[0].key, "A");
    assert_eq!(invalids[0].mode, ConvertMode::Split);
    assert_eq!(invalids[0].file, &("A".into()));
    assert_eq!(
      invalids[0].problem,
      Problem::FileDoesNotExist(vec![&Path::new("A.srm")])
    );

    assert_eq!(invalids[1].key, "B");
    assert_eq!(invalids[1].mode, ConvertMode::Create);
    assert_eq!(invalids[1].file, &("B".into()));
    assert_eq!(invalids[1].problem, Problem::NoInput);
  }
}
