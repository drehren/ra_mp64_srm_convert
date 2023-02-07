use crate::convert_params::{Problem, SrmPaths};
use crate::{
  ControllerPackKind, ConvertMode, ConvertParams, SaveFile, SaveFileInferError, SrmFile,
};

use std::collections::HashMap;
use std::ffi;
use std::ops::{Deref, DerefMut};
use std::path::PathBuf;

use log::{debug, info, warn};

/// Represents an invalid group entry
#[derive(Debug)]
pub struct InvalidGroup<'g> {
  key: &'g str,
  mode: &'g ConvertMode,
  file: &'g SrmFile,
  problem: Problem<'g>,
}

impl<'g> InvalidGroup<'g> {
  fn new(key: &'g str, mode: &'g ConvertMode, file: &'g SrmFile, problem: Problem<'g>) -> Self {
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
  pub fn mode(&self) -> &ConvertMode {
    self.mode
  }

  /// Gets the [SrmFile]
  pub fn srm_file(&self) -> &SrmFile {
    self.file
  }
}

/// Validates the groups, giving any group (by name) that has an invalid path
pub fn validate_groups(groups: &GroupedSaves) -> Vec<InvalidGroup<'_>> {
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
  Srm(SrmFile),
  Other(SaveFile),
}

impl ValidFile {
  fn save_type_display(&self) -> String {
    match self {
      Self::Srm(_) => "SRM".into(),
      Self::Other(file) => format!("{}", file.save_type),
    }
  }
}

/// Groups the files depending on the given [GroupOpts]
pub fn group_saves(files: Vec<PathBuf>, opts: Grouping) -> GroupedSaves {
  let mut order = HashMap::<String, usize>::new();

  struct GroupData {
    key: String,
    mode: Option<ConvertMode>,
    file: Option<SrmFile>,
    paths: SrmPaths,
  }

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

    // determine if this is an SrmFile or an SaveFile
    let valid_file = match SaveFile::try_from(path.clone()) {
      Ok(mut save_file) => {
        if opts.merge_cp && save_file.is_controller_pack() {
          save_file.save_type = ControllerPackKind::Mupen.into()
        }
        ValidFile::Other(save_file)
      }
      Err(SaveFileInferError::IsAnSrmFile) => match SrmFile::try_from(path) {
        Ok(srm_file) => ValidFile::Srm(srm_file),
        Err(err) => {
          warn!("Not a valid file: {err}");
          continue;
        }
      },
      Err(err) => {
        warn!("Not a valid file: {err}");
        continue;
      }
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

    match order.get(&key) {
      Some(&index) => {
        let data = &mut values[index];
        let save_type = valid_file.save_type_display();
        match match valid_file {
          ValidFile::Srm(srm) => (data.file.replace(srm.clone()).map(ValidFile::Srm), srm.0),
          ValidFile::Other(save) => (
            data.paths.set(save.clone()).map(ValidFile::Other),
            save.file,
          ),
        } {
          (Some(ValidFile::Srm(SrmFile(old))), new)
          | (Some(ValidFile::Other(SaveFile { file: old, .. })), new) => {
            info!(
              "{save_type}: replaced {} with {}",
              old.display(),
              new.display()
            )
          }
          (None, _) => {
            debug!("{save_type}: added")
          }
        }
      }
      None => {
        warn!("Created new group: \"{key}\"");
        let mode = opts.mode.get_convert_mode(&valid_file);
        let mut paths = SrmPaths::default();
        let file = match valid_file {
          ValidFile::Srm(srm_file) => Some(srm_file),
          ValidFile::Other(save_file) => {
            paths.set(save_file);
            None
          }
        };
        order.insert(key.clone(), values.len());
        values.push(GroupData {
          key,
          mode,
          file,
          paths,
        });
      }
    }
  }

  let values = values
    .into_iter()
    .map(|data| {
      let GroupData {
        key,
        mode,
        file,
        paths,
      } = data;
      (
        key.clone(),
        ConvertParams::new(
          mode.unwrap_or_else(|| {
            debug!("> Group \"{key}\": Will Create SRM");
            ConvertMode::Create
          }),
          file.unwrap_or_else(|| {
            debug!("> Group \"{key}\": Default SRM");
            SrmFile::from_name(key)
          }),
        )
        .set_paths(paths),
      )
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
        ValidFile::Other(_) => None,
      },
    }
  }
}

/// Provides the grouping options
#[derive(Debug, Clone, Copy)]
pub struct Grouping {
  mode: GroupMode,
  merge_cp: bool,
}
impl Grouping {
  fn new(mode: GroupMode) -> Self {
    Self {
      mode,
      merge_cp: false,
    }
  }
  /// Creates an automatic grouping options
  pub fn automatic() -> Self {
    Self::new(GroupMode::Automatic)
  }
  /// Creates a forced creation grouping options
  pub fn force_create() -> Self {
    Self::new(GroupMode::Create)
  }
  /// Creates a forces split grouping options
  pub fn force_split() -> Self {
    Self::new(GroupMode::Split)
  }
  /// Set to true to treat any controller pack as Mupen64 mempack
  pub fn set_merge_controller_pack(self, merge_cp: bool) -> Self {
    Self { merge_cp, ..self }
  }
}
impl From<GroupMode> for Grouping {
  fn from(mode: GroupMode) -> Self {
    Self::new(mode)
  }
}

#[cfg(test)]
mod tests {
  use std::path::Path;

  use crate::{
    convert_params::tests::{check_empty_files, SaveFlag, SaveFlagExt},
    grouping::{group_saves, validate_groups, Problem},
    ControllerPackKind, ConvertMode, ConvertParams, Grouping, SaveType,
  };

  use super::GroupedSaves;

  impl GroupedSaves {
    fn names(&self) -> Vec<&String> {
      self.0.iter().map(|(k, ..)| k).collect::<Vec<_>>()
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
          assert_eq!(value.mode(), &ConvertMode::Split);
          assert_eq!(value.srm_file(), &key.into());
          check_empty_files(&value, SaveFlag::ALL);
        }
        "B" => {
          // split to named mpk
          assert_eq!(value.mode(), &ConvertMode::Split);
          assert_eq!(value.srm_file(), &key.into());
          assert_eq!(
            value.save_file(ControllerPackKind::Player1.into()),
            &Some("B.mpk1".try_into().expect("File name is ok"))
          );
          check_empty_files(&value, SaveFlag::ALL & !SaveFlag::CP1);
        }
        "B1" => {
          // create from B1.eep, no srm given
          assert_eq!(value.mode(), &ConvertMode::Create);
          assert_eq!(value.srm_file(), &key.into());
          assert_eq!(
            value.save_file(SaveType::Eeprom),
            &Some("B1.eep".try_into().expect("File name is ok"))
          );
          check_empty_files(&value, SaveFlag::ALL & !SaveFlag::EEP);
        }
        "D" => {
          // create from D.fla & folder/D.mpk, to D.srm
          assert_eq!(value.mode(), &ConvertMode::Create);
          assert_eq!(value.srm_file(), &key.into());
          assert_eq!(
            value.save_file(ControllerPackKind::Player1.into()),
            &Some("folder/D.mpk".try_into().expect("Path is good"))
          );
          assert_eq!(
            value.save_file(SaveType::FlashRam),
            &Some("D.fla".try_into().expect("File name is ok"))
          );
          check_empty_files(&value, SaveFlag::ALL & !SaveFlag::CP1 & !SaveFlag::FLA);
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
    assert_eq!(value.mode(), &ConvertMode::Split);
    assert_eq!(value.srm_file(), &"folder2/A".into());
    check_empty_files(value, SaveFlag::ALL);
  }

  #[test]
  fn verify_create_grouping() {
    let files = vec!["Space.mpk".into(), "folder/extracted.sra".into()];

    let groups = group_saves(files, Grouping::force_create());

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "Space");
    assert_eq!(value.mode(), &ConvertMode::Create);
    assert_eq!(value.srm_file(), &"Space".into());
    assert_eq!(
      value.save_file(ControllerPackKind::Player1.into()),
      &Some("Space.mpk".try_into().expect("File name is ok"))
    );
    assert_eq!(
      value.save_file(SaveType::Sram),
      &Some("folder/extracted.sra".try_into().expect("File name is ok"))
    );
    check_empty_files(&value, SaveFlag::ALL & !SaveFlag::CP1 & !SaveFlag::SRA);
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
    assert_eq!(value.mode(), &ConvertMode::Create);
    assert_eq!(value.srm_file(), &"actual".into());
    assert_eq!(
      value.save_file(SaveType::Sram),
      &Some("real.sra".try_into().expect("File name is ok"))
    );
    assert_eq!(
      value.save_file(ControllerPackKind::Player1.into()),
      &Some("last.mpk".try_into().expect("File name is ok"))
    );
    check_empty_files(&value, SaveFlag::ALL & !SaveFlag::CP1 & !SaveFlag::SRA);
  }

  #[test]
  fn verify_split_grouping() {
    let files = vec!["Space.srm".into(), "folder/extracted.sra".into()];

    let groups = group_saves(files, Grouping::force_split());

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "Space");
    assert_eq!(value.mode(), &ConvertMode::Split);
    assert_eq!(value.srm_file(), &"Space".into());
    assert_eq!(
      value.save_file(SaveType::Sram),
      &Some("folder/extracted.sra".try_into().expect("File name is ok"))
    );
    check_empty_files(&value, SaveFlag::ALL & !SaveFlag::SRA);
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
    assert_eq!(value.mode(), &ConvertMode::Split);
    assert_eq!(value.srm_file(), &"actual".into());
    assert_eq!(
      value.save_file(SaveType::Sram),
      &Some("real.sra".try_into().expect("File name is ok"))
    );
    assert_eq!(
      value.save_file(ControllerPackKind::Player1.into()),
      &Some("last.mpk".try_into().expect("File name is ok"))
    );
    check_empty_files(&value, SaveFlag::ALL & !SaveFlag::CP1 & !SaveFlag::SRA);
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
    assert_eq!(invalids[0].mode, &ConvertMode::Split);
    assert_eq!(invalids[0].file, &("A".into()));
    assert_eq!(
      invalids[0].problem,
      Problem::FileDoesNotExist(vec![&Path::new("A.srm")])
    );

    assert_eq!(invalids[1].key, "B");
    assert_eq!(invalids[1].mode, &ConvertMode::Create);
    assert_eq!(invalids[1].file, &("B".into()));
    assert_eq!(invalids[1].problem, Problem::NoInput);
  }
}
