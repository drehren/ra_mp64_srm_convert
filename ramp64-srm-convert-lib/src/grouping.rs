use crate::{ConvertMode, ConvertParams, GroupOpts, SavedPath};

use std::collections::HashMap;
use std::ffi;
use std::path::{Path, PathBuf};

use log::{debug, info, warn};

/// The validation problem a group had
#[derive(Debug, PartialEq, Eq)]
pub enum Problem<'g> {
  /// No input error
  NoInput,
  /// Files do not exist
  FileDoesNotExist(Vec<&'g Path>),
  /// Path is not a file
  NotAFile,
}

/// Represents an invalid group entry
#[derive(Debug)]
pub struct InvalidGroup<'g> {
  key: &'g str,
  mode: &'g Option<ConvertMode>,
  problem: Problem<'g>,
}

impl<'k> InvalidGroup<'k> {
  fn new(key: &'k str, mode: &'k Option<ConvertMode>, problem: Problem<'k>) -> Self {
    Self { key, mode, problem }
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
  pub fn mode(&self) -> &Option<ConvertMode> {
    self.mode
  }
}

/// Validates [GroupedSaves], giving out any group (by name) that has an invalid path
pub fn validate_groups<'g>(groups: &'g GroupedSaves) -> Vec<InvalidGroup<'g>> {
  use Problem::*;

  let mut invalid_groups = Vec::new();
  for (key, value) in groups {
    match &value.mode {
      mode @ Some(ConvertMode::Create(_)) => {
        if value.paths.is_empty() {
          invalid_groups.push(InvalidGroup::new(key, mode, NoInput));
        } else if !value.paths.any_is_file() {
          invalid_groups.push({
            let files = value.paths.get_invalid_paths();
            InvalidGroup::new(key, &value.mode, FileDoesNotExist(files))
          });
        }
      }
      mode @ Some(ConvertMode::Split(path)) => {
        if !path.exists() {
          invalid_groups.push(InvalidGroup::new(key, mode, FileDoesNotExist(vec![path])))
        } else if !path.is_file() {
          invalid_groups.push(InvalidGroup::new(key, mode, NotAFile))
        }
      }
      None => invalid_groups.push(InvalidGroup::new(key, &None, NoInput)),
    };
  }
  invalid_groups
}

/// Contains the groups of save conversion parameters, keyed by a group name
pub struct GroupedSaves(Vec<(String, ConvertParams)>);

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

/// Groups the files depending on the given [GroupOpts]
pub fn group_saves(files: Vec<PathBuf>, opts: GroupOpts) -> GroupedSaves {
  let mut order = HashMap::<String, usize>::new();

  let mut values: Vec<(String, ConvertParams)> = Vec::new();

  // collect files from arguments
  for path in files.into_iter() {
    let mut _pad = debug!("File {}:", path.display());
    if path.is_dir() {
      warn!("Path {} is a directory", path.display());
      continue;
    }
    debug!("Getting name...");
    let Some(name) = path.file_stem().and_then(ffi::OsStr::to_str) else {
      warn!("Path {} did not name a file", path.display());
      continue;
    };
    debug!("Name is \"{name}\"");

    let key = if opts.is_automatic() {
      name
    } else {
      values.first().map(|(k, _)| k.as_str()).unwrap_or(name)
    };

    let convert_args = match order.get(key) {
      Some(&index) => &mut values.get_mut(index).unwrap().1,
      None => {
        warn!("Created group \"{key}\"");
        let key = key.to_string();
        let mut convert_args = ConvertParams::default();
        if opts.is_create() {
          if let Err(err) = convert_args.add(PathBuf::from(format!("{key}.srm")), opts) {
            warn!("{err}");
            continue;
          }
        }
        order.insert(key.clone(), values.len());
        values.push((key.clone(), convert_args));
        &mut values.last_mut().unwrap().1
      }
    };

    match convert_args.add(path.clone(), opts) {
      Ok(SavedPath(save_type, Some(old_path))) => {
        info!(
          "{save_type}: replaced {} with {}",
          old_path.display(),
          path.display()
        )
      }
      Ok(SavedPath(save_type, None)) => debug!("{save_type}: added"),
      Err(err) => warn!("{err}"),
    }
  }

  GroupedSaves(values)
}

#[cfg(test)]
mod tests {
  use std::path::Path;

  use crate::{
    grouping::{group_saves, validate_groups, Problem},
    tests::{test_for_none, SaveFlag, SaveFlagExt},
    ConvertMode, ConvertParams, GroupOpts,
  };

  use super::GroupedSaves;

  impl GroupedSaves {
    fn names(&self) -> Vec<&String> {
      self.0.iter().map(|(k, _)| k).collect::<Vec<_>>()
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

    let groups = group_saves(files, GroupOpts::Automatic(false));

    assert_eq!(groups.names(), vec!["A", "B", "C", "B1", "D"]);

    for (key, value) in groups {
      match key.as_str() {
        "A" | "C" => {
          // simple auto-name split
          assert_eq!(
            value.mode,
            Some(ConvertMode::Split(format!("{key}.srm").into()))
          );
          assert!(value.paths.is_empty());
        }
        "B" => {
          // split to named mpk
          assert_eq!(value.mode, Some(ConvertMode::Split("B.srm".into())));
          assert!(!value.paths.is_empty());
          assert_eq!(value.paths.cp[0], Some("B.mpk1".into()));
          test_for_none(&value.paths, SaveFlag::ALL & !SaveFlag::CP1);
        }
        "B1" => {
          // create from B1.eep, no srm given
          assert_eq!(value.mode, None);
          assert!(!value.paths.is_empty());
          assert_eq!(value.paths.eep, Some("B1.eep".into()));
          test_for_none(&value.paths, SaveFlag::ALL & !SaveFlag::EEP);
        }
        "D" => {
          // create from D.fla & folder/D.mpk, to D.srm
          assert_eq!(value.mode, Some(ConvertMode::Create("D.srm".into())));
          assert!(!value.paths.is_empty());
          assert_eq!(value.paths.cp[0], Some("folder/D.mpk".into()));
          assert_eq!(value.paths.fla, Some("D.fla".into()));
          test_for_none(
            &value.paths,
            SaveFlag::ALL & !SaveFlag::CP1 & !SaveFlag::FLA,
          );
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
    let groups = group_saves(files, GroupOpts::Automatic(false));

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "A");
    assert_eq!(value.mode, Some(ConvertMode::Split("folder2/A.srm".into())));
    assert!(value.paths.is_empty());
  }

  #[test]
  fn verify_create_grouping() {
    let files = vec!["Space.mpk".into(), "folder/extracted.sra".into()];

    let groups = group_saves(files, GroupOpts::ForceCreate(false));

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "Space");
    assert_eq!(value.mode, Some(ConvertMode::Create("Space.srm".into())));
    assert!(!value.paths.is_empty());
    assert_eq!(value.paths.cp[0], Some("Space.mpk".into()));
    assert_eq!(value.paths.sra, Some("folder/extracted.sra".into()));
    test_for_none(
      &value.paths,
      SaveFlag::ALL & !SaveFlag::CP1 & !SaveFlag::SRA,
    );
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

    let groups = group_saves(files, GroupOpts::ForceCreate(false));

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "initial");
    assert_eq!(value.mode, Some(ConvertMode::Create("actual.srm".into())));
    assert!(!value.paths.is_empty());
    assert_eq!(value.paths.sra, Some("real.sra".into()));
    assert_eq!(value.paths.cp[0], Some("last.mpk".into()));
    test_for_none(
      &value.paths,
      SaveFlag::ALL & !SaveFlag::CP1 & !SaveFlag::SRA,
    );
  }

  #[test]
  fn verify_split_grouping() {
    let files = vec!["Space.srm".into(), "folder/extracted.sra".into()];

    let groups = group_saves(files, GroupOpts::ForceSplit(false));

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "Space");
    assert_eq!(value.mode, Some(ConvertMode::Split("Space.srm".into())));
    assert!(!value.paths.is_empty());
    assert_eq!(value.paths.sra, Some("folder/extracted.sra".into()));
    test_for_none(&value.paths, SaveFlag::ALL & !SaveFlag::SRA);
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

    let groups = group_saves(files, GroupOpts::ForceSplit(false));

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "this_not_it");
    assert_eq!(value.mode, Some(ConvertMode::Split("actual.srm".into())));
    assert!(!value.paths.is_empty());
    assert_eq!(value.paths.sra, Some("real.sra".into()));
    assert_eq!(value.paths.cp[0], Some("last.mpk".into()));
    test_for_none(
      &value.paths,
      SaveFlag::ALL & !SaveFlag::CP1 & !SaveFlag::SRA,
    );
  }

  #[test]
  fn verify_validator() -> std::io::Result<()> {
    let mut groups = GroupedSaves(vec![("A".into(), ConvertParams::default())]);

    let invalids = validate_groups(&groups);

    assert!(!invalids.is_empty());
    assert_eq!(invalids[0].key, "A");
    assert_eq!(invalids[0].mode, &None);
    assert_eq!(invalids[0].problem, Problem::NoInput);

    groups.0[0]
      .1
      .add("A.srm".into(), GroupOpts::Automatic(false))?;

    let invalids = validate_groups(&groups);
    assert!(!invalids.is_empty());
    assert_eq!(invalids[0].key, "A");
    assert_eq!(invalids[0].mode, &Some(ConvertMode::Split("A.srm".into())));
    assert_eq!(
      invalids[0].problem,
      Problem::FileDoesNotExist(vec![&Path::new("A.srm")])
    );
    std::mem::take(&mut groups.0[0].1);

    groups.0[0]
      .1
      .add("A.mpk".into(), GroupOpts::Automatic(false))?;
    let invalids = validate_groups(&groups);
    assert!(!invalids.is_empty());
    assert_eq!(invalids[0].key, "A");
    assert_eq!(invalids[0].mode, &None);
    assert_eq!(invalids[0].problem, Problem::NoInput);

    Ok(())
  }
}
