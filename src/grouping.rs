use std::collections::HashMap;
use std::ffi;
use std::ops::{Deref, DerefMut};
use std::path::PathBuf;

use log::{debug, error, info, warn};

use ramp64_srm_convert_lib::{
  ControllerPackKind, ConvertMode, ConvertParams, OutputDir, Problem, SaveFile, SaveFileInferError,
  SaveType, SrmFile,
};

#[derive(Debug)]
pub(crate) struct InvalidGroup<'g> {
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
  Srm(SrmFile),
  Save(SaveFile),
}

impl ValidFile {
  fn display(&self) -> std::path::Display<'_> {
    match self {
      ValidFile::Srm(f) => f.as_ref().display(),
      ValidFile::Save(f) => f.as_ref().display(),
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
    //paths: SrmPaths,
    paths: Vec<SaveFile>,
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

    let key = if opts.mode == GroupMode::Automatic {
      debug!("Group name is: \"{name}\"");
      name
    } else {
      values
        .first()
        .map(|GroupData { key, .. }| key.clone())
        .unwrap_or(name)
    };

    // determine if this is an SrmFile or an SaveFile
    let valid_file = match SaveFile::try_from(path.clone()) {
      Ok(save_file) => ValidFile::Save(save_file),
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

    match order.get(&key) {
      Some(&index) => {
        // update existing group

        // let data = &mut values[index];
        // let save_type = valid_file.save_type_display();
        // match match valid_file {
        //   ValidFile::Srm(srm) => (
        //     data.file.replace(srm.clone()).map(PathBuf::from),
        //     PathBuf::from(srm),
        //   ),
        //   ValidFile::Other(save) => (
        //     {
        //       if opts.merge_cp && save_file.is_controller_pack() {
        //         let Ok(save_file_change) = save_file.try_change_type(ControllerPackKind::Mupen) else {
        //           warn!("Could not set controller pack as a Mupen file");
        //           continue;
        //         };
        //         save_file = save_file_change;
        //       }
        //       data.paths.set(save.clone()).map(PathBuf::from)
        //     },
        //     PathBuf::from(save),
        //   ),
        // } {
        //   (Some(old), new) => {
        //     info!(
        //       "{save_type}: replaced {} with {}",
        //       old.display(),
        //       new.display()
        //     )
        //   }
        //   (None, _) => {
        //     debug!("{save_type}: added")
        //   }
        // }
        let file_str = format!("{}", valid_file.display());
        match valid_file {
          ValidFile::Srm(srm) => match values[index].file.replace(srm) {
            Some(old) => warn!("{key} SRM: replaced {} with {file_str}", old.display()),
            None => debug!("{key} SRM: set"),
          },
          ValidFile::Save(save) => values[index].paths.push(save),
        }
      }
      None => {
        info!("Created new group: \"{key}\"");
        let mode = opts.mode.get_convert_mode(&valid_file);
        let mut paths = Vec::with_capacity(5);
        let file = match valid_file {
          ValidFile::Srm(srm_file) => Some(srm_file),
          ValidFile::Save(save_file) => {
            paths.push(save_file);
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

      let mut params = ConvertParams::new(
        mode.unwrap_or_else(|| {
          debug!("> Group \"{key}\": Will Create SRM");
          ConvertMode::Create
        }),
        file.unwrap_or_else(|| {
          debug!("> Group \"{key}\": Default SRM");
          key.as_str().into()
        }),
      );

      // get the mode so we know which files should change output
      let mode = *params.mode();

      for mut save_file in paths {
        let save_type = save_file.save_type();
        let save_file_str = format!("{}", save_file.display());

        if mode == ConvertMode::Split {
          use ControllerPackKind::*;
          use SaveType::ControllerPack;

          let out = OutputDir::new(opts.output_dir, params.srm_file().as_ref());
          if opts.merge_cp
            && matches!(
              save_type,
              ControllerPack(Player1 | Player2 | Player3 | Player4)
            )
          {
            let real_path = SaveFile::try_from(out.to_out_dir(&save_file))
              .expect("lib failing to support file dir change");
            save_file = match real_path.try_change_type(Mupen) {
              Ok(real_file) => real_file,
              Err(err) => {
                error!("Could not set controller pack as Mupen: {err}");
                continue;
              }
            }
          }
        } else {
        }
        match params.set_or_replace_file(save_file) {
          Some(old) => warn!(
            "{key} {save_type}: replaced {} with {save_file_str}",
            old.display()
          ),
          None => debug!("{key} {save_type}: set"),
        }
      }

      (key.clone(), params)
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
        ValidFile::Save(_) => None,
      },
    }
  }
}

/// Provides the grouping options
#[derive(Debug, Clone, Copy)]
pub struct Grouping<'out_dir> {
  mode: GroupMode,
  merge_cp: bool,
  output_dir: &'out_dir Option<PathBuf>,
}
impl<'out_dir> Grouping<'out_dir> {
  fn new(mode: GroupMode, output_dir: &'out_dir Option<PathBuf>) -> Self {
    Self {
      mode,
      merge_cp: false,
      output_dir,
    }
  }
  pub(crate) fn automatic(output_dir: &'out_dir Option<PathBuf>) -> Self {
    Self::new(GroupMode::Automatic, output_dir)
  }
  pub(crate) fn force_create(output_dir: &'out_dir Option<PathBuf>) -> Self {
    Self::new(GroupMode::Create, output_dir)
  }
  pub(crate) fn force_split(output_dir: &'out_dir Option<PathBuf>) -> Self {
    Self::new(GroupMode::Split, output_dir)
  }
  pub(crate) fn set_merge_controller_pack(self, merge_cp: bool) -> Self {
    Self { merge_cp, ..self }
  }
}

#[cfg(test)]
mod tests {
  use std::{fs, path::Path};

  use assert_fs::{prelude::PathChild, TempDir};

  use super::{group_saves, validate_groups, Grouping, Problem};

  use ramp64_srm_convert_lib::{
    ControllerPackKind, ConvertMode, ConvertParams, SaveFile, SaveType, SrmFile,
  };

  use super::GroupedSaves;

  use ControllerPackKind::*;
  use SaveType::*;

  impl GroupedSaves {
    fn names(&self) -> Vec<&String> {
      self.0.iter().map(|(k, ..)| k).collect::<Vec<_>>()
    }
  }

  const ALL_TYPES: &[SaveType; 7] = &[
    Eeprom,
    Sram,
    FlashRam,
    ControllerPack(ControllerPackKind::Player1),
    ControllerPack(ControllerPackKind::Player2),
    ControllerPack(ControllerPackKind::Player3),
    ControllerPack(ControllerPackKind::Player4),
  ];

  fn check_all_empty_saves_but(params: &ConvertParams, skip: &[SaveType]) {
    for save_type in ALL_TYPES.iter().filter(|t| !skip.contains(&t)) {
      assert!(matches!(params.save_file(*save_type), None))
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

    let groups = group_saves(files, Grouping::automatic(&None));

    assert_eq!(groups.names(), vec!["A", "B", "C", "B1", "D"]);

    for (key, value) in groups {
      let key = key.as_str();
      match key {
        "A" | "C" => {
          // simple auto-name split
          assert_eq!(value.mode(), &ConvertMode::Split);
          assert_eq!(value.srm_file(), &key.into());
          check_all_empty_saves_but(&value, &[]);
        }
        "B" => {
          // split to named mpk
          assert_eq!(value.mode(), &ConvertMode::Split);
          assert_eq!(value.srm_file(), &key.into());
          assert_eq!(
            value.save_file(ControllerPackKind::Player1),
            &Some("B.mpk1".try_into().expect("File name is ok"))
          );
          check_all_empty_saves_but(&value, &[Player1.into()]);
        }
        "B1" => {
          // create from B1.eep, no srm given
          assert_eq!(value.mode(), &ConvertMode::Create);
          assert_eq!(value.srm_file(), &key.into());
          assert_eq!(
            value.save_file(SaveType::Eeprom),
            &Some("B1.eep".try_into().expect("File name is ok"))
          );
          check_all_empty_saves_but(&value, &[Eeprom]);
        }
        "D" => {
          // create from D.fla & folder/D.mpk, to D.srm
          assert_eq!(value.mode(), &ConvertMode::Create);
          assert_eq!(value.srm_file(), &key.into());
          assert_eq!(
            value.save_file(ControllerPackKind::Player1),
            &Some("folder/D.mpk".try_into().expect("Path is good"))
          );
          assert_eq!(
            value.save_file(SaveType::FlashRam),
            &Some("D.fla".try_into().expect("File name is ok"))
          );
          check_all_empty_saves_but(&value, &[FlashRam, Player1.into()]);
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
    let groups = group_saves(files, Grouping::automatic(&None));

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "A");
    assert_eq!(value.mode(), &ConvertMode::Split);
    assert_eq!(value.srm_file(), &"folder2/A".into());
    check_all_empty_saves_but(value, &[]);
  }

  #[test]
  fn verify_create_grouping() {
    let files = vec!["Space.mpk".into(), "folder/extracted.sra".into()];

    let groups = group_saves(files, Grouping::force_create(&None));

    assert_eq!(groups.0.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "Space");
    assert_eq!(value.mode(), &ConvertMode::Create);
    assert_eq!(value.srm_file(), &"Space".into());
    assert_eq!(
      value.save_file(ControllerPackKind::Player1),
      &Some("Space.mpk".try_into().expect("File name is ok"))
    );
    assert_eq!(
      value.save_file(SaveType::Sram),
      &Some("folder/extracted.sra".try_into().expect("File name is ok"))
    );
    check_all_empty_saves_but(&value, &[Player1.into(), Sram]);
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

    let groups = group_saves(files, Grouping::force_create(&None));

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
      value.save_file(ControllerPackKind::Player1),
      &Some("last.mpk".try_into().expect("File name is ok"))
    );
    check_all_empty_saves_but(&value, &[Player1.into(), Sram]);
  }

  #[test]
  fn verify_split_grouping() {
    let files = vec!["Space.srm".into(), "folder/extracted.sra".into()];

    let groups = group_saves(files, Grouping::force_split(&None));

    assert_eq!(groups.len(), 1);

    let (name, value) = groups.0.first().unwrap();

    assert_eq!(name, "Space");
    assert_eq!(value.mode(), &ConvertMode::Split);
    assert_eq!(value.srm_file(), &"Space".into());
    assert_eq!(
      value.save_file(SaveType::Sram),
      &Some("folder/extracted.sra".try_into().expect("File name is ok"))
    );
    check_all_empty_saves_but(&value, &[Sram]);
  }

  #[test]
  fn verify_split_grouping_mupen() {
    let input = vec!["A.srm".into(), "F.mpk3".into()];

    let group = group_saves(
      input,
      Grouping::force_split(&None).set_merge_controller_pack(true),
    );

    assert_eq!(group.len(), 1);

    let (name, params) = group.first().unwrap();

    assert_eq!(name, "A");
    assert_eq!(params.mode(), &ConvertMode::Split);
    assert_eq!(params.srm_file(), &"A".into());
    assert_eq!(
      params.save_file(ControllerPackKind::Mupen),
      &Some(
        SaveFile::try_from("F.mpk3")
          .expect("File name is ok")
          .try_change_type(ControllerPackKind::Mupen)
          .expect("should change")
      )
    );
    check_all_empty_saves_but(&params, &[Player1.into()]);
  }

  #[test]
  fn verify_split_grouping_mupen_existing() {
    use ControllerPackKind::Mupen;

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

    let group = group_saves(
      input,
      Grouping::force_split(&None).set_merge_controller_pack(true),
    );

    assert_eq!(group.len(), 1);

    let (name, params) = group.first().unwrap();

    assert_eq!(name, "A");
    assert_eq!(params.mode(), &ConvertMode::Split);
    assert_eq!(
      params.srm_file(),
      &SrmFile::try_from(a_path).expect("file is ok")
    );
    assert_eq!(
      params.save_file(Mupen),
      &Some(
        SaveFile::try_from(f_path)
          .expect("File is mpk")
          .try_change_type(Mupen)
          .expect("should change")
      )
    );
    check_all_empty_saves_but(&params, &[Player1.into()]);
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

    let groups = group_saves(files, Grouping::force_split(&None));

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
      value.save_file(ControllerPackKind::Player1),
      &Some("last.mpk".try_into().expect("File name is ok"))
    );
    check_all_empty_saves_but(&value, &[Player1.into(), Sram]);
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
