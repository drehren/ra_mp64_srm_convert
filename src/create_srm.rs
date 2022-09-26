use std::fs::OpenOptions;
use std::io::{Read, Write};
use std::path::PathBuf;

use crate::logger::Logger;
use crate::retroarch_srm::RetroArchSrm;
use crate::{log, ConvertArgs, SingleMulti};

macro_rules! read_battery {
  ($l:expr; $opts:expr; ($($f:expr => $t:expr),+)) => {
    $(if let Some(input) = $f.as_ref() {
      if let Err(err) = $opts.open(&input).and_then(|mut f| f.read_exact($t.as_mut()))
      {
        log!(e: $l, "While loading save from {:#?}: {err}", input);
      }
    })+
  };
}

pub fn create_srm(
  logger: &mut Logger,
  overwrite: bool,
  input: ConvertArgs,
  output_path: PathBuf,
) -> std::io::Result<()> {
  // here we should get the files to put into the srm
  let mut srm = Box::new(RetroArchSrm::new());
  srm.init();

  let mut load_opts = OpenOptions::new();
  load_opts.read(true);

  // setup now the save options in case the srm file exists, which we should update
  let mut save_opts = OpenOptions::new();
  save_opts
    .create(overwrite)
    .create_new(!overwrite)
    .write(true);

  // If the srm file exists, its update mode!
  if output_path.is_file() {
    load_opts
      .open(&output_path)?
      .read_exact(srm.as_mut().as_mut())?;
    save_opts.create(true).create_new(false);
    log!(logger, "Loaded existing SRM file {:#?}", output_path);
  }

  let input = &input;
  read_battery!(logger; load_opts; (
    input.eep_path => srm.eeprom,
    input.sra_path => srm.sram,
    input.fla_path => srm.flashram
  ));

  match &input.mpk_path {
    SingleMulti::Empty => {}
    SingleMulti::Single(mpk_path) => {
      read_battery!(logger; load_opts; (Some(mpk_path) => srm.all_controller_packs_mut()));
    }
    SingleMulti::Multiple(mpk_paths) => {
      for (i, mpk_path) in mpk_paths.into_iter().enumerate() {
        read_battery!(logger; load_opts; (mpk_path => srm.mempack[i]));
      }
    }
  }

  save_opts
    .open(&output_path)?
    .write_all(srm.as_ref().as_ref())
}
