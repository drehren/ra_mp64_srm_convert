use std::fs::File;
use std::io::{ErrorKind, Read, Write};

use rand_pcg::Pcg64Mcg;

use crate::retroarch_srm::RetroArchSrm;
use crate::{change_endianness, lerrln, linfln, BaseArgs, SrmPaths};

macro_rules! read_battery {
  ($file_opt:expr, $battery:expr) => {{
    match $file_opt.as_mut() {
      Some(Ok(file)) => file.read_exact($battery.as_mut()),
      Some(Err(err)) => Ok(lerrln!("Could not read save: {err}")),
      None => Ok(()),
    }
  }};
}

pub(crate) fn create_srm(mut args: BaseArgs, input: SrmPaths) -> std::io::Result<()> {
  // check the output path
  let output_path = match args.output_dir.take() {
    Some(path) => path,
    None => {
      return Err(std::io::Error::new(
        ErrorKind::Other,
        "Output directory required to create an srm",
      ))
    }
  };

  // here we should get the files to put into the srm
  let mut srm = Box::new(RetroArchSrm::new_init(Pcg64Mcg::new(rand::random())));

  // If the srm file exists, read it first to update
  if output_path.is_file() {
    File::open(&output_path)?.read_exact(srm.as_mut().as_mut())?;
    linfln!("Loaded existing SRM file {output_path:#?}");
  }

  read_battery!(input.eep.as_ref().map(File::open), srm.eeprom)?;
  read_battery!(input.sra.as_ref().map(File::open), srm.sram)?;
  read_battery!(input.fla.as_ref().map(File::open), srm.flashram)?;

  if args.change_endianness {
    change_endianness(srm.sram.as_mut());
    change_endianness(srm.flashram.as_mut());
  }

  if args.merge_mempacks {
    let cp_path = input.cp.into_iter().find(Option::is_some).flatten();
    let mut cp_file = cp_path.map(File::open);
    for i in 0..4 {
      read_battery!(cp_file, srm.controller_pack[i])?;
    }
  } else {
    for (i, cp_path) in input.cp.into_iter().enumerate() {
      read_battery!(cp_path.map(File::open), srm.controller_pack[i])?;
    }
  }

  File::options()
    .create(args.overwrite)
    .create_new(!args.overwrite)
    .write(true)
    .open(&output_path)?
    .write_all(srm.as_ref().as_ref())
}
