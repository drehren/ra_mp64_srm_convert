use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;

use crate::retroarch_srm::RetroArchSrm;
use crate::{change_endianness, BaseArgs, OutputDir, PathError, Result, SrmPaths};

macro_rules! read_battery {
  ($path_opt:expr, $battery:expr) => {{
    $path_opt.map_or(Ok(()), |path| {
      File::open(&path)
        .and_then(|mut f| f.read_exact($battery.as_mut()))
        .or_else(|e| Err(PathError(path, e)))
    })
  }};
}

pub(crate) fn create_srm(output_path: PathBuf, args: &BaseArgs, input: SrmPaths) -> Result {
  let mut srm = Box::new(RetroArchSrm::new_init());

  // If the srm file exists, read it first to update
  if output_path.is_file() {
    File::open(&output_path)
      .and_then(|mut f| f.read_exact(srm.as_mut().as_mut()))
      .or_else(|e| Err(PathError(output_path.clone(), e)))?;
  }

  if let Some(path) = input.eep {
    let len = path.metadata().unwrap().len() as usize;
    read_battery!(Some(path), srm.eeprom.as_mut()[..len])?;
  }
  read_battery!(input.sra, srm.sram)?;
  read_battery!(input.fla, srm.flashram)?;

  if args.change_endianness {
    change_endianness(srm.sram.as_mut());
    change_endianness(srm.flashram.as_mut());
  }

  if args.merge_mempacks {
    if let [Some(cp_path), _, _, _] = input.cp {
      File::open(&cp_path)
        .and_then(|mut f| {
          for i in 0..4 {
            f.read_exact(srm.controller_pack[i].as_mut())?
          }
          Ok(())
        })
        .or_else(|e| Err(PathError(cp_path, e)))?;
    }
  } else {
    for (i, cp_path) in input.cp.into_iter().enumerate() {
      read_battery!(cp_path, srm.controller_pack[i])?;
    }
  }

  let out_dir = OutputDir::new(&args.output_dir, &output_path);
  let out_path = out_dir.from_base("srm").to_path_buf();
  File::options()
    .create(args.overwrite)
    .create_new(!args.overwrite)
    .write(true)
    .open(&out_path)
    .and_then(|mut f| f.write_all(srm.as_ref().as_ref()))
    .or_else(|e| Err(PathError(out_path, e)))
}
