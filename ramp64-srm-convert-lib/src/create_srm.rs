use std::fs::File;
use std::io::{Read, Write};

use crate::retroarch_srm::RetroArchSrm;
use crate::SrmFile;
use crate::{convert_params::SrmPaths, word_byte_swap, BaseArgs, OutputDir, PathError, Result};

macro_rules! read_battery {
  ($path_opt:expr, $battery:expr) => {{
    $path_opt.map_or(Ok(()), |path| {
      File::open(&path)
        .and_then(|mut f| f.read_exact($battery.as_mut()))
        .map_err(|e| PathError(path.into(), e))
    })
  }};
}

pub(crate) fn create_srm(output_path: SrmFile, args: &BaseArgs, input: SrmPaths) -> Result {
  let mut srm = Box::new(RetroArchSrm::new_init());

  // If the srm file exists, read it first to update
  if output_path.is_file() {
    File::open(&output_path)
      .and_then(|mut f| f.read_exact(srm.as_mut().as_mut()))
      .map_err(|e| PathError(output_path.clone().into(), e))?;
  }

  if let Some(path) = input.eep {
    let len = path.metadata().unwrap().len() as usize;
    read_battery!(Some(path), srm.eeprom.as_mut()[..len])?;
  }
  read_battery!(input.sra, srm.sram)?;
  read_battery!(input.fla, srm.flashram)?;

  if args.change_endianness {
    word_byte_swap(srm.sram.as_mut());
    word_byte_swap(srm.flashram.as_mut());
  }

  if args.merge_mempacks {
    if let [Some(cp_path), ..] = input.cp {
      File::open(&cp_path)
        .and_then(|mut f| {
          for i in 0..4 {
            f.read_exact(srm.controller_pack[i].as_mut())?
          }
          Ok(())
        })
        .map_err(|e| PathError(cp_path.into(), e))?;
    }
  } else {
    for (i, cp_path) in input.cp.into_iter().enumerate() {
      read_battery!(cp_path, srm.controller_pack[i])?;
    }
  }

  let out_dir = OutputDir::new(&args.output_dir, &output_path);
  let out_path = out_dir.base_with_extension("srm");
  File::options()
    .create(args.overwrite)
    .create_new(!args.overwrite)
    .write(true)
    .open(&out_path)
    .and_then(|mut f| f.write_all(srm.as_ref().as_ref()))
    .map_err(|e| PathError(out_path, e))
}
