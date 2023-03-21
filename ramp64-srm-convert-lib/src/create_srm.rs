use std::{
  fs::File,
  io::{Read, Write},
};

use crate::{
  convert_params::SrmPaths, retroarch_srm::RetroArchSrm, word_byte_swap, BaseArgs, BatteryType,
  OutputDir, PathError, ReadExt, Result, SrmFile,
};

pub(crate) fn create_srm(output_path: SrmFile, args: &BaseArgs, input: SrmPaths) -> Result {
  let mut srm = Box::new(RetroArchSrm::new());

  // If the srm file exists, read it first to update
  if output_path.is_file() {
    File::open(&output_path)
      .and_then(|mut f| f.read_exact(srm.as_mut().as_mut()))
      .map_err(|e| PathError(output_path.clone().into(), e))?;
  }

  if let Some(battery) = input.battery() {
    let srm_battery = match battery.battery_type() {
      BatteryType::Eeprom => srm.eeprom.as_mut(),
      BatteryType::Sram => srm.sram.as_mut(),
      BatteryType::FlashRam => srm.flashram.as_mut(),
    };
    File::open(battery)
      .and_then(|mut file| file.read_up_to(srm_battery))
      .map_err(|err| PathError(battery.clone().into(), err))?;
  }

  if args.change_endianness {
    word_byte_swap(srm.sram.as_mut());
    word_byte_swap(srm.flashram.as_mut());
  }

  if args.merge_mempacks {
    if let Some(cp_path) = input.mupen_controller_pack() {
      File::open(cp_path)
        .and_then(|mut f| {
          for i in 0..4 {
            f.read_exact(srm.controller_pack[i].as_mut())?
          }
          Ok(())
        })
        .map_err(|e| PathError(cp_path.to_path_buf(), e))?;
    }
  } else if let Some(cps) = input.player_controller_packs() {
    for (i, cp) in cps.into_iter().enumerate() {
      if let Some(path) = cp {
        File::open(path)
          .and_then(|mut file| file.read_up_to(srm.controller_pack[i].as_mut()))
          .map_err(|err| PathError(path.to_path_buf(), err))?;
      }
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
