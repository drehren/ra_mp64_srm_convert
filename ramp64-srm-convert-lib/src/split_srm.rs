use std::{
  fs::{File, OpenOptions},
  io::{Read, Write},
};

use crate::{
  convert_params::SrmPaths, retroarch_srm::RetroArchSrm, word_byte_swap, BaseArgs, BatteryFile,
  OutputDir, PathError, Result, SrmFile,
};

pub(crate) fn split_srm(input_path: SrmFile, args: &BaseArgs, output: SrmPaths) -> Result {
  use crate::BatteryType::*;

  let mut srm = Box::new(RetroArchSrm::new());
  File::open(&input_path)
    .and_then(|mut f| f.read_exact(srm.as_mut().as_mut()))
    .map_err(|e| PathError(input_path.clone().into(), e))?;

  if args.change_endianness {
    word_byte_swap(srm.sram.as_mut());
    word_byte_swap(srm.flashram.as_mut());
  }

  let out_dir = OutputDir::new(&args.output_dir, &input_path);
  let mut opts = OpenOptions::new();
  opts
    .create(args.overwrite)
    .create_new(!args.overwrite)
    .write(true);

  if let Some(battery) = output.battery().cloned().or_else(|| {
    if !srm.eeprom.is_empty() {
      BatteryFile::try_from(out_dir.base_with_extension(Eeprom.extension())).ok()
    } else if !srm.sram.is_empty() {
      BatteryFile::try_from(out_dir.base_with_extension(Sram.extension())).ok()
    } else if !srm.flashram.is_empty() {
      BatteryFile::try_from(out_dir.base_with_extension(FlashRam.extension())).ok()
    } else {
      None
    }
  }) {
    let data = match battery.battery_type() {
      Eeprom => {
        // we need to figure out if this eep is 4k or 16k..
        if srm.eeprom.as_ref()[512..].iter().all(|b| b == &0xff) {
          &srm.eeprom.as_ref()[..512]
        } else {
          srm.eeprom.as_ref()
        }
      }
      Sram => srm.sram.as_ref(),
      FlashRam => srm.flashram.as_ref(),
    };

    let path = out_dir.to_out_dir(&battery);
    opts
      .open(&path)
      .and_then(|mut file| file.write_all(data))
      .map_err(|err| PathError(path, err))?;
  }

  if args.merge_mempacks {
    if srm.controller_pack.iter().any(|cp| !cp.is_empty()) {
      let path = output.mupen_controller_pack().map_or_else(
        || out_dir.base_with_extension("mpk"),
        |p| out_dir.to_out_dir(p),
      );
      opts
        .open(&path)
        .and_then(|mut f| {
          for cp in &srm.controller_pack {
            f.write_all(cp.as_ref())?
          }
          Ok(())
        })
        .map_err(|e| PathError(path, e))?;
    }
  } else if let Some(cps) = output.player_controller_packs() {
    for (i, cp) in cps.into_iter().enumerate() {
      if !srm.controller_pack[i].is_empty() {
        let path = cp.as_ref().map_or_else(
          || out_dir.base_with_extension(&format!("mpk{}", i + 1)),
          |p| out_dir.to_out_dir(p),
        );
        opts
          .open(&path)
          .and_then(|mut f| f.write_all(srm.controller_pack[i].as_ref()))
          .map_err(|e| PathError(path, e))?;
      }
    }
  }

  Ok(())
}
