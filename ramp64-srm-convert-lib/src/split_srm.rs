use std::fs::{File, OpenOptions};
use std::io::{Read, Write};

use crate::retroarch_srm::RetroArchSrm;
use crate::save_file::By;
use crate::{convert_params::SrmPaths, word_byte_swap, BaseArgs, OutputDir, PathError, Result};
use crate::{ControllerPackSlot, SaveType, SrmFile};

macro_rules! open_write_battery {
  ($out:expr, $type:expr, $battery:expr, $out_dir:expr, $opts:expr) => {{
    let path = $out.get($type).as_ref().map_or_else(
      || $out_dir.base_with_extension($type.extension()),
      |p| $out_dir.to_out_dir(&p),
    );
    $opts
      .open(&path)
      .and_then(|mut f| f.write_all($battery.as_ref()))
      .map_err(|e| PathError(path, e))
  }};
}

pub(crate) fn split_srm(input_path: SrmFile, args: &BaseArgs, output: SrmPaths) -> Result {
  use crate::SaveType::*;

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

  if !srm.eeprom.is_empty() {
    // we need to figure out if this eep is 4k or 16k..
    if srm.eeprom.as_ref()[512..].iter().all(|b| b == &0xff) {
      open_write_battery!(output, Eeprom, srm.eeprom.as_mut()[..512], out_dir, opts)?;
    } else {
      open_write_battery!(output, Eeprom, srm.eeprom, out_dir, opts)?;
    }
  }

  if !srm.sram.is_empty() {
    open_write_battery!(output, Sram, srm.sram, out_dir, opts)?;
  }

  if !srm.flashram.is_empty() {
    open_write_battery!(output, FlashRam, srm.flashram, out_dir, opts)?;
  }

  if args.merge_mempacks {
    if srm.controller_pack.iter().any(|cp| !cp.is_empty()) {
      use ControllerPackSlot::Mupen;
      let path = output.get(Mupen).as_ref().map_or_else(
        || out_dir.base_with_extension(SaveType::from(Mupen).extension()),
        |p| out_dir.to_out_dir(&p),
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
  } else {
    use By::*;
    for (i, cp) in [Player1, Player2, Player3, Player4].into_iter().enumerate() {
      if !srm.controller_pack[i].is_empty() {
        open_write_battery!(
          output,
          SaveType::from(cp),
          srm.controller_pack[i],
          out_dir,
          opts
        )?;
      }
    }
  }

  Ok(())
}
