use std::fs::{File, OpenOptions};
use std::io::{Read, Write};

use crate::retroarch_srm::RetroArchSrm;
use crate::SrmFile;
use crate::{convert_params::SrmPaths, word_byte_swap, BaseArgs, OutputDir, PathError, Result};

macro_rules! open_write_battery {
  ($path:expr, $ext:expr, $battery:expr, $out_dir:expr, $opts:expr) => {{
    let path = $path.map_or_else(
      || $out_dir.base_with_extension($ext),
      |p| $out_dir.to_out_dir(&p),
    );
    $opts
      .open(&path)
      .and_then(|mut f| f.write_all($battery.as_ref()))
      .map_err(|e| PathError(path, e))
  }};
}

pub(crate) fn split_srm(input_path: SrmFile, args: &BaseArgs, output: SrmPaths) -> Result {
  let mut srm = Box::<RetroArchSrm>::default();
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
      open_write_battery!(output.eep, "eep", srm.eeprom.as_mut()[..512], out_dir, opts)?;
    } else {
      open_write_battery!(output.eep.as_ref(), "eep", srm.eeprom, out_dir, opts)?;
    }
  }

  if !srm.sram.is_empty() {
    open_write_battery!(output.sra.as_ref(), "sra", srm.sram, out_dir, opts)?;
  }

  if !srm.flashram.is_empty() {
    open_write_battery!(output.fla.as_ref(), "fla", srm.flashram, out_dir, opts)?;
  }

  if args.merge_mempacks {
    if srm.controller_pack.iter().any(|cp| !cp.is_empty()) {
      let [path, ..] = output.cp;
      let path = path.map_or_else(
        || out_dir.base_with_extension("mpk"),
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
    for (i, path) in output.cp.into_iter().enumerate() {
      if !srm.controller_pack[i].is_empty() {
        let ext = &format!("mpk{}", i + 1);
        open_write_battery!(path, ext, srm.controller_pack[i], out_dir, opts)?;
      }
    }
  }

  Ok(())
}
