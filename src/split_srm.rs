use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::path::PathBuf;

use crate::retroarch_srm::RetroArchSrm;
use crate::{change_endianness, BaseArgs, OutputDir, PathError, Result, SrmPaths};

macro_rules! open_write_battery {
  ($path:expr, $ext:expr, $battery:expr, $out_dir:expr, $opts:expr) => {{
    let path = $path.map_or_else(|| $out_dir.from_base($ext), |p| $out_dir.output_path(&p));
    $opts
      .open(&path)
      .and_then(|mut f| f.write_all($battery.as_ref()))
      .or_else(|e| Err(PathError(path, e)))
  }};
}

pub(crate) fn split_srm(input_path: PathBuf, args: &BaseArgs, output: SrmPaths) -> Result {
  let mut srm = Box::<RetroArchSrm>::default();
  File::open(&input_path)
    .and_then(|mut f| f.read_exact(srm.as_mut().as_mut()))
    .or_else(|e| Err(PathError(input_path.clone(), e)))?;

  if args.change_endianness {
    change_endianness(srm.sram.as_mut());
    change_endianness(srm.flashram.as_mut());
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
      let [path, _, _, _] = output.cp;
      let path = path.map_or_else(|| out_dir.from_base("mpk"), |p| out_dir.output_path(&p));
      opts
        .open(&path)
        .and_then(|mut f| {
          for cp in &srm.controller_pack {
            f.write_all(cp.as_ref())?
          }
          Ok(())
        })
        .or_else(|e| Err(PathError(path, e)))?;
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
