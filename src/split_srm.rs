use std::fs::{File, OpenOptions};
use std::io::{self, Read, Write};
use std::path::PathBuf;

use crate::retroarch_srm::RetroArchSrm;
use crate::{change_endianness, ldbgln, lerrln, BaseArgs, OutputDir, SrmPaths};

macro_rules! write_battery {
  ($battery:expr, $file:expr) => {{
    if let Err(err) = $file.as_mut().map(|f| f.write_all($battery.as_ref())) {
      lerrln!("Could not store save: {err}");
    }
  }};
}

pub(crate) fn split_srm(input_path: PathBuf, args: &BaseArgs, output: SrmPaths) -> io::Result<()> {
  let input_path = std::fs::canonicalize(input_path).expect("SRM file should exist");
  let mut srm = Box::<RetroArchSrm>::default();
  File::open(&input_path)?.read_exact(srm.as_mut().as_mut())?;

  let out_dir = OutputDir::new(&args.output_dir, &input_path);
  if args.change_endianness {
    change_endianness(srm.sram.as_mut());
    change_endianness(srm.flashram.as_mut());
  }

  let mut opts = OpenOptions::new();
  opts
    .create(args.overwrite)
    .create_new(!args.overwrite)
    .write(true);

  let open = |path, ext: &str| match path {
    Some(path) => opts.open(out_dir.output_path(path)),
    None => {
      let path = out_dir.from_base(ext);
      ldbgln!("Splitting to a new file {path:#?}");
      opts.open(path)
    }
  };

  if !srm.eeprom.is_empty() {
    // we need to figure out if this eep is 4k or 16k..
    let is_16k = srm.eeprom.as_ref()[512..].iter().any(|b| b != &0);
    if !is_16k {
      write_battery!(srm.eeprom.as_ref()[..512], open(output.eep.as_ref(), "eep"));
    } else {
      write_battery!(srm.eeprom, open(output.eep.as_ref(), "eep"));
    }
  }

  if !srm.sram.is_empty() {
    write_battery!(srm.sram, open(output.sra.as_ref(), "sra"));
  }

  if !srm.flashram.is_empty() {
    write_battery!(srm.flashram, open(output.fla.as_ref(), "fla"));
  }

  if args.merge_mempacks {
    if srm.controller_pack.iter().any(|cp| !cp.is_empty()) {
      let cp_path = output.cp.iter().find(|&p| Option::is_some(p)).unwrap();
      let mut cp_file = open(cp_path.as_ref(), "mpk");
      let data = srm
        .controller_pack
        .iter()
        .map(|cp| cp.as_ref())
        .collect::<Vec<_>>()
        .concat();
      write_battery!(data, cp_file);
    }
  } else {
    for (i, path) in output.cp.iter().enumerate() {
      if !srm.controller_pack[i].is_empty() {
        write_battery!(
          srm.controller_pack[i],
          open(path.as_ref(), &format!("mpk{}", i + 1))
        );
      }
    }
  }

  Ok(())
}
