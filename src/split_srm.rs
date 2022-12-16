use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::path::PathBuf;

use crate::retroarch_srm::RetroArchSrm;
use crate::{change_endianness, ldbgln, lerrln, BaseArgs, SrmPaths};

macro_rules! write_battery {
  ($battery:expr, $file:expr) => {{
    if !$battery.is_empty() {
      if let Err(err) = $file.as_mut().map(|f| f.write_all($battery.as_ref())) {
        lerrln!("Could not store save: {err}");
      }
    }
  }};
}

pub(crate) fn split_srm(
  input_path: PathBuf,
  args: &BaseArgs,
  output: SrmPaths,
) -> std::io::Result<()> {
  let input_path = std::fs::canonicalize(input_path).expect("SRM file should exist");
  let mut srm = Box::<RetroArchSrm>::default();
  File::open(&input_path)?.read_exact(srm.as_mut().as_mut())?;

  // prepare out path
  let mut new_file_base = args
    .output_dir
    .as_ref()
    .map(|p| p.as_path())
    .unwrap_or_else(|| input_path.parent().unwrap())
    .to_path_buf();
  new_file_base.push(input_path.file_stem().unwrap());
  ldbgln!("Base name for new output files is {:?}", new_file_base);

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
    Some(path) => opts.open(path),
    None => {
      let path = new_file_base.with_extension(ext);
      ldbgln!("Splitting to a new file {path:#?}");
      opts.open(path)
    }
  };

  write_battery!(srm.eeprom, open(output.eep.as_ref(), "eep"));
  write_battery!(srm.sram, open(output.sra.as_ref(), "sra"));
  write_battery!(srm.flashram, open(output.fla.as_ref(), "fla"));

  if args.merge_mempacks {
    let cp_path = output.cp.iter().find(|&p| Option::is_some(p)).unwrap();
    let mut cp_file = open(cp_path.as_ref(), "mpk");
    for i in 0..4 {
      write_battery!(srm.controller_pack[i], cp_file);
    }
  } else {
    for (i, path) in output.cp.iter().enumerate() {
      write_battery!(
        srm.controller_pack[i],
        open(path.as_ref(), &format!("mpk{}", i + 1))
      );
    }
  }

  Ok(())
}
