use std::ffi::OsStr;
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use crate::logger::Logger;
use crate::retroarch_srm::RetroArchSrm;
use crate::{log, ConvertArgs, SingleMulti};

struct SplitImpl<'a> {
  logger: &'a mut Logger,
  input_path: &'a Path,
  output_dir: &'a Option<PathBuf>,
}

impl<'a> SplitImpl<'a> {
  fn output_file<S: AsRef<OsStr>>(&self, extension: S) -> PathBuf {
    let parent_path = self.output_dir.as_ref().map_or_else(
      || {
        self
          .input_path
          .parent()
          .map_or_else(|| PathBuf::new(), |p| p.to_path_buf())
      },
      |d| d.clone(),
    );
    parent_path
      .with_file_name(self.input_path.file_stem().unwrap())
      .with_extension(extension)
  }
}

macro_rules! write_battery {
  ($a:ident; $oo:expr; ($($n:literal:$b:expr => $p:expr),+)) => {
    $(if !$b.is_empty() {
      let out_path = $p.take().unwrap_or_else(|| $a.output_file($n));
      if let Err(err) = $oo.open(&out_path).and_then(|mut f| f.write_all($b.as_ref())) {
        log!(e: $a.logger, "Could not store '{}' at {:#?}: {err}", $n, out_path);
      }
    })+
  };
}

pub fn split_srm(
  logger: &mut Logger,
  overwrite: bool,
  output_dir: Option<PathBuf>,
  output_mupen_cp: bool,
  input_path: PathBuf,
  mut output: ConvertArgs,
) -> std::io::Result<()> {
  let mut srm = Box::from(RetroArchSrm::new());
  File::open(&input_path)?.read_exact(AsMut::<[u8]>::as_mut(&mut *srm))?;

  let mut open_opts = OpenOptions::new();
  open_opts
    .create(overwrite)
    .create_new(!overwrite)
    .write(true);

  let args = SplitImpl {
    logger,
    input_path: &input_path,
    output_dir: &output_dir,
  };

  write_battery!(args; open_opts; (
    "eep": srm.eeprom => output.eep_path,
    "sra": srm.sram => output.sra_path,
    "fla": srm.flashram => output.fla_path
  ));
  if output_mupen_cp {
    write_battery!(args; open_opts; (
      "mpk": srm.all_controller_packs() => output.mpk_path.take().single()
    ))
  } else if let SingleMulti::Multiple(v) = output.mpk_path.take() {
    let mut iter = v.into_iter();
    write_battery!(args; open_opts;(
      "1.mpk": srm.mempack[0] => iter.next().flatten(),
      "2.mpk": srm.mempack[1] => iter.next().flatten(),
      "3.mpk": srm.mempack[2] => iter.next().flatten(),
      "4.mpk": srm.mempack[3] => iter.next().flatten()
    ));
  }

  Ok(())
}
