use clap::Parser;

use log::{debug, error, info, warn};

use ramp64_srm_convert_lib::{
  can_be_srm, create, split, to_battery, to_controller_pack, Converter, UserParams,
};

use simplelog::SimpleLogger;

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

#[derive(Copy, Clone, Debug, PartialEq, clap::ValueEnum, Default)]
#[repr(u8)]
enum Verbosity {
  Quiet = 0,
  #[default]
  Normal = 1,
  Debug = 2,
}

impl From<Verbosity> for simplelog::LevelFilter {
  fn from(value: Verbosity) -> Self {
    match value {
      Verbosity::Quiet => simplelog::LevelFilter::Off,
      Verbosity::Normal => simplelog::LevelFilter::Info,
      Verbosity::Debug => simplelog::LevelFilter::Debug,
    }
  }
}

/// A simple converter for Retroarch's Mupen64Plus core save files.
#[derive(Parser, Debug, Default)]
#[command(version)]
struct RaMp64ConvertArgs {
  /// Sets the output verbosity
  #[arg(short, long, default_value = "normal")]
  verbosity: Verbosity,

  /// Forces the creation of a SRM file from all the given files
  #[arg(short, long)]
  create_srm: bool,

  /// Forces the split of an existing SRM to all the given files
  #[arg(short, long, conflicts_with = "create_srm")]
  split_srm: bool,

  /// When splitting a SRM, merge all controller packs in one file
  #[arg(short, long)]
  merge_mempacks: bool,

  /// If set, any existing file will be overwritten
  #[arg(long)]
  overwrite: bool,

  /// Sets the output directory
  #[arg(short, long)]
  output_dir: Option<PathBuf>,

  /// If set, EEP and FlashRAM bytes will be swapped
  #[arg(long)]
  change_endianness: bool,

  /// Use this flag to convert files from directory
  #[arg(short, long)]
  input_dir: Option<std::path::PathBuf>,

  /// The input file(s)
  #[arg(name = "FILE", num_args = 1.., required_unless_present = "input_dir")]
  files: Vec<std::path::PathBuf>,
}

fn get_key<P: AsRef<Path>>(path: &P) -> Option<&str> {
  path.as_ref().file_stem().and_then(|s| s.to_str())
}

fn main() -> ExitCode {
  let mut args = RaMp64ConvertArgs::parse();

  let _ = SimpleLogger::init(
    args.verbosity.into(),
    simplelog::ConfigBuilder::new()
      .set_time_level(log::LevelFilter::Off)
      .set_thread_level(log::LevelFilter::Off)
      .set_target_level(log::LevelFilter::Off)
      .build(),
  );

  // if there is an output directory, check and create if missing
  if let Some(out_dir) = args.output_dir.as_ref() {
    if !out_dir.is_dir() && !out_dir.exists() {
      match std::fs::create_dir_all(out_dir) {
        Ok(()) => debug!("Created output directory at: {}", out_dir.display()),
        Err(err) => {
          error!("Could not create output directory: {err}");
          return ExitCode::FAILURE;
        }
      }
    }
  }

  // build the user params value
  let user_params = UserParams {
    overwrite: args.overwrite,
    swap_bytes: args.change_endianness,
  };

  // scan the directory and append the results to the files vector
  if let Some(path) = args.input_dir.take() {
    match path.read_dir() {
      Ok(read_dir) => {
        for entry in read_dir.flatten() {
          let Ok(file_type) = entry.file_type() else {
            continue;
          };
          if !file_type.is_file() {
            continue;
          }
          let path = entry.path();
          debug!("Input Directory: Found \"{}\"", path.display());
          args.files.push(path);
        }
      }
      Err(err) => {
        error!("Could not read input directory: {}", err);
        return ExitCode::FAILURE;
      }
    }
  }

  if args.create_srm || args.split_srm {
    if args.split_srm {
      debug!("MODE: Forced SRM split");
      forced_split(user_params, args)
    } else {
      debug!("MODE: Forced SRM creation");
      forced_create(user_params, args)
    }
  } else {
    debug!("MODE: Automatic");
    automatic_from_files(user_params, args)
  }
}

fn forced_split(user_params: UserParams, args: RaMp64ConvertArgs) -> ExitCode {
  let RaMp64ConvertArgs {
    merge_mempacks: merge_cp,
    files,
    output_dir,
    ..
  } = args;

  // find first srm-like file, else first file name
  let srm_path = files
    .iter()
    .filter(|p| p.is_file())
    .find(|p| split::can_be_srm(p).is_ok())
    .or_else(|| files.first())
    .map(|f| f.with_extension("srm"));

  if srm_path.is_none() {
    error!("Could not find a SRM file to split");
    return ExitCode::FAILURE;
  }

  let srm_path = srm_path.unwrap();
  let mut params = split::Params::new(srm_path.clone())
    .set_output_mupen_pack(merge_cp)
    .set_out_dir(output_dir)
    .set_name(srm_path.file_name().and_then(|f| f.to_str()));

  for path in files {
    if path == srm_path {
      continue;
    }
    add_to_split_params(path, &mut params);
  }

  process_groups([("Split".into(), params.into())].into(), user_params)
}

fn forced_create(user_params: UserParams, args: RaMp64ConvertArgs) -> ExitCode {
  let RaMp64ConvertArgs {
    files,
    output_dir: out_dir,
    ..
  } = args;

  let mut params = create::Params::default().set_out_dir(out_dir);
  let mut iter_files = files.into_iter();
  if let Some(file) = iter_files.next() {
    params
      .as_mut()
      .set_name(file.file_name().and_then(|f| f.to_str()));
    add_to_create_params(file, &mut params);
  }

  for file in iter_files {
    add_to_create_params(file, &mut params);
  }

  process_groups([("Create".into(), params.into())].into(), user_params)
}

enum AutoParams {
  Split(split::Params),
  Create(create::Params),
}

impl AutoParams {
  fn is_create(&self) -> bool {
    matches!(self, Self::Create(_))
  }
}

impl From<split::Params> for AutoParams {
  fn from(value: split::Params) -> Self {
    Self::Split(value)
  }
}

impl From<create::Params> for AutoParams {
  fn from(value: create::Params) -> Self {
    Self::Create(value)
  }
}

fn automatic_from_files(user_params: UserParams, args: RaMp64ConvertArgs) -> ExitCode {
  let mut params = BTreeMap::<String, AutoParams>::new();

  let RaMp64ConvertArgs {
    merge_mempacks: merge_cp,
    files,
    output_dir,
    ..
  } = args;

  for path in files.into_iter() {
    debug!(r#"Checking file "{}""#, path.display());
    if path.is_dir() {
      warn!(r#"Path "{}" is a directory"#, path.display());
      continue;
    }
    if let Some(key) = get_key(&path) {
      use std::collections::btree_map::Entry::*;
      match params.entry(key.to_string()) {
        Occupied(mut occupied) => {
          debug!("{key}: Modify");
          match occupied.get_mut() {
            AutoParams::Create(create_params) => add_to_create_params(path, create_params),
            AutoParams::Split(split_params) => add_to_split_params(path, split_params),
          }
        }
        Vacant(entry) => match to_controller_pack(path)
          .map(|controller_pack| {
            debug!(r#"Created new group: "{}""#, entry.key());
            debug!("{}: Create SRM from Controller Pack", entry.key());
            AutoParams::Create(
              create::Params::default()
                .set_controller_pack(controller_pack)
                .set_out_dir(output_dir.clone()),
            )
          })
          .or_else(|(path, last_err)| {
            debug!("Not a Controller Pack: {last_err}");
            to_battery(path).map(|battery_file| {
              debug!(r#"Created new group: "{}""#, entry.key());
              debug!("{}: Create SRM from Battery", entry.key());
              AutoParams::Create(
                create::Params::default()
                  .set_battery(battery_file)
                  .set_out_dir(output_dir.clone()),
              )
            })
          })
          .or_else(|(path, last_err)| {
            debug!("Not a Battery: {last_err}");
            can_be_srm(path).map(|path| {
              debug!(r#"Created new group: "{}""#, entry.key());
              if path.exists() {
                debug!("{}: Split SRM", entry.key());
                AutoParams::Split(
                  split::Params::new(path)
                    .set_output_mupen_pack(merge_cp)
                    .set_out_dir(output_dir.clone()),
                )
              } else {
                debug!("{}: Create SRM", entry.key());
                AutoParams::Create(
                  create::Params::default().set_name(path.file_name().and_then(|s| s.to_str())),
                )
              }
            })
          }) {
          Ok(params) => {
            entry.insert(params);
          }
          Err((path, last_err)) => {
            debug!("Not a SRM: {last_err}");
            warn!("Could not create group from \"{}\"", path.display());
          }
        },
      }
    } else {
      warn!("Could not get name from path: {}", path.display());
    }
  }

  process_groups(params, user_params)
}

fn add_to_create_params(path: PathBuf, params: &mut create::Params) {
  to_controller_pack(path)
    .map(|controller_pack| {
      let path_display = format!(r#""{}""#, controller_pack.display());
      let cp_display = format!(r#"{controller_pack} ({path_display})"#);
      debug!("{path_display}: Controller Pack");
      if let Some(old) = params.as_mut().set_controller_pack(controller_pack) {
        warn!(r#"{old} ("{}") replaced with {cp_display}"#, old.display());
      }
    })
    .or_else(|(path, last_err)| {
      debug!("Not a Controller Pack: {last_err}");
      to_battery(path).map(|battery_file| {
        let path_display = format!(r#""{}""#, battery_file.display());
        let new_type = battery_file.battery_type();
        debug!("{path_display}: {new_type} Battery");
        if let Some(old) = params.as_mut().set_battery(battery_file) {
          warn!(
            "Battery {} (\"{}\") replaced with {new_type} ({path_display})",
            old.battery_type(),
            old.display()
          );
        }
      })
    })
    .or_else(|(path, last_err)| {
      debug!("Not a Battery: {last_err}");
      can_be_srm(path).map(|path| {
        if let Some(name) = path.file_name().as_ref().and_then(|e| e.to_str()) {
          debug!("Use {name} for SRM file name");
          params.as_mut().set_name(Some(&name.to_string()));
        }
      })
    })
    .unwrap_or_else(|(path, last_err)| {
      debug!("Not a SRM: {last_err}");
      warn!("Could set \"{}\" to an existing group", path.display());
    })
}

fn add_to_split_params(path: PathBuf, params: &mut split::Params) {
  match split::can_be_srm(path) {
    Ok(path) => params.as_mut().set_srm_file(path),
    Err((path, err)) => {
      debug!("Not a SRM: {err}");
      warn!("Could not set \"{}\" to an existing group", path.display());
    }
  }
}

fn process_groups(mut params: BTreeMap<String, AutoParams>, user_params: UserParams) -> ExitCode {
  if params.is_empty() {
    error!("No groups to process");
    return ExitCode::FAILURE;
  }

  debug!("----- Validating groups -----");

  // validate parameters
  params.retain(|key, param| match param {
    AutoParams::Split(param) => {
      let validation = param.validate();
      if !validation.is_ok() {
        error!("Validating \"{key}\": {validation}");
      }
      validation.into()
    }
    AutoParams::Create(param) => {
      let validation = param.validate();
      if !validation.is_ok() {
        error!("Validating \"{key}\": {validation}");
      }
      validation.into()
    }
  });

  if params.is_empty() {
    return ExitCode::FAILURE;
  }
  debug!("----- Validation OK -----");

  if params.len() == 1 {
    info!("A single group was found:");
  } else {
    info!("{} groups were found:", params.len());
  }
  let create_groups = params.iter().filter(|(_, v)| v.is_create()).count();
  if create_groups > 0 {
    info!("  Will create {create_groups} SRM");
  }
  if create_groups < params.len() {
    info!("  Will split {} SRM", (params.len() - create_groups));
  }

  let mut exit_code = ExitCode::SUCCESS;
  for (key, param) in params {
    if let Err(err) = match param {
      AutoParams::Split(param) => {
        info!("Splitting: \"{key}\"");
        param.convert(&user_params)
      }
      AutoParams::Create(param) => {
        info!("Creating \"{key}\"");
        param.convert(&user_params)
      }
    } {
      use std::io::ErrorKind::*;
      error!(
        "Could not convert group \"{key}\": {}",
        match err.kind() {
          NotFound => format!("\"{}\" not found", err.path().display()),
          PermissionDenied => format!("could not access \"{}\"", err.path().display()),
          AlreadyExists => format!("will overwrite \"{}\"", err.path().display()),
          WriteZero => format!("could not write all data to \"{}\"", err.path().display()),
          UnexpectedEof => format!("could not read file \"{}\"", err.path().display()),
          _ => format!("{err}"),
        }
      );
      exit_code = ExitCode::FAILURE;
    }
  }

  exit_code
}

#[cfg(test)]
mod tests {
  use crate::RaMp64ConvertArgs;

  #[test]
  fn verify_cli() {
    use clap::CommandFactory;
    RaMp64ConvertArgs::command().debug_assert()
  }
}
