use std::fmt::Debug;
use std::fs;
use std::path::PathBuf;
use std::process::ExitCode;

use clap::Parser;

use log::{debug, error, info};

use ramp64_srm_convert_lib::{
  convert, group_saves, validate_groups, BaseArgs, ConvertMode, Grouping, Problem,
};

use simplelog::SimpleLogger;

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
struct MupenSrmConvert {
  /// Sets the output verbosity
  #[arg(short, long, default_value = "normal")]
  verbosity: Verbosity,

  /// Forces the creation of a SRM file from all the given files
  #[arg(short, long)]
  create_srm: bool,

  /// Forces the split of an existing SRM to all the given files
  #[arg(short, long, conflicts_with = "create_srm")]
  split_srm: bool,

  #[command(flatten)]
  base: BaseArgs,

  /// The input file(s)
  #[arg(name = "FILE", num_args = 1.., required = true)]
  files: Vec<PathBuf>,
}

fn main() -> ExitCode {
  // get the arguments
  let mut args = MupenSrmConvert::parse();

  // init our simple logger
  let _ = SimpleLogger::init(
    std::mem::take(&mut args.verbosity).into(),
    simplelog::ConfigBuilder::new().build(),
  );

  if args.split_srm {
    debug!("MODE: Forced SRM split")
  } else if args.create_srm {
    debug!("MODE: Forced SRM creation")
  } else {
    debug!("MODE: Automatic")
  }

  // if there is an output directory, check and create if missing
  if let Some(out_dir) = args.base.output_dir.as_ref() {
    if !out_dir.is_dir() && !out_dir.exists() {
      match fs::create_dir_all(&out_dir) {
        Ok(()) => debug!("Created output directory at: {}", out_dir.display()),
        Err(err) => {
          error!("Could not create output directory: {err}");
          return ExitCode::FAILURE;
        }
      }
    }
  }

  debug!("\n--- Grouping file(s) ---");
  let groups = group_saves(std::mem::take(&mut args.files), {
    if args.create_srm {
      Grouping::force_create()
    } else if args.split_srm {
      Grouping::force_split()
    } else {
      Grouping::automatic()
    }
    .set_merge_controller_pack(args.base.merge_mempacks)
  });

  debug!("\n--- Validating group(s) ---");

  let invalid_groups = validate_groups(&groups);
  if !invalid_groups.is_empty() {
    debug!("Validation failed");
    for entry in invalid_groups {
      let name = entry.group_name();
      match (entry.mode(), entry.problem()) {
        (ConvertMode::Create, problem) => match problem {
          Problem::NoInput => error!("Group \"{name}\": Cannot create SRM: no input files"),
          Problem::FileDoesNotExist(files) => {
            error!("Group \"{name}\": Cannot create SRM: the following file(s) do not exist");
            for f in files {
              error!("  {}", f.display());
            }
          }
          Problem::NotAFile => unreachable!(),
        },
        (ConvertMode::Split, problem) => match problem {
          Problem::NoInput => unreachable!(),
          Problem::FileDoesNotExist(_) => {
            error!(
              "Group \"{name}\": Cannot split SRM: {} does not exist",
              entry.srm_file().display()
            )
          }
          Problem::NotAFile => {
            error!(
              "Group \"{name}\": Cannot split SRM: {} is not a file",
              entry.srm_file().display()
            )
          }
        },
      }
    }

    return ExitCode::FAILURE;
  }
  debug!("All groups validated");

  debug!("\n--- Start Conversion ---");

  let mut exit_code = ExitCode::SUCCESS;

  // Now work per file name
  for (name, params) in groups {
    info!("Group \"{name}\":");

    if let Err(err) = convert(params, &args.base) {
      error!("{err}");
      exit_code = ExitCode::FAILURE
    }
  }

  exit_code
}

#[cfg(test)]
mod tests {
  use crate::MupenSrmConvert;

  #[test]
  fn verify_cli() {
    use clap::CommandFactory;
    MupenSrmConvert::command().debug_assert()
  }
}
