use std::fmt::{Arguments, Display};
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicUsize, Ordering};

use crate::Verbosity;

static VERBOSITY: AtomicU8 = AtomicU8::new(0);
pub(crate) fn get_verbosity() -> Verbosity {
  let v = VERBOSITY.load(Ordering::Acquire);
  if v == (Verbosity::Normal as u8) {
    Verbosity::Normal
  } else if v == (Verbosity::Debug as u8) {
    Verbosity::Debug
  } else {
    Verbosity::Quiet
  }
}
pub(crate) fn set_verbosity(v: Verbosity) {
  VERBOSITY.store(v as u8, Ordering::Release)
}

static PADDING: AtomicUsize = AtomicUsize::new(0);
pub fn add_padding(size: usize) {
  PADDING.store(size, Ordering::Release);
}
pub fn sub_padding(size: usize) {
  PADDING.fetch_sub(size, Ordering::AcqRel);
}

static CAN_PAD: AtomicBool = AtomicBool::new(true);

pub enum OutKind {
  Info,
  Debug,
}
impl Display for OutKind {
  fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    Ok(())
  }
}

pub enum ErrKind {
  Err,
  Warn,
}
impl Display for ErrKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ErrKind::Err => f.write_str("Error: "),
      ErrKind::Warn => f.write_str("Warning: "),
    }
  }
}

pub enum Kind {
  Out(OutKind),
  Err(ErrKind),
}

fn resolve(v: Verbosity, kind: Kind) -> Option<Kind> {
  match v {
    Verbosity::Quiet => None,
    Verbosity::Normal => match kind {
      Kind::Out(OutKind::Debug) | Kind::Err(ErrKind::Warn) => None,
      _ => Some(kind),
    },
    Verbosity::Debug => Some(kind),
  }
}

struct Padding;
impl Display for Padding {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match PADDING.load(Ordering::Acquire) {
      0 => Ok(()),
      n if CAN_PAD.swap(false, Ordering::AcqRel) => f.write_fmt(format_args!("{0:n$}", " ")),
      _ => Ok(()),
    }
  }
}

pub(crate) fn _log<F>(v: Verbosity, kind: Kind, fmt: F) -> F::Return
where
  F: Returner,
{
  let p = Padding;
  match resolve(v, kind) {
    Some(Kind::Out(k)) => {
      print!("{k}{p}{fmt}");
      fmt.displayed()
    }
    Some(Kind::Err(k)) => {
      eprint!("{k}{p}{fmt}");
      fmt.displayed()
    }
    None => fmt.not_displayed(),
  }
}

pub(crate) fn _logln<F>(v: Verbosity, kind: Kind, fmt: F) -> F::Return
where
  F: Returner,
{
  let p = Padding;
  let result = match resolve(v, kind) {
    Some(Kind::Out(k)) => {
      println!("{k}{p}{fmt}");
      fmt.displayed()
    }
    Some(Kind::Err(k)) => {
      eprintln!("{k}{p}{fmt}");
      fmt.displayed()
    }
    None => fmt.not_displayed(),
  };

  CAN_PAD.swap(true, Ordering::AcqRel);
  result
}

pub trait Returner
where
  Self: Display,
{
  type Return: Default;

  fn displayed(&self) -> Self::Return {
    Self::Return::default()
  }
  fn not_displayed(&self) -> Self::Return {
    Self::Return::default()
  }
}

impl<'a> Returner for Arguments<'a> {
  type Return = ();
}

pub struct Padded<'a>(pub usize, pub Arguments<'a>);
impl<'a> Display for Padded<'a> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(self.1)
  }
}

impl<'a> Returner for Padded<'a> {
  type Return = PaddingHolder;
  fn displayed(&self) -> Self::Return {
    add_padding(self.0);
    PaddingHolder(self.0)
  }
}

#[derive(Default)]
pub struct PaddingHolder(usize);
impl Drop for PaddingHolder {
  fn drop(&mut self) {
    sub_padding(self.0)
  }
}

impl PartialEq<usize> for PaddingHolder {
  fn eq(&self, other: &usize) -> bool {
    &self.0 == other
  }
}

#[macro_export]
macro_rules! _get_padding {
  (>$n:literal$($fmt:tt)*) => {
    $crate::logger::Padded($n, format_args!($($fmt)*))
  };
  ($($fmt:tt)+) => {
    format_args!($($fmt)+)
  };
  () => {
    format_args!("")
  };
}

#[macro_export]
macro_rules! linf {
  ($($fmt:tt)*) => {
    $crate::logger::_log(
      $crate::logger::get_verbosity(),
      $crate::logger::Kind::Out($crate::logger::OutKind::Info),
      $crate::_get_padding!($($fmt)*)
    )
  };
}
#[macro_export]
macro_rules! linfln {
  ($($fmt:tt)*) => {
    $crate::logger::_logln(
      $crate::logger::get_verbosity(),
      $crate::logger::Kind::Out($crate::logger::OutKind::Info),
      $crate::_get_padding!($($fmt)*)
    )
  };
}
#[macro_export]
macro_rules! ldbg {
  ($($fmt:tt)*) => {
    $crate::logger::_log(
      $crate::logger::get_verbosity(),
      $crate::logger::Kind::Out($crate::logger::OutKind::Debug),
      $crate::_get_padding!($($fmt)*)
    )
  };
}
#[macro_export]
macro_rules! ldbgln {
  ($($fmt:tt)*) => {
    $crate::logger::_logln(
      $crate::logger::get_verbosity(),
      $crate::logger::Kind::Out($crate::logger::OutKind::Debug),
      $crate::_get_padding!($($fmt)*)
    )
  };
}

#[macro_export]
macro_rules! lwarn {
  ($($fmt:tt)*) => {
    $crate::logger::_log(
      $crate::logger::get_verbosity(),
      $crate::logger::Kind::Err($crate::logger::ErrKind::Warn),
      $crate::_get_padding!($($fmt)*)
    )
  };
}
#[macro_export]
macro_rules! lwarnln {
  ($($fmt:tt)*) => {
    $crate::logger::_logln(
      $crate::logger::get_verbosity(),
      $crate::logger::Kind::Err($crate::logger::ErrKind::Warn),
      $crate::_get_padding!($($fmt)*)
    )
  };
}
#[macro_export]
macro_rules! lerr {
  ($($fmt:tt)*) => {
    $crate::logger::_log(
      $crate::logger::get_verbosity(),
      $crate::logger::Kind::Err($crate::logger::ErrKind::Err),
      $crate::_get_padding!($($fmt)*)
    )
  };
}
#[macro_export]
macro_rules! lerrln {
  ($($fmt:tt)*) => {
    $crate::logger::_logln(
      $crate::logger::get_verbosity(),
      $crate::logger::Kind::Err($crate::logger::ErrKind::Err),
      $crate::_get_padding!($($fmt)*)
    )
  };
}
