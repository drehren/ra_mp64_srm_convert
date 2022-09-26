use std::fmt;
use std::io::Write;

pub struct LoggerBuilder(u8);
impl LoggerBuilder {
  pub fn debug() -> Self {
    Self(0)
  }
  pub fn error() -> Self {
    Self(1)
  }
  pub fn info() -> Self {
    Self(2)
  }
  pub fn create<W: Write + 'static>(&self, output: W) -> Logger {
    Logger {
      kind: self.0,
      output: Box::new(output),
    }
  }
}

pub struct Logger {
  kind: u8,
  output: Box<dyn Write>,
}

impl Logger {
  pub fn log<'b>(&mut self, msg: LogKind<'b>) {
    let _ = match msg {
      LogKind::Debug(args) if self.kind == 0 => writeln!(&mut self.output, "[DEBUG] {args}"),
      LogKind::Error(args) if self.kind <= 1 => writeln!(&mut self.output, "[ERROR] {args}"),
      LogKind::Info(args) if self.kind <= 2 => writeln!(&mut self.output, "{args}"),
      _ => Ok(()),
    };
  }
}

pub enum LogKind<'a> {
  Debug(fmt::Arguments<'a>),
  Error(fmt::Arguments<'a>),
  Info(fmt::Arguments<'a>),
}

#[macro_export]
macro_rules! log {
  (d: $l:expr,$($arg:tt)*) => {{
    $l.log($crate::logger::LogKind::Debug(format_args!($($arg)*)));
  }};

  (e: $l:expr,$($arg:tt)*) => {{
    $l.log($crate::logger::LogKind::Error(format_args!($($arg)*)));
  }};

  ($l:expr,$($arg:tt)*) => {{
    $l.log($crate::logger::LogKind::Info(format_args!($($arg)*)));
  }};
}
