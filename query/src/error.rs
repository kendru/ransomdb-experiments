use std::error::Error as StdError;
use std::fmt;
use std::result::Result as StdResult;
// use std::io::{Error as IOError, ErrorKind as IOErrorKind};

pub type Result<T> = StdResult<T, Error>;

#[derive(Debug)]
pub enum Error {
    LexError {
        line: usize,
        col: usize,
        context: String,
        msg: String,
        // TODO Add current token, expected next chars, etc.
    },
    ParseError {
        line: usize,
        col: usize,
        context: String,
        msg: String,
    },
    AnalysisError {
        name: String,
        object_type: &'static str,
        msg: String,
    },
    InvalidCoercion,
    GenericError(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::LexError {
                line,
                col,
                ref context,
                ref msg,
            } => f.write_fmt(format_args!(
                "Lex error ({}:{}): {}\n{}",
                line,
                col,
                msg,
                line_context(col, context)
            )),
            Error::ParseError {
                line,
                col,
                ref context,
                ref msg,
            } => f.write_fmt(format_args!(
                "Parse error ({}:{}): {}\n{}",
                line,
                col,
                msg,
                line_context(col, context)
            )),
            Error::AnalysisError {
                ref name,
                ref object_type,
                ref msg,
            } => f.write_fmt(format_args!(
                "Binding error ({} = {}): {}",
                object_type, name, msg,
            )),
            Error::InvalidCoercion => f.write_str("Invalid coercion"),
            Error::GenericError(ref s) => f.write_str(s),
        }
    }
}

impl StdError for Error {
    fn description(&self) -> &str {
        match *self {
            Error::LexError { .. } => "Lex error",
            Error::ParseError { .. } => "Parse error",
            Error::AnalysisError { .. } => "Binding error",
            Error::InvalidCoercion => "Invalid Coercion",
            Error::GenericError(_) => "Generic error",
        }
    }
}

// impl From<IOError> for Error {
//     fn from(e: IOError) -> Self {
//         match e.kind() {
//             IOErrorKind::NotFound => Error::FileNotFound,
//             _ => Error::GenericError(format!("Unknown IO error: {}", e)),
//         }
//     }
// }

impl From<&'static str> for Error {
    fn from(e: &str) -> Self {
        Error::GenericError(e.to_string())
    }
}

fn line_context(col: usize, line: &str) -> String {
    let mut marker = " ".repeat(col);
    marker.push('^');

    format!("{}\n{}\n", line, marker)
}
