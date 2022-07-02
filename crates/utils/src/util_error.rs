use snafu::{Backtrace, Snafu};

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum UtilError {
    #[snafu(display(
        "IndexOfFailed: Element {} was not found in collection {}.",
        elt_str,
        collection_str
    ))]
    IndexOfFailed {
        elt_str: String,
        collection_str: String,
        backtrace: Backtrace,
    },
    #[snafu(display("KeyNotFound: key {} was not found in HashMap.", key_str,))]
    KeyNotFound {
        key_str: String,
        backtrace: Backtrace,
    },
    #[snafu(display(
        "OutOfBounds: index {} was out of bounds for {} with length {}.",
        index,
        collection_name,
        len
    ))]
    OutOfBounds {
        index: usize,
        collection_name: String,
        len: usize,
        backtrace: Backtrace,
    },
}

pub type UtilResult<T, E = UtilError> = std::result::Result<T, E>;
