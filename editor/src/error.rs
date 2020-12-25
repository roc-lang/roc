use std::{error::Error, fmt};

#[derive(Debug)]
pub struct OutOfBounds;

impl Error for OutOfBounds {}

impl fmt::Display for OutOfBounds {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TODO proper error")
    }
}
