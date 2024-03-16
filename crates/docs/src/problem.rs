use std::{io, path::PathBuf};

pub enum Problem {
    FailedToLoadModule,
    FailedToDeleteDir(PathBuf, io::Error),
    FailedToCreateDir(PathBuf, io::Error),
    FailedToWrite(PathBuf, io::Error),
}
