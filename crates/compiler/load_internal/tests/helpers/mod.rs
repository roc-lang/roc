extern crate bumpalo;

use std::path::{Path, PathBuf};

pub fn fixtures_dir() -> PathBuf {
    Path::new("tests").join("fixtures").join("build")
}
