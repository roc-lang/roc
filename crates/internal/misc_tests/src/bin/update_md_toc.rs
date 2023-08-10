//! This binary only exists as an intermediary for the `cargo update-md-toc` alias
//! (configured in the `[alias]` section of `.cargo/config.toml`),
//! since cargo aliases cannot set environment variables.

use std::process::Command;

fn main() {
    Command::new("cargo")
        .args([
            "test",
            "-p",
            env!("CARGO_PKG_NAME"),
            "markdown_tocs_are_up_to_date",
        ])
        .env("SNAPSHOTS", "overwrite")
        .spawn()
        .unwrap();
}
