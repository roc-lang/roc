use std::{
    env::{self, VarError},
    path::PathBuf,
    process::Command,
};

#[cfg(windows)]
use std::path::Path;

/// On windows, the path is prefixed with `\\?\`, the "verbatim" prefix.
/// Such a path does not works as an argument to `zig` and other command line tools,
/// and there seems to be no good way to strip it. So we resort to some string manipulation.
#[cfg(windows)]
pub fn strip_windows_prefix(path_buf: &Path) -> PathBuf {
    let path_str = path_buf.display().to_string();

    Path::new(path_str.trim_start_matches(r"\\?\")).to_path_buf()
}

/// get the Path of the root of the repository
pub fn root_dir() -> PathBuf {
    let mut path = env::current_exe().ok().unwrap();

    // Get rid of the filename in target/debug/deps/cli_tests-99c65e4e9a1fbd06
    path.pop();

    // If we're in deps/ get rid of deps/ in target/debug/deps/
    if path.ends_with("deps") {
        path.pop();
    }

    // Get rid of target/debug/ so we're back at the project root
    path.pop();
    path.pop();

    // running cargo with --target will put us in the target dir
    if path.ends_with("target") {
        path.pop();
    }

    path
}

/// Gives a friendly error if cargo is not installed.
/// Also makes it easy to track where we use cargo in the codebase.
pub fn cargo() -> Command {
    let command_str = "cargo";

    if check_command_available(command_str) {
        Command::new(command_str)
    } else {
        panic!("I could not find the cargo command.\nVisit https://rustup.rs/ to install cargo.",)
    }
}

/// Gives a friendly error if rustup is not installed.
/// Also makes it easy to track where we use rustup in the codebase.
pub fn rustup() -> Command {
    // on windows, we need the version of cargo installed by rustup. The meaning of `cargo` is
    // different within a process that runs rust. So we need to explicitly refer to where
    // rustup put the binary
    let command_str = "rustup";

    if check_command_available(command_str) {
        Command::new(command_str)
    } else {
        panic!("I could not find the rustup command.\nVisit https://rustup.rs/ to install rustup.",)
    }
}

/// Gives a friendly error if clang is not installed.
/// Also makes it easy to track where we use clang in the codebase.
pub fn clang() -> Command {
    let command_str = "clang";

    if check_command_available(command_str) {
        Command::new(command_str)
    } else {
        panic!("I could not find the clang command.\nPlease install clang.",)
        //TODO detect OS and provide detailed install instructions
    }
}

/// Gives a friendly error if zig is not installed.
/// Also makes it easy to track where we use zig in the codebase.
pub fn zig() -> Command {
    let command_str = match std::env::var("ROC_ZIG") {
        Ok(path) => path,
        Err(_) => "zig".into(),
    };

    if check_command_available(&command_str) {
        Command::new(command_str)
    } else {
        panic!("I could not find the zig command.\nPlease install zig, see instructions at https://ziglang.org/learn/getting-started/.",)
    }
}

fn check_command_available(command_name: &str) -> bool {
    if cfg!(target_family = "unix") {
        let unparsed_path = match std::env::var("PATH") {
            Ok(var) => var,
            Err(VarError::NotPresent) => return false,
            Err(VarError::NotUnicode(_)) => {
                panic!("found PATH, but it included invalid unicode data!")
            }
        };

        std::env::split_paths(&unparsed_path).any(|dir| dir.join(command_name).exists())
    } else if cfg!(target = "windows") {
        let mut cmd = Command::new("Get-Command");

        cmd.args([command_name]);

        let cmd_str = format!("{cmd:?}");

        let cmd_out = cmd.output().unwrap_or_else(|err| {
            panic!(
                "Failed to execute `{cmd_str}` to check if {command_name} is available:\n    {err}"
            )
        });

        cmd_out.status.success()
    } else {
        // We're in uncharted waters, best not to panic if
        // things may end up working out down the line.
        true
    }
}

pub fn pretty_command_string(command: &Command) -> std::ffi::OsString {
    let mut command_string = std::ffi::OsString::new();
    command_string.push(command.get_program());

    for arg in command.get_args() {
        command_string.push(" ");
        command_string.push(arg);
    }

    command_string
}
