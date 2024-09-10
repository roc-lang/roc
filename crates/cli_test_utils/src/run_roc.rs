use std::{ffi::OsString, path::PathBuf, process::Command};

use crate::{command::run_command, helpers::path_to_roc_binary};
use const_format::concatcp;
use crate::command::CmdOut;

#[derive(Default, Debug, Clone)]
pub struct RocCmdParams {
    /// dev, built, test, etc.
    pub sub_command: &'static str,
    pub args: Vec<OsString>,
    pub env: Vec<(&'static str, &'static str)>,
    pub stdin_vals: Vec<&'static str>,
}

pub fn run_roc_cmd(roc_cmd_params: RocCmdParams) -> CmdOut {
    let mut roc_cmd = Command::new(path_to_roc_binary());

    roc_cmd.arg(roc_cmd_params.sub_command);
    roc_cmd.args(&roc_cmd_params.args);

    for (key, val) in roc_cmd_params.env {
        roc_cmd.env(key, val);
    }

    run_command(roc_cmd, &roc_cmd_params.stdin_vals)
}

const LEGACY_LINKER_FLAG: &str = concatcp!("--", roc_cli::FLAG_LINKER, "=", "legacy");

/// Runs roc with both the default (probably surgical) and the legacy linker
pub fn run_roc_maybe_both_linkers(mut roc_cmd_params: RocCmdParams, both_linkers: bool) -> CmdOut {
    let cmd_out_default_linker = run_roc_cmd(roc_cmd_params.clone());
    
    // we run with the legacy linker last for the best valgrind output
    if both_linkers {
        #[cfg(any(windows, target_os = "macos"))]
        panic!(
            "Testing with both linkers on windows or macos! The lagacy linker is the same as the \
            default linker on windows and macos at the time of writing. Are you sure you are not \
            executing with the same linker twice?"
        ); // TODO ask Luke; is this currently correct for windows?
        
        roc_cmd_params.args.push(LEGACY_LINKER_FLAG.into());
        
        let cmd_out_legacy_linker = run_roc_cmd(roc_cmd_params);
        
        assert!(cmd_out_identical(&cmd_out_default_linker, &cmd_out_legacy_linker));
        
        cmd_out_legacy_linker
    } else {
        cmd_out_default_linker
    }
}

fn cmd_out_identical(cmd_out1: &CmdOut, cmd_out2: &CmdOut) -> bool {
    cmd_out1.stdout == cmd_out2.stdout && cmd_out1.stderr == cmd_out2.stderr && cmd_out1.status == cmd_out2.status
}

/// Run the executable produced by `roc dev` or `roc build`
/// Notes:
/// - this function does not call the roc CLI
/// - only on windows will the provided path contain the .exe extension
pub fn run_roc_exe(roc_exe_path: &PathBuf) -> CmdOut {
    assert!(roc_exe_path.exists(), "The provided Roc executable (roc_exe_path) does not exist:\n\t{}", roc_exe_path.display());

    run_command(Command::new(roc_exe_path), &[])
}