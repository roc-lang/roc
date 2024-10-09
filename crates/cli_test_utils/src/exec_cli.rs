use std::ffi::{OsStr, OsString};
use std::path::PathBuf;
use std::process::{Command};

use crate::command::{run_command, CmdOut};
use crate::helpers::path_to_roc_binary;
use const_format::concatcp;

const LINKER_FLAG: &str = concatcp!("--", roc_cli::FLAG_LINKER, "=", "legacy");


/// A builder for running the Roc CLI.
///
/// Unlike `std::process::Command`, this builder is clonable. This is necessary to be able to test a command with both linkers.
#[derive(Debug, Clone)]
pub struct ExecCli {
    sub_command: &'static str, // build, dev, test...
    roc_file_path: PathBuf,
    args: Vec<OsString>,
}

impl ExecCli {
    pub fn new(sub_command: &'static str, roc_file_path: PathBuf) -> Self {
        Self {
            sub_command,
            roc_file_path,
            args: vec![],
        }
    }

    pub fn arg<S>(mut self, arg: S) -> Self
    where
        S: Into<OsString>,
    {
        self.args.push(arg.into());
        self
    }

    pub fn add_args<I, S>(mut self, args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        for arg in args {
            self = self.arg(&arg);
        }
        self
    }

    pub fn run(&self) -> CmdOut {
        let mut roc_cli_command = Command::new(path_to_roc_binary());
        
        roc_cli_command.arg(self.sub_command);
        roc_cli_command.arg(self.roc_file_path.clone());
        roc_cli_command.args(&self.args);
        
        run_command(roc_cli_command, &[])
    }
    
    pub fn full_check(mut self, expected_output: &'static str, both_linkers: bool, with_valgrind: bool) {
        self.check_build_and_run(expected_output, with_valgrind);

        if both_linkers {
            self = self.arg(LINKER_FLAG);
            self.check_build_and_run(expected_output, with_valgrind);
        }
    }
    
    fn check_build_and_run(&self, expected_output: &'static str, with_valgrind: bool) {
        let build_cmd_out = self.run();
        build_cmd_out.assert_clean_success();

        let executable_output = self.run_executable(with_valgrind);
        executable_output.assert_clean_success();
        assert_eq!(executable_output.stdout, expected_output);
    }
    
    // run executable produced by e.g. `roc build`
    fn run_executable(&self, with_valgrind: bool) -> CmdOut {
        let executable = self.get_executable();
        
        if with_valgrind {
            let mut command = Command::new("valgrind");
            
            command.args(&[
                "--leak-check=full",
                "--error-exitcode=1",
                "--errors-for-leak-kinds=all",
                executable.to_str().unwrap(),
            ]);
            
            run_command(command, &[])
        } else {
            let command = Command::new(executable);
            run_command(command, &[])
        }
    }
    
    fn get_executable(&self) -> PathBuf {
        let mut executable_path = self.roc_file_path.with_extension("");

        if cfg!(windows) {
            executable_path.set_extension("exe");
        }

        if executable_path.exists() {
            executable_path
        } else {
            panic!("Executable {:?} does not exist.", executable_path)
        }
    }
    
    
}