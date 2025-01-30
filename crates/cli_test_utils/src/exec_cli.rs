use std::ffi::{OsStr, OsString};
use std::path::PathBuf;
use std::process::Command;

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
    pub roc_file_path: PathBuf,
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

        let app_stdin_opt = None;
        run_command(roc_cli_command, app_stdin_opt)
    }

    pub fn full_check_build_and_run(
        mut self,
        expected_output: &'static str,
        both_linkers: bool,
        with_valgrind: bool,
        app_stdin_opt: Option<&str>,
        app_args_opt: Option<&[&str]>,
    ) {
        self.check_build_and_run(expected_output, with_valgrind, app_stdin_opt, app_args_opt);

        if both_linkers {
            self = self.arg(LINKER_FLAG);
            self.check_build_and_run(expected_output, with_valgrind, app_stdin_opt, app_args_opt);
        }
    }

    pub fn check_build_and_run(
        &self,
        expected_output: &'static str,
        with_valgrind: bool,
        app_stdin_opt: Option<&str>,
        app_args_opt: Option<&[&str]>,
    ) {
        assert!(self.sub_command == roc_cli::CMD_BUILD, "check_build_and_run should be run with the build command, instead it was run with '{}'.", self.sub_command);

        let build_cmd_out = self.run();
        build_cmd_out.assert_clean_success();

        let executable_output = self.run_executable(false, app_stdin_opt, app_args_opt);
        executable_output.assert_clean_success();
        assert_eq!(executable_output.stdout, expected_output);

        if with_valgrind {
            let executable_output_w_valgrind =
                self.run_executable(true, app_stdin_opt, app_args_opt);
            assert!(
                executable_output_w_valgrind.status.success(),
                "Valgrind found issue(s):\n{}\nCommand used for building:\n\t{:?}",
                executable_output_w_valgrind,
                build_cmd_out.cmd_str
            );
        }
    }

    // run executable produced by e.g. `roc build`
    pub fn run_executable(
        &self,
        with_valgrind: bool,
        app_stdin_opt: Option<&str>,
        app_args_opt: Option<&[&str]>,
    ) -> CmdOut {
        let executable = self.get_executable();

        if with_valgrind {
            let mut command = Command::new("valgrind");

            command.args([
                "--leak-check=full",
                "--error-exitcode=1",
                "--errors-for-leak-kinds=definite,possible",
                executable.to_str().unwrap(),
            ]);
            if let Some(args) = app_args_opt {
                command.args(args);
            }

            run_command(command, app_stdin_opt)
        } else {
            let mut command = Command::new(executable);
            if let Some(args) = app_args_opt {
                command.args(args);
            }
            run_command(command, app_stdin_opt)
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
