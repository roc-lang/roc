#[cfg(test)]
mod editor_launch_test {

    use core::time;
    use std::{
        env,
        process::{Command, Stdio},
        thread,
    };

    use cli_utils::helpers::build_roc_bin;
    use roc_cli::CMD_EDIT;
    use roc_command_utils::root_dir;
    use std::io::Read;

    #[ignore = "We don't want to bring up the editor window during regular tests, only on specific CI machines."]
    #[test]
    fn launch_test() {
        launch(None);

        // with a file arg
        launch(Some("roc-projects/new-roc-project-1/main.roc"));

        // with a folder arg
        launch(Some("roc-projects/new-roc-project-1"));
    }

    fn launch(arg_path_str_opt: Option<&str>) {
        let root_dir = root_dir();

        // The editor expects to be run from the root of the repo, so it can find the cli-platform to init a new project folder.
        env::set_current_dir(&root_dir)
            .unwrap_or_else(|_| panic!("Failed to set current dir to {root_dir:?}"));

        let roc_binary_path = build_roc_bin(&["--features", "editor"]);

        let mut cmd_args = vec![CMD_EDIT];

        if let Some(arg_path_str) = arg_path_str_opt {
            cmd_args.push(arg_path_str)
        }

        let mut roc_process = Command::new(roc_binary_path)
            .args(cmd_args)
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to start editor from cli.");

        // wait for editor to show
        thread::sleep(time::Duration::from_millis(2000));

        // We extract 12 bytes from the logs for verification
        let mut stdout_buffer = [0; 12];
        let mut stdout = roc_process.stdout.take().unwrap();
        stdout.read_exact(&mut stdout_buffer).unwrap();

        match roc_process.try_wait() {
            Ok(Some(status)) => panic!(
                "The editor exited with status \"{status}\" but I expected it to still be running."
            ),
            Ok(None) => {
                // The editor is still running as desired, we check if logs are as expected:
                assert_eq!("Loading file", std::str::from_utf8(&stdout_buffer).unwrap());
                // Kill the editor, we don't want it to stay open forever.
                roc_process.kill().unwrap();
            }
            Err(e) => panic!("Failed to wait launch editor cli command: {e}"),
        }
    }
}
