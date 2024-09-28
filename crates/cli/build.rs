use chrono::prelude::*;
use std::fs;
use std::process::Command;
use std::str;

fn main() {
    // Rebuild if this build.rs file changes
    println!("cargo:rerun-if-changed=build.rs");

    // The version file is located at the root of the repository
    let version_file_path = "../../version.txt";

    // Rebuild if version file changes
    println!("cargo:rerun-if-changed={}", version_file_path);

    // Read the version file
    let version_file_contents = fs::read_to_string(version_file_path).unwrap();

    // If the version is "built-from-source", replace it with the git commit information
    let version = match version_file_contents.trim() {
        "built-from-source" => {
            let git_head_file_path = "../../.git/HEAD";
            // Rebuild if a new Git commit is made
            println!("cargo:rerun-if-changed={}", git_head_file_path);

            // Check if the .git/HEAD file exists
            let git_head_exists = fs::metadata(git_head_file_path).is_ok();
            if git_head_exists {
                // Get the hash of the current commit
                let git_describe_output = Command::new("git")
                    .arg("describe")
                    .arg("--always")
                    .arg("--dirty= with additional changes") // Add a suffix if the working directory is dirty
                    .output()
                    .expect("Failed to execute git describe command");
                println!("git_describe_output: {:?}", git_describe_output);
                let git_commit_hash = str::from_utf8(&git_describe_output.stdout)
                    .expect("Failed to parse git describe output")
                    .trim();

                // Get the datetime of the last commit
                let git_show_output = Command::new("git")
                    .arg("show")
                    .arg("--no-patch")
                    .arg("--format=%ct") // Outputting a UNIX timestamp is the only way to always use UTC
                    .output()
                    .expect("Failed to execute git show command");
                println!("git_show_output: {:?}", git_show_output);
                let git_commit_timestamp = {
                    let timestamp = str::from_utf8(&git_show_output.stdout)
                        .expect("Failed to parse git show output as a string")
                        .trim()
                        .parse::<i64>()
                        .expect("Failed to parse timestamp as an integer");
                    DateTime::from_timestamp(timestamp, 0)
                        .expect("Failed to parse timestamp")
                        .format("%Y-%m-%d %H:%M:%S")
                };
                format!(
                    "built from commit {git_commit_hash}, committed at {git_commit_timestamp} UTC"
                )
            } else {
                // If the .git/HEAD file does not exist, e.g. in a Nix build, use a generic message
                "built from source".to_string()
            }
        }
        _ => version_file_contents.trim().to_string(),
    };
    // Emit the version to a build-time environment variable
    println!("cargo:rustc-env=ROC_VERSION={}", version);
}
