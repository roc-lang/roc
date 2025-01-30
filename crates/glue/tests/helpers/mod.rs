use std::env;
use std::path::PathBuf;

#[allow(dead_code)]
pub fn fixtures_dir(dir_name: &str) -> PathBuf {
    let mut path = root_dir();

    // Descend into cli/tests/fixtures/{dir_name}
    path.push("crates");
    path.push("glue");
    path.push("tests");
    path.push("fixtures");
    path.push(dir_name);

    path
}

#[allow(dead_code)]
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

    path
}
