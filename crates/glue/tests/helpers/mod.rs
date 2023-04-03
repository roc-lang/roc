use roc_glue::load::{load_types, IgnoreErrors};
use roc_glue::rust_glue;
use roc_load::Threading;
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

#[allow(dead_code)]
pub fn generate_bindings(decl_src: &str) -> Vec<roc_glue::types::File> {
    use tempfile::tempdir;

    let mut src = indoc!(
        r#"
            platform "main"
                requires {} { nothing : {} }
                exposes []
                packages {}
                imports []
                provides [main]

        "#
    )
    .to_string();

    src.push_str(decl_src);

    let types = {
        let dir = tempdir().expect("Unable to create tempdir");
        let filename = PathBuf::from("platform.roc");
        let file_path = dir.path().join(filename);
        let full_file_path = file_path.clone();
        let mut file = File::create(file_path).unwrap();
        writeln!(file, "{}", &src).unwrap();

        let result = load_types(
            full_file_path,
            Threading::Single,
            // required `nothing` is unused; that error is okay
            IgnoreErrors { can: true },
        );

        dir.close().expect("Unable to close tempdir");

        result.expect("had problems loading")
    };

    rust_glue::emit(&types)
}

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

    // Get rid of the filename in target/debug/deps/cli_run-99c65e4e9a1fbd06
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
