use roc_bindgen::bindgen_rs;
use roc_bindgen::load::load_types;
use roc_load::Threading;
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

#[allow(dead_code)]
pub fn generate_bindings(decl_src: &str) -> String {
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
        let filename = PathBuf::from("Package-Config.roc");
        let file_path = dir.path().join(filename);
        let full_file_path = file_path.clone();
        let mut file = File::create(file_path).unwrap();
        writeln!(file, "{}", &src).unwrap();

        let result = load_types(full_file_path, dir.path(), Threading::Single);

        dir.close().expect("Unable to close tempdir");

        result.expect("had problems loading")
    };

    // Reuse the `src` allocation since we're done with it.
    let mut buf = src;
    buf.clear();

    bindgen_rs::write_types(&types, &mut buf).expect("I/O error when writing bindgen string");

    buf
}

#[allow(dead_code)]
pub fn fixtures_dir(dir_name: &str) -> PathBuf {
    let mut path = root_dir();

    // Descend into cli/tests/fixtures/{dir_name}
    path.push("bindgen");
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
