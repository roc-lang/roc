use cli_utils::helpers::{has_error, run_glue, run_roc, Out};
use indoc::indoc;
use roc_glue::load::{load_types, IgnoreErrors};
use roc_glue::rust_glue;
use roc_load::Threading;
use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};

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

#[allow(dead_code)]
pub fn generate_glue_for<'a, I: IntoIterator<Item = &'a str>>(
    platform_dir: &'a Path,
    args: I,
) -> Out {
    let platform_module_path = platform_dir.join("platform.roc");
    let glue_dir = platform_dir.join("test_glue");
    let fixture_templates_dir = platform_dir
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("fixture-templates");

    // Copy the rust template from the templates directory into the fixture dir.
    dircpy::CopyBuilder::new(fixture_templates_dir.join("rust"), platform_dir)
        .overwrite(true) // overwrite any files that were already present
        .run()
        .unwrap();

    // Delete the glue file to make sure we're actually regenerating it!
    if glue_dir.exists() {
        fs::remove_dir_all(&glue_dir)
            .expect("Unable to remove test_glue dir in order to regenerate it in the test");
    }

    let rust_glue_spec = fixture_templates_dir
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("src")
        .join("RustGlue.roc");

    // Generate a fresh test_glue for this platform
    let glue_out = run_glue(
        // converting these all to String avoids lifetime issues
        std::iter::once("glue".to_string()).chain(
            args.into_iter().map(|arg| arg.to_string()).chain([
                rust_glue_spec.to_str().unwrap().to_string(),
                glue_dir.to_str().unwrap().to_string(),
                platform_module_path.to_str().unwrap().to_string(),
            ]),
        ),
    );

    if has_error(&glue_out.stderr) {
        panic!(
            "`roc glue` command had unexpected stderr: {}",
            glue_out.stderr
        );
    }

    assert!(glue_out.status.success(), "bad status {glue_out:?}");

    glue_out
}

#[allow(dead_code)]
pub fn run_app<'a, I: IntoIterator<Item = &'a str>>(app_file: &'a Path, args: I) -> Out {
    // Generate test_glue for this platform
    let compile_out = run_roc(
        // converting these all to String avoids lifetime issues
        args.into_iter()
            .map(|arg| arg.to_string())
            .chain([app_file.to_str().unwrap().to_string()]),
        &[],
        &[],
    );

    if has_error(&compile_out.stderr) {
        panic!(
            "`roc` command had unexpected stderr: {}",
            compile_out.stderr
        );
    }

    assert!(compile_out.status.success(), "bad status {compile_out:?}");

    compile_out
}
