fn main() {
    let temp_dir = tempfile::tempdir().unwrap();
    let app_module_path = temp_dir.path().join("app.roc");

    let pf = std::env::current_dir()
        .unwrap()
        .join("zig-platform/main.roc");

    let app_module_source: String = format!(
        indoc::indoc!(
            r#"
            app "test"
                packages {{ pf: "{}" }}
                imports []
                provides [main] to pf

            main = "hello world"
            "#
        ),
        pf.to_str().unwrap()
    );

    let arena = bumpalo::Bump::new();
    let assume_prebuilt = false;
    let res_binary_path = roc_cli::build::build_str_test(
        &arena,
        &app_module_path,
        &app_module_source,
        assume_prebuilt,
    );

    res_binary_path.unwrap();
}
