mod helpers;

#[cfg(test)]
mod fuzz_glue {
    use crate::helpers::{fixtures_dir, generate_glue_for, run_app};
    use cli_utils::helpers::{has_error, run_glue, run_roc, Out};

    #[test]
    fn fuzz_int_args() {
        // generate the platform and app .roc files, as well as the expected stdout
        let out = run_app(fuzz_glue_script_path, ["--", "all-ints"]);
        assert!(out.status.success());

        // run `roc glue` to generate the glue
        let out = generate_glue_for(&generated_glue_dir, std::iter::empty());
        assert!(out.status.success());

        // run the generated app
        let out = run_app(&dir.join("app.roc"), std::iter::empty());
        assert!(out.status.success());

        let ignorable = "🔨 Rebuilding platform...\n";
        let stderr = out.stderr.replacen(ignorable, "", 1);
        assert_eq!(stderr, "");
        let expected_stdout = fs::read_to_string(expected_stdout_txt).unwrap();

        assert_eq!(out.stdout, expected_stdout);
    }
}
