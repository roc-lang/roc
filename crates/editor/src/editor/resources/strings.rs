#![allow(dead_code)]

pub const NOTHING_OPENED: &str =
    "Execute `cargo run edit` from the root folder of the repo to try the editor.";

pub const START_TIP: &str = r#"Currently supported: lists, records, string, numbers and value definitions.

Use `Ctrl+Shift+Up` or `Cmd+Shift+Up` to select surrounding expression.
Use backspace after `Ctrl+Shift+Up` to delete the selected expression.

`Ctrl+S` or `Cmd+S` to save.
`Ctrl+R` to run.

Input chars that would create parse errors or change formatting will be ignored.
For convenience and consistency, there is only one way to format roc.
"#;

pub const HELLO_WORLD: &str = r#"
app "helloWorld"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Program.{ Program }]
    provides [main] to pf

main = Program.noArgs mainTask

mainTask =
    Stdout.line "Hello, World!"
    |> Program.exit 0

"#;

pub fn nr_hello_world_lines() -> usize {
    HELLO_WORLD.matches('\n').count() - 1
}

pub const PLATFORM_DIR_NAME: &str = "cli-platform";
pub const PLATFORM_FILE_NAME: &str = "main.roc";

pub const PLATFORM_STR: &str = r#"
platform "test-platform"
    requires {} { main : Str }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : Str
mainForHost = main
"#;
