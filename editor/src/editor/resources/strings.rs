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
app "test-app"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

main = "Hello, world!"


"#;
