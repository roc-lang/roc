use std::fs;

use roc_checkmate_schema::AllEvents;

fn main() {
    let schema = AllEvents::schema();
    fs::write(
        "schema.json",
        serde_json::to_string_pretty(&schema).unwrap(),
    )
    .unwrap();
}
