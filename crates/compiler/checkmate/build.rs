use std::fs;

use roc_checkmate_schema::Event;

fn main() {
    let schema = Event::schema();
    fs::write(
        "schema.json",
        serde_json::to_string_pretty(&schema).unwrap(),
    )
    .unwrap();
}
