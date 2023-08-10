use std::{fs, path::Path};

use itertools::Itertools;
use snapbox;

fn repo_root_path() -> &'static Path {
    Path::new("../../../")
}

#[test]
fn authors_are_not_duplicate() {
    let authors_path = repo_root_path().join("AUTHORS");
    let authors = fs::read_to_string(&authors_path).unwrap();
    let authors_deduplicated = authors.split('\n').unique().join("\n");
    snapbox::assert_eq_path(authors_path, authors_deduplicated);
}
