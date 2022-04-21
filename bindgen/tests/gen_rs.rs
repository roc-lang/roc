#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate indoc;

use core::mem;
use roc_bindgen::{
    bindgen_rs::write_roc_type,
    enums::Enums,
    structs::Structs,
    types::{self, RocType},
};

#[test]
fn struct_without_different_pointer_alignment() {
    let mut structs = Structs::default();
    let mut enums = Enums::default();

    let mut rec = types::RocRecord::new(vec![
        ("second".to_string(), Box::new(RocType::Str)),
        ("first".to_string(), Box::new(RocType::Str)),
        ("third".to_string(), Box::new(RocType::Str)),
    ]);

    let mut out = String::default();
    write_roc_type(RocType::Record(rec), &mut structs, &mut enums, &mut out);

    assert_eq!(
        indoc!(
            r#"
                struct R1 {
                    first: RocStr,
                    second: RocStr,
                    third: RocStr,
                }
            "#
        )
        .to_string(),
        out,
    );
}
