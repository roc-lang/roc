#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate indoc;

use roc_bindgen::bindgen_rs;
use roc_bindgen::load::load_types;
use roc_load::Threading;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

fn generate_bindings(decl_src: &str) -> String {
    use tempfile::tempdir;

    let mut src = indoc!(
        r#"
            platform "main"
                requires {} { nothing : {} }
                exposes []
                packages {}
                imports []
                provides [ main ]

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

#[test]
fn record_aliased() {
    let module = indoc!(
        r#"
            MyRcd : { a : U64, b : U128 }

            main : MyRcd
            main = { a: 1u64, b: 2u128 }
        "#
    );

    assert_eq!(
        generate_bindings(module)
            .strip_prefix('\n')
            .unwrap_or_default(),
        indoc!(
            r#"
                #[derive(Clone, PartialEq, PartialOrd, Copy, Default, Eq, Ord, Hash, Debug)]
                #[repr(C)]
                pub struct MyRcd {
                    b: u128,
                    a: u64,
                }
            "#
        )
    );
}

#[test]
fn nested_record_aliased() {
    let module = indoc!(
        r#"
            Outer : { x : Inner, y : Str, z : List U8 }

            Inner : { a : U16, b : F32 }

            main : Outer
            main = { x: { a: 5, b: 24 }, y: "foo", z: [ 1, 2 ] }
        "#
    );

    assert_eq!(
        generate_bindings(module)
            .strip_prefix('\n')
            .unwrap_or_default(),
        indoc!(
            r#"
                #[derive(Clone, PartialEq, PartialOrd, Default, Debug)]
                #[repr(C)]
                pub struct Outer {
                    y: roc_std::RocStr,
                    z: roc_std::RocList<u8>,
                    x: Inner,
                }

                #[derive(Clone, PartialEq, PartialOrd, Copy, Default, Debug)]
                #[repr(C)]
                pub struct Inner {
                    b: f32,
                    a: u16,
                }
            "#
        )
    );
}

#[test]
fn record_anonymous() {
    let module = "main = { a: 1u64, b: 2u128 }";

    assert_eq!(
        generate_bindings(module)
            .strip_prefix('\n')
            .unwrap_or_default(),
        indoc!(
            r#"
                #[derive(Clone, PartialEq, PartialOrd, Copy, Default, Eq, Ord, Hash, Debug)]
                #[repr(C)]
                pub struct R1 {
                    b: u128,
                    a: u64,
                }
            "#
        )
    );
}

#[test]
fn nested_record_anonymous() {
    let module = r#"main = { x: { a: 5u16, b: 24f32 }, y: "foo", z: [ 1u8, 2 ] }"#;

    assert_eq!(
        generate_bindings(module)
            .strip_prefix('\n')
            .unwrap_or_default(),
        indoc!(
            r#"
                #[derive(Clone, PartialEq, PartialOrd, Default, Debug)]
                #[repr(C)]
                pub struct R1 {
                    y: roc_std::RocStr,
                    z: roc_std::RocList<u8>,
                    x: R2,
                }

                #[derive(Clone, PartialEq, PartialOrd, Copy, Default, Debug)]
                #[repr(C)]
                pub struct R2 {
                    b: f32,
                    a: u16,
                }
            "#
        )
    );
}

#[test]
fn tag_union_aliased() {
    let module = indoc!(
        r#"
            MyTagUnion : [ Foo U64, Bar U128 ]

            main : MyTagUnion
            main = Foo 123
        "#
    );

    assert_eq!(
        generate_bindings(module)
            .strip_prefix('\n')
            .unwrap_or_default(),
        indoc!(
            r#"
                #[derive(Clone, PartialEq, PartialOrd, Copy, Default, Eq, Ord, Hash, Debug)]
                #[repr(u8)]
                pub enum tag_MyTagUnion {
                    Bar,
                    Foo,
                }
            "#
        )
    );
}

#[test]
fn tag_union_enumeration() {
    let module = indoc!(
        r#"
            MyTagUnion : [ Blah, Foo, Bar, ]

            main : MyTagUnion
            main = Foo
        "#
    );

    assert_eq!(
        generate_bindings(module)
            .strip_prefix('\n')
            .unwrap_or_default(),
        indoc!(
            r#"
                #[derive(Clone, PartialEq, PartialOrd, Copy, Eq, Ord, Hash, Debug)]
                #[repr(u8)]
                pub enum MyTagUnion {
                    Bar,
                    Blah,
                    Foo,
                }
            "#
        )
    );
}

#[test]
fn single_tag_union_with_payloads() {
    let module = indoc!(
        r#"
            UserId : [ Id U32 Str ]

            main : UserId
            main = Id 42 "blah"
        "#
    );

    assert_eq!(
        generate_bindings(module)
            .strip_prefix('\n')
            .unwrap_or_default(),
        indoc!(
            r#"
                #[derive(Clone, PartialEq, PartialOrd, Default, Eq, Ord, Hash, Debug)]
                #[repr(C)]
                pub struct UserId {
                    f1: roc_std::RocStr,
                    f0: u32,
                }
            "#
        )
    );
}

#[test]
fn single_tag_union_with_one_payload_field() {
    let module = indoc!(
        r#"
            UserId : [ Id Str ]

            main : UserId
            main = Id "blah"
        "#
    );

    assert_eq!(
        generate_bindings(module)
            .strip_prefix('\n')
            .unwrap_or_default(),
        indoc!(
            r#"
                #[derive(Clone, PartialEq, PartialOrd, Default, Eq, Ord, Hash, Debug)]
                #[repr(transparent)]
                pub struct UserId(roc_std::RocStr);
            "#
        )
    );
}
