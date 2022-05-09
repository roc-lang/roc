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
            .strip_prefix("\n")
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
            .strip_prefix("\n")
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
            .strip_prefix("\n")
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
            .strip_prefix("\n")
            .unwrap_or_default(),
        indoc!(
            r#"
                #[derive(Clone, PartialEq, PartialOrd, Default, Debug)]
                #[repr(C)]
                pub struct R2 {
                    y: roc_std::RocStr,
                    z: roc_std::RocList<u8>,
                    x: R1,
                }

                #[derive(Clone, PartialEq, PartialOrd, Copy, Default, Debug)]
                #[repr(C)]
                pub struct R1 {
                    b: f32,
                    a: u16,
                }
            "#
        )
    );
}

#[test]
#[ignore]
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
            .strip_prefix("\n")
            .unwrap_or_default(),
        indoc!(
            r#"
                #[repr(C)]
                pub struct MyTagUnion {
                    tag: tag_MyTagUnion,
                    variant: variant_MyTagUnion,
                }

                #[repr(C)]
                union variant_MyTagUnion {
                    Bar: u128,
                    Foo: std::mem::ManuallyDrop<Payload2<roc_std::RocStr, i32>>,
                }

                #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
                #[repr(C)]
                pub struct Payload2<V0, V1> {
                    _0: V0,
                    _1: V1,
                }

                #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
                #[repr(u8)]
                pub enum tag_MyTagUnion {
                    Bar,
                    Foo,
                }

                impl MyTagUnion {
                    pub fn tag(&self) -> tag_MyTagUnion {
                        self.tag
                    }

                    /// Assume this is the tag named Foo, and return a reference to its payload.
                    pub unsafe fn as_Foo(&self) -> &Payload2<roc_std::RocStr, i32> {
                        &*self.variant.Foo
                    }

                    /// Assume this is the tag named Foo, and return a mutable reference to its payload.
                    pub unsafe fn as_mut_Foo(&mut self) -> &mut Payload2<roc_std::RocStr, i32> {
                        &mut *self.variant.Foo
                    }

                    /// Assume this is the tag named Bar, and return a reference to its payload.
                    pub unsafe fn as_Bar(&self) -> u128 {
                        self.variant.Bar
                    }

                    /// Assume this is the tag named Bar, and return a mutable reference to its payload.
                    pub unsafe fn as_mut_Bar(&mut self) -> &mut u128 {
                        &mut self.variant.Bar
                    }

                    /// Construct a tag named Foo, with the appropriate payload
                    pub fn Foo(_0: roc_std::RocStr, _1: i32) -> Self {
                        Self {
                            tag: tag_MyTagUnion::Foo,
                            variant: variant_MyTagUnion {
                                Foo: std::mem::ManuallyDrop::new(Payload2 { _0, _1 }),
                            },
                        }
                    }

                    /// Construct a tag named Bar, with the appropriate payload
                    pub fn Bar(arg0: u128) -> Self {
                        Self {
                            tag: tag_MyTagUnion::Bar,
                            variant: variant_MyTagUnion { Bar: arg0 },
                        }
                    }
                }

                impl Drop for MyTagUnion {
                    fn drop(&mut self) {
                        match self.tag {
                            tag_MyTagUnion::Bar => {}
                            tag_MyTagUnion::Foo => unsafe { std::mem::ManuallyDrop::drop(&mut self.variant.Foo) },
                        }
                    }
                }

                impl PartialEq for MyTagUnion {
                    fn eq(&self, other: &Self) -> bool {
                        if self.tag != other.tag {
                            return false;
                        }

                        unsafe {
                            match self.tag {
                                tag_MyTagUnion::Bar => self.variant.Bar == other.variant.Bar,
                                tag_MyTagUnion::Foo => self.variant.Foo == other.variant.Foo,
                            }
                        }
                    }
                }

                impl Eq for MyTagUnion {}

                impl PartialOrd for MyTagUnion {
                    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
                        match self.tag.partial_cmp(&other.tag) {
                            Some(core::cmp::Ordering::Equal) => {}
                            not_eq => return not_eq,
                        }

                        unsafe {
                            match self.tag {
                                tag_MyTagUnion::Bar => self.variant.Bar.partial_cmp(&other.variant.Bar),
                                tag_MyTagUnion::Foo => self.variant.Foo.partial_cmp(&other.variant.Foo),
                            }
                        }
                    }
                }

                impl Ord for MyTagUnion {
                    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                        match self.tag.cmp(&other.tag) {
                            core::cmp::Ordering::Equal => {}
                            not_eq => return not_eq,
                        }

                        unsafe {
                            match self.tag {
                                tag_MyTagUnion::Bar => self.variant.Bar.cmp(&other.variant.Bar),
                                tag_MyTagUnion::Foo => self.variant.Foo.cmp(&other.variant.Foo),
                            }
                        }
                    }
                }
            "#
        )
    );
}
