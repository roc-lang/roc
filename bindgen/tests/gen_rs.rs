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
            MyTagUnion : [ Foo Str, Bar U128, Blah I32, Baz ]

            main : MyTagUnion
            main = Foo "blah"
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
                pub enum tag_MyTagUnion {
                    Bar = 0,
                    Baz = 1,
                    Blah = 2,
                    Foo = 3,
                }

                #[repr(C)]
                pub union union_MyTagUnion {
                    Bar: u128,
                    Blah: i32,
                    Foo: core::mem::ManuallyDrop<roc_std::RocStr>,
                }

                #[repr(C)]
                pub struct MyTagUnion {
                    variant: union_MyTagUnion,
                    tag: tag_MyTagUnion,
                }

                impl MyTagUnion {
                    pub fn tag(&self) -> tag_MyTagUnion {
                        self.tag
                    }

                    /// Construct a tag named Bar, with the appropriate payload
                    pub fn Bar(payload: u128) -> Self {
                        Self {
                            tag: tag_MyTagUnion::Bar,
                            variant: union_MyTagUnion {
                                Bar: payload
                            },
                        }
                    }

                    /// Construct a tag named Baz
                    pub fn Baz() -> Self {
                        Self {
                            tag: tag_MyTagUnion::Baz,
                            variant: unsafe {
                                core::mem::transmute::<
                                    core::mem::MaybeUninit<union_MyTagUnion>,
                                    union_MyTagUnion,
                                >(core::mem::MaybeUninit::uninit())
                            },
                        }
                    }

                    /// Construct a tag named Blah, with the appropriate payload
                    pub fn Blah(payload: i32) -> Self {
                        Self {
                            tag: tag_MyTagUnion::Blah,
                            variant: union_MyTagUnion {
                                Blah: payload
                            },
                        }
                    }

                    /// Construct a tag named Foo, with the appropriate payload
                    pub fn Foo(payload: roc_std::RocStr) -> Self {
                        Self {
                            tag: tag_MyTagUnion::Foo,
                            variant: union_MyTagUnion {
                                Foo: core::mem::ManuallyDrop::new(payload)
                            },
                        }
                    }
                }

                impl Drop for MyTagUnion {
                    fn drop(&mut self) {
                        match self.tag {
                            tag_MyTagUnion::Bar => {}
                            tag_MyTagUnion::Baz => {}
                            tag_MyTagUnion::Blah => {}
                            tag_MyTagUnion::Foo => unsafe { core::mem::ManuallyDrop::drop(&mut self.variant.Foo) },
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
                                tag_MyTagUnion::Baz => true,
                                tag_MyTagUnion::Blah => self.variant.Blah == other.variant.Blah,
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
                                tag_MyTagUnion::Baz => Some(core::cmp::Ordering::Equal),
                                tag_MyTagUnion::Blah => self.variant.Blah.partial_cmp(&other.variant.Blah),
                                tag_MyTagUnion::Foo => self.variant.Foo.partial_cmp(&other.variant.Foo),
                            }
                        }
                    }
                }

                impl Ord for MyTagUnion {
                    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
                        match self.tag.cmp(&other.tag) {
                            core::cmp::Ordering::Equal => {}
                            not_eq => return not_eq,
                        }

                        unsafe {
                            match self.tag {
                                tag_MyTagUnion::Bar => self.variant.Bar.cmp(&other.variant.Bar),
                                tag_MyTagUnion::Baz => core::cmp::Ordering::Equal,
                                tag_MyTagUnion::Blah => self.variant.Blah.cmp(&other.variant.Blah),
                                tag_MyTagUnion::Foo => self.variant.Foo.cmp(&other.variant.Foo),
                            }
                        }
                    }
                }

                impl Clone for MyTagUnion {
                    fn clone(&self) -> Self {
                        match self.tag {
                            tag_MyTagUnion::Bar => Self {
                                variant: union_MyTagUnion {
                                    Bar: unsafe { self.variant.Bar.clone() },
                                },
                                tag: tag_MyTagUnion::Bar,
                            },
                            tag_MyTagUnion::Baz => Self {
                                variant: unsafe {
                                    core::mem::transmute::<
                                        core::mem::MaybeUninit<union_MyTagUnion>,
                                        union_MyTagUnion,
                                    >(core::mem::MaybeUninit::uninit())
                                },
                                tag: tag_MyTagUnion::Baz,
                            },
                            tag_MyTagUnion::Blah => Self {
                                variant: union_MyTagUnion {
                                    Blah: unsafe { self.variant.Blah.clone() },
                                },
                                tag: tag_MyTagUnion::Blah,
                            },
                            tag_MyTagUnion::Foo => Self {
                                variant: union_MyTagUnion {
                                    Foo: unsafe { self.variant.Foo.clone() },
                                },
                                tag: tag_MyTagUnion::Foo,
                            },
                        }
                    }
                }

                impl core::fmt::Debug for MyTagUnion {
                    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                        f.write_str("MyTagUnion::")?;

                        unsafe {
                            match self.tag {
                                tag_MyTagUnion::Bar => f.debug_tuple("Bar").field(&self.variant.Bar).finish(),
                                tag_MyTagUnion::Baz => f.write_str("Baz"),
                                tag_MyTagUnion::Blah => f.debug_tuple("Blah").field(&self.variant.Blah).finish(),
                                tag_MyTagUnion::Foo => f.debug_tuple("Foo").field(&self.variant.Foo).finish(),
                            }
                        }
                    }
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
                    Bar = 0,
                    Blah = 1,
                    Foo = 2,
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
