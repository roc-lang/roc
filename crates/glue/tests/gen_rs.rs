#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate indoc;

mod helpers;

#[cfg(test)]
mod test_gen_rs {
    use crate::helpers::generate_bindings;
    use roc_glue::rust_glue::HEADER;
    use roc_glue::types::File;

    #[test]
    fn basic_record_aliased() {
        let module = indoc!(
            r#"
            MyRcd : { a : U64, b : I128 }

            main : MyRcd
            main = { a: 1u64, b: 2i128 }
            "#
        );

        let full_header = std::str::from_utf8(HEADER).unwrap().to_string() + "\n";
        assert_eq!(
            generate_bindings(module),
            vec![File {
                name: "mod.rs".to_string(),
                content: full_header
                    + indoc!(
                        r#"
                    #[cfg(any(
                        target_arch = "arm",
                        target_arch = "aarch64",
                        target_arch = "wasm32",
                        target_arch = "x86",
                        target_arch = "x86_64"
                    ))]
                    #[derive(Clone, Copy, Debug, Default, Eq, Ord, Hash, PartialEq, PartialOrd)]
                    #[repr(C)]
                    pub struct MyRcd {
                        pub b: roc_std::I128,
                        pub a: u64,
                    }
                    "#
                    )
            }]
        );
    }

    #[test]
    fn nested_record_aliased() {
        let module = indoc!(
            r#"
            Outer : { x : Inner, y : Str, z : List U8 }

            Inner : { a : U16, b : F32 }

            main : Outer
            main = { x: { a: 5, b: 24 }, y: "foo", z: [1, 2] }
            "#
        );

        let full_header = std::str::from_utf8(HEADER).unwrap().to_string() + "\n";
        assert_eq!(
            generate_bindings(module),
            vec![File {
                name: "mod.rs".to_string(),
                content: full_header
                    + indoc!(
                        r#"
                    #[cfg(any(
                        target_arch = "arm",
                        target_arch = "wasm32",
                        target_arch = "x86"
                    ))]
                    #[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
                    #[repr(C)]
                    pub struct Outer {
                        pub x: Inner,
                        pub y: roc_std::RocStr,
                        pub z: roc_std::RocList<u8>,
                    }

                    #[cfg(any(
                        target_arch = "arm",
                        target_arch = "aarch64",
                        target_arch = "wasm32",
                        target_arch = "x86",
                        target_arch = "x86_64"
                    ))]
                    #[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd)]
                    #[repr(C)]
                    pub struct Inner {
                        pub b: f32,
                        pub a: u16,
                    }

                    #[cfg(any(
                        target_arch = "aarch64",
                        target_arch = "x86_64"
                    ))]
                    #[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
                    #[repr(C)]
                    pub struct Outer {
                        pub y: roc_std::RocStr,
                        pub z: roc_std::RocList<u8>,
                        pub x: Inner,
                    }
                    "#
                    )
            }]
        );
    }

    #[test]
    fn record_anonymous() {
        let module = "main = { a: 1u64, b: 2u128 }";

        let full_header = std::str::from_utf8(HEADER).unwrap().to_string() + "\n";
        assert_eq!(
            generate_bindings(module),
            vec![File {
                name: "mod.rs".to_string(),
                content: full_header
                    + indoc!(
                        r#"
                    #[cfg(any(
                        target_arch = "arm",
                        target_arch = "aarch64",
                        target_arch = "wasm32",
                        target_arch = "x86",
                        target_arch = "x86_64"
                    ))]
                    #[derive(Clone, Copy, Debug, Default, Eq, Ord, Hash, PartialEq, PartialOrd)]
                    #[repr(C)]
                    pub struct R1 {
                        pub b: roc_std::U128,
                        pub a: u64,
                    }
                    "#
                    )
            }]
        );
    }

    #[test]
    fn nested_record_anonymous() {
        let module = r#"main = { x: { a: 5u16, b: 24f32 }, y: "foo", z: [1u8, 2] }"#;

        let full_header = std::str::from_utf8(HEADER).unwrap().to_string() + "\n";
        assert_eq!(
            generate_bindings(module),
            vec![File {
                name: "mod.rs".to_string(),
                content: full_header
                    + indoc!(
                        r#"
                    #[cfg(any(
                        target_arch = "arm",
                        target_arch = "wasm32",
                        target_arch = "x86"
                    ))]
                    #[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
                    #[repr(C)]
                    pub struct R1 {
                        pub x: R2,
                        pub y: roc_std::RocStr,
                        pub z: roc_std::RocList<u8>,
                    }

                    #[cfg(any(
                        target_arch = "arm",
                        target_arch = "aarch64",
                        target_arch = "wasm32",
                        target_arch = "x86",
                        target_arch = "x86_64"
                    ))]
                    #[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd)]
                    #[repr(C)]
                    pub struct R2 {
                        pub b: f32,
                        pub a: u16,
                    }

                    #[cfg(any(
                        target_arch = "aarch64",
                        target_arch = "x86_64"
                    ))]
                    #[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
                    #[repr(C)]
                    pub struct R1 {
                        pub y: roc_std::RocStr,
                        pub z: roc_std::RocList<u8>,
                        pub x: R2,
                    }
                    "#
                    )
            }]
        );
    }

    #[test]
    fn tag_union_enumeration() {
        let module = indoc!(
            r#"
            Enumeration : [Blah, Foo, Bar,]

            main : Enumeration
            main = Foo
            "#
        );

        let full_header = std::str::from_utf8(HEADER).unwrap().to_string() + "\n";
        assert_eq!(
            generate_bindings(module),
            vec![File {
                name: "mod.rs".to_string(),
                content: full_header
                    + indoc!(
                        r#"
                    #[cfg(any(
                        target_arch = "arm",
                        target_arch = "aarch64",
                        target_arch = "wasm32",
                        target_arch = "x86",
                        target_arch = "x86_64"
                    ))]
                    #[derive(Clone, Copy, Eq, Ord, Hash, PartialEq, PartialOrd)]
                    #[repr(u8)]
                    pub enum Enumeration {
                        Bar = 0,
                        Blah = 1,
                        Foo = 2,
                    }

                    impl core::fmt::Debug for Enumeration {
                        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                            match self {
                                Self::Bar => f.write_str("Enumeration::Bar"),
                                Self::Blah => f.write_str("Enumeration::Blah"),
                                Self::Foo => f.write_str("Enumeration::Foo"),
                            }
                        }
                    }
                    "#
                    )
            }]
        );
    }
}
