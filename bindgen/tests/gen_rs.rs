#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate indoc;

use bumpalo::Bump;
use roc_bindgen::bindgen_rs::{write_layout_type, Env};
use roc_can::{
    def::{Declaration, Def},
    pattern::Pattern,
};
use roc_load::LoadedModule;
use roc_mono::layout::LayoutCache;
use roc_reporting::report::RenderTarget;
use roc_target::TargetInfo;
use std::io::Write;
use std::path::PathBuf;
use std::{fs::File, mem::ManuallyDrop};

fn run_load_and_typecheck(
    subdir: &str,
    src: &str,
    target_info: TargetInfo,
) -> Result<LoadedModule, std::io::Error> {
    let arena = &Bump::new();

    assert!(
        src.starts_with("app \"") || src.starts_with("platform \""),
        "This test needs a platform or application module, not an expr"
    );

    let subs_by_module = Default::default();
    let loaded = {
        // Use a deterministic temporary directory.
        // We can't have all tests use "tmp" because tests run in parallel,
        // so append the test name to the tmp path.
        let tmp = format!("tmp/{}", subdir);
        let dir = roc_test_utils::TmpDir::new(&tmp);

        let filename = PathBuf::from("Test.roc");
        let file_path = dir.path().join(filename);
        let full_file_path = file_path.clone();
        let mut file = File::create(file_path).unwrap();
        writeln!(file, "{}", &src).unwrap();
        let result = roc_load::load_and_typecheck(
            arena,
            full_file_path,
            dir.path(),
            subs_by_module,
            target_info,
            RenderTarget::Generic,
        );

        result
    };

    Ok(loaded.expect("had problems loading"))
}

pub fn generate_bindings(subdir: &str, src: &str, target_info: TargetInfo) -> String {
    let LoadedModule {
        module_id: home,
        mut can_problems,
        mut type_problems,
        mut declarations_by_id,
        mut solved,
        interns,
        ..
    } = run_load_and_typecheck(subdir, src, target_info).expect("Something went wrong with IO");

    let decls = declarations_by_id.remove(&home).unwrap();
    let subs = solved.inner_mut();

    let can_problems = can_problems.remove(&home).unwrap_or_default();
    let type_problems = type_problems.remove(&home).unwrap_or_default();

    if !can_problems.is_empty() || !type_problems.is_empty() {
        assert!(
            false,
            "There were problems: {:?}, {:?}",
            can_problems, type_problems
        );
    }

    let arena = Bump::new();
    let mut layout_cache = LayoutCache::new(target_info);
    let mut env = Env {
        arena: &arena,
        layout_cache: &mut layout_cache,
        interns: &interns,
        struct_names: Default::default(),
        enum_names: Default::default(),
        deriving: Default::default(),
        subs,
    };

    let mut bindgen_result = String::new();

    for decl in decls.into_iter() {
        let defs = match decl {
            Declaration::Declare(def) => {
                vec![def]
            }
            Declaration::DeclareRec(defs) => defs,
            Declaration::Builtin(..) => {
                unreachable!("Builtin decl in userspace module?")
            }
            Declaration::InvalidCycle(..) => {
                vec![]
            }
        };
        for Def {
            loc_pattern,
            pattern_vars,
            ..
        } in defs.into_iter()
        {
            match loc_pattern.value {
                Pattern::Identifier(sym) => {
                    let var = pattern_vars
                        .get(&sym)
                        .expect("Indetifier known but it has no var?");
                    let layout = env
                        .layout_cache
                        .from_var(&arena, *var, &subs)
                        .expect("Something weird ended up in the content");

                    write_layout_type(&mut env, layout, *var, &mut bindgen_result)
                        .expect("I/O error when writing bindgen string");
                }
                _ => {
                    // figure out if we need to export non-identifier defs - when would that
                    // happen?
                }
            }
        }
    }

    bindgen_result
}

#[test]
fn record_aliased() {
    let module = indoc!(
        r#"
            app "main" provides [ main ] to "./platform"

            MyRcd : { a : U64, b : U128 }

            main : MyRcd
            main = { a: 1u64, b: 2u128 }
        "#
    );

    let bindings_rust =
        generate_bindings("record_type_aliased", module, TargetInfo::default_x86_64());

    assert_eq!(
        bindings_rust,
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
            app "main" provides [ main ] to "./platform"

            Outer : { x : Inner, y : Str, z : List U8 }

            Inner : { a : U16, b : F32 }

            main : Outer
            main = { x: { a: 5, b: 24 }, y: "foo", z: [ 1, 2 ] }
        "#
    );

    let bindings_rust =
        generate_bindings("record_type_aliased", module, TargetInfo::default_x86_64());

    assert_eq!(
        bindings_rust,
        indoc!(
            r#"
                #[derive(Clone, PartialEq, PartialOrd, Copy, Default, Eq, Ord, Hash, Debug)]
                #[repr(C)]
                pub struct Outer {
                    y: RocStr,
                    z: RocList<u8>,
                    x: Inner,
                }

                #[derive(Clone, PartialEq, PartialOrd, Copy, Default, Eq, Ord, Hash, Debug)]
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
    let module = indoc!(
        r#"
            app "main" provides [ main ] to "./platform"

            main = { a: 1u64, b: 2u128 }
        "#
    );

    let bindings_rust = generate_bindings(
        "record_type_anonymous",
        module,
        TargetInfo::default_x86_64(),
    );

    assert_eq!(
        bindings_rust,
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
    let module = indoc!(
        r#"
            app "main" provides [ main ] to "./platform"

            main = { x: { a: 5u16, b: 24f32 }, y: "foo", z: [ 1u8, 2 ] }
        "#
    );

    let bindings_rust =
        generate_bindings("record_type_aliased", module, TargetInfo::default_x86_64());

    assert_eq!(
        bindings_rust,
        indoc!(
            r#"
                #[derive(Clone, PartialEq, PartialOrd, Default, Eq, Ord, Hash, Debug)]
                #[repr(C)]
                pub struct R1 {
                    y: RocStr,
                    z: RocList<u8>,
                    x: R2,
                }

                #[derive(Clone, PartialEq, PartialOrd, Copy, Default, Eq, Ord, Hash, Debug)]
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
fn tag_union_type_aliased() {
    let module = indoc!(
        r#"
            app "main" provides [ main ] to "./platform"

            MyTagUnion : [ Foo U64, Bar U128 ]

            main : MyTagUnion
            main = Foo 123
        "#
    );

    let bindings_rust = generate_bindings(
        "tag_union_type_aliased",
        module,
        TargetInfo::default_x86_64(),
    );

    assert_eq!(
        bindings_rust,
        indoc!(
            r#"
                pub struct MyTagUnion {
                    tag: u8,
                    payload: MyTagUnionPayload,
                }

                union payload_MyTagUnion {
                    Foo: u64,
                    Bar: u128,
                }

                impl MyTagUnion {
                    pub enum Key {

                    }

                    pub fn Foo(u64) -> Self {
                        Self()
                    }
                }
            "#
        )
    );
}

#[repr(C)]
pub struct MyTagUnion {
    tag: tag_MyTagUnion,
    variant: variant_MyTagUnion,
}

#[repr(C)]
union variant_MyTagUnion {
    Bar: u128,
    Foo: ManuallyDrop<Payload2<roc_std::RocStr, i32>>,
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
                Foo: ManuallyDrop::new(Payload2 { _0, _1 }),
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
            tag_MyTagUnion::Foo => unsafe { ManuallyDrop::drop(&mut self.variant.Foo) },
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
