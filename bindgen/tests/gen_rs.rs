#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate indoc;

use bumpalo::Bump;
use core::mem;
use roc_bindgen::{
    bindgen_rs::{write_layout_type, write_roc_type, Env},
    enums::Enums,
    structs::Structs,
    types::{self, RocType},
};
use roc_can::{
    def::{Declaration, Def},
    pattern::Pattern,
};
use roc_load::LoadedModule;
use roc_mono::layout::LayoutCache;
use roc_reporting::report::RenderTarget;
use roc_target::TargetInfo;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

fn run_load_and_typecheck(
    src: &str,
    target_info: TargetInfo,
) -> Result<(LoadedModule), std::io::Error> {
    use bumpalo::Bump;
    use tempfile::tempdir;

    let arena = &Bump::new();

    assert!(
        src.starts_with("app"),
        "I need a module source, not an expr"
    );

    let subs_by_module = Default::default();
    let loaded = {
        let dir = tempdir()?;
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
            roc_target::TargetInfo::default_x86_64(),
            RenderTarget::Generic,
        );

        dir.close()?;

        result
    };

    Ok(loaded.expect("had problems loading"))
}

pub fn generate_bindings(src: &str, target_info: TargetInfo) -> String {
    let (LoadedModule {
        module_id: home,
        mut can_problems,
        mut type_problems,
        mut declarations_by_id,
        mut solved,
        interns,
        ..
    }) = run_load_and_typecheck(src, target_info).expect("Something went wrong with IO");

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
                    let content = subs.get_content_without_compacting(*var);

                    write_layout_type(&mut env, layout, content, subs, &mut bindgen_result);
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

#[test]
fn my_struct_in_rust() {
    let module = r#"app "main" provides [ main ] to "./platform"

MyRcd : { a: U64, b: U128 }

main : MyRcd
main = { a: 1u64, b: 2u128 }
"#;

    let bindings_rust = generate_bindings(module, TargetInfo::default_x86_64());

    assert_eq!(
        bindings_rust,
        "struct MyRcd {
    b: u128,
    a: u64,
}
"
    );
}
