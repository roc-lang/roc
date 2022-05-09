use crate::bindgen::{self, Env};
use crate::types::Types;
use bumpalo::Bump;
use roc_can::{
    def::{Declaration, Def},
    pattern::Pattern,
};
use roc_load::{LoadedModule, Threading};
use roc_mono::layout::LayoutCache;
use roc_reporting::report::RenderTarget;
use std::io;
use std::path::{Path, PathBuf};
use target_lexicon::Triple;

pub fn load_types(
    full_file_path: PathBuf,
    dir: &Path,
    threading: Threading,
) -> Result<Types, io::Error> {
    // TODO: generate both 32-bit and 64-bit #[cfg] macros if structs are different
    // depending on 32-bit vs 64-bit targets.
    let target_info = (&Triple::host()).into();

    let arena = &Bump::new();
    let subs_by_module = Default::default();
    let LoadedModule {
        module_id: home,
        mut can_problems,
        mut type_problems,
        mut declarations_by_id,
        mut solved,
        interns,
        ..
    } = roc_load::load_and_typecheck(
        arena,
        full_file_path,
        dir,
        subs_by_module,
        target_info,
        RenderTarget::Generic,
        threading,
    )
    .expect("Problem loading platform module");

    let decls = declarations_by_id.remove(&home).unwrap();
    let subs = solved.inner_mut();

    let can_problems = can_problems.remove(&home).unwrap_or_default();
    let type_problems = type_problems.remove(&home).unwrap_or_default();

    if !can_problems.is_empty() || !type_problems.is_empty() {
        todo!(
            "Gracefully report compilation problems during bindgen: {:?}, {:?}",
            can_problems,
            type_problems
        );
    }

    let mut layout_cache = LayoutCache::new(target_info);
    let mut env = Env {
        arena,
        layout_cache: &mut layout_cache,
        interns: &interns,
        struct_names: Default::default(),
        enum_names: Default::default(),
        subs,
    };

    let mut types = Types::default();

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
            if let Pattern::Identifier(sym) = loc_pattern.value {
                let var = pattern_vars
                    .get(&sym)
                    .expect("Indetifier known but it has no var?");
                let layout = env
                    .layout_cache
                    .from_var(arena, *var, subs)
                    .expect("Something weird ended up in the content");

                bindgen::add_type(&mut env, layout, *var, &mut types);
            } else {
                // figure out if we need to export non-identifier defs - when would that
                // happen?
            }
        }
    }

    Ok(types)
}
