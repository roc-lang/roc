use crate::bindgen::Env;
use crate::types::Types;
use bumpalo::Bump;
use roc_can::{
    def::{Declaration, Def},
    pattern::Pattern,
};
use roc_load::{LoadedModule, Threading};
use roc_mono::layout::LayoutCache;
use roc_reporting::report::RenderTarget;
use roc_target::Architecture;
use std::io;
use std::path::{Path, PathBuf};
use strum::IntoEnumIterator;
use target_lexicon::Triple;

pub fn load_types(
    full_file_path: PathBuf,
    dir: &Path,
    threading: Threading,
) -> Result<Vec<(Architecture, Types)>, io::Error> {
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

    let mut answer = Vec::with_capacity(Architecture::iter().size_hint().0);

    for architecture in Architecture::iter() {
        let defs_iter = decls.iter().flat_map(|decl| match decl {
            Declaration::Declare(def) => {
                vec![def.clone()]
            }
            Declaration::DeclareRec(defs, cycle_mark) => {
                if cycle_mark.is_illegal(subs) {
                    Vec::new()
                } else {
                    defs.clone()
                }
            }
            Declaration::Builtin(..) => {
                unreachable!("Builtin decl in userspace module?")
            }
            Declaration::InvalidCycle(..) => Vec::new(),
        });

        let vars_iter = defs_iter.filter_map(
            |Def {
                 loc_pattern,
                 pattern_vars,
                 ..
             }| {
                if let Pattern::Identifier(sym) = loc_pattern.value {
                    let var = pattern_vars
                        .get(&sym)
                        .expect("Indetifier known but it has no var?");

                    Some(*var)
                } else {
                    // figure out if we need to export non-identifier defs - when
                    // would that happen?
                    None
                }
            },
        );

        let mut layout_cache = LayoutCache::new(architecture.into());
        let mut env = Env {
            arena,
            layout_cache: &mut layout_cache,
            interns: &interns,
            subs,
            struct_names: Default::default(),
            enum_names: Default::default(),
            pending_recursive_types: Default::default(),
            known_recursive_types: Default::default(),
        };
        let types = env.vars_to_types(vars_iter);

        answer.push((architecture, types));
    }

    Ok(answer)
}
