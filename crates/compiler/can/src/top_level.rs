use bumpalo::Bump;
use roc_region::all::Region;
use roc_scope::{LowercaseId, ModuleId, UppercaseId};

use crate::effect_module::GeneratedInfo;

type TopLevelScope<'a> = roc_scope::TopLevelScope<'a, LcStrId, UcStrId, ModuleStrId, Region>;

/// Canonicalize the top level of a module.
pub fn canonicalize_tl<'a, LcStrId, UcStrId, ModuleStrId, Region>(
    arena: &'a Bump,
    /// This should have been initialized during parsing and populated with top-level identifiers, type alias defs, etc.
    tl_scope: TopLevelScope<'a, LcStrId, UcStrId, ModuleStrId, Region>,
    header_type: &roc_parse::header::HeaderType,
) {
    use roc_parse::header::HeaderType;

    let mut env = Env::new(arena, tl_scope);

    // TODO desugaring should have happened before we call canonicalize!
    // crate::desugar::desugar_defs_node_values(arena, loc_defs, src, &mut None, module_path, true);

    let generated_info = env.canonicalize_module_header(header_type);

    problems.extend(env.never_referenced());
}

struct Env<'a> {
    arena: &'a Bump,
    tl_scope: TopLevelScope<'a>,
}

impl<'a> Env<'a> {
    pub fn new(arena: &'a Bump, tl_scope: TopLevelScope<'a>) -> Self {
        Env { arena, tl_scope }
    }

    /// End a declaration. This does a few things:
    /// * Report any unused defs
    /// * Report any qualified lookups of bindings that weren't exposed, or imports of modules that weren't exposed
    /// * Reset decl scope
    pub fn end_decl<
        LcIsExposed: Fn(LowercaseId) -> bool,
        UcIsExposed: Fn(UppercaseId) -> bool,
        ModuleIsExposed: Fn(ModuleId) -> bool,
    >() {
        // TODO: figure out how to report unused record fields and tag union variants
        // TODO: Report any unused bindings or imports
        // TODO: Report any qualified lookups of bindings that weren't exposed, or imports of modules that weren't exposed
        // TODO: Reset decl scope
    }

    /// End the module. This does a few things:
    /// * Report any unused defs
    /// * Report any qualified lookups of bindings that weren't exposed, or imports of modules that weren't exposed
    /// * Reset tl scope to just the builtins, so this can be recycled for another module without copying those in again.
    pub fn end_module<
        LcIsExposed: Fn(LowercaseId) -> bool,
        UcIsExposed: Fn(UppercaseId) -> bool,
        ModuleIsExposed: Fn(ModuleId) -> bool,
    >() {
        // TODO: figure out how to report unused record fields and tag union variants
        // TODO: Report any unused bindings or imports
        // TODO: Report any qualified lookups of bindings that weren't exposed, or imports of modules that weren't exposed
        // TODO: Reset tl scope to just the builtins, so this can be recycled for another module without copying those in again.
    }

    fn canonicalize_module_header(
        &self,
        header_type: &roc_parse::header::HeaderType,
    ) -> GeneratedInfo {
        match header_type {
            HeaderType::App { provides, .. } => {
                // TODO add lookups for `provides`

                GeneratedInfo::NotSpecial
            }
            HeaderType::Hosted {
                exposes,
                generates,
                generates_with,
                ..
            } => {
                let name: &str = generates.into();
                let (generated_functions, unknown_generated) =
                    validate_generate_with(generates_with);

                for unknown in unknown_generated {
                    env.problem(Problem::UnknownGeneratesWith(unknown));
                }

                let effect_symbol = scope.introduce(name.into(), Region::zero()).unwrap();

                {
                    let a_var = var_store.fresh();

                    let actual =
                        crate::effect_module::build_effect_actual(Type::Variable(a_var), var_store);

                    scope.add_alias(
                        effect_symbol,
                        Region::zero(),
                        vec![Loc::at_zero(AliasVar::unbound("a".into(), a_var))],
                        vec![],
                        actual,
                        AliasKind::Opaque,
                    );
                }

                // TODO Add uc and lc lookups for `exposes`

                GeneratedInfo::Hosted {
                    effect_symbol,
                    generated_functions,
                }
            }
            HeaderType::Package { exposes, .. } => {
                // TODO Add module lookups for `exposes`

                GeneratedInfo::NotSpecial
            }
            HeaderType::Platform {
                requires, exposes, ..
            } => {
                // TODO canonicalize `requires` by binding it into scope and then resolving its annotation.
                // TODO Add module lookups for `exposes`

                GeneratedInfo::NotSpecial
            }
            HeaderType::Builtin { exposes, .. } => {
                // TODO Add uc and lc lookups for `exposes`

                GeneratedInfo::Builtin
            }
            HeaderType::Module { exposes, .. } => {
                // TODO Add uc and lc lookups for `exposes`

                GeneratedInfo::NotSpecial
            }
        }
    }

    // This reports things that are unused because they are never referenced.
    // This is not *quite* the same as unused, because it misses the case where
    // things only refer to each other due to mutual recursion. We handle those elsewhere,
    // when sorting defs.
    pub fn never_referenced(&self) -> impl Iterator<Item = Problem> {
        let mut problems = Vec::new_in(self.arena);

        for (str_id, region) in self.tl_scope.unused_imports() {
            // TODO push a problem
        }

        for (str_id, region) in self.tl_scope.unused_lc() {
            // TODO push a problem
        }

        for (str_id, region) in self.tl_scope.unused_uc() {
            // TODO push a problem
        }
    }
}
