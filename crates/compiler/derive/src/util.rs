use roc_can::{abilities::SpecializationLambdaSets, module::ExposedByModule};
use roc_error_macros::internal_error;
use roc_module::symbol::{IdentIds, Symbol};
use roc_types::{
    subs::{instantiate_rigids, Subs, Variable},
    types::Polarity,
};

use crate::DERIVED_SYNTH;

/// An environment representing the Derived_synth module, for use in building derived
/// implementations.
pub(crate) struct Env<'a> {
    /// NB: This **must** be subs for the derive module!
    pub subs: &'a mut Subs,
    pub exposed_types: &'a ExposedByModule,
    pub derived_ident_ids: &'a mut IdentIds,
}

impl Env<'_> {
    pub fn new_symbol(&mut self, name_hint: &str) -> Symbol {
        if cfg!(any(
            debug_assertions,
            test,
            feature = "debug-derived-symbols"
        )) {
            let mut i = 0;
            let debug_name = loop {
                i += 1;
                let name = if i == 1 {
                    name_hint.to_owned()
                } else {
                    format!("{}{}", name_hint, i)
                };
                if self.derived_ident_ids.get_id(&name).is_none() {
                    break name;
                }
            };

            let ident_id = self.derived_ident_ids.get_or_insert(&debug_name);

            Symbol::new(DERIVED_SYNTH, ident_id)
        } else {
            self.unique_symbol()
        }
    }

    pub fn unique_symbol(&mut self) -> Symbol {
        let ident_id = self.derived_ident_ids.gen_unique();
        Symbol::new(DERIVED_SYNTH, ident_id)
    }

    pub fn import_builtin_symbol_var(&mut self, symbol: Symbol) -> Variable {
        let module_id = symbol.module_id();
        debug_assert!(module_id.is_builtin());

        let module_types = &self
            .exposed_types
            .get(&module_id)
            .unwrap()
            .exposed_types_storage_subs;
        let storage_var = module_types.stored_vars_by_symbol.get(&symbol).unwrap();
        let imported = module_types
            .storage_subs
            .export_variable_to_directly_to_use_site(self.subs, *storage_var);

        instantiate_rigids(self.subs, imported.variable);

        imported.variable
    }

    pub fn unify(&mut self, left: Variable, right: Variable) {
        use roc_unify::unify::{unify, Env, Mode, Unified};

        let unified = unify(
            &mut Env::new(self.subs),
            left,
            right,
            Mode::EQ,
            Polarity::OF_PATTERN,
        );

        match unified {
            Unified::Success {
                vars: _,
                must_implement_ability: _,
                lambda_sets_to_specialize,
                extra_metadata: _,
            } => {
                if !lambda_sets_to_specialize.is_empty() {
                    internal_error!("Did not expect derivers to need to specialize unspecialized lambda sets, but we got some: {:?}", lambda_sets_to_specialize)
                }
            }
            Unified::Failure(..) => {
                internal_error!("Unification failed in deriver - that's a deriver bug!")
            }
        }
    }

    pub fn get_specialization_lambda_sets(
        &mut self,
        specialization_type: Variable,
        ability_member: Symbol,
    ) -> SpecializationLambdaSets {
        use roc_unify::unify::{unify_introduced_ability_specialization, Env, Mode, Unified};

        let member_signature = self.import_builtin_symbol_var(ability_member);

        let unified = unify_introduced_ability_specialization(
            &mut Env::new(self.subs),
            member_signature,
            specialization_type,
            Mode::EQ,
        );

        match unified {
            Unified::Success {
                vars: _,
                must_implement_ability: _,
                lambda_sets_to_specialize: _lambda_sets_to_specialize,
                extra_metadata: specialization_lsets,
            } => {
                let specialization_lsets: SpecializationLambdaSets = specialization_lsets
                    .0
                    .into_iter()
                    .map(|((spec_member, region), var)| {
                        debug_assert_eq!(spec_member, ability_member);
                        (region, var)
                    })
                    .collect();

                // Since we're doing `{foo} ~ a | a has Encoding`, we may see "lambda sets to
                // specialize" for e.g. `{foo}:toEncoder:1`, but these are actually just the
                // specialization lambda sets, so we don't need to do any extra work!
                //
                // If there are other lambda sets to specialize in here, that's unexpected, because
                // that means we would have been deriving something like `toEncoder {foo: bar}`,
                // and now seen that we needed `toEncoder bar` where `bar` is a concrete type. But
                // we only expect `bar` to polymorphic at this stage!
                //
                // TODO: it would be better if `unify` could prune these for us. See also
                // https://github.com/roc-lang/roc/issues/3207; that is a blocker for this TODO.
                #[cfg(debug_assertions)]
                {
                    for (spec_var, lambda_sets) in _lambda_sets_to_specialize.drain() {
                        for lambda_set in lambda_sets {
                            let belongs_to_specialized_lambda_sets =
                                specialization_lsets.iter().any(|(_, var)| {
                                    self.subs.get_root_key_without_compacting(*var)
                                        == self.subs.get_root_key_without_compacting(lambda_set)
                                });
                            debug_assert!(belongs_to_specialized_lambda_sets,
                                "Did not expect derivers to need to specialize unspecialized lambda sets, but we got one: {:?} for {:?}", lambda_set, spec_var)
                        }
                    }
                }
                specialization_lsets
            }
            Unified::Failure(..) => {
                internal_error!("Unification failed in deriver - that's a deriver bug!")
            }
        }
    }

    /// Creates an extension variable for a tag union or record.
    ///
    /// Derivers should always construct tag union and record types such that they are closed.
    /// If the `open-extension-vars` feature is turned on, flex extension vars will be
    /// returned; otherwise, the appropriate closed extension variable for the type will be
    /// returned.
    #[inline(always)]
    pub fn new_ext_var(&mut self, kind: ExtensionKind) -> Variable {
        if cfg!(feature = "open-extension-vars") {
            self.subs.fresh_unnamed_flex_var()
        } else {
            match kind {
                ExtensionKind::Record => Variable::EMPTY_RECORD,
                ExtensionKind::TagUnion => Variable::EMPTY_TAG_UNION,
            }
        }
    }
}

pub(crate) enum ExtensionKind {
    Record,
    TagUnion,
}
