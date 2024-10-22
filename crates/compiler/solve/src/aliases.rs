use roc_can::abilities::AbilitiesStore;
use roc_collections::{soa::slice_extend_new, MutMap};
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_solve_problem::TypeError;
use roc_types::{
    subs::{AliasVariables, Content, FlatType, Rank, Subs, SubsSlice, TagExt, UnionTags, Variable},
    types::{Alias, AliasKind, OptAbleVar, Type, TypeTag, Types},
};
use soa::Index;

use crate::to_var::type_to_var_help;
use crate::{ability::ObligationCache, env::InferenceEnv};

#[derive(Debug, Clone, Copy)]
struct DelayedAliasVariables {
    start: u32,
    type_variables_len: u8,
    lambda_set_variables_len: u8,
    recursion_variables_len: u8,
    infer_ext_in_output_variables_len: u8,
}

impl DelayedAliasVariables {
    fn recursion_variables(self, variables: &mut [OptAbleVar]) -> &mut [OptAbleVar] {
        let start = self.start as usize
            + (self.type_variables_len + self.lambda_set_variables_len) as usize;
        let length = self.recursion_variables_len as usize;

        &mut variables[start..][..length]
    }

    fn lambda_set_variables(self, variables: &mut [OptAbleVar]) -> &mut [OptAbleVar] {
        let start = self.start as usize + self.type_variables_len as usize;
        let length = self.lambda_set_variables_len as usize;

        &mut variables[start..][..length]
    }

    fn type_variables(self, variables: &mut [OptAbleVar]) -> &mut [OptAbleVar] {
        let start = self.start as usize;
        let length = self.type_variables_len as usize;

        &mut variables[start..][..length]
    }

    fn infer_ext_in_output_variables(self, variables: &mut [OptAbleVar]) -> &mut [OptAbleVar] {
        let start = self.start as usize
            + (self.type_variables_len
                + self.lambda_set_variables_len
                + self.recursion_variables_len) as usize;
        let length = self.infer_ext_in_output_variables_len as usize;

        &mut variables[start..][..length]
    }
}

#[derive(Debug, Default)]
pub struct Aliases {
    aliases: Vec<(Symbol, Index<TypeTag>, DelayedAliasVariables, AliasKind)>,
    variables: Vec<OptAbleVar>,
}

impl Aliases {
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            aliases: Vec::with_capacity(cap),
            variables: Vec::with_capacity(cap * 2),
        }
    }

    pub fn insert(&mut self, types: &mut Types, symbol: Symbol, alias: Alias) {
        let alias_variables =
            {
                let start = self.variables.len() as _;

                self.variables.extend(
                    alias
                        .type_variables
                        .iter()
                        .map(|x| OptAbleVar::from(&x.value)),
                );

                self.variables.extend(alias.lambda_set_variables.iter().map(
                    |x| match x.as_inner() {
                        Type::Variable(v) => OptAbleVar::unbound(*v),
                        _ => unreachable!("lambda set type is not a variable"),
                    },
                ));

                let recursion_variables_len = alias.recursion_variables.len() as _;
                self.variables.extend(
                    alias
                        .recursion_variables
                        .iter()
                        .copied()
                        .map(OptAbleVar::unbound),
                );

                self.variables.extend(
                    alias
                        .infer_ext_in_output_variables
                        .iter()
                        .map(|v| OptAbleVar::unbound(*v)),
                );

                DelayedAliasVariables {
                    start,
                    type_variables_len: alias.type_variables.len() as _,
                    lambda_set_variables_len: alias.lambda_set_variables.len() as _,
                    recursion_variables_len,
                    infer_ext_in_output_variables_len: alias.infer_ext_in_output_variables.len()
                        as _,
                }
            };

        // TODO: can we construct Aliases from TypeTag directly?
        let alias_typ = types.from_old_type(&alias.typ);

        self.aliases
            .push((symbol, alias_typ, alias_variables, alias.kind));
    }

    fn instantiate_result_result(
        env: &mut InferenceEnv,
        rank: Rank,
        alias_variables: AliasVariables,
    ) -> Variable {
        let tag_names_slice = Subs::RESULT_TAG_NAMES;

        let err_slice = SubsSlice::new(alias_variables.variables_start + 1, 1);
        let ok_slice = SubsSlice::new(alias_variables.variables_start, 1);

        let variable_slices =
            slice_extend_new(&mut env.subs.variable_slices, [err_slice, ok_slice]);

        let union_tags = UnionTags::from_slices(tag_names_slice, variable_slices);
        let ext_var = TagExt::Any(Variable::EMPTY_TAG_UNION);
        let flat_type = FlatType::TagUnion(union_tags, ext_var);
        let content = Content::Structure(flat_type);

        env.register(rank, content)
    }

    /// Build an alias of the form `Num range := range`
    fn build_num_opaque(
        env: &mut InferenceEnv,
        rank: Rank,
        symbol: Symbol,
        range_var: Variable,
    ) -> Variable {
        let content = Content::Alias(
            symbol,
            AliasVariables::insert_into_subs(env.subs, [range_var], [], []),
            range_var,
            AliasKind::Opaque,
        );

        env.register(rank, content)
    }

    fn instantiate_builtin_aliases_real_var(
        &mut self,
        env: &mut InferenceEnv,
        rank: Rank,
        symbol: Symbol,
        alias_variables: AliasVariables,
    ) -> Option<(Variable, AliasKind)> {
        match symbol {
            Symbol::RESULT_RESULT => {
                let var = Self::instantiate_result_result(env, rank, alias_variables);

                Some((var, AliasKind::Structural))
            }
            Symbol::NUM_NUM | Symbol::NUM_INTEGER | Symbol::NUM_FLOATINGPOINT => {
                // Num range := range | Integer range := range | FloatingPoint range := range
                let range_var = env.subs.variables[alias_variables.variables_start as usize];
                Some((range_var, AliasKind::Opaque))
            }
            Symbol::NUM_INT => {
                // Int range : Num (Integer range)
                //
                // build `Integer range := range`
                let integer_content_var = Self::build_num_opaque(
                    env,
                    rank,
                    Symbol::NUM_INTEGER,
                    env.subs.variables[alias_variables.variables_start as usize],
                );

                // build `Num (Integer range) := Integer range`
                let num_content_var =
                    Self::build_num_opaque(env, rank, Symbol::NUM_NUM, integer_content_var);

                Some((num_content_var, AliasKind::Structural))
            }
            Symbol::NUM_FRAC => {
                // Frac range : Num (FloatingPoint range)
                //
                // build `FloatingPoint range := range`
                let fpoint_content_var = Self::build_num_opaque(
                    env,
                    rank,
                    Symbol::NUM_FLOATINGPOINT,
                    env.subs.variables[alias_variables.variables_start as usize],
                );

                // build `Num (FloatingPoint range) := FloatingPoint range`
                let num_content_var =
                    Self::build_num_opaque(env, rank, Symbol::NUM_NUM, fpoint_content_var);

                Some((num_content_var, AliasKind::Structural))
            }
            Symbol::NUM_SIGNED8 => Some((Variable::SIGNED8, AliasKind::Opaque)),
            Symbol::NUM_SIGNED16 => Some((Variable::SIGNED16, AliasKind::Opaque)),
            Symbol::NUM_SIGNED32 => Some((Variable::SIGNED32, AliasKind::Opaque)),
            Symbol::NUM_SIGNED64 => Some((Variable::SIGNED64, AliasKind::Opaque)),
            Symbol::NUM_SIGNED128 => Some((Variable::SIGNED128, AliasKind::Opaque)),
            Symbol::NUM_UNSIGNED8 => Some((Variable::UNSIGNED8, AliasKind::Opaque)),
            Symbol::NUM_UNSIGNED16 => Some((Variable::UNSIGNED16, AliasKind::Opaque)),
            Symbol::NUM_UNSIGNED32 => Some((Variable::UNSIGNED32, AliasKind::Opaque)),
            Symbol::NUM_UNSIGNED64 => Some((Variable::UNSIGNED64, AliasKind::Opaque)),
            Symbol::NUM_UNSIGNED128 => Some((Variable::UNSIGNED128, AliasKind::Opaque)),
            Symbol::NUM_BINARY32 => Some((Variable::BINARY32, AliasKind::Opaque)),
            Symbol::NUM_BINARY64 => Some((Variable::BINARY64, AliasKind::Opaque)),
            _ => None,
        }
    }

    pub fn instantiate_real_var(
        &mut self,
        env: &mut InferenceEnv,
        rank: Rank,
        problems: &mut Vec<TypeError>,
        abilities_store: &AbilitiesStore,
        obligation_cache: &mut ObligationCache,
        arena: &bumpalo::Bump,
        types: &mut Types,
        symbol: Symbol,
        alias_variables: AliasVariables,
    ) -> (Variable, AliasKind) {
        // hardcoded instantiations for builtin aliases
        if let Some((var, kind)) =
            self.instantiate_builtin_aliases_real_var(env, rank, symbol, alias_variables)
        {
            return (var, kind);
        }

        let (typ, delayed_variables, kind) =
            match self.aliases.iter().find(|(s, _, _, _)| *s == symbol) {
                None => internal_error!(
                    "Alias {:?} not registered in delayed aliases! {:?}",
                    symbol,
                    &self.aliases
                ),
                Some(&(_, typ, delayed_variables, kind)) => (typ, delayed_variables, kind),
            };

        let mut substitutions: MutMap<_, _> = Default::default();

        let old_type_variables = delayed_variables.type_variables(&mut self.variables);
        let new_type_variables = &env.subs.variables[alias_variables.type_variables().indices()];

        for (old, new) in old_type_variables.iter_mut().zip(new_type_variables) {
            // if constraint gen duplicated a type these variables could be the same
            // (happens very often in practice)
            if old.var != *new {
                substitutions.insert(old.var, *new);
            }
        }

        for OptAbleVar {
            var: rec_var,
            opt_abilities,
        } in delayed_variables
            .recursion_variables(&mut self.variables)
            .iter_mut()
        {
            debug_assert!(opt_abilities.is_none());
            let new_var = env.subs.fresh_unnamed_flex_var();
            substitutions.insert(*rec_var, new_var);
        }

        let old_lambda_set_variables = delayed_variables.lambda_set_variables(&mut self.variables);
        let new_lambda_set_variables =
            &env.subs.variables[alias_variables.lambda_set_variables().indices()];

        for (old, new) in old_lambda_set_variables
            .iter_mut()
            .zip(new_lambda_set_variables)
        {
            debug_assert!(old.opt_abilities.is_none());
            if old.var != *new {
                substitutions.insert(old.var, *new);
            }
        }

        let old_infer_ext_vars =
            delayed_variables.infer_ext_in_output_variables(&mut self.variables);
        let new_infer_ext_vars =
            &env.subs.variables[alias_variables.infer_ext_in_output_variables().indices()];

        for (old, new) in old_infer_ext_vars.iter_mut().zip(new_infer_ext_vars) {
            debug_assert!(old.opt_abilities.is_none());
            if old.var != *new {
                substitutions.insert(old.var, *new);
            }
        }

        let typ = if !substitutions.is_empty() {
            types.clone_with_variable_substitutions(typ, &substitutions)
        } else {
            typ
        };

        let alias_variable = type_to_var_help(
            env,
            rank,
            problems,
            abilities_store,
            obligation_cache,
            arena,
            self,
            types,
            typ,
            false,
        );
        (alias_variable, kind)
    }
}
