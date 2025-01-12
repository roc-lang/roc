use std::cell::RefCell;

use roc_can::{abilities::AbilitiesStore, constraint::TypeOrVar, expected::Expected};
use roc_collections::soa::slice_extend_new;
use roc_error_macros::internal_error;
use roc_module::{ident::TagName, symbol::Symbol};
use roc_region::all::Loc;
use roc_solve_problem::TypeError;
use roc_solve_schema::UnificationMode;
use roc_types::{
    subs::{
        self, AliasVariables, Content, FlatType, GetSubsSlice, LambdaSet, OptVariable, Rank,
        RecordFields, Subs, SubsSlice, TagExt, TupleElems, UnionLabels, UnionLambdas, UnionTags,
        Variable, VariableSubsSlice,
    },
    types::{
        gather_fields_unsorted_iter, gather_tuple_elems_unsorted_iter, AliasKind, AliasShared,
        Category, ExtImplicitOpenness, Polarity, TypeTag, Types,
    },
};
use roc_unify::unify::{unify, Unified};
use soa::{Index, Slice};

use crate::{
    ability::{AbilityImplError, ObligationCache},
    deep_copy::deep_copy_var_in,
    env::InferenceEnv,
    Aliases, FunctionKind,
};

std::thread_local! {
    /// Scratchpad arena so we don't need to allocate a new one all the time
    static SCRATCHPAD: RefCell<Option<bumpalo::Bump>> = RefCell::new(Some(bumpalo::Bump::with_capacity(4 * 1024)));
}

fn take_scratchpad() -> bumpalo::Bump {
    SCRATCHPAD.with(|f| f.take().unwrap())
}

fn put_scratchpad(scratchpad: bumpalo::Bump) {
    SCRATCHPAD.with(|f| {
        f.replace(Some(scratchpad));
    });
}

pub(crate) fn either_type_index_to_var(
    env: &mut InferenceEnv,
    rank: Rank,
    problems: &mut Vec<TypeError>,
    abilities_store: &mut AbilitiesStore,
    obligation_cache: &mut ObligationCache,
    types: &mut Types,
    aliases: &mut Aliases,
    either_type_index: TypeOrVar,
) -> Variable {
    match either_type_index.split() {
        Ok(type_index) => {
            // Converts the celled type to a variable, emplacing the new variable for re-use.
            let var = type_to_var(
                env,
                rank,
                problems,
                abilities_store,
                obligation_cache,
                types,
                aliases,
                type_index,
            );

            debug_assert!(
                matches!(types[type_index], TypeTag::Variable(v) if v == var)
                    || matches!(
                        types[type_index],
                        TypeTag::EmptyTuple | TypeTag::EmptyRecord | TypeTag::EmptyTagUnion
                    ),
                "different variable was returned for type index variable cell!"
            );
            var
        }
        Err(var_index) => {
            // we cheat, and  store the variable directly in the index
            unsafe { Variable::from_index(var_index.index() as _) }
        }
    }
}

pub fn type_to_var(
    env: &mut InferenceEnv,
    rank: Rank,
    problems: &mut Vec<TypeError>,
    abilities_store: &mut AbilitiesStore,
    obligation_cache: &mut ObligationCache,
    types: &mut Types,
    aliases: &mut Aliases,
    typ: Index<TypeTag>,
) -> Variable {
    if let TypeTag::Variable(var) = types[typ] {
        var
    } else {
        let mut arena = take_scratchpad();

        let var = type_to_var_help(
            env,
            rank,
            problems,
            abilities_store,
            obligation_cache,
            &arena,
            aliases,
            types,
            typ,
            false,
        );

        arena.reset();
        put_scratchpad(arena);

        var
    }
}

enum RegisterVariable {
    /// Based on the Type, we already know what variable this will be
    Direct(Variable),
    /// This Type needs more complicated Content. We reserve a Variable
    /// for it, but put a placeholder Content in subs
    Deferred,
}

impl RegisterVariable {
    fn from_type(
        env: &mut InferenceEnv,
        rank: Rank,
        arena: &'_ bumpalo::Bump,
        types: &mut Types,
        typ: Index<TypeTag>,
    ) -> Self {
        use RegisterVariable::*;

        match types[typ] {
            TypeTag::Variable(var) => Direct(var),
            TypeTag::EmptyRecord => Direct(Variable::EMPTY_RECORD),
            TypeTag::EmptyTuple => Direct(Variable::EMPTY_TUPLE),
            TypeTag::EmptyTagUnion => Direct(Variable::EMPTY_TAG_UNION),
            TypeTag::DelayedAlias { shared }
            | TypeTag::StructuralAlias { shared, .. }
            | TypeTag::OpaqueAlias { shared, .. } => {
                let AliasShared { symbol, .. } = types[shared];
                if let Some(reserved) = Variable::get_reserved(symbol) {
                    let direct_var = if rank.is_generalized() {
                        // reserved variables are stored with rank NONE
                        reserved
                    } else {
                        // for any other rank, we need to copy; it takes care of adjusting the rank
                        deep_copy_var_in(&mut env.as_solve_env(), rank, reserved, arena)
                    };
                    // Safety: the `destination` will become the source-of-truth for the type index, since it
                    // was not already transformed before (if it was, we'd be in the Variable branch!)
                    let _old_typ = unsafe { types.emplace_variable(typ, direct_var) };
                    return Direct(direct_var);
                }

                Deferred
            }
            _ => Deferred,
        }
    }

    #[inline(always)]
    fn with_stack(
        env: &mut InferenceEnv,
        rank: Rank,
        arena: &'_ bumpalo::Bump,
        types: &mut Types,
        typ_index: Index<TypeTag>,
        stack: &mut bumpalo::collections::Vec<'_, TypeToVar>,
    ) -> Variable {
        match Self::from_type(env, rank, arena, types, typ_index) {
            Self::Direct(var) => var,
            Self::Deferred => {
                let var = env.subs.fresh_unnamed_flex_var();
                // Safety: the `destination` will become the source-of-truth for the type index, since it
                // was not already transformed before (if it was, it wouldn't be deferred!)
                let typ = unsafe { types.emplace_variable(typ_index, var) };
                stack.push(TypeToVar::Defer {
                    typ,
                    typ_index,
                    destination: var,
                    ambient_function: AmbientFunctionPolicy::NoFunction,
                });
                var
            }
        }
    }
}

/// Instantiation of ambient functions in unspecialized lambda sets is somewhat tricky due to other
/// optimizations we have in place. This struct tells us how they should be instantiated.
#[derive(Debug)]
enum AmbientFunctionPolicy {
    /// We're not in a function. This variant may never hold for unspecialized lambda sets.
    NoFunction,
    /// We're in a known function.
    Function(Variable),
}

impl AmbientFunctionPolicy {
    fn link_to_alias_lambda_set_var(&self, subs: &mut Subs, var: Variable) {
        let ambient_function = match self {
            AmbientFunctionPolicy::Function(var) => *var,
            _ => {
                // Might be linked at a deeper point in time, ignore for now
                return;
            }
        };
        let content = subs.get_content_without_compacting(var);
        let new_content = match content {
            Content::LambdaSet(LambdaSet {
                solved,
                recursion_var,
                unspecialized,
                ambient_function: _,
            }) => Content::LambdaSet(LambdaSet {
                solved: *solved,
                recursion_var: *recursion_var,
                unspecialized: *unspecialized,
                ambient_function,
            }),
            Content::FlexVar(_) => {
                // Something like
                //   Encoder fmt <a> : List U8, fmt -a-> List U8 | fmt has EncoderFormatting
                // THEORY: Replace these with empty lambda sets. They will unify the same as a flex
                // var does, but allows us to record the ambient function properly.
                Content::LambdaSet(LambdaSet {
                    solved: UnionLabels::default(),
                    recursion_var: OptVariable::NONE,
                    unspecialized: SubsSlice::default(),
                    ambient_function,
                })
            }
            content => internal_error!("{:?}({:?}) not a lambda set", content, var),
        };
        subs.set_content_unchecked(var, new_content);
    }
}

#[derive(Debug)]
enum TypeToVar {
    Defer {
        typ: TypeTag,
        typ_index: Index<TypeTag>,
        destination: Variable,
        ambient_function: AmbientFunctionPolicy,
    },
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn type_to_var_help(
    env: &mut InferenceEnv,
    rank: Rank,
    problems: &mut Vec<TypeError>,
    abilities_store: &AbilitiesStore,
    obligation_cache: &mut ObligationCache,
    arena: &bumpalo::Bump,
    aliases: &mut Aliases,
    types: &mut Types,
    typ: Index<TypeTag>,
    // Helpers for instantiating ambient functions of lambda set variables from type aliases.
    is_alias_lambda_set_arg: bool,
) -> Variable {
    use bumpalo::collections::Vec;

    let mut stack = Vec::with_capacity_in(8, arena);
    let mut bind_to_abilities = Vec::new_in(arena);

    macro_rules! helper {
        ($typ:expr, $ambient_function_policy:expr) => {{
            match RegisterVariable::from_type(env, rank, arena, types, $typ) {
                RegisterVariable::Direct(var) => {
                    // If the variable is just a type variable but we know we're in a lambda set
                    // context, try to link to the ambient function.
                    $ambient_function_policy.link_to_alias_lambda_set_var(env.subs, var);

                    var
                }
                RegisterVariable::Deferred => {
                    let var = env.subs.fresh_unnamed_flex_var();

                    // Safety: the `destination` will become the source-of-truth for the type index, since it
                    // was not already transformed before (if it was, it wouldn't be deferred!)
                    let typ = unsafe { types.emplace_variable($typ, var) };

                    stack.push(TypeToVar::Defer {
                        typ,
                        typ_index: $typ,
                        destination: var,
                        ambient_function: $ambient_function_policy,
                    });

                    var
                }
            }
        }};
        ($typ:expr) => {{
            helper!($typ, AmbientFunctionPolicy::NoFunction)
        }};
    }

    let result = helper!(typ);

    while let Some(TypeToVar::Defer {
        typ_index,
        typ,
        destination,
        ambient_function,
    }) = stack.pop()
    {
        use TypeTag::*;
        match typ {
            Variable(_) | EmptyRecord | EmptyTuple | EmptyTagUnion => {
                unreachable!("This variant should never be deferred!",)
            }
            RangedNumber(range) => {
                let content = Content::RangedNumber(range);

                env.register_with_known_var(destination, rank, content)
            }
            Apply {
                symbol,
                type_argument_regions: _,
                region: _,
            } => {
                let arguments = types.get_type_arguments(typ_index);
                let new_arguments = env.subs.reserve_into_vars(arguments.len());
                for (target_index, var_index) in
                    (new_arguments.indices()).zip(arguments.into_iter())
                {
                    let var = helper!(var_index);
                    env.subs.variables[target_index] = var;
                }

                let flat_type = FlatType::Apply(symbol, new_arguments);
                let content = Content::Structure(flat_type);

                env.register_with_known_var(destination, rank, content)
            }

            ClosureTag {
                name,
                ambient_function,
            } => {
                match env.function_kind {
                    FunctionKind::LambdaSet => {
                        let captures = types.get_type_arguments(typ_index);
                        let union_lambdas = create_union_lambda(
                            env, rank, arena, types, name, captures, &mut stack,
                        );

                        let content = Content::LambdaSet(subs::LambdaSet {
                            solved: union_lambdas,
                            // We may figure out the lambda set is recursive during solving, but it never
                            // is to begin with.
                            recursion_var: OptVariable::NONE,
                            unspecialized: SubsSlice::default(),
                            ambient_function,
                        });

                        env.register_with_known_var(destination, rank, content)
                    }
                    FunctionKind::Erased => {
                        // TODO(erased-lambda): can we merge in with Variable::ERASED_LAMBDA instead?
                        env.register_with_known_var(destination, rank, Content::ErasedLambda)
                    }
                }
            }
            UnspecializedLambdaSet { unspecialized } => {
                let unspecialized_slice = slice_extend_new(
                    &mut env.subs.unspecialized_lambda_sets,
                    std::iter::once(unspecialized),
                );

                // `ClosureTag` ambient functions are resolved during constraint generation.
                // But `UnspecializedLambdaSet`s can only ever live in a type signature, and don't
                // correspond to a expression, so they are never constrained.
                // Instead, we resolve their ambient functions during type translation, observing
                // the invariant that a lambda set can only ever appear under a function type.
                let ambient_function = match ambient_function {
                    AmbientFunctionPolicy::NoFunction => {
                        debug_assert!(is_alias_lambda_set_arg);
                        // To be filled in during delayed type alias instantiation
                        roc_types::subs::Variable::NULL
                    }
                    AmbientFunctionPolicy::Function(var) => var,
                };

                let content = Content::LambdaSet(subs::LambdaSet {
                    unspecialized: unspecialized_slice,
                    solved: UnionLabels::default(),
                    recursion_var: OptVariable::NONE,
                    ambient_function,
                });

                env.register_with_known_var(destination, rank, content)
            }
            // This case is important for the rank of boolean variables
            Function(closure_type, ret_type, fx_type) => {
                let arguments = types.get_type_arguments(typ_index);
                let new_arguments = env.subs.reserve_into_vars(arguments.len());
                for (target_index, var_index) in
                    (new_arguments.indices()).zip(arguments.into_iter())
                {
                    let var = helper!(var_index);
                    env.subs.variables[target_index] = var;
                }

                let ret_var = helper!(ret_type);
                let fx_var = helper!(fx_type);
                let closure_var =
                    helper!(closure_type, AmbientFunctionPolicy::Function(destination));
                let content =
                    Content::Structure(FlatType::Func(new_arguments, closure_var, ret_var, fx_var));

                env.register_with_known_var(destination, rank, content)
            }
            Record(fields) => {
                let ext_slice = types.get_type_arguments(typ_index);

                // An empty fields is inefficient (but would be correct)
                // If hit, try to turn the value into an EmptyRecord in canonicalization
                debug_assert!(!fields.is_empty() || !ext_slice.is_empty());

                let mut field_vars = Vec::with_capacity_in(fields.len(), arena);

                let (fields_names, field_kinds, field_tys) = types.record_fields_slices(fields);

                for ((field, field_kind), field_type) in (fields_names.into_iter())
                    .zip(field_kinds.into_iter())
                    .zip(field_tys.into_iter())
                {
                    let field_var = {
                        let t = helper!(field_type);
                        types[field_kind].replace(t)
                    };

                    field_vars.push((types[field].clone(), field_var));
                }

                debug_assert!(ext_slice.len() <= 1);
                let temp_ext_var = match ext_slice.into_iter().next() {
                    None => roc_types::subs::Variable::EMPTY_RECORD,
                    Some(ext) => helper!(ext),
                };

                let (it, new_ext_var) =
                    gather_fields_unsorted_iter(env.subs, RecordFields::empty(), temp_ext_var)
                        .expect("Something ended up weird in this record type");

                let it = it
                    .into_iter()
                    .map(|(field, field_type)| (field.clone(), field_type));

                field_vars.extend(it);
                insertion_sort_by(&mut field_vars, RecordFields::compare);

                let record_fields = RecordFields::insert_into_subs(env.subs, field_vars);

                let content = Content::Structure(FlatType::Record(record_fields, new_ext_var));

                env.register_with_known_var(destination, rank, content)
            }

            Tuple(elems) => {
                let ext_slice = types.get_type_arguments(typ_index);

                // Elems should never be empty; we don't support empty tuples
                debug_assert!(!elems.is_empty() || !ext_slice.is_empty());

                let mut elem_vars = Vec::with_capacity_in(elems.len(), arena);

                let (indices, elem_tys) = types.tuple_elems_slices(elems);

                for (index, elem_type) in indices.into_iter().zip(elem_tys.into_iter()) {
                    let elem_var = helper!(elem_type);
                    elem_vars.push((types[index], elem_var));
                }

                debug_assert!(ext_slice.len() <= 1);
                let temp_ext_var = match ext_slice.into_iter().next() {
                    None => roc_types::subs::Variable::EMPTY_TUPLE,
                    Some(ext) => helper!(ext),
                };

                let (it, new_ext_var) =
                    gather_tuple_elems_unsorted_iter(env.subs, TupleElems::empty(), temp_ext_var)
                        .expect("Something ended up weird in this tuple type");

                elem_vars.extend(it);
                let tuple_elems = TupleElems::insert_into_subs(env.subs, elem_vars);

                let content = Content::Structure(FlatType::Tuple(tuple_elems, new_ext_var));

                env.register_with_known_var(destination, rank, content)
            }

            TagUnion(tags, ext_openness) => {
                let ext_slice = types.get_type_arguments(typ_index);

                // An empty tags is inefficient (but would be correct)
                // If hit, try to turn the value into an EmptyTagUnion in canonicalization
                debug_assert!(!tags.is_empty() || !ext_slice.is_empty());

                let (union_tags, ext) = type_to_union_tags(
                    env,
                    rank,
                    arena,
                    types,
                    tags,
                    ext_slice,
                    ext_openness,
                    &mut stack,
                );
                let content = Content::Structure(FlatType::TagUnion(union_tags, ext));

                env.register_with_known_var(destination, rank, content)
            }
            FunctionOrTagUnion(symbol, ext_openness) => {
                let ext_slice = types.get_type_arguments(typ_index);
                let tag_name = types.get_tag_name(&typ_index).clone();

                debug_assert!(ext_slice.len() <= 1);
                let temp_ext = match ext_slice.into_iter().next() {
                    Some(ext) => {
                        let var = helper!(ext);
                        TagExt::from_can(var, ext_openness)
                    }
                    None => TagExt::Any(roc_types::subs::Variable::EMPTY_TAG_UNION),
                };

                let (it, ext) = roc_types::types::gather_tags_unsorted_iter(
                    env.subs,
                    UnionTags::default(),
                    temp_ext,
                )
                .expect("extension var could not be seen as a tag union");

                #[allow(clippy::never_loop)]
                for _ in it {
                    unreachable!("we assert that the ext var is empty; otherwise we'd already know it was a tag union!");
                }

                let tag_names = slice_extend_new(&mut env.subs.tag_names, [tag_name]);
                let symbols = slice_extend_new(&mut env.subs.symbol_names, [symbol]);

                let content =
                    Content::Structure(FlatType::FunctionOrTagUnion(tag_names, symbols, ext));

                env.register_with_known_var(destination, rank, content)
            }
            RecursiveTagUnion(rec_var, tags, ext_openness) => {
                let ext_slice = types.get_type_arguments(typ_index);

                // An empty tags is inefficient (but would be correct)
                // If hit, try to turn the value into an EmptyTagUnion in canonicalization
                debug_assert!(!tags.is_empty() || !ext_slice.is_empty());

                let (union_tags, ext) = type_to_union_tags(
                    env,
                    rank,
                    arena,
                    types,
                    tags,
                    ext_slice,
                    ext_openness,
                    &mut stack,
                );
                let content =
                    Content::Structure(FlatType::RecursiveTagUnion(rec_var, union_tags, ext));

                let tag_union_var = destination;
                env.register_with_known_var(tag_union_var, rank, content);

                env.register_with_known_var(
                    rec_var,
                    rank,
                    Content::RecursionVar {
                        opt_name: None,
                        structure: tag_union_var,
                    },
                );

                tag_union_var
            }

            DelayedAlias { shared } => {
                let AliasShared {
                    symbol,
                    type_argument_abilities,
                    type_argument_regions,
                    lambda_set_variables,
                    infer_ext_in_output_variables,
                } = types[shared];

                let type_arguments = types.get_type_arguments(typ_index);

                let alias_variables = {
                    let all_vars_length = type_arguments.len()
                        + lambda_set_variables.len()
                        + infer_ext_in_output_variables.len();
                    let new_variables = env.subs.reserve_into_vars(all_vars_length);

                    let type_arguments_offset = 0;
                    let lambda_set_vars_offset = type_arguments_offset + type_arguments.len();
                    let infer_ext_vars_offset = lambda_set_vars_offset + lambda_set_variables.len();

                    for (((target_index, arg_type), arg_region), abilities) in
                        (new_variables.indices().skip(type_arguments_offset))
                            .zip(type_arguments.into_iter())
                            .zip(type_argument_regions.into_iter())
                            .zip(type_argument_abilities.into_iter())
                    {
                        let copy_var = helper!(arg_type);
                        env.subs.variables[target_index] = copy_var;
                        if !types[abilities].is_empty() {
                            let arg_region = types[arg_region];
                            bind_to_abilities.push((Loc::at(arg_region, copy_var), abilities));
                        }
                    }

                    let it = (new_variables.indices().skip(lambda_set_vars_offset))
                        .zip(lambda_set_variables.into_iter());
                    for (target_index, ls) in it {
                        // We MUST do this now, otherwise when linking the ambient function during
                        // instantiation of the real var, there will be nothing to link against.
                        let copy_var = type_to_var_help(
                            env,
                            rank,
                            problems,
                            abilities_store,
                            obligation_cache,
                            arena,
                            aliases,
                            types,
                            ls,
                            true,
                        );
                        env.subs.variables[target_index] = copy_var;
                    }

                    let it = (new_variables.indices().skip(infer_ext_vars_offset))
                        .zip(infer_ext_in_output_variables.into_iter());
                    for (target_index, ext_typ) in it {
                        let copy_var = helper!(ext_typ);
                        env.subs.variables[target_index] = copy_var;
                    }

                    AliasVariables {
                        variables_start: new_variables.start(),
                        type_variables_len: type_arguments.len() as _,
                        lambda_set_variables_len: lambda_set_variables.len() as _,
                        all_variables_len: all_vars_length as _,
                    }
                };

                let (alias_variable, kind) = aliases.instantiate_real_var(
                    env,
                    rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    arena,
                    types,
                    symbol,
                    alias_variables,
                );

                let content = Content::Alias(symbol, alias_variables, alias_variable, kind);

                env.register_with_known_var(destination, rank, content)
            }

            StructuralAlias { shared, actual } | OpaqueAlias { shared, actual } => {
                let kind = match typ {
                    StructuralAlias { .. } => AliasKind::Structural,
                    OpaqueAlias { .. } => AliasKind::Opaque,
                    _ => internal_error!(),
                };

                let AliasShared {
                    symbol,
                    type_argument_abilities,
                    type_argument_regions,
                    lambda_set_variables,
                    infer_ext_in_output_variables,
                } = types[shared];

                debug_assert!(roc_types::subs::Variable::get_reserved(symbol).is_none());

                let type_arguments = types.get_type_arguments(typ_index);

                let alias_variables = {
                    let all_vars_length = type_arguments.len()
                        + lambda_set_variables.len()
                        + infer_ext_in_output_variables.len();

                    let type_arguments_offset = 0;
                    let lambda_set_vars_offset = type_arguments_offset + type_arguments.len();
                    let infer_ext_vars_offset = lambda_set_vars_offset + lambda_set_variables.len();

                    let new_variables = env.subs.reserve_into_vars(all_vars_length);

                    for (((target_index, typ), region), abilities) in
                        (new_variables.indices().skip(type_arguments_offset))
                            .zip(type_arguments.into_iter())
                            .zip(type_argument_regions.into_iter())
                            .zip(type_argument_abilities.into_iter())
                    {
                        let copy_var = helper!(typ);
                        env.subs.variables[target_index] = copy_var;
                        if !types[abilities].is_empty() {
                            let region = types[region];
                            bind_to_abilities.push((Loc::at(region, copy_var), abilities));
                        }
                    }

                    let it = (new_variables.indices().skip(lambda_set_vars_offset))
                        .zip(lambda_set_variables.into_iter());
                    for (target_index, ls) in it {
                        let copy_var = helper!(ls);
                        env.subs.variables[target_index] = copy_var;
                    }

                    let it = (new_variables.indices().skip(infer_ext_vars_offset))
                        .zip(infer_ext_in_output_variables.into_iter());
                    for (target_index, ext_typ) in it {
                        let copy_var = helper!(ext_typ);
                        env.subs.variables[target_index] = copy_var;
                    }

                    AliasVariables {
                        variables_start: new_variables.start(),
                        type_variables_len: type_arguments.len() as _,
                        lambda_set_variables_len: lambda_set_variables.len() as _,
                        all_variables_len: all_vars_length as _,
                    }
                };

                let alias_variable = if let Symbol::RESULT_RESULT = symbol {
                    roc_result_to_var(env, rank, arena, types, actual, &mut stack)
                } else {
                    helper!(actual)
                };
                let content = Content::Alias(symbol, alias_variables, alias_variable, kind);

                env.register_with_known_var(destination, rank, content)
            }
            Pure => {
                let content = Content::Pure;

                env.register_with_known_var(destination, rank, content)
            }
            Effectful => {
                let content = Content::Effectful;

                env.register_with_known_var(destination, rank, content)
            }
            Error => {
                let content = Content::Error;

                env.register_with_known_var(destination, rank, content)
            }
        };
    }

    for (Loc { value: var, region }, abilities) in bind_to_abilities {
        let abilities = &types[abilities];
        match *env.subs.get_content_unchecked(var) {
            Content::RigidVar(a) => {
                // TODO(multi-abilities): check run cache
                let abilities_slice =
                    slice_extend_new(&mut env.subs.symbol_names, abilities.sorted_iter().copied());
                env.subs
                    .set_content(var, Content::RigidAbleVar(a, abilities_slice));
            }
            Content::RigidAbleVar(_, abs)
                if (env.subs.get_subs_slice(abs).iter()).eq(abilities.sorted_iter()) =>
            {
                // pass, already bound
            }
            _ => {
                let abilities_slice =
                    slice_extend_new(&mut env.subs.symbol_names, abilities.sorted_iter().copied());

                let flex_ability = env.register(rank, Content::FlexAbleVar(None, abilities_slice));

                let category = Category::OpaqueArg;
                match unify(
                    &mut env.uenv(),
                    var,
                    flex_ability,
                    UnificationMode::EQ,
                    Polarity::OF_VALUE,
                ) {
                    Unified::Success {
                        vars: _,
                        must_implement_ability,
                        lambda_sets_to_specialize,
                        extra_metadata: _,
                    } => {
                        // No introduction needed

                        if !must_implement_ability.is_empty() {
                            let new_problems = obligation_cache.check_obligations(
                                env.subs,
                                abilities_store,
                                must_implement_ability,
                                AbilityImplError::BadExpr(region, category, flex_ability),
                            );
                            problems.extend(new_problems);
                        }
                        debug_assert!(lambda_sets_to_specialize
                            .drain()
                            .all(|(_, vals)| vals.is_empty()));
                    }
                    Unified::Failure(_vars, actual_type, expected_type, _bad_impls) => {
                        // No introduction needed

                        let problem = TypeError::BadExpr(
                            region,
                            category,
                            actual_type,
                            Expected::NoExpectation(expected_type),
                        );

                        problems.push(problem);
                    }
                }
            }
        }
    }

    result
}

#[inline(always)]
fn roc_result_to_var(
    env: &mut InferenceEnv,
    rank: Rank,
    arena: &'_ bumpalo::Bump,
    types: &mut Types,
    result_type: Index<TypeTag>,
    stack: &mut bumpalo::collections::Vec<'_, TypeToVar>,
) -> Variable {
    match types[result_type] {
        TypeTag::TagUnion(tags, _ext_openness) => {
            let ext_slice = types.get_type_arguments(result_type);

            debug_assert!(ext_slice.is_empty());
            debug_assert!(tags.len() == 2);

            let (tags_slice, payload_slices_slice) = types.union_tag_slices(tags);

            if let ([err, ok], [err_args, ok_args]) =
                (&types[tags_slice], &types[payload_slices_slice])
            {
                debug_assert_eq!(err, &env.subs.tag_names[0]);
                debug_assert_eq!(ok, &env.subs.tag_names[1]);

                debug_assert_eq!(err_args.len(), 1);
                debug_assert_eq!(ok_args.len(), 1);

                if let (Some(err_type), Some(ok_type)) =
                    (err_args.into_iter().next(), ok_args.into_iter().next())
                {
                    let err_var =
                        RegisterVariable::with_stack(env, rank, arena, types, err_type, stack);
                    let ok_var =
                        RegisterVariable::with_stack(env, rank, arena, types, ok_type, stack);

                    let start = env.subs.variables.len() as u32;
                    let err_slice = SubsSlice::new(start, 1);
                    let ok_slice = SubsSlice::new(start + 1, 1);

                    env.subs.variables.push(err_var);
                    env.subs.variables.push(ok_var);

                    let variables = SubsSlice::new(env.subs.variable_slices.len() as _, 2);
                    env.subs.variable_slices.push(err_slice);
                    env.subs.variable_slices.push(ok_slice);

                    let union_tags = UnionTags::from_slices(Subs::RESULT_TAG_NAMES, variables);
                    let ext = TagExt::Any(Variable::EMPTY_TAG_UNION);

                    let content = Content::Structure(FlatType::TagUnion(union_tags, ext));

                    return env.register(rank, content);
                }
            }

            unreachable!("invalid arguments to Result.Result; canonicalization should catch this!")
        }
        _ => unreachable!("not a valid type inside a Result.Result alias"),
    }
}

fn insertion_sort_by<T, F>(arr: &mut [T], mut compare: F)
where
    F: FnMut(&T, &T) -> std::cmp::Ordering,
{
    for i in 1..arr.len() {
        let val = &arr[i];
        let mut j = i;
        let pos = arr[..i]
            .binary_search_by(|x| compare(x, val))
            .unwrap_or_else(|pos| pos);
        // Swap all elements until specific position.
        while j > pos {
            arr.swap(j - 1, j);
            j -= 1;
        }
    }
}

fn sorted_no_duplicate_tags(tag_slices: &[TagName]) -> bool {
    match tag_slices.split_first() {
        None => true,
        Some((first, rest)) => {
            let mut current = first;

            for next in rest {
                if current >= next {
                    return false;
                } else {
                    current = next;
                }
            }

            true
        }
    }
}

fn sort_and_deduplicate<T>(tag_vars: &mut bumpalo::collections::Vec<(TagName, T)>) {
    insertion_sort_by(tag_vars, |(a, _), (b, _)| a.cmp(b));

    // deduplicate, keeping the right-most occurrence of a tag name
    let mut i = 0;

    while i < tag_vars.len() {
        match (tag_vars.get(i), tag_vars.get(i + 1)) {
            (Some((t1, _)), Some((t2, _))) => {
                if t1 == t2 {
                    tag_vars.remove(i);
                } else {
                    i += 1;
                }
            }
            _ => break,
        }
    }
}

/// Find whether the current run of tag names is in the subs.tag_names array already. If so,
/// we take a SubsSlice to the existing tag names, so we don't have to add/clone those tag names
/// and keep subs memory consumption low
fn find_tag_name_run(slice: &[TagName], subs: &mut Subs) -> Option<SubsSlice<TagName>> {
    use std::cmp::Ordering;

    let tag_name = slice.first()?;

    let mut result = None;

    // the `SubsSlice<TagName>` that inserting `slice` into subs would give
    let bigger_slice = SubsSlice::new(subs.tag_names.len() as _, slice.len() as _);

    match subs.tag_name_cache.get_mut(tag_name) {
        Some(occupied) => {
            let subs_slice = *occupied;

            let prefix_slice = SubsSlice::new(subs_slice.start(), slice.len() as _);

            if slice.len() == 1 {
                return Some(prefix_slice);
            }

            match slice.len().cmp(&subs_slice.len()) {
                Ordering::Less => {
                    // we might have a prefix
                    let tag_names = &subs.tag_names[subs_slice.start() as usize..];

                    for (from_subs, from_slice) in tag_names.iter().zip(slice.iter()) {
                        if from_subs != from_slice {
                            return None;
                        }
                    }

                    result = Some(prefix_slice);
                }
                Ordering::Equal => {
                    let tag_names = &subs.tag_names[subs_slice.indices()];

                    for (from_subs, from_slice) in tag_names.iter().zip(slice.iter()) {
                        if from_subs != from_slice {
                            return None;
                        }
                    }

                    result = Some(subs_slice);
                }
                Ordering::Greater => {
                    // switch to the bigger slice that is not inserted yet, but will be soon
                    *occupied = bigger_slice;
                }
            }
        }
        None => {
            subs.tag_name_cache.push(tag_name, bigger_slice);
        }
    }

    result
}

#[inline(always)]
fn register_tag_arguments(
    env: &mut InferenceEnv,
    rank: Rank,
    arena: &'_ bumpalo::Bump,
    types: &mut Types,
    stack: &mut bumpalo::collections::Vec<'_, TypeToVar>,
    arguments: Slice<TypeTag>,
) -> VariableSubsSlice {
    if arguments.is_empty() {
        VariableSubsSlice::default()
    } else {
        let new_variables = env.subs.reserve_into_vars(arguments.len());
        let it = new_variables.indices().zip(arguments);

        for (target_index, argument) in it {
            let var = RegisterVariable::with_stack(env, rank, arena, types, argument, stack);
            env.subs.variables[target_index] = var;
        }

        new_variables
    }
}

/// Assumes that the tags are sorted and there are no duplicates!
fn insert_tags_fast_path(
    env: &mut InferenceEnv,
    rank: Rank,
    arena: &'_ bumpalo::Bump,
    types: &mut Types,
    union_tags: UnionTags,
    stack: &mut bumpalo::collections::Vec<'_, TypeToVar>,
) -> UnionTags {
    let (tags, payload_slices) = types.union_tag_slices(union_tags);

    debug_assert_eq!(tags.len(), payload_slices.len());

    if let [arguments_slice] = &types[payload_slices] {
        let arguments_slice = *arguments_slice;

        let variable_slice =
            register_tag_arguments(env, rank, arena, types, stack, arguments_slice);

        let new_variable_slices = slice_extend_new(&mut env.subs.variable_slices, [variable_slice]);

        macro_rules! subs_tag_name {
            ($tag_name_slice:expr) => {
                return UnionTags::from_slices($tag_name_slice, new_variable_slices)
            };
        }

        match types[tags][0].0.as_str() {
            "Ok" => subs_tag_name!(Subs::TAG_NAME_OK.as_slice()),
            "Err" => subs_tag_name!(Subs::TAG_NAME_ERR.as_slice()),
            "InvalidNumStr" => subs_tag_name!(Subs::TAG_NAME_INVALID_NUM_STR.as_slice()),
            "BadUtf8" => subs_tag_name!(Subs::TAG_NAME_BAD_UTF_8.as_slice()),
            "OutOfBounds" => subs_tag_name!(Subs::TAG_NAME_OUT_OF_BOUNDS.as_slice()),
            _other => {}
        }
    }

    let new_variable_slices = env.subs.reserve_variable_slices(tags.len());
    match find_tag_name_run(&types[tags], env.subs) {
        Some(new_tag_names) => {
            let it = (new_variable_slices.indices()).zip(payload_slices);

            for (variable_slice_index, arguments_index) in it {
                let arguments = types[arguments_index];
                env.subs.variable_slices[variable_slice_index] =
                    register_tag_arguments(env, rank, arena, types, stack, arguments);
            }

            UnionTags::from_slices(new_tag_names, new_variable_slices)
        }
        None => {
            let new_tag_names = env.subs.reserve_tag_names(tags.len());

            let it = (new_variable_slices.indices())
                .zip(new_tag_names.indices())
                .zip(tags)
                .zip(payload_slices);

            for (((variable_slice_index, tag_name_index), tag_name), arguments_index) in it {
                let arguments = types[arguments_index];
                env.subs.variable_slices[variable_slice_index] =
                    register_tag_arguments(env, rank, arena, types, stack, arguments);

                env.subs.tag_names[tag_name_index] = types[tag_name].clone();
            }

            UnionTags::from_slices(new_tag_names, new_variable_slices)
        }
    }
}

fn insert_tags_slow_path(
    env: &mut InferenceEnv,
    rank: Rank,
    arena: &'_ bumpalo::Bump,
    types: &mut Types,
    union_tags: UnionTags,
    mut tag_vars: bumpalo::collections::Vec<(TagName, VariableSubsSlice)>,
    stack: &mut bumpalo::collections::Vec<'_, TypeToVar>,
) -> UnionTags {
    let (tags, payload_slices) = types.union_tag_slices(union_tags);

    for (tag_index, tag_argument_types_index) in (tags.into_iter()).zip(payload_slices.into_iter())
    {
        let tag_argument_types = &types[tag_argument_types_index];

        let new_slice = env.subs.reserve_into_vars(tag_argument_types.len());

        for (i, arg) in (new_slice.indices()).zip(tag_argument_types.into_iter()) {
            let var = RegisterVariable::with_stack(env, rank, arena, types, arg, stack);
            env.subs.variables[i] = var;
        }

        tag_vars.push((types[tag_index].clone(), new_slice));
    }

    sort_and_deduplicate(&mut tag_vars);

    UnionTags::insert_slices_into_subs(env.subs, tag_vars)
}

fn type_to_union_tags(
    env: &mut InferenceEnv,
    rank: Rank,
    arena: &'_ bumpalo::Bump,
    types: &mut Types,
    union_tags: UnionTags,
    opt_ext_slice: Slice<TypeTag>,
    ext_openness: ExtImplicitOpenness,
    stack: &mut bumpalo::collections::Vec<'_, TypeToVar>,
) -> (UnionTags, TagExt) {
    use bumpalo::collections::Vec;

    let (tags, _) = types.union_tag_slices(union_tags);

    let sorted = tags.len() == 1 || sorted_no_duplicate_tags(&types[tags]);

    debug_assert!(opt_ext_slice.len() <= 1);

    match opt_ext_slice.into_iter().next() {
        None => {
            let ext = Variable::EMPTY_TAG_UNION;

            let union_tags = if sorted {
                insert_tags_fast_path(env, rank, arena, types, union_tags, stack)
            } else {
                let tag_vars = Vec::with_capacity_in(tags.len(), arena);
                insert_tags_slow_path(env, rank, arena, types, union_tags, tag_vars, stack)
            };

            (union_tags, TagExt::Any(ext))
        }
        Some(ext) => {
            let mut tag_vars = Vec::with_capacity_in(tags.len(), arena);

            let temp_ext = {
                let temp_ext_var =
                    RegisterVariable::with_stack(env, rank, arena, types, ext, stack);
                TagExt::from_can(temp_ext_var, ext_openness)
            };
            let (it, ext) = roc_types::types::gather_tags_unsorted_iter(
                env.subs,
                UnionTags::default(),
                temp_ext,
            )
            .expect("extension var could not be seen as tag union");

            tag_vars.extend(it.map(|(n, v)| (n.clone(), v)));

            let union_tags = if tag_vars.is_empty() && sorted {
                insert_tags_fast_path(env, rank, arena, types, union_tags, stack)
            } else {
                insert_tags_slow_path(env, rank, arena, types, union_tags, tag_vars, stack)
            };

            (union_tags, ext)
        }
    }
}

fn create_union_lambda(
    env: &mut InferenceEnv,
    rank: Rank,
    arena: &'_ bumpalo::Bump,
    types: &mut Types,
    closure: Symbol,
    capture_types: Slice<TypeTag>,
    stack: &mut bumpalo::collections::Vec<'_, TypeToVar>,
) -> UnionLambdas {
    let variable_slice = register_tag_arguments(env, rank, arena, types, stack, capture_types);
    let new_variable_slices = slice_extend_new(&mut env.subs.variable_slices, [variable_slice]);

    let lambda_name_slice = slice_extend_new(&mut env.subs.symbol_names, [closure]);

    UnionLambdas::from_slices(lambda_name_slice, new_variable_slices)
}
