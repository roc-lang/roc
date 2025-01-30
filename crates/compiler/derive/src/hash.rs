//! Derivers for the `Hash` ability.

use std::iter::once;

use roc_can::{
    expr::{AnnotatedMark, ClosureData, Expr, IntValue, Recursive, WhenBranch, WhenBranchPattern},
    num::{IntBound, IntLitWidth},
    pattern::Pattern,
};
use roc_collections::soa::{index_push_new, slice_extend_new};
use roc_derive_key::hash::FlatHashKey;
use roc_error_macros::internal_error;
use roc_module::{
    called_via::CalledVia,
    ident::{Lowercase, TagName},
    symbol::Symbol,
};
use roc_region::all::{Loc, Region};
use roc_types::{
    num::int_lit_width_to_variable,
    subs::{
        Content, ExhaustiveMark, FlatType, GetSubsSlice, LambdaSet, OptVariable, RecordFields,
        RedundantMark, Subs, SubsSlice, TagExt, TupleElems, UnionLambdas, UnionTags, Variable,
    },
    types::RecordField,
};

use crate::{synth_var, util::Env, DerivedBody};

pub(crate) fn derive_hash(env: &mut Env<'_>, key: FlatHashKey, def_symbol: Symbol) -> DerivedBody {
    let (body_type, body) = match key {
        FlatHashKey::Record(fields) => hash_record(env, def_symbol, fields),
        FlatHashKey::Tuple(arity) => hash_tuple(env, def_symbol, arity),
        FlatHashKey::TagUnion(tags) => {
            if tags.len() == 1 {
                hash_newtype_tag_union(env, def_symbol, tags.into_iter().next().unwrap())
            } else {
                hash_tag_union(env, def_symbol, tags)
            }
        }
    };

    let specialization_lambda_sets =
        env.get_specialization_lambda_sets(body_type, Symbol::HASH_HASH);

    DerivedBody {
        body,
        body_type,
        specialization_lambda_sets,
    }
}

fn hash_record(env: &mut Env<'_>, fn_name: Symbol, fields: Vec<Lowercase>) -> (Variable, Expr) {
    // Suppose rcd = { f1, ..., fn }.
    // Build a generalized type t_rcd = { f1: t1, ..., fn: tn }, with fresh t1, ..., tn,
    // so that we can re-use the derived impl for many records of the same fields.
    let (record_var, record_fields) = {
        let flex_fields = fields
            .into_iter()
            .map(|name| {
                (
                    name,
                    RecordField::Required(env.subs.fresh_unnamed_flex_var()),
                )
            })
            .collect::<Vec<(Lowercase, _)>>();
        let fields = RecordFields::insert_into_subs(env.subs, flex_fields);
        let record_var = synth_var(
            env.subs,
            Content::Structure(FlatType::Record(fields, Variable::EMPTY_RECORD)),
        );

        (record_var, fields)
    };

    // Now, a hasher for this record is
    //
    // hash_rcd : hasher, { f1: t1, ..., fn: tn } -> hasher where hasher implements Hasher
    // hash_rcd = \hasher, rcd ->
    //   Hash.hash (
    //      Hash.hash
    //          ...
    //          (Hash.hash hasher rcd.f1)
    //          ...
    //      rcd.f_n1)
    //   rcd.fn
    //
    // So, just a build a fold travelling up the fields.
    let rcd_sym = env.new_symbol("rcd");

    let hasher_sym = env.new_symbol("hasher");
    let hasher_var = synth_var(env.subs, Content::FlexAbleVar(None, Subs::AB_HASHER));

    let (body_var, body) = record_fields.iter_all().fold(
        (hasher_var, Expr::Var(hasher_sym, hasher_var)),
        |total_hasher, (field_name, field_var, _)| {
            let field_name = env.subs[field_name].clone();
            let field_var = env.subs[field_var];

            let field_access = Expr::RecordAccess {
                record_var,
                field_var,
                ext_var: env.subs.fresh_unnamed_flex_var(),
                loc_expr: Box::new(Loc::at_zero(Expr::Var(
                    rcd_sym,
                    env.subs.fresh_unnamed_flex_var(),
                ))),
                field: field_name,
            };

            call_hash_hash(env, total_hasher, (field_var, field_access))
        },
    );

    // Finally, build the closure
    // \hasher, rcd -> body
    build_outer_derived_closure(
        env,
        fn_name,
        (hasher_var, hasher_sym),
        (record_var, Pattern::Identifier(rcd_sym)),
        (body_var, body),
    )
}

fn hash_tuple(env: &mut Env<'_>, fn_name: Symbol, arity: u32) -> (Variable, Expr) {
    // Suppose tup = (v1, ..., vn).
    // Build a generalized type t_tup = (t1, ..., tn), with fresh t1, ..., tn,
    // so that we can re-use the derived impl for many tuples of the same arity.
    let (tuple_var, tuple_elems) = {
        // TODO: avoid an allocation here by pre-allocating the indices and variables `TupleElems`
        // will be instantiated with.
        let flex_elems: Vec<_> = (0..arity)
            .map(|i| (i as usize, env.subs.fresh_unnamed_flex_var()))
            .collect();
        let elems = TupleElems::insert_into_subs(env.subs, flex_elems);
        let tuple_var = synth_var(
            env.subs,
            Content::Structure(FlatType::Tuple(elems, Variable::EMPTY_TUPLE)),
        );

        (tuple_var, elems)
    };

    // Now, a hasher for this tuple is
    //
    // hash_tup : hasher, (t1, ..., tn) -> hasher where hasher implements Hasher
    // hash_tup = \hasher, tup ->
    //   Hash.hash (
    //      Hash.hash
    //          ...
    //          (Hash.hash hasher tup.0)
    //          ...
    //      tup.n1)
    //   tup.n
    //
    // So, just a build a fold travelling up the elements.
    let tup_sym = env.new_symbol("tup");

    let hasher_sym = env.new_symbol("hasher");
    let hasher_var = synth_var(env.subs, Content::FlexAbleVar(None, Subs::AB_HASHER));

    let (body_var, body) = tuple_elems.iter_all().fold(
        (hasher_var, Expr::Var(hasher_sym, hasher_var)),
        |total_hasher, (elem_idx, elem_var)| {
            let index = env.subs[elem_idx];
            let elem_var = env.subs[elem_var];

            let elem_access = Expr::TupleAccess {
                tuple_var,
                elem_var,
                ext_var: env.subs.fresh_unnamed_flex_var(),
                loc_expr: Box::new(Loc::at_zero(Expr::Var(
                    tup_sym,
                    env.subs.fresh_unnamed_flex_var(),
                ))),
                index,
            };

            call_hash_hash(env, total_hasher, (elem_var, elem_access))
        },
    );

    // Finally, build the closure
    // \hasher, rcd -> body
    build_outer_derived_closure(
        env,
        fn_name,
        (hasher_var, hasher_sym),
        (tuple_var, Pattern::Identifier(tup_sym)),
        (body_var, body),
    )
}

/// Build a `hash` implementation for a non-singleton tag union.
fn hash_tag_union(
    env: &mut Env<'_>,
    fn_name: Symbol,
    tags: Vec<(TagName, u16)>,
) -> (Variable, Expr) {
    // Suppose tags = [ A p11 .. p1n, ..., Q pq1 .. pqm ]
    // Build a generalized type t_tags = [ A t11 .. t1n, ..., Q tq1 .. tqm ],
    // with fresh t1, ..., tqm, so that we can re-use the derived impl for many
    // unions of the same tags and payloads.
    let (union_var, union_tags) = {
        let flex_tag_labels = tags
            .into_iter()
            .map(|(label, arity)| {
                let variables_slice = env.subs.reserve_into_vars(arity.into());
                for var_index in variables_slice {
                    env.subs[var_index] = env.subs.fresh_unnamed_flex_var();
                }
                (label, variables_slice)
            })
            .collect::<Vec<_>>();
        let union_tags = UnionTags::insert_slices_into_subs(env.subs, flex_tag_labels);
        let tag_union_var = synth_var(
            env.subs,
            Content::Structure(FlatType::TagUnion(
                union_tags,
                TagExt::Any(Variable::EMPTY_TAG_UNION),
            )),
        );

        (tag_union_var, union_tags)
    };

    // Now, a hasher for this tag union is
    //
    // hash_union : hasher, [ A t11 .. t1n, ..., Q tq1 .. tqm ] -> hasher where hasher implements Hasher
    // hash_union = \hasher, union ->
    //   when union is
    //      A x11 .. x1n -> Hash.hash (... (Hash.hash (Hash.uN hasher 0) x11) ...) x1n
    //      ...
    //      Q xq1 .. xqm -> Hash.hash (... (Hash.hash (Hash.uN hasher (q - 1)) xq1) ...) xqm
    //
    // where `Hash.uN` is the appropriate hasher for the discriminant value - typically a `u8`, but
    // if there are more than `u8::MAX` tags, we use `u16`, and so on.
    let union_sym = env.new_symbol("union");

    let hasher_sym = env.new_symbol("hasher");
    let hasher_var = synth_var(env.subs, Content::FlexAbleVar(None, Subs::AB_HASHER));

    let (discr_width, discr_precision_var, hash_discr_member) = if union_tags.len() > u64::MAX as _
    {
        // Should never happen, `usize` isn't more than 64 bits on most machines, but who knows?
        // Maybe someday soon someone will try to compile a huge Roc program on a 128-bit one.
        internal_error!("new record unlocked: you fit more than 18 billion, billion tags in a Roc program, and the compiler didn't fall over! But it will now. 🤯")
    } else if union_tags.len() > u32::MAX as _ {
        (IntLitWidth::U64, Variable::UNSIGNED64, Symbol::HASH_ADD_U64)
    } else if union_tags.len() > u16::MAX as _ {
        (IntLitWidth::U32, Variable::UNSIGNED32, Symbol::HASH_ADD_U32)
    } else if union_tags.len() > u8::MAX as _ {
        (IntLitWidth::U16, Variable::UNSIGNED16, Symbol::HASH_ADD_U16)
    } else {
        (IntLitWidth::U8, Variable::UNSIGNED8, Symbol::HASH_ADD_U8)
    };
    let discr_num_var = int_lit_width_to_variable(discr_width);

    // Build the branches of the body
    let whole_hasher_var = env.subs.fresh_unnamed_flex_var();
    let branches = union_tags
        .iter_all()
        .enumerate()
        .map(|(discr_n, (tag, payloads))| {
            // A
            let tag_name = env.subs[tag].clone();
            // t11 .. t1n
            let payload_vars = env.subs.get_subs_slice(env.subs[payloads]).to_vec();
            // x11 .. x1n
            let payload_syms: Vec<_> = std::iter::repeat_with(|| env.unique_symbol())
                .take(payload_vars.len())
                .collect();

            // `A x1 .. x1n` pattern
            let pattern = Pattern::AppliedTag {
                whole_var: union_var,
                tag_name,
                ext_var: Variable::EMPTY_TAG_UNION,
                // (t1, v1) (t2, v2)
                arguments: (payload_vars.iter())
                    .zip(payload_syms.iter())
                    .map(|(var, sym)| (*var, Loc::at_zero(Pattern::Identifier(*sym))))
                    .collect(),
            };
            let branch_pattern = WhenBranchPattern {
                pattern: Loc::at_zero(pattern),
                degenerate: false,
            };

            // discrHasher = (Hash.uN hasher n)
            let (discr_hasher_var, disc_hasher_expr) = call_hash_ability_member(
                env,
                hash_discr_member,
                (hasher_var, Expr::Var(hasher_sym, hasher_var)),
                (
                    discr_num_var,
                    Expr::Int(
                        discr_num_var,
                        discr_precision_var,
                        format!("{discr_n}").into_boxed_str(),
                        IntValue::I128((discr_n as i128).to_ne_bytes()),
                        IntBound::Exact(discr_width),
                    ),
                ),
            );

            // Fold up `Hash.hash (... (Hash.hash discrHasher x11) ...) x1n`
            let (body_var, body_expr) = (payload_vars.into_iter()).zip(payload_syms).fold(
                (discr_hasher_var, disc_hasher_expr),
                |total_hasher, (payload_var, payload_sym)| {
                    call_hash_hash(
                        env,
                        total_hasher,
                        (payload_var, Expr::Var(payload_sym, payload_var)),
                    )
                },
            );

            env.unify(whole_hasher_var, body_var);

            WhenBranch {
                patterns: vec![branch_pattern],
                value: Loc::at_zero(body_expr),
                guard: None,
                redundant: RedundantMark::known_non_redundant(),
            }
        })
        .collect();

    // when union is
    //   ...
    let when_var = whole_hasher_var;
    let when_expr = Expr::When {
        loc_cond: Box::new(Loc::at_zero(Expr::Var(union_sym, union_var))),
        cond_var: union_var,
        expr_var: when_var,
        region: Region::zero(),
        branches,
        branches_cond_var: union_var,
        exhaustive: ExhaustiveMark::known_exhaustive(),
    };

    // Finally, build the closure
    // \hasher, rcd -> body
    build_outer_derived_closure(
        env,
        fn_name,
        (hasher_var, hasher_sym),
        (union_var, Pattern::Identifier(union_sym)),
        (when_var, when_expr),
    )
}

/// Build a `hash` implementation for a newtype (singleton) tag union.
/// If a tag union is a newtype, we do not need to hash its discriminant.
fn hash_newtype_tag_union(
    env: &mut Env<'_>,
    fn_name: Symbol,
    tag: (TagName, u16),
) -> (Variable, Expr) {
    // Suppose tags = [ A p1 .. pn ]
    // Build a generalized type t_tags = [ A t1 .. tn ],
    // with fresh t1, ..., tn, so that we can re-use the derived impl for many
    // unions of the same tag and payload arity.
    let (union_var, tag_name, payload_variables) = {
        let (label, arity) = tag;

        let variables_slice = env.subs.reserve_into_vars(arity.into());
        for var_index in variables_slice {
            env.subs[var_index] = env.subs.fresh_unnamed_flex_var();
        }

        let variables_slices_slice =
            slice_extend_new(&mut env.subs.variable_slices, [variables_slice]);
        let tag_name_index = index_push_new(&mut env.subs.tag_names, label.clone());

        let union_tags = UnionTags::from_slices(tag_name_index.as_slice(), variables_slices_slice);
        let tag_union_var = synth_var(
            env.subs,
            Content::Structure(FlatType::TagUnion(
                union_tags,
                TagExt::Any(Variable::EMPTY_TAG_UNION),
            )),
        );

        (
            tag_union_var,
            label,
            env.subs.get_subs_slice(variables_slice).to_vec(),
        )
    };

    // Now, a hasher for this tag union is
    //
    // hash_union : hasher, [ A t1 .. tn ] -> hasher where hasher implements Hasher
    // hash_union = \hasher, A x1 .. xn ->
    //   Hash.hash (... (Hash.hash discrHasher x1) ...) xn
    let hasher_sym = env.new_symbol("hasher");
    let hasher_var = synth_var(env.subs, Content::FlexAbleVar(None, Subs::AB_HASHER));

    // A
    // let tag_name = tag_name;
    // t1 .. tn
    let payload_vars = payload_variables;
    // x11 .. x1n
    let payload_syms: Vec<_> = std::iter::repeat_with(|| env.unique_symbol())
        .take(payload_vars.len())
        .collect();

    // `A x1 .. x1n` pattern
    let pattern = Pattern::AppliedTag {
        whole_var: union_var,
        tag_name,
        ext_var: Variable::EMPTY_TAG_UNION,
        // (t1, v1) (t2, v2)
        arguments: (payload_vars.iter())
            .zip(payload_syms.iter())
            .map(|(var, sym)| (*var, Loc::at_zero(Pattern::Identifier(*sym))))
            .collect(),
    };

    // Fold up `Hash.hash (... (Hash.hash discrHasher x11) ...) x1n`
    let (body_var, body_expr) = (payload_vars.into_iter()).zip(payload_syms).fold(
        (hasher_var, Expr::Var(hasher_sym, hasher_var)),
        |total_hasher, (payload_var, payload_sym)| {
            call_hash_hash(
                env,
                total_hasher,
                (payload_var, Expr::Var(payload_sym, payload_var)),
            )
        },
    );

    // Finally, build the closure
    // \hasher, rcd -> body
    build_outer_derived_closure(
        env,
        fn_name,
        (hasher_var, hasher_sym),
        (union_var, pattern),
        (body_var, body_expr),
    )
}

fn call_hash_hash(
    env: &mut Env<'_>,
    hasher: (Variable, Expr),
    val: (Variable, Expr),
) -> (Variable, Expr) {
    call_hash_ability_member(env, Symbol::HASH_HASH, hasher, val)
}

fn call_hash_ability_member(
    env: &mut Env<'_>,
    member: Symbol,
    hasher: (Variable, Expr),
    val: (Variable, Expr),
) -> (Variable, Expr) {
    let (in_hasher_var, in_hasher_expr) = hasher;
    let (in_val_var, in_val_expr) = val;

    // build `member ...` function type. `member` here is `Hash.hash` or `Hash.addU16`.
    //
    // hasher, val -[uls]-> hasher where hasher implements Hasher, val implements Hash
    let exposed_hash_fn_var = env.import_builtin_symbol_var(member);

    // (typeof body), (typeof field) -[clos]-> hasher_result
    let this_arguments_slice = env.subs.insert_into_vars([in_hasher_var, in_val_var]);
    let this_hash_clos_var = env.subs.fresh_unnamed_flex_var();
    let this_out_hasher_var = env.subs.fresh_unnamed_flex_var();
    let this_hash_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            this_arguments_slice,
            this_hash_clos_var,
            this_out_hasher_var,
            Variable::PURE,
        )),
    );

    //   hasher,        val            -[uls]->  hasher where hasher implements Hasher, val implements Hash
    // ~ (typeof body), (typeof field) -[clos]-> hasher_result
    env.unify(exposed_hash_fn_var, this_hash_fn_var);

    // Hash.hash : hasher, (typeof field) -[clos]-> hasher where hasher implements Hasher, (typeof field) implements Hash
    let hash_fn_head = Expr::AbilityMember(member, None, this_hash_fn_var);
    let hash_fn_data = Box::new((
        this_hash_fn_var,
        Loc::at_zero(hash_fn_head),
        this_hash_clos_var,
        this_out_hasher_var,
        Variable::PURE,
    ));

    let hash_arguments = vec![
        (in_hasher_var, Loc::at_zero(in_hasher_expr)),
        (in_val_var, Loc::at_zero(in_val_expr)),
    ];
    let call_hash = Expr::Call(hash_fn_data, hash_arguments, CalledVia::Space);

    (this_out_hasher_var, call_hash)
}

fn build_outer_derived_closure(
    env: &mut Env<'_>,
    fn_name: Symbol,
    hasher: (Variable, Symbol),
    val: (Variable, Pattern),
    body: (Variable, Expr),
) -> (Variable, Expr) {
    let (hasher_var, hasher_sym) = hasher;
    let (val_var, val_pattern) = val;
    let (body_var, body_expr) = body;

    let (fn_var, fn_clos_var) = {
        // Create fn_var for ambient capture; we fix it up below.
        let fn_var = synth_var(env.subs, Content::Error);

        // -[fn_name]->
        let fn_captures = vec![];
        let fn_name_labels = UnionLambdas::insert_into_subs(env.subs, once((fn_name, fn_captures)));
        let fn_clos_var = synth_var(
            env.subs,
            Content::LambdaSet(LambdaSet {
                solved: fn_name_labels,
                recursion_var: OptVariable::NONE,
                unspecialized: SubsSlice::default(),
                ambient_function: fn_var,
            }),
        );

        // hasher, rcd_var -[fn_name]-> (hasher = body_var)
        let args_slice = env.subs.insert_into_vars([hasher_var, val_var]);
        env.subs.set_content(
            fn_var,
            Content::Structure(FlatType::Func(
                args_slice,
                fn_clos_var,
                body_var,
                Variable::PURE,
            )),
        );

        (fn_var, fn_clos_var)
    };

    let clos_expr = Expr::Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: body_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: fn_name,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![
            (
                hasher_var,
                AnnotatedMark::known_exhaustive(),
                Loc::at_zero(Pattern::Identifier(hasher_sym)),
            ),
            (
                val_var,
                AnnotatedMark::known_exhaustive(),
                Loc::at_zero(val_pattern),
            ),
        ],
        loc_body: Box::new(Loc::at_zero(body_expr)),
    });

    (fn_var, clos_expr)
}
