//! Derivers for the `Hash` ability.

use std::iter::once;

use roc_can::{
    expr::{AnnotatedMark, ClosureData, Expr, Recursive},
    pattern::Pattern,
};
use roc_derive_key::hash::FlatHashKey;
use roc_module::{called_via::CalledVia, ident::Lowercase, symbol::Symbol};
use roc_region::all::Loc;
use roc_types::{
    subs::{
        Content, FlatType, LambdaSet, OptVariable, RecordFields, SubsSlice, UnionLambdas, Variable,
        VariableSubsSlice,
    },
    types::RecordField,
};

use crate::{synth_var, util::Env, DerivedBody};

pub(crate) fn derive_hash(env: &mut Env<'_>, key: FlatHashKey, def_symbol: Symbol) -> DerivedBody {
    let (body, body_type) = match key {
        FlatHashKey::Record(fields) => hash_record(env, def_symbol, fields),
        FlatHashKey::TagUnion(_) => todo!(),
    };

    let specialization_lambda_sets =
        env.get_specialization_lambda_sets(body_type, Symbol::HASH_HASH);

    DerivedBody {
        body,
        body_type,
        specialization_lambda_sets,
    }
}

fn hash_record(env: &mut Env<'_>, fn_name: Symbol, fields: Vec<Lowercase>) -> (Expr, Variable) {
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
    // hash_rcd : hasher, { f1: t1, ..., fn: tn } -> hasher | hasher has Hasher
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
    let hasher_var = synth_var(env.subs, Content::FlexAbleVar(None, Symbol::HASH_HASHER));

    let (body, body_var) = record_fields.iter_all().fold(
        (Expr::Var(hasher_sym), hasher_var),
        |(body, body_var), (field_name, field_var, _)| {
            let field_name = env.subs[field_name].clone();
            let field_var = env.subs[field_var];

            let field_access = Expr::Access {
                record_var,
                field_var,
                ext_var: env.subs.fresh_unnamed_flex_var(),
                loc_expr: Box::new(Loc::at_zero(Expr::Var(rcd_sym))),
                field: field_name,
            };

            let (hash_fn_data, returned_hasher_var) = {
                // build `Hash.hash ...` function type
                //
                // hasher, val -[uls]-> hasher | hasher has Hasher, val has Hash
                let exposed_hash_fn_var = env.import_builtin_symbol_var(Symbol::HASH_HASH);

                // (typeof body), (typeof field) -[clos]-> hasher_result
                let this_arguments_slice =
                    VariableSubsSlice::insert_into_subs(env.subs, [body_var, field_var]);
                let this_hash_clos_var = env.subs.fresh_unnamed_flex_var();
                let this_hasher_result_var = env.subs.fresh_unnamed_flex_var();
                let this_hash_fn_var = synth_var(
                    env.subs,
                    Content::Structure(FlatType::Func(
                        this_arguments_slice,
                        this_hash_clos_var,
                        this_hasher_result_var,
                    )),
                );

                //   hasher,        val            -[uls]->  hasher | hasher has Hasher, val has Hash
                // ~ (typeof body), (typeof field) -[clos]-> hasher_result
                env.unify(exposed_hash_fn_var, this_hash_fn_var);

                // Hash.hash : hasher, (typeof field) -[clos]-> hasher | hasher has Hasher, (typeof field) has Hash
                let hash_fn_head = Expr::AbilityMember(Symbol::HASH_HASH, None, this_hash_fn_var);
                let hash_fn_data = Box::new((
                    this_hash_fn_var,
                    Loc::at_zero(hash_fn_head),
                    this_hash_clos_var,
                    this_hasher_result_var,
                ));

                (hash_fn_data, this_hasher_result_var)
            };

            let hash_arguments = vec![
                (body_var, Loc::at_zero(body)),
                (field_var, Loc::at_zero(field_access)),
            ];
            let call_hash = Expr::Call(hash_fn_data, hash_arguments, CalledVia::Space);

            (call_hash, returned_hasher_var)
        },
    );

    // Finally, build the closure
    // \hasher, rcd -> body

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
        let args_slice = SubsSlice::insert_into_subs(env.subs, [hasher_var, record_var]);
        env.subs.set_content(
            fn_var,
            Content::Structure(FlatType::Func(args_slice, fn_clos_var, body_var)),
        );

        (fn_var, fn_clos_var)
    };

    let clos_expr = Expr::Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: body_var,
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
                record_var,
                AnnotatedMark::known_exhaustive(),
                Loc::at_zero(Pattern::Identifier(rcd_sym)),
            ),
        ],
        loc_body: Box::new(Loc::at_zero(body)),
    });

    (clos_expr, fn_var)
}
