//! Derivers for the `Inspect` ability.

use std::iter::once;

use roc_can::expr::{
    AnnotatedMark, ClosureData, Expr, Field, Recursive, WhenBranch, WhenBranchPattern,
};
use roc_can::pattern::Pattern;
use roc_collections::SendMap;
use roc_derive_key::inspect::FlatInspectableKey;
use roc_module::called_via::CalledVia;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::{
    Content, ExhaustiveMark, FlatType, GetSubsSlice, LambdaSet, OptVariable, RecordFields,
    RedundantMark, SubsSlice, TagExt, TupleElems, UnionLambdas, UnionTags, Variable,
    VariableSubsSlice,
};
use roc_types::types::RecordField;

use crate::util::Env;
use crate::{synth_var, DerivedBody};

pub(crate) fn derive_to_inspector(
    env: &mut Env<'_>,
    key: FlatInspectableKey,
    def_symbol: Symbol,
) -> DerivedBody {
    let (body, body_type) = match key {
        FlatInspectableKey::List() => to_inspector_list(env, def_symbol),
        FlatInspectableKey::Set() => unreachable!(),
        FlatInspectableKey::Dict() => unreachable!(),
        FlatInspectableKey::Record(fields) => {
            // Generalized record var so we can reuse this impl between many records:
            // if fields = { a, b }, this is { a: t1, b: t2 } for fresh t1, t2.
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

            to_inspector_record(env, record_var, fields, def_symbol)
        }
        FlatInspectableKey::Tuple(arity) => {
            // Generalized tuple var so we can reuse this impl between many tuples:
            // if arity = n, this is (t1, ..., tn) for fresh t1, ..., tn.
            let flex_elems = (0..arity)
                .map(|idx| (idx as usize, env.subs.fresh_unnamed_flex_var()))
                .collect::<Vec<_>>();
            let elems = TupleElems::insert_into_subs(env.subs, flex_elems);
            let tuple_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Tuple(elems, Variable::EMPTY_TUPLE)),
            );

            to_inspector_tuple(env, tuple_var, elems, def_symbol)
        }
        FlatInspectableKey::TagUnion(tags) => {
            // Generalized tag union var so we can reuse this impl between many unions:
            // if tags = [ A arity=2, B arity=1 ], this is [ A t1 t2, B t3 ] for fresh t1, t2, t3
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

            to_inspector_tag_union(env, tag_union_var, union_tags, def_symbol)
        }
        FlatInspectableKey::Function(_arity) => {
            // Desired output: \x, y, z -> ... ===> "<function>"

            todo!();
        }
        FlatInspectableKey::Opaque => todo!(),
        FlatInspectableKey::Error => todo!(),
    };

    let specialization_lambda_sets =
        env.get_specialization_lambda_sets(body_type, Symbol::INSPECT_TO_INSPECTOR);

    DerivedBody {
        body,
        body_type,
        specialization_lambda_sets,
    }
}

fn to_inspector_list(env: &mut Env<'_>, fn_name: Symbol) -> (Expr, Variable) {
    // Build \lst -> list, List.walk, (\elem -> Inspect.to_inspector elem)

    use Expr::*;

    let lst_sym = env.new_symbol("lst");
    let elem_sym = env.new_symbol("elem");

    // List elem
    let elem_var = env.subs.fresh_unnamed_flex_var();
    let elem_var_slice = env.subs.insert_into_vars([elem_var]);
    let list_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Apply(Symbol::LIST_LIST, elem_var_slice)),
    );

    // build `to_inspector elem` type
    // val -[uls]-> Inspector fmt where fmt implements InspectorFormatter
    let to_inspector_fn_var = env.import_builtin_symbol_var(Symbol::INSPECT_TO_INSPECTOR);

    // elem -[clos]-> t1
    let to_inspector_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
    let elem_inspector_var = env.subs.fresh_unnamed_flex_var(); // t1
    let elem_to_inspector_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            elem_var_slice,
            to_inspector_clos_var,
            elem_inspector_var,
            Variable::PURE,
        )),
    );

    //   val  -[uls]->  Inspector fmt where fmt implements InspectorFormatter
    // ~ elem -[clos]-> t1
    env.unify(to_inspector_fn_var, elem_to_inspector_fn_var);

    // to_inspector : (typeof rcd.a) -[clos]-> Inspector fmt where fmt implements InspectorFormatter
    let to_inspector_var =
        AbilityMember(Symbol::INSPECT_TO_INSPECTOR, None, elem_to_inspector_fn_var);
    let to_inspector_fn = Box::new((
        to_inspector_fn_var,
        Loc::at_zero(to_inspector_var),
        to_inspector_clos_var,
        elem_inspector_var,
        Variable::PURE,
    ));

    // to_inspector elem
    let to_inspector_call = Call(
        to_inspector_fn,
        vec![(elem_var, Loc::at_zero(Var(elem_sym, elem_var)))],
        CalledVia::Space,
    );

    // elem -[to_elem_inspector]-> to_inspector elem
    let to_elem_inspector_sym = env.new_symbol("to_elem_inspector");

    // Create fn_var for ambient capture; we fix it up below.
    let to_elem_inspector_fn_var = synth_var(env.subs, Content::Error);

    // -[to_elem_inspector]->
    let to_elem_inspector_labels =
        UnionLambdas::insert_into_subs(env.subs, once((to_elem_inspector_sym, vec![])));
    let to_elem_inspector_lset = synth_var(
        env.subs,
        Content::LambdaSet(LambdaSet {
            solved: to_elem_inspector_labels,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: to_elem_inspector_fn_var,
        }),
    );
    // elem -[to_elem_inspector]-> to_inspector elem
    env.subs.set_content(
        to_elem_inspector_fn_var,
        Content::Structure(FlatType::Func(
            elem_var_slice,
            to_elem_inspector_lset,
            elem_inspector_var,
            Variable::PURE,
        )),
    );

    // \elem -> to_inspector elem
    let to_elem_inspector = Closure(ClosureData {
        function_type: to_elem_inspector_fn_var,
        closure_type: to_elem_inspector_lset,
        return_type: elem_inspector_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: to_elem_inspector_sym,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            elem_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(elem_sym)),
        )],
        loc_body: Box::new(Loc::at_zero(to_inspector_call)),
    });

    // build `Inspect.list lst (\elem -> Inspect.to_inspector elem)` type
    // List e, (e -> Inspector fmt) -[uls]-> Inspector fmt where fmt implements InspectorFormatter
    let inspect_list_fn_var = env.import_builtin_symbol_var(Symbol::INSPECT_LIST);

    // List elem, List.walk, to_elem_inspector_fn_var -[clos]-> t1
    let list_walk_fn_var = env.import_builtin_symbol_var(Symbol::LIST_WALK);
    let this_inspect_list_args_slice =
        env.subs
            .insert_into_vars([list_var, list_walk_fn_var, to_elem_inspector_fn_var]);
    let this_inspect_list_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
    let this_list_inspector_var = env.subs.fresh_unnamed_flex_var(); // t1
    let this_inspect_list_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            this_inspect_list_args_slice,
            this_inspect_list_clos_var,
            this_list_inspector_var,
            Variable::PURE,
        )),
    );

    //   List e,    (e -> Inspector fmt)     -[uls]->  Inspector fmt where fmt implements InspectorFormatter
    // ~ List elem, to_elem_inspector_fn_var -[clos]-> t1
    env.unify(inspect_list_fn_var, this_inspect_list_fn_var);

    // Inspect.list : List elem, to_elem_inspector_fn_var -[clos]-> Inspector fmt where fmt implements InspectorFormatter
    let inspect_list = AbilityMember(Symbol::INSPECT_LIST, None, this_inspect_list_fn_var);
    let inspect_list_fn = Box::new((
        this_inspect_list_fn_var,
        Loc::at_zero(inspect_list),
        this_inspect_list_clos_var,
        this_list_inspector_var,
        Variable::PURE,
    ));

    // Inspect.list lst to_elem_inspector
    let inspect_list_call = Call(
        inspect_list_fn,
        vec![
            (list_var, Loc::at_zero(Var(lst_sym, list_var))),
            (
                list_walk_fn_var,
                Loc::at_zero(Var(Symbol::LIST_WALK, list_walk_fn_var)),
            ),
            (to_elem_inspector_fn_var, Loc::at_zero(to_elem_inspector)),
        ],
        CalledVia::Space,
    );

    // Inspect.custom \fmt -> Inspect.apply (Inspect.list ..) fmt
    let (body, this_inspector_var) = wrap_in_inspect_custom(
        env,
        inspect_list_call,
        this_list_inspector_var,
        lst_sym,
        list_var,
    );

    // \lst -> Inspect.list lst (\elem -> Inspect.to_inspector elem)
    // Create fn_var for ambient capture; we fix it up below.
    let fn_var = synth_var(env.subs, Content::Error);

    // -[fn_name]->
    let fn_name_labels = UnionLambdas::insert_into_subs(env.subs, once((fn_name, vec![])));
    let fn_clos_var = synth_var(
        env.subs,
        Content::LambdaSet(LambdaSet {
            solved: fn_name_labels,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: fn_var,
        }),
    );
    // List elem -[fn_name]-> Inspector fmt
    let list_var_slice = env.subs.insert_into_vars(once(list_var));
    env.subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(
            list_var_slice,
            fn_clos_var,
            this_inspector_var,
            Variable::PURE,
        )),
    );

    // \lst -[fn_name]-> Inspect.list lst (\elem -> Inspect.to_inspector elem)
    let clos = Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: this_inspector_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: fn_name,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            list_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(lst_sym)),
        )],
        loc_body: Box::new(Loc::at_zero(body)),
    });

    (clos, fn_var)
}

fn to_inspector_record(
    env: &mut Env<'_>,
    record_var: Variable,
    fields: RecordFields,
    fn_name: Symbol,
) -> (Expr, Variable) {
    // Suppose rcd = { a: t1, b: t2 }. Build
    //
    // \rcd -> Inspect.record [
    //      { key: "a", value: Inspect.to_inspector rcd.a },
    //      { key: "b", value: Inspect.to_inspector rcd.b },
    //   ]

    let rcd_sym = env.new_symbol("rcd");
    let whole_rcd_var = env.subs.fresh_unnamed_flex_var(); // type of the { key, value } records in the list

    use Expr::*;

    let fields_list = fields
        .iter_all()
        .map(|(field_name_index, field_var_index, _)| {
            let field_name = env.subs[field_name_index].clone();
            let field_var = env.subs[field_var_index];
            let field_var_slice = VariableSubsSlice::new(field_var_index.index() as u32, 1);

            // key: "a"
            let key_field = Field {
                var: Variable::STR,
                region: Region::zero(),
                loc_expr: Box::new(Loc::at_zero(Str(field_name.as_str().into()))),
            };

            // rcd.a
            let field_access = RecordAccess {
                record_var,
                ext_var: env.subs.fresh_unnamed_flex_var(),
                field_var,
                loc_expr: Box::new(Loc::at_zero(Var(
                    rcd_sym,
                    env.subs.fresh_unnamed_flex_var(),
                ))),
                field: field_name,
            };

            // build `to_inspector rcd.a` type
            // val -[uls]-> Inspector fmt where fmt implements InspectorFormatter
            let to_inspector_fn_var = env.import_builtin_symbol_var(Symbol::INSPECT_TO_INSPECTOR);

            // (typeof rcd.a) -[clos]-> t1
            let to_inspector_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
            let inspector_var = env.subs.fresh_unnamed_flex_var(); // t1
            let this_to_inspector_fn_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Func(
                    field_var_slice,
                    to_inspector_clos_var,
                    inspector_var,
                    Variable::PURE,
                )),
            );

            //   val            -[uls]->  Inspector fmt where fmt implements InspectorFormatter
            // ~ (typeof rcd.a) -[clos]-> t1
            env.unify(to_inspector_fn_var, this_to_inspector_fn_var);

            // to_inspector : (typeof rcd.a) -[clos]-> Inspector fmt where fmt implements InspectorFormatter
            let to_inspector_var =
                AbilityMember(Symbol::INSPECT_TO_INSPECTOR, None, to_inspector_fn_var);
            let to_inspector_fn = Box::new((
                to_inspector_fn_var,
                Loc::at_zero(to_inspector_var),
                to_inspector_clos_var,
                inspector_var,
                Variable::PURE,
            ));

            // to_inspector rcd.a
            let to_inspector_call = Call(
                to_inspector_fn,
                vec![(field_var, Loc::at_zero(field_access))],
                CalledVia::Space,
            );

            // value: to_inspector rcd.a
            let value_field = Field {
                var: inspector_var,
                region: Region::zero(),
                loc_expr: Box::new(Loc::at_zero(to_inspector_call)),
            };

            // { key: "a", value: to_inspector rcd.a }
            let mut kv = SendMap::default();
            kv.insert("key".into(), key_field);
            kv.insert("value".into(), value_field);

            let this_record_fields = RecordFields::insert_into_subs(
                env.subs,
                (once(("key".into(), RecordField::Required(Variable::STR))))
                    .chain(once(("value".into(), RecordField::Required(inspector_var)))),
            );
            let this_record_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Record(this_record_fields, Variable::EMPTY_RECORD)),
            );
            // NOTE: must be done to unify the lambda sets under `inspector_var`
            env.unify(this_record_var, whole_rcd_var);

            Loc::at_zero(Record {
                record_var: whole_rcd_var,
                fields: kv,
            })
        })
        .collect::<Vec<_>>();

    // typeof [ { key: .., value: .. }, { key: .., value: .. } ]
    let fields_rcd_var_slice = env.subs.insert_into_vars(once(whole_rcd_var));
    let fields_list_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Apply(Symbol::LIST_LIST, fields_rcd_var_slice)),
    );

    // [ { key: .., value: ..}, .. ]
    let fields_list = List {
        elem_var: whole_rcd_var,
        loc_elems: fields_list,
    };

    // build `Inspect.record [ { key: .., value: ..}, .. ]` type
    // List { key : Str, value : Inspector fmt } -[uls]-> Inspector fmt where fmt implements InspectorFormatter
    let inspect_record_fn_var = env.import_builtin_symbol_var(Symbol::INSPECT_RECORD);

    // fields_list_var -[clos]-> t1
    let fields_list_var_slice = env.subs.insert_into_vars(once(fields_list_var));
    let inspect_record_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
    let inspector_var = env.subs.fresh_unnamed_flex_var(); // t1
    let this_inspect_record_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            fields_list_var_slice,
            inspect_record_clos_var,
            inspector_var,
            Variable::PURE,
        )),
    );

    //   List { key : Str, value : Inspector fmt } -[uls]->  Inspector fmt where fmt implements InspectorFormatter
    // ~ fields_list_var                         -[clos]-> t1
    env.unify(inspect_record_fn_var, this_inspect_record_fn_var);

    // Inspect.record : fields_list_var -[clos]-> Inspector fmt where fmt implements InspectorFormatter
    let inspect_record_var = AbilityMember(Symbol::INSPECT_RECORD, None, inspect_record_fn_var);
    let inspect_record_fn = Box::new((
        inspect_record_fn_var,
        Loc::at_zero(inspect_record_var),
        inspect_record_clos_var,
        inspector_var,
        Variable::PURE,
    ));

    // Inspect.record [ { key: .., value: .. }, .. ]
    let inspect_record_call = Call(
        inspect_record_fn,
        vec![(fields_list_var, Loc::at_zero(fields_list))],
        CalledVia::Space,
    );

    // Inspect.custom \fmt -> Inspect.apply (Inspect.record ..) fmt
    let (body, this_inspector_var) =
        wrap_in_inspect_custom(env, inspect_record_call, inspector_var, rcd_sym, record_var);

    // Create fn_var for ambient capture; we fix it up below.
    let fn_var = synth_var(env.subs, Content::Error);

    // -[fn_name]->
    let fn_name_labels = UnionLambdas::insert_into_subs(env.subs, once((fn_name, vec![])));
    let fn_clos_var = synth_var(
        env.subs,
        Content::LambdaSet(LambdaSet {
            solved: fn_name_labels,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: fn_var,
        }),
    );
    // typeof rcd -[fn_name]-> (typeof Inspect.record [ .. ] = Inspector fmt)
    let record_var_slice = env.subs.insert_into_vars(once(record_var));
    env.subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(
            record_var_slice,
            fn_clos_var,
            this_inspector_var,
            Variable::PURE,
        )),
    );

    // \rcd -[fn_name]-> Inspect.record [ { key: .., value: .. }, .. ]
    let clos = Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: this_inspector_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: fn_name,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            record_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(rcd_sym)),
        )],
        loc_body: Box::new(Loc::at_zero(body)),
    });

    (clos, fn_var)
}

fn to_inspector_tuple(
    env: &mut Env<'_>,
    tuple_var: Variable,
    elems: TupleElems,
    fn_name: Symbol,
) -> (Expr, Variable) {
    // Suppose tup = (t1, t2). Build
    //
    // \tup -> Inspect.tuple [
    //      Inspect.to_inspector tup.0,
    //      Inspect.to_inspector tup.1,
    //   ]

    let tup_sym = env.new_symbol("tup");
    let whole_inspector_in_list_var = env.subs.fresh_unnamed_flex_var(); // type of the inspector in the list

    use Expr::*;

    let elem_inspectors_list = elems
        .iter_all()
        .map(|(elem_index, elem_var_index)| {
            let index = env.subs[elem_index];
            let elem_var = env.subs[elem_var_index];
            let elem_var_slice = VariableSubsSlice::new(elem_var_index.index() as u32, 1);

            // tup.0
            let tuple_access = TupleAccess {
                tuple_var,
                ext_var: env.subs.fresh_unnamed_flex_var(),
                elem_var,
                loc_expr: Box::new(Loc::at_zero(Var(
                    tup_sym,
                    env.subs.fresh_unnamed_flex_var(),
                ))),
                index,
            };

            // build `to_inspector tup.0` type
            // val -[uls]-> Inspector fmt where fmt implements InspectorFormatter
            let to_inspector_fn_var = env.import_builtin_symbol_var(Symbol::INSPECT_TO_INSPECTOR);

            // (typeof tup.0) -[clos]-> t1
            let to_inspector_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
            let inspector_var = env.subs.fresh_unnamed_flex_var(); // t1
            let this_to_inspector_fn_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Func(
                    elem_var_slice,
                    to_inspector_clos_var,
                    inspector_var,
                    Variable::PURE,
                )),
            );

            //   val            -[uls]->  Inspector fmt where fmt implements InspectorFormatter
            // ~ (typeof tup.0) -[clos]-> t1
            env.unify(to_inspector_fn_var, this_to_inspector_fn_var);

            // to_inspector : (typeof tup.0) -[clos]-> Inspector fmt where fmt implements InspectorFormatter
            let to_inspector_var =
                AbilityMember(Symbol::INSPECT_TO_INSPECTOR, None, to_inspector_fn_var);
            let to_inspector_fn = Box::new((
                to_inspector_fn_var,
                Loc::at_zero(to_inspector_var),
                to_inspector_clos_var,
                inspector_var,
                Variable::PURE,
            ));

            // to_inspector tup.0
            let to_inspector_call = Call(
                to_inspector_fn,
                vec![(elem_var, Loc::at_zero(tuple_access))],
                CalledVia::Space,
            );

            // NOTE: must be done to unify the lambda sets under `inspector_var`
            env.unify(inspector_var, whole_inspector_in_list_var);

            Loc::at_zero(to_inspector_call)
        })
        .collect::<Vec<_>>();

    // typeof [ to_inspector tup.0, to_inspector tup.1 ]
    let whole_inspector_in_list_var_slice =
        env.subs.insert_into_vars(once(whole_inspector_in_list_var));
    let elem_inspectors_list_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Apply(
            Symbol::LIST_LIST,
            whole_inspector_in_list_var_slice,
        )),
    );

    // [ to_inspector tup.0, to_inspector tup.1 ]
    let elem_inspectors_list = List {
        elem_var: whole_inspector_in_list_var,
        loc_elems: elem_inspectors_list,
    };

    // build `Inspect.tuple [ to_inspector tup.0, to_inspector tup.1 ]` type
    // List (Inspector fmt) -[uls]-> Inspector fmt where fmt implements InspectorFormatter
    let inspect_tuple_fn_var = env.import_builtin_symbol_var(Symbol::INSPECT_TUPLE);

    // elem_inspectors_list_var -[clos]-> t1
    let elem_inspectors_list_var_slice = env.subs.insert_into_vars(once(elem_inspectors_list_var));
    let inspect_tuple_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
    let inspector_var = env.subs.fresh_unnamed_flex_var(); // t1
    let this_inspect_tuple_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            elem_inspectors_list_var_slice,
            inspect_tuple_clos_var,
            inspector_var,
            Variable::PURE,
        )),
    );

    //   List (Inspector fmt)     -[uls]->  Inspector fmt where fmt implements InspectorFormatter
    // ~ elem_inspectors_list_var -[clos]-> t1
    env.unify(inspect_tuple_fn_var, this_inspect_tuple_fn_var);

    // Inspect.tuple : elem_inspectors_list_var -[clos]-> Inspector fmt where fmt implements InspectorFormatter
    let inspect_tuple_var = AbilityMember(Symbol::INSPECT_TUPLE, None, inspect_tuple_fn_var);
    let inspect_tuple_fn = Box::new((
        inspect_tuple_fn_var,
        Loc::at_zero(inspect_tuple_var),
        inspect_tuple_clos_var,
        inspector_var,
        Variable::PURE,
    ));

    // Inspect.tuple [ { key: .., value: .. }, .. ]
    let inspect_tuple_call = Call(
        inspect_tuple_fn,
        vec![(elem_inspectors_list_var, Loc::at_zero(elem_inspectors_list))],
        CalledVia::Space,
    );

    // Inspect.custom \fmt -> Inspect.apply (Inspect.tuple_var ..) fmt
    let (body, this_inspector_var) =
        wrap_in_inspect_custom(env, inspect_tuple_call, inspector_var, tup_sym, tuple_var);

    // Create fn_var for ambient capture; we fix it up below.
    let fn_var = synth_var(env.subs, Content::Error);

    // -[fn_name]->
    let fn_name_labels = UnionLambdas::insert_into_subs(env.subs, once((fn_name, vec![])));
    let fn_clos_var = synth_var(
        env.subs,
        Content::LambdaSet(LambdaSet {
            solved: fn_name_labels,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: fn_var,
        }),
    );
    // typeof tup -[fn_name]-> (typeof Inspect.tuple [ .. ] = Inspector fmt)
    let tuple_var_slice = env.subs.insert_into_vars(once(tuple_var));
    env.subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(
            tuple_var_slice,
            fn_clos_var,
            this_inspector_var,
            Variable::PURE,
        )),
    );

    // \tup -[fn_name]-> Inspect.tuple [ { key: .., value: .. }, .. ]
    let clos = Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: this_inspector_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: fn_name,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            tuple_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(tup_sym)),
        )],
        loc_body: Box::new(Loc::at_zero(body)),
    });

    (clos, fn_var)
}

fn to_inspector_tag_union(
    env: &mut Env<'_>,
    tag_union_var: Variable,
    tags: UnionTags,
    fn_name: Symbol,
) -> (Expr, Variable) {
    // Suppose tag = [ A t1 t2, B t3 ]. Build
    //
    // \tag -> when tag is
    //     A v1 v2 -> Inspect.tag "A" [ Inspect.to_inspector v1, Inspect.to_inspector v2 ]
    //     B v3 -> Inspect.tag "B" [ Inspect.to_inspector v3 ]

    let tag_sym = env.new_symbol("tag");
    let whole_tag_inspectors_var = env.subs.fresh_unnamed_flex_var(); // type of the Inspect.tag ... calls in the branch bodies

    use Expr::*;

    let branches = tags
        .iter_all()
        .map(|(tag_name_index, tag_vars_slice_index)| {
            // A
            let tag_name = &env.subs[tag_name_index].clone();
            let vars_slice = env.subs[tag_vars_slice_index];
            // t1 t2
            let payload_vars = env.subs.get_subs_slice(vars_slice).to_vec();
            // v1 v2
            let payload_syms: Vec<_> = std::iter::repeat_with(|| env.unique_symbol())
                .take(payload_vars.len())
                .collect();

            // `A v1 v2` pattern
            let pattern = Pattern::AppliedTag {
                whole_var: tag_union_var,
                tag_name: tag_name.clone(),
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

            // whole type of the elements in [ Inspect.to_inspector v1, Inspect.to_inspector v2 ]
            let whole_payload_inspectors_var = env.subs.fresh_unnamed_flex_var();
            // [ Inspect.to_inspector v1, Inspect.to_inspector v2 ]
            let payload_to_inspectors = (payload_syms.iter())
                .zip(payload_vars.iter())
                .map(|(&sym, &sym_var)| {
                    // build `to_inspector v1` type
                    // expected: val -[uls]-> Inspector fmt where fmt implements InspectorFormatter
                    let to_inspector_fn_var =
                        env.import_builtin_symbol_var(Symbol::INSPECT_TO_INSPECTOR);

                    // wanted: t1 -[clos]-> t'
                    let var_slice_of_sym_var = env.subs.insert_into_vars([sym_var]); // [ t1 ]
                    let to_inspector_clos_var = env.subs.fresh_unnamed_flex_var(); // clos
                    let inspector_var = env.subs.fresh_unnamed_flex_var(); // t'
                    let this_to_inspector_fn_var = synth_var(
                        env.subs,
                        Content::Structure(FlatType::Func(
                            var_slice_of_sym_var,
                            to_inspector_clos_var,
                            inspector_var,
                            Variable::PURE,
                        )),
                    );

                    //   val -[uls]->  Inspector fmt where fmt implements InspectorFormatter
                    // ~ t1  -[clos]-> t'
                    env.unify(to_inspector_fn_var, this_to_inspector_fn_var);

                    // to_inspector : t1 -[clos]-> Inspector fmt where fmt implements InspectorFormatter
                    let to_inspector_var =
                        AbilityMember(Symbol::INSPECT_TO_INSPECTOR, None, this_to_inspector_fn_var);
                    let to_inspector_fn = Box::new((
                        this_to_inspector_fn_var,
                        Loc::at_zero(to_inspector_var),
                        to_inspector_clos_var,
                        inspector_var,
                        Variable::PURE,
                    ));

                    // to_inspector rcd.a
                    let to_inspector_call = Call(
                        to_inspector_fn,
                        vec![(sym_var, Loc::at_zero(Var(sym, sym_var)))],
                        CalledVia::Space,
                    );

                    // NOTE: must be done to unify the lambda sets under `inspector_var`
                    env.unify(inspector_var, whole_payload_inspectors_var);

                    Loc::at_zero(to_inspector_call)
                })
                .collect();

            // typeof [ Inspect.to_inspector v1, Inspect.to_inspector v2 ]
            let whole_inspectors_var_slice =
                env.subs.insert_into_vars([whole_payload_inspectors_var]);
            let payload_inspectors_list_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Apply(
                    Symbol::LIST_LIST,
                    whole_inspectors_var_slice,
                )),
            );

            // [ Inspect.to_inspector v1, Inspect.to_inspector v2 ]
            let payload_inspectors_list = List {
                elem_var: whole_payload_inspectors_var,
                loc_elems: payload_to_inspectors,
            };

            // build `Inspect.tag "A" [ ... ]` type
            // expected: Str, List (Inspector fmt) -[uls]-> Inspector fmt where fmt implements InspectorFormatter
            let inspect_tag_fn_var = env.import_builtin_symbol_var(Symbol::INSPECT_TAG);

            // wanted: Str, List whole_inspectors_var -[clos]-> t'
            let this_inspect_tag_args_var_slice = env
                .subs
                .insert_into_vars([Variable::STR, payload_inspectors_list_var]);
            let this_inspect_tag_clos_var = env.subs.fresh_unnamed_flex_var(); // -[clos]->
            let this_inspector_var = env.subs.fresh_unnamed_flex_var(); // t'
            let this_inspect_tag_fn_var = synth_var(
                env.subs,
                Content::Structure(FlatType::Func(
                    this_inspect_tag_args_var_slice,
                    this_inspect_tag_clos_var,
                    this_inspector_var,
                    Variable::PURE,
                )),
            );

            //   Str, List (Inspector fmt)      -[uls]->  Inspector fmt where fmt implements InspectorFormatter
            // ~ Str, List whole_inspectors_var -[clos]-> t'
            env.unify(inspect_tag_fn_var, this_inspect_tag_fn_var);

            // Inspect.tag : Str, List whole_inspectors_var -[clos]-> Inspector fmt where fmt implements InspectorFormatter
            let inspect_tag_var = AbilityMember(Symbol::INSPECT_TAG, None, this_inspect_tag_fn_var);
            let inspect_tag_fn = Box::new((
                this_inspect_tag_fn_var,
                Loc::at_zero(inspect_tag_var),
                this_inspect_tag_clos_var,
                this_inspector_var,
                Variable::PURE,
            ));

            // Inspect.tag "A" [ Inspect.to_inspector v1, Inspect.to_inspector v2 ]
            let inspect_tag_call = Call(
                inspect_tag_fn,
                vec![
                    // (Str, "A")
                    (Variable::STR, Loc::at_zero(Str(tag_name.0.as_str().into()))),
                    // (List (Inspector fmt), [ Inspect.to_inspector v1, Inspect.to_inspector v2 ])
                    (
                        payload_inspectors_list_var,
                        Loc::at_zero(payload_inspectors_list),
                    ),
                ],
                CalledVia::Space,
            );

            // NOTE: must be done to unify the lambda sets under `inspector_var`
            // Inspect.tag "A" [ Inspect.to_inspector v1, Inspect.to_inspector v2 ] ~ whole_inspectors
            env.unify(this_inspector_var, whole_tag_inspectors_var);

            WhenBranch {
                patterns: vec![branch_pattern],
                value: Loc::at_zero(inspect_tag_call),
                guard: None,
                redundant: RedundantMark::known_non_redundant(),
            }
        })
        .collect::<Vec<_>>();

    // when tag is
    //     A v1 v2 -> Inspect.tag "A" [ Inspect.to_inspector v1, Inspect.to_inspector v2 ]
    //     B v3 -> Inspect.tag "B" [ Inspect.to_inspector v3 ]
    let when_branches = When {
        loc_cond: Box::new(Loc::at_zero(Var(tag_sym, tag_union_var))),
        cond_var: tag_union_var,
        expr_var: whole_tag_inspectors_var,
        region: Region::zero(),
        branches,
        branches_cond_var: tag_union_var,
        exhaustive: ExhaustiveMark::known_exhaustive(),
    };

    // Inspect.custom \fmt -> Inspect.apply (when ..) fmt
    let (body, this_inspector_var) = wrap_in_inspect_custom(
        env,
        when_branches,
        whole_tag_inspectors_var,
        tag_sym,
        tag_union_var,
    );

    // Create fn_var for ambient capture; we fix it up below.
    let fn_var = synth_var(env.subs, Content::Error);

    // -[fn_name]->
    let fn_name_labels = UnionLambdas::insert_into_subs(env.subs, once((fn_name, vec![])));
    let fn_clos_var = synth_var(
        env.subs,
        Content::LambdaSet(LambdaSet {
            solved: fn_name_labels,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: fn_var,
        }),
    );
    // tag_union_var -[fn_name]-> whole_tag_inspectors_var
    let tag_union_var_slice = env.subs.insert_into_vars(once(tag_union_var));
    env.subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(
            tag_union_var_slice,
            fn_clos_var,
            this_inspector_var,
            Variable::PURE,
        )),
    );

    // \tag ->
    //   Inspect.custom \fmt -> Inspect.apply (
    //     when tag is
    //        A v1 v2 -> Inspect.tag "A" [ Inspect.to_inspector v1, Inspect.to_inspector v2 ]
    //        B v3 -> Inspect.tag "B" [ Inspect.to_inspector v3 ])
    //     fmt
    let clos = Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: this_inspector_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: fn_name,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            tag_union_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(tag_sym)),
        )],
        loc_body: Box::new(Loc::at_zero(body)),
    });

    (clos, fn_var)
}

/// Lift `inspector` to `Inspect.custom \fmt -> Inspect.apply inspector fmt`
fn wrap_in_inspect_custom(
    env: &mut Env,
    inspector: Expr,
    inspector_var: Variable,
    captured_symbol: Symbol,
    captured_var: Variable,
) -> (Expr, Variable) {
    use Expr::*;

    let fn_name = env.new_symbol("custom");

    // fmt: fmt where fmt implements InspectorFormatter
    let fmt_sym = env.new_symbol("fmt");
    let fmt_var = env.subs.fresh_unnamed_flex_var();

    // build `Inspect.apply inspector fmt` type
    // expected: Inspect.apply : Inspector fmt, fmt -[apply]-> fmt where fmt implements InspectorFormatter
    let apply_fn_var = env.import_builtin_symbol_var(Symbol::INSPECT_APPLY);

    // wanted: Inspect.apply : inspector_var, fmt -[clos]-> fmt where fmt implements InspectorFormatter
    let this_apply_args_var_slice = env.subs.insert_into_vars([inspector_var, fmt_var]);
    let this_apply_clos_var = env.subs.fresh_unnamed_flex_var(); // -[clos]->
    let this_apply_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            this_apply_args_var_slice,
            this_apply_clos_var,
            fmt_var,
            Variable::PURE,
        )),
    );

    //   Inspector fmt, fmt -[apply]-> ft where fmt implements InspectorFormatter
    // ~ inspector_var, fmt -[clos]->       fmt where fmt implements InspectorFormatter
    env.unify(apply_fn_var, this_apply_fn_var);

    // Inspect.apply : inspector_var, fmt -[apply]-> fmt where fmt implements InspectorFormatter
    let apply_fn = Box::new((
        this_apply_fn_var,
        Loc::at_zero(Var(Symbol::INSPECT_APPLY, this_apply_fn_var)),
        this_apply_clos_var,
        fmt_var,
        Variable::PURE,
    ));

    // Inspect.apply inspector fmt
    let apply_call = Call(
        apply_fn,
        vec![
            // (inspector_var, inspector)
            (inspector_var, Loc::at_zero(inspector)),
            // (fmt, fmt_var)
            (fmt_var, Loc::at_zero(Var(fmt_sym, fmt_var))),
        ],
        CalledVia::Space,
    );

    // Create fn_var for ambient capture; we fix it up below.
    let fn_var = synth_var(env.subs, Content::Error);

    // -[[FN_name captured_var]]->
    let fn_name_labels =
        UnionLambdas::insert_into_subs(env.subs, once((fn_name, vec![captured_var])));
    let fn_clos_var = synth_var(
        env.subs,
        Content::LambdaSet(LambdaSet {
            solved: fn_name_labels,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: fn_var,
        }),
    );

    // fmt -[[FN_name captured_var]]-> Inspect.apply inspector fmt
    let args_slice = env.subs.insert_into_vars(vec![fmt_var]);
    env.subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(
            args_slice,
            fn_clos_var,
            fmt_var,
            Variable::PURE,
        )),
    );

    // \fmt -[[fn_name captured_var]]-> Inspect.apply inspector fmt
    let clos = Closure(ClosureData {
        function_type: fn_var,
        closure_type: fn_clos_var,
        return_type: fmt_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: fn_name,
        captured_symbols: vec![(captured_symbol, captured_var)],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            fmt_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(fmt_sym)),
        )],
        loc_body: Box::new(Loc::at_zero(apply_call)),
    });

    // Build
    // Inspect.custom \fmt -> Inspect.apply inspector fmt
    //
    // expected: Inspect.custom : (fmt -> fmt) -> Inspector fmt where fmt implements InspectorFormatter
    let custom_fn_var = env.import_builtin_symbol_var(Symbol::INSPECT_CUSTOM);

    // wanted: Inspect.custom : fn_var -[clos]-> t'
    let this_custom_args_var_slice = env.subs.insert_into_vars([fn_var]);
    let this_custom_clos_var = env.subs.fresh_unnamed_flex_var(); // -[clos]->
    let this_custom_inspector_var = env.subs.fresh_unnamed_flex_var(); // t'
    let this_custom_fn_var = synth_var(
        env.subs,
        Content::Structure(FlatType::Func(
            this_custom_args_var_slice,
            this_custom_clos_var,
            this_custom_inspector_var,
            Variable::PURE,
        )),
    );

    //   (fmt -> fmt) -[..]->   Inspector fmt where fmt implements InspectorFormatter
    // ~ fn_var                    -[clos]-> t'
    env.unify(custom_fn_var, this_custom_fn_var);

    // Inspect.custom : (fmt -> fmt) -> Inspector fmt where fmt implements InspectorFormatter
    let custom_fn = Box::new((
        this_custom_fn_var,
        Loc::at_zero(Var(Symbol::INSPECT_CUSTOM, this_custom_fn_var)),
        this_custom_clos_var,      // -[clos]->
        this_custom_inspector_var, // t' ~ Inspector fmt
        Variable::PURE,
    ));

    // Inspect.custom \fmt -> Inspect.apply inspector fmt
    let custom_call = Call(
        custom_fn,
        vec![(fn_var, Loc::at_zero(clos))],
        CalledVia::Space,
    );

    (custom_call, this_custom_inspector_var)
}
