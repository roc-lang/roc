#![cfg(test)]

use std::{
    hash::{BuildHasher, Hash, Hasher},
    path::PathBuf,
};

use bumpalo::Bump;
use indoc::indoc;
use pretty_assertions::assert_eq;
use ven_pretty::DocAllocator;

use crate::pretty_print::{pretty_print, Ctx};
use roc_can::{
    abilities::{AbilitiesStore, ResolvedSpecializations},
    constraint::Constraints,
    expected::Expected,
    expr::Expr,
    module::RigidVariables,
};
use roc_collections::{default_hasher, VecSet};
use roc_constrain::{
    expr::constrain_expr,
    module::{ExposedByModule, ExposedForModule, ExposedModuleTypes},
};
use roc_debug_flags::dbg_do;
use roc_derive_key::DeriveKey;
use roc_load_internal::file::{add_imports, default_aliases, LoadedModule, Threading};
use roc_module::{
    ident::{ModuleName, TagName},
    symbol::{IdentIds, Interns, ModuleId, Symbol},
};
use roc_mono::derive::{
    encoding::{self, Env},
    synth_var,
};
use roc_region::all::{LineInfo, Region};
use roc_reporting::report::{type_problem, RocDocAllocator};
use roc_types::{
    pretty_print::{name_and_print_var, DebugPrint},
    subs::{
        AliasVariables, Content, ExposedTypesStorageSubs, FlatType, RecordFields, Subs, SubsIndex,
        SubsSlice, UnionTags, Variable,
    },
    types::{AliasKind, RecordField, Type},
};

fn encode_path() -> PathBuf {
    let repo_root = std::env::var("ROC_WORKSPACE_DIR").expect("are you running with `cargo test`?");
    PathBuf::from(repo_root)
        .join("compiler")
        .join("builtins")
        .join("roc")
        .join("Encode.roc")
}

fn check_derived_typechecks(
    derived: Expr,
    test_module: ModuleId,
    mut test_subs: Subs,
    interns: &Interns,
    exposed_encode_types: ExposedTypesStorageSubs,
    encode_abilities_store: AbilitiesStore,
    expected_type: &str,
) {
    // constrain the derived
    let mut constraints = Constraints::new();
    let mut env = roc_constrain::expr::Env {
        rigids: Default::default(),
        resolutions_to_make: Default::default(),
        home: test_module,
    };
    let real_type = test_subs.fresh_unnamed_flex_var();
    let constr = constrain_expr(
        &mut constraints,
        &mut env,
        Region::zero(),
        &derived,
        Expected::NoExpectation(Type::Variable(real_type)),
    );

    // the derived depends on stuff from Encode, so
    //   - we need to add those dependencies as imported on the constraint
    //   - we need to add Encode ability info to a local abilities store
    let encode_values_to_import = exposed_encode_types
        .stored_vars_by_symbol
        .keys()
        .copied()
        .collect::<VecSet<_>>();
    let pending_abilities = encode_abilities_store.closure_from_imported(&encode_values_to_import);
    let mut exposed_by_module = ExposedByModule::default();
    exposed_by_module.insert(
        ModuleId::ENCODE,
        ExposedModuleTypes {
            exposed_types_storage_subs: exposed_encode_types,
            resolved_specializations: ResolvedSpecializations::default(),
        },
    );
    let exposed_for_module =
        ExposedForModule::new(encode_values_to_import.iter(), exposed_by_module);
    let mut def_types = Default::default();
    let mut rigid_vars = Default::default();
    let (import_variables, abilities_store) = add_imports(
        test_module,
        &mut test_subs,
        pending_abilities,
        exposed_for_module,
        &mut def_types,
        &mut rigid_vars,
    );
    let constr =
        constraints.let_import_constraint(rigid_vars, def_types, constr, &import_variables);

    // run the solver, print and fail if we have errors
    dbg_do!(
        roc_debug_flags::ROC_PRINT_UNIFICATIONS_DERIVED,
        std::env::set_var(roc_debug_flags::ROC_PRINT_UNIFICATIONS_DERIVED, "1")
    );
    let (mut solved_subs, _, problems, _) = roc_solve::module::run_solve(
        &constraints,
        constr,
        RigidVariables::default(),
        test_subs,
        default_aliases(),
        abilities_store,
        Default::default(),
    );
    let subs = solved_subs.inner_mut();

    if !problems.is_empty() {
        let filename = PathBuf::from("Test.roc");
        let lines = LineInfo::new(" ");
        let src_lines = vec![" "];
        let mut reports = Vec::new();
        let alloc = RocDocAllocator::new(&src_lines, test_module, interns);

        for problem in problems.into_iter() {
            if let Some(report) = type_problem(&alloc, &lines, filename.clone(), problem.clone()) {
                reports.push(report);
            }
        }

        let has_reports = !reports.is_empty();

        let doc = alloc
            .stack(reports.into_iter().map(|v| v.pretty(&alloc)))
            .append(if has_reports {
                alloc.line()
            } else {
                alloc.nil()
            });

        let mut buf = String::new();
        doc.1
            .render_raw(80, &mut roc_reporting::report::CiWrite::new(&mut buf))
            .unwrap();

        panic!("Derived does not typecheck:\n{}", buf);
    }

    let pretty_type =
        name_and_print_var(real_type, subs, test_module, interns, DebugPrint::NOTHING);

    assert_eq!(expected_type, pretty_type);
}

fn derive_test<S>(synth_input: S, expected_type: &str, expected_source: &str)
where
    S: FnOnce(&mut Subs) -> Variable,
{
    let arena = Bump::new();
    let source = roc_builtins::roc::module_source(ModuleId::ENCODE);
    let target_info = roc_target::TargetInfo::default_x86_64();

    let LoadedModule {
        mut interns,
        exposed_types_storage: mut exposed_encode_types,
        abilities_store,
        ..
    } = roc_load_internal::file::load_and_typecheck_str(
        &arena,
        encode_path().file_name().unwrap().into(),
        source,
        encode_path().parent().unwrap(),
        Default::default(),
        target_info,
        roc_reporting::report::RenderTarget::ColorTerminal,
        Threading::AllAvailable,
    )
    .unwrap();

    let test_module = interns.module_id(&ModuleName::from("Test"));
    let mut test_subs = Subs::new();
    interns
        .all_ident_ids
        .insert(test_module, IdentIds::default());

    let mut env = Env {
        home: test_module,
        arena: &arena,
        subs: &mut test_subs,
        ident_ids: interns.all_ident_ids.get_mut(&test_module).unwrap(),
        exposed_encode_types: &mut exposed_encode_types,
    };

    let signature_var = synth_input(env.subs);

    let derived = encoding::derive_to_encoder(&mut env, signature_var);
    test_module.register_debug_idents(interns.all_ident_ids.get(&test_module).unwrap());

    let ctx = Ctx { interns: &interns };
    let derived_program = pretty_print(&ctx, &derived);
    assert_eq!(expected_source, derived_program);

    check_derived_typechecks(
        derived,
        test_module,
        test_subs,
        &interns,
        exposed_encode_types,
        abilities_store,
        expected_type,
    );
}

fn check_hash<S1, S2>(eq: bool, synth1: S1, synth2: S2)
where
    S1: FnOnce(&mut Subs) -> Variable,
    S2: FnOnce(&mut Subs) -> Variable,
{
    let mut subs = Subs::new();
    let var1 = synth1(&mut subs);
    let var2 = synth2(&mut subs);

    let hash1 = DeriveKey::encoding(&subs, var1);
    let hash2 = DeriveKey::encoding(&subs, var2);

    let hash1 = {
        let mut hasher = default_hasher().build_hasher();
        hash1.hash(&mut hasher);
        hasher.finish()
    };
    let hash2 = {
        let mut hasher = default_hasher().build_hasher();
        hash2.hash(&mut hasher);
        hasher.finish()
    };

    if eq {
        assert_eq!(hash1, hash2);
    } else {
        assert_ne!(hash1, hash2);
    }
}

// Writing out the types into content is terrible, so let's use a DSL at least for testing
macro_rules! v {
    ({ $($field:ident: $make_v:expr),* }) => {
        |subs: &mut Subs| {
            $(let $field = $make_v(subs);)*
            let fields = RecordFields::insert_into_subs(subs, vec![ $( (stringify!($field).into(), RecordField::Required($field)) ,)* ]);
            synth_var(subs, Content::Structure(FlatType::Record(fields, Variable::EMPTY_RECORD)))
        }
    };
    ([ $($tag:ident $($payload:expr)*),* ]) => {
        |subs: &mut Subs| {
            $(
            let $tag = vec![ $( $payload(subs), )* ];
            )*
            let tags = UnionTags::insert_into_subs::<_, Vec<Variable>>(subs, vec![ $( (TagName(stringify!($tag).into()), $tag) ,)* ]);
            synth_var(subs, Content::Structure(FlatType::TagUnion(tags, Variable::EMPTY_TAG_UNION)))
        }
    };
    ([ $($tag:ident $($payload:expr)*),* ] as $rec_var:ident) => {
        |subs: &mut Subs| {
            let $rec_var = subs.fresh_unnamed_flex_var();
            let rec_name_index =
                SubsIndex::push_new(&mut subs.field_names, stringify!($rec).into());

            $(
            let $tag = vec![ $( $payload(subs), )* ];
            )*
            let tags = UnionTags::insert_into_subs::<_, Vec<Variable>>(subs, vec![ $( (TagName(stringify!($tag).into()), $tag) ,)* ]);
            let tag_union_var = synth_var(subs, Content::Structure(FlatType::RecursiveTagUnion($rec_var, tags, Variable::EMPTY_TAG_UNION)));

            subs.set_content(
                $rec_var,
                Content::RecursionVar {
                    structure: tag_union_var,
                    opt_name: Some(rec_name_index),
                },
            );
            tag_union_var
        }
    };
    (Symbol::$sym:ident $($arg:expr)*) => {
        |subs: &mut Subs| {
            let $sym = vec![ $( $arg(subs) ,)* ];
            let var_slice = SubsSlice::insert_into_subs(subs, $sym);
            synth_var(subs, Content::Structure(FlatType::Apply(Symbol::$sym, var_slice)))
        }
    };
    (Symbol::$alias:ident $($arg:expr)* => $real_var:expr) => {
        |subs: &mut Subs| {
            let args = vec![$( $arg(subs) )*];
            let alias_variables = AliasVariables::insert_into_subs::<Vec<_>, Vec<_>>(subs, args, vec![]);
            let real_var = $real_var(subs);
            synth_var(subs, Content::Alias(Symbol::$alias, alias_variables, real_var, AliasKind::Structural))
        }
    };
    (@Symbol::$alias:ident $($arg:expr)* => $real_var:expr) => {
        |subs: &mut Subs| {
            let args = vec![$( $arg(subs) )*];
            let alias_variables = AliasVariables::insert_into_subs::<Vec<_>, Vec<_>>(subs, args, vec![]);
            let real_var = $real_var(subs);
            synth_var(subs, Content::Alias(Symbol::$alias, alias_variables, real_var, AliasKind::Opaque))
        }
    };
    (*$rec_var:ident) => {
        |_: &mut Subs| { $rec_var }
    };
    ($var:ident) => {
        |_: &mut Subs| { Variable::$var }
    };
}

macro_rules! test_hash_eq {
    ($($name:ident: $synth1:expr, $synth2:expr)*) => {$(
        #[test]
        fn $name() {
            #![allow(non_snake_case)]
            check_hash(true, $synth1, $synth2)
        }
    )*};
}

macro_rules! test_hash_neq {
    ($($name:ident: $synth1:expr, $synth2:expr)*) => {$(
        #[test]
        fn $name() {
            #![allow(non_snake_case)]
            check_hash(false, $synth1, $synth2)
        }
    )*};
}

// {{{ hash tests

test_hash_eq! {
    same_record:
        v!({ a: v!(U8) }), v!({ a: v!(U8) })
    same_record_fields_diff_types:
        v!({ a: v!(U8) }), v!({ a: v!(STR) })
    same_record_fields_any_order:
        v!({ a: v!(U8), b: v!(U8), c: v!(U8) }),
        v!({ c: v!(U8), a: v!(U8), b: v!(U8) })
    explicit_empty_record_and_implicit_empty_record:
        v!(EMPTY_RECORD), v!({})

    same_tag_union:
        v!([ A v!(U8) v!(STR), B v!(STR) ]), v!([ A v!(U8) v!(STR), B v!(STR) ])
    same_tag_union_tags_diff_types:
        v!([ A v!(U8) v!(U8), B v!(U8) ]), v!([ A v!(STR) v!(STR), B v!(STR) ])
    same_tag_union_tags_any_order:
        v!([ A v!(U8) v!(U8), B v!(U8), C ]), v!([ C, B v!(STR), A v!(STR) v!(STR) ])
    explicit_empty_tag_union_and_implicit_empty_tag_union:
        v!(EMPTY_TAG_UNION), v!([])

    same_recursive_tag_union:
        v!([ Nil, Cons v!(*lst)] as lst), v!([ Nil, Cons v!(*lst)] as lst)
    same_tag_union_and_recursive_tag_union_fields:
        v!([ Nil, Cons v!(STR)]), v!([ Nil, Cons v!(*lst)] as lst)

    list_list_diff_types:
        v!(Symbol::LIST_LIST v!(STR)), v!(Symbol::LIST_LIST v!(U8))
    set_set_diff_types:
        v!(Symbol::SET_SET v!(STR)), v!(Symbol::SET_SET v!(U8))
    dict_dict_diff_types:
        v!(Symbol::DICT_DICT v!(STR) v!(STR)), v!(Symbol::DICT_DICT v!(U8) v!(U8))
    str_str:
        v!(Symbol::STR_STR), v!(Symbol::STR_STR)

    alias_eq_real_type:
        v!(Symbol::BOOL_BOOL => v!([ True, False ])), v!([False, True])
    diff_alias_same_real_type:
        v!(Symbol::BOOL_BOOL => v!([ True, False ])), v!(Symbol::UNDERSCORE => v!([False, True]))

    opaque_eq_real_type:
        v!(@Symbol::BOOL_BOOL => v!([ True, False ])), v!([False, True])
    diff_opaque_same_real_type:
        v!(@Symbol::BOOL_BOOL => v!([ True, False ])), v!(@Symbol::UNDERSCORE => v!([False, True]))

    opaque_real_type_eq_alias_real_type:
        v!(@Symbol::BOOL_BOOL => v!([ True, False ])), v!(Symbol::UNDERSCORE => v!([False, True]))
}

test_hash_neq! {
    different_record_fields:
        v!({ a: v!(U8) }), v!({ b: v!(U8) })
    record_empty_vs_nonempty:
        v!(EMPTY_RECORD), v!({ a: v!(U8) })

    different_tag_union_tags:
        v!([ A v!(U8) ]), v!([ B v!(U8) ])
    tag_union_empty_vs_nonempty:
        v!(EMPTY_TAG_UNION), v!([ B v!(U8) ])
    different_recursive_tag_union_tags:
        v!([ Nil, Cons v!(*lst) ] as lst), v!([ Nil, Next v!(*lst) ] as lst)

    same_alias_diff_real_type:
        v!(Symbol::BOOL_BOOL => v!([ True, False ])), v!(Symbol::BOOL_BOOL => v!([ False, True, Maybe ]))
    diff_alias_diff_real_type:
        v!(Symbol::BOOL_BOOL => v!([ True, False ])), v!(Symbol::UNDERSCORE => v!([ False, True, Maybe ]))

    same_opaque_diff_real_type:
        v!(@Symbol::BOOL_BOOL => v!([ True, False ])), v!(@Symbol::BOOL_BOOL => v!([ False, True, Maybe ]))
    diff_opaque_diff_real_type:
        v!(@Symbol::BOOL_BOOL => v!([ True, False ])), v!(@Symbol::UNDERSCORE => v!([ False, True, Maybe ]))
}

// }}} hash tests

// {{{ deriver tests

#[test]
fn empty_record() {
    derive_test(
        v!(EMPTY_RECORD),
        "{} -> Encoder fmt | fmt has EncoderFormatting",
        indoc!(
            r#"
            \Test.0 -> (Encode.record [ ])
            "#
        ),
    )
}

#[test]
fn zero_field_record() {
    derive_test(
        v!({}),
        "{} -> Encoder fmt | fmt has EncoderFormatting",
        indoc!(
            r#"
            \Test.0 -> (Encode.record [ ])
            "#
        ),
    )
}

#[test]
fn one_field_record() {
    derive_test(
        v!({ a: v!(U8) }),
        "{ a : val } -> Encoder fmt | fmt has EncoderFormatting, val has Encoding",
        indoc!(
            r#"
            \Test.0 ->
              (Encode.record [ { value: (Encode.toEncoder (Test.0).a), key: "a", }, ])
            "#
        ),
    )
}

#[test]
fn two_field_record() {
    derive_test(
        v!({ a: v!(U8), b: v!(STR) }),
        "{ a : val, b : a } -> Encoder fmt | a has Encoding, fmt has EncoderFormatting, val has Encoding",
        indoc!(
            r#"
            \Test.0 ->
              (Encode.record [
                { value: (Encode.toEncoder (Test.0).a), key: "a", },
                { value: (Encode.toEncoder (Test.0).b), key: "b", },
              ])
            "#
        ),
    )
}

// }}} deriver tests
