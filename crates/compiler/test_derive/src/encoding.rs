#![cfg(test)]
// Even with #[allow(non_snake_case)] on individual idents, rust-analyzer issues diagnostics.
// See https://github.com/rust-lang/rust-analyzer/issues/6541.
// For the `v!` macro we use uppercase variables when constructing tag unions.
#![allow(non_snake_case)]

use std::path::PathBuf;

use bumpalo::Bump;
use insta::assert_snapshot;
use pretty_assertions::assert_eq;
use ven_pretty::DocAllocator;

use crate::pretty_print::{pretty_print_def, Ctx};
use roc_can::{
    abilities::{AbilitiesStore, ResolvedSpecializations, SpecializationLambdaSets},
    constraint::Constraints,
    def::Def,
    expr::Declarations,
    module::{ExposedByModule, ExposedForModule, ExposedModuleTypes, RigidVariables},
};
use roc_collections::VecSet;
use roc_constrain::expr::constrain_decls;
use roc_debug_flags::dbg_do;
use roc_derive::{synth_var, DerivedModule};
use roc_derive_key::{DeriveKey, Derived};
use roc_load_internal::file::{add_imports, default_aliases, LoadedModule, Threading};
use roc_module::{
    ident::TagName,
    symbol::{IdentIds, Interns, ModuleId, Symbol},
};
use roc_region::all::LineInfo;
use roc_reporting::report::{type_problem, RocDocAllocator};
use roc_types::{
    pretty_print::{name_and_print_var, DebugPrint},
    subs::{
        AliasVariables, Content, ExposedTypesStorageSubs, FlatType, RecordFields, Subs, SubsIndex,
        SubsSlice, UnionTags, Variable,
    },
    types::{AliasKind, RecordField},
};

const DERIVED_MODULE: ModuleId = ModuleId::DERIVED_SYNTH;

fn encode_path() -> PathBuf {
    let repo_root = std::env::var("ROC_WORKSPACE_DIR").expect("are you running with `cargo test`?");
    PathBuf::from(repo_root)
        .join("compiler")
        .join("builtins")
        .join("roc")
        .join("Encode.roc")
}

#[allow(clippy::too_many_arguments)]
fn assemble_derived_golden(
    subs: &mut Subs,
    test_module: ModuleId,
    interns: &Interns,
    source_var: Variable,
    derived_source: &str,
    typ: Variable,
    specialization_lsets: SpecializationLambdaSets,
) -> String {
    let mut print_var = |var: Variable, print_only_under_alias| {
        let snapshot = subs.snapshot();
        let pretty_type = name_and_print_var(
            var,
            subs,
            test_module,
            interns,
            DebugPrint {
                print_lambda_sets: true,
                print_only_under_alias,
            },
        );
        subs.rollback_to(snapshot);
        pretty_type
    };

    let mut pretty_buf = String::new();

    pretty_buf.push_str(&format!("# derived for {}\n", print_var(source_var, false)));

    let pretty_type = print_var(typ, false);
    pretty_buf.push_str(&format!("# {}\n", &pretty_type));

    let pretty_type_under_aliases = print_var(typ, true);
    pretty_buf.push_str(&format!("# {}\n", &pretty_type_under_aliases));

    pretty_buf.push_str("# Specialization lambda sets:\n");
    let mut specialization_lsets = specialization_lsets.into_iter().collect::<Vec<_>>();
    specialization_lsets.sort_by_key(|(region, _)| *region);
    for (region, var) in specialization_lsets {
        let pretty_lset = print_var(var, false);
        pretty_buf.push_str(&format!("#   @<{}>: {}\n", region, pretty_lset));
    }

    pretty_buf.push_str(derived_source);

    pretty_buf
}

#[allow(clippy::too_many_arguments)]
fn check_derived_typechecks_and_golden(
    derived_def: Def,
    test_module: ModuleId,
    mut test_subs: Subs,
    interns: &Interns,
    exposed_encode_types: ExposedTypesStorageSubs,
    encode_abilities_store: AbilitiesStore,
    source_var: Variable,
    derived_program: &str,
    specialization_lsets: SpecializationLambdaSets,
    check_golden: impl Fn(&str),
) {
    // constrain the derived
    let mut constraints = Constraints::new();
    let def_var = derived_def.expr_var;
    let mut decls = Declarations::new();
    decls.push_def(derived_def);
    let constr = constrain_decls(&mut constraints, test_module, &decls);

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
        &exposed_for_module,
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
        test_module,
        &constraints,
        constr,
        RigidVariables::default(),
        test_subs,
        default_aliases(),
        abilities_store,
        Default::default(),
        &exposed_for_module.exposed_by_module,
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

        panic!(
            "Derived does not typecheck:\n{}\nDerived def:\n{}",
            buf, derived_program
        );
    }

    let golden = assemble_derived_golden(
        subs,
        test_module,
        interns,
        source_var,
        derived_program,
        def_var,
        specialization_lsets,
    );

    check_golden(&golden)
}

fn derive_test<S>(synth_input: S, check_golden: impl Fn(&str))
where
    S: FnOnce(&mut Subs) -> Variable,
{
    let arena = Bump::new();
    let source = roc_builtins::roc::module_source(ModuleId::ENCODE);
    let target_info = roc_target::TargetInfo::default_x86_64();

    let LoadedModule {
        mut interns,
        exposed_types_storage: exposed_encode_types,
        abilities_store,
        resolved_specializations,
        ..
    } = roc_load_internal::file::load_and_typecheck_str(
        &arena,
        encode_path().file_name().unwrap().into(),
        source,
        encode_path().parent().unwrap().to_path_buf(),
        Default::default(),
        target_info,
        roc_reporting::report::RenderTarget::ColorTerminal,
        Threading::AllAvailable,
    )
    .unwrap();

    let mut subs = Subs::new();
    let ident_ids = IdentIds::default();
    let source_var = synth_input(&mut subs);
    let key = get_key(&subs, source_var);

    let mut derived_module = unsafe { DerivedModule::from_components(subs, ident_ids) };

    let mut exposed_by_module = ExposedByModule::default();
    exposed_by_module.insert(
        ModuleId::ENCODE,
        ExposedModuleTypes {
            exposed_types_storage_subs: exposed_encode_types.clone(),
            resolved_specializations,
        },
    );

    let (_derived_symbol, derived_def, specialization_lsets) =
        derived_module.get_or_insert(&exposed_by_module, key);
    let specialization_lsets = specialization_lsets.clone();
    let derived_def = derived_def.clone();

    let (subs, ident_ids) = derived_module.decompose();

    interns.all_ident_ids.insert(DERIVED_MODULE, ident_ids);
    DERIVED_MODULE.register_debug_idents(interns.all_ident_ids.get(&DERIVED_MODULE).unwrap());

    let ctx = Ctx { interns: &interns };
    let derived_program = pretty_print_def(&ctx, &derived_def);

    check_derived_typechecks_and_golden(
        derived_def,
        DERIVED_MODULE,
        subs,
        &interns,
        exposed_encode_types,
        abilities_store,
        source_var,
        &derived_program,
        specialization_lsets,
        check_golden,
    );
}

fn get_key(subs: &Subs, var: Variable) -> DeriveKey {
    match Derived::encoding(subs, var) {
        Ok(Derived::Key(key)) => key,
        _ => unreachable!(),
    }
}

fn check_key<S1, S2>(eq: bool, synth1: S1, synth2: S2)
where
    S1: FnOnce(&mut Subs) -> Variable,
    S2: FnOnce(&mut Subs) -> Variable,
{
    let mut subs = Subs::new();
    let var1 = synth1(&mut subs);
    let var2 = synth2(&mut subs);

    let key1 = Derived::encoding(&subs, var1);
    let key2 = Derived::encoding(&subs, var2);

    if eq {
        assert_eq!(key1, key2);
    } else {
        assert_ne!(key1, key2);
    }
}

fn check_immediate<S>(synth: S, immediate: Symbol)
where
    S: FnOnce(&mut Subs) -> Variable,
{
    let mut subs = Subs::new();
    let var = synth(&mut subs);

    let key = Derived::encoding(&subs, var);

    assert_eq!(key, Ok(Derived::Immediate(immediate)));
}

// Writing out the types into content is terrible, so let's use a DSL at least for testing
macro_rules! v {
    ({ $($field:ident: $make_v:expr,)* $(?$opt_field:ident : $make_opt_v:expr,)* }) => {
        |subs: &mut Subs| {
            $(let $field = $make_v(subs);)*
            $(let $opt_field = $make_opt_v(subs);)*
            let fields = vec![
                $( (stringify!($field).into(), RecordField::Required($field)) ,)*
                $( (stringify!($opt_field).into(), RecordField::Required($opt_field)) ,)*
            ];
            let fields = RecordFields::insert_into_subs(subs, fields);
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
            check_key(true, $synth1, $synth2)
        }
    )*};
}

macro_rules! test_hash_neq {
    ($($name:ident: $synth1:expr, $synth2:expr)*) => {$(
        #[test]
        fn $name() {
            check_key(false, $synth1, $synth2)
        }
    )*};
}

// {{{ hash tests

test_hash_eq! {
    same_record:
        v!({ a: v!(U8), }), v!({ a: v!(U8), })
    same_record_fields_diff_types:
        v!({ a: v!(U8), }), v!({ a: v!(STR), })
    same_record_fields_any_order:
        v!({ a: v!(U8), b: v!(U8), c: v!(U8), }),
        v!({ c: v!(U8), a: v!(U8), b: v!(U8), })
    explicit_empty_record_and_implicit_empty_record:
        v!(EMPTY_RECORD), v!({})
    same_record_fields_required_vs_optional:
        v!({ a: v!(U8), b: v!(U8), }),
        v!({ ?a: v!(U8), ?b: v!(U8), })

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
        v!({ a: v!(U8), }), v!({ b: v!(U8), })
    record_empty_vs_nonempty:
        v!(EMPTY_RECORD), v!({ a: v!(U8), })

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
fn immediates() {
    check_immediate(v!(U8), Symbol::ENCODE_U8);
    check_immediate(v!(U16), Symbol::ENCODE_U16);
    check_immediate(v!(U32), Symbol::ENCODE_U32);
    check_immediate(v!(U64), Symbol::ENCODE_U64);
    check_immediate(v!(U128), Symbol::ENCODE_U128);
    check_immediate(v!(I8), Symbol::ENCODE_I8);
    check_immediate(v!(I16), Symbol::ENCODE_I16);
    check_immediate(v!(I32), Symbol::ENCODE_I32);
    check_immediate(v!(I64), Symbol::ENCODE_I64);
    check_immediate(v!(I128), Symbol::ENCODE_I128);
    check_immediate(v!(DEC), Symbol::ENCODE_DEC);
    check_immediate(v!(F32), Symbol::ENCODE_F32);
    check_immediate(v!(F64), Symbol::ENCODE_F64);
    check_immediate(v!(STR), Symbol::ENCODE_STRING);
}

#[test]
fn empty_record() {
    derive_test(v!(EMPTY_RECORD), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for {}
        # {} -[[toEncoder_{}(0)]]-> Encoder fmt | fmt has EncoderFormatting
        # {} -[[toEncoder_{}(0)]]-> (List U8, fmt -[[custom(2) {}]]-> List U8) | fmt has EncoderFormatting
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_{}(0)]]
        #   @<2>: [[custom(2) {}]]
        #Derived.toEncoder_{} =
          \#Derived.rcd ->
            Encode.custom \#Derived.bytes, #Derived.fmt ->
              Encode.appendWith #Derived.bytes (Encode.record []) #Derived.fmt
        "###
        )
    })
}

#[test]
fn zero_field_record() {
    derive_test(v!({}), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for {}
        # {} -[[toEncoder_{}(0)]]-> Encoder fmt | fmt has EncoderFormatting
        # {} -[[toEncoder_{}(0)]]-> (List U8, fmt -[[custom(2) {}]]-> List U8) | fmt has EncoderFormatting
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_{}(0)]]
        #   @<2>: [[custom(2) {}]]
        #Derived.toEncoder_{} =
          \#Derived.rcd ->
            Encode.custom \#Derived.bytes, #Derived.fmt ->
              Encode.appendWith #Derived.bytes (Encode.record []) #Derived.fmt
        "###
        )
    })
}

#[test]
fn one_field_record() {
    derive_test(v!({ a: v!(U8), }), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for { a : U8 }
        # { a : val } -[[toEncoder_{a}(0)]]-> Encoder fmt | fmt has EncoderFormatting, val has Encoding
        # { a : val } -[[toEncoder_{a}(0)]]-> (List U8, fmt -[[custom(2) { a : val }]]-> List U8) | fmt has EncoderFormatting, val has Encoding
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_{a}(0)]]
        #   @<2>: [[custom(2) { a : val }]] | val has Encoding
        #Derived.toEncoder_{a} =
          \#Derived.rcd ->
            Encode.custom \#Derived.bytes, #Derived.fmt ->
              Encode.appendWith #Derived.bytes (Encode.record [
                { value: Encode.toEncoder #Derived.rcd.a, key: "a", },
              ]) #Derived.fmt
        "###
        )
    })
}

#[test]
#[ignore = "TODO #3421 unification of unspecialized variables in lambda sets currently causes this to be derived incorrectly"]
fn two_field_record() {
    derive_test(v!({ a: v!(U8), b: v!(STR), }), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for { a : U8, b : Str }
        # { a : val, b : a } -[[toEncoder_{a,b}(0)]]-> Encoder fmt | a has Encoding, fmt has EncoderFormatting, val has Encoding
        # { a : val, b : a } -[[toEncoder_{a,b}(0)]]-> (List U8, fmt -[[custom(2) { a : val, b : a }]]-> List U8) | a has Encoding, fmt has EncoderFormatting, val has Encoding
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_{a,b}(0)]]
        #   @<2>: [[custom(2) { a : val, b : a }]] | a has Encoding, val has Encoding
        #Derived.toEncoder_{a,b} =
          \#Derived.rcd ->
            Encode.custom \#Derived.bytes, #Derived.fmt ->
              Encode.appendWith #Derived.bytes (Encode.record [
                { value: Encode.toEncoder #Derived.rcd.a, key: "a", },
                { value: Encode.toEncoder #Derived.rcd.b, key: "b", },
              ]) #Derived.fmt
        "###
        )
    })
}

#[test]
#[ignore = "NOTE: this would never actually happen, because [] is uninhabited, and hence toEncoder can never be called with a value of []!
Rightfully it induces broken assertions in other parts of the compiler, so we ignore it."]
fn empty_tag_union() {
    derive_test(v!(EMPTY_TAG_UNION), |golden| {
        assert_snapshot!(
            golden,
            @r#"
            "#
        )
    })
}

#[test]
fn tag_one_label_zero_args() {
    derive_test(v!([A]), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for [A]
        # [A] -[[toEncoder_[A 0](0)]]-> Encoder fmt | fmt has EncoderFormatting
        # [A] -[[toEncoder_[A 0](0)]]-> (List U8, fmt -[[custom(2) [A]]]-> List U8) | fmt has EncoderFormatting
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_[A 0](0)]]
        #   @<2>: [[custom(2) [A]]]
        #Derived.toEncoder_[A 0] =
          \#Derived.tag ->
            Encode.custom \#Derived.bytes, #Derived.fmt ->
              Encode.appendWith #Derived.bytes (when #Derived.tag is
                A -> Encode.tag "A" []) #Derived.fmt
        "###
        )
    })
}

#[test]
#[ignore = "TODO #3421 unification of unspecialized variables in lambda sets currently causes this to be derived incorrectly"]
fn tag_one_label_two_args() {
    derive_test(v!([A v!(U8) v!(STR)]), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for [A U8 Str]
        # [A val a] -[[toEncoder_[A 2](0)]]-> Encoder fmt | a has Encoding, fmt has EncoderFormatting, val has Encoding
        # [A val a] -[[toEncoder_[A 2](0)]]-> (List U8, fmt -[[custom(4) [A val a]]]-> List U8) | a has Encoding, fmt has EncoderFormatting, val has Encoding
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_[A 2](0)]]
        #   @<2>: [[custom(4) [A val a]]] | a has Encoding, val has Encoding
        #Derived.toEncoder_[A 2] =
          \#Derived.tag ->
            Encode.custom \#Derived.bytes, #Derived.fmt ->
              Encode.appendWith #Derived.bytes (when #Derived.tag is
                A #Derived.2 #Derived.3 ->
                  Encode.tag "A" [
                    Encode.toEncoder #Derived.2,
                    Encode.toEncoder #Derived.3,
                  ]) #Derived.fmt
        "###
        )
    })
}

#[test]
#[ignore = "TODO #3421 unification of unspecialized variables in lambda sets currently causes this to be derived incorrectly"]
fn tag_two_labels() {
    derive_test(v!([A v!(U8) v!(STR) v!(U16), B v!(STR)]), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for [A U8 Str U16, B Str]
        # [A val a b, B c] -[[toEncoder_[A 3,B 1](0)]]-> Encoder fmt | a has Encoding, b has Encoding, c has Encoding, fmt has EncoderFormatting, val has Encoding
        # [A val a b, B c] -[[toEncoder_[A 3,B 1](0)]]-> (List U8, fmt -[[custom(6) [A val a b, B c]]]-> List U8) | a has Encoding, b has Encoding, c has Encoding, fmt has EncoderFormatting, val has Encoding
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_[A 3,B 1](0)]]
        #   @<2>: [[custom(6) [A val a b, B c]]] | a has Encoding, b has Encoding, c has Encoding, val has Encoding
        #Derived.toEncoder_[A 3,B 1] =
          \#Derived.tag ->
            Encode.custom \#Derived.bytes, #Derived.fmt ->
              Encode.appendWith #Derived.bytes (when #Derived.tag is
                A #Derived.2 #Derived.3 #Derived.4 ->
                  Encode.tag "A" [
                    Encode.toEncoder #Derived.2,
                    Encode.toEncoder #Derived.3,
                    Encode.toEncoder #Derived.4,
                  ]
                B #Derived.5 -> Encode.tag "B" [Encode.toEncoder #Derived.5])
              #Derived.fmt
        "###
        )
    })
}

#[test]
#[ignore = "TODO #3421 unification of unspecialized variables in lambda sets currently causes this to be derived incorrectly"]
fn recursive_tag_union() {
    derive_test(v!([Nil, Cons v!(U8) v!(*lst) ] as lst), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for [Cons U8 $rec, Nil] as $rec
        # [Cons val a, Nil] -[[toEncoder_[Cons 2,Nil 0](0)]]-> Encoder fmt | a has Encoding, fmt has EncoderFormatting, val has Encoding
        # [Cons val a, Nil] -[[toEncoder_[Cons 2,Nil 0](0)]]-> (List U8, fmt -[[custom(4) [Cons val a, Nil]]]-> List U8) | a has Encoding, fmt has EncoderFormatting, val has Encoding
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_[Cons 2,Nil 0](0)]]
        #   @<2>: [[custom(4) [Cons val a, Nil]]] | a has Encoding, val has Encoding
        #Derived.toEncoder_[Cons 2,Nil 0] =
          \#Derived.tag ->
            Encode.custom \#Derived.bytes, #Derived.fmt ->
              Encode.appendWith #Derived.bytes (when #Derived.tag is
                Cons #Derived.2 #Derived.3 ->
                  Encode.tag "Cons" [
                    Encode.toEncoder #Derived.2,
                    Encode.toEncoder #Derived.3,
                  ]
                Nil -> Encode.tag "Nil" []) #Derived.fmt
        "###
        )
    })
}

// }}} deriver tests
