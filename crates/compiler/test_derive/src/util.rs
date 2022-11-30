use std::fmt::Write as _; // import without risk of name clashing
use std::path::PathBuf;

use bumpalo::Bump;
use roc_packaging::cache::RocCacheDir;
use ven_pretty::DocAllocator;

use crate::pretty_print::{pretty_print_def, Ctx};
use roc_can::{
    abilities::{AbilitiesStore, SpecializationLambdaSets},
    constraint::Constraints,
    def::Def,
    expr::Declarations,
    module::{
        ExposedByModule, ExposedForModule, ExposedModuleTypes, ResolvedImplementations,
        RigidVariables,
    },
};
use roc_collections::VecSet;
use roc_constrain::expr::constrain_decls;
use roc_debug_flags::dbg_do;
use roc_derive::DerivedModule;
use roc_derive_key::{DeriveBuiltin, DeriveError, DeriveKey, Derived};
use roc_load_internal::file::{add_imports, LoadedModule, Threading};
use roc_module::symbol::{IdentIds, Interns, ModuleId, Symbol};
use roc_region::all::LineInfo;
use roc_reporting::report::{type_problem, RocDocAllocator};
use roc_types::{
    pretty_print::{name_and_print_var, DebugPrint},
    subs::{ExposedTypesStorageSubs, Subs, Variable},
    types::Types,
};

const DERIVED_MODULE: ModuleId = ModuleId::DERIVED_SYNTH;

fn module_source_and_path(builtin: DeriveBuiltin) -> (ModuleId, &'static str, PathBuf) {
    use roc_builtins::roc::module_source;

    let repo_root = std::env::var("ROC_WORKSPACE_DIR").expect("are you running with `cargo test`?");
    let builtins_path = PathBuf::from(repo_root)
        .join("compiler")
        .join("builtins")
        .join("roc");

    match builtin {
        DeriveBuiltin::ToEncoder => (
            ModuleId::ENCODE,
            module_source(ModuleId::ENCODE),
            builtins_path.join("Encode.roc"),
        ),
        DeriveBuiltin::Decoder => (
            ModuleId::DECODE,
            module_source(ModuleId::DECODE),
            builtins_path.join("Decode.roc"),
        ),
        DeriveBuiltin::Hash => (
            ModuleId::HASH,
            module_source(ModuleId::HASH),
            builtins_path.join("Hash.roc"),
        ),
        DeriveBuiltin::IsEq => (
            ModuleId::BOOL,
            module_source(ModuleId::BOOL),
            builtins_path.join("Bool.roc"),
        ),
    }
}

/// DSL for creating [`Content`][roc_types::subs::Content].
#[macro_export]
macro_rules! v {
     ({ $($field:ident: $make_v:expr,)* $(?$opt_field:ident : $make_opt_v:expr,)* }$( $($ext:tt)+ )?) => {{
         #[allow(unused)]
         use roc_types::types::RecordField;
         use roc_types::subs::{Subs, RecordFields, Content, FlatType, Variable};
         |subs: &mut Subs| {
             $(let $field = $make_v(subs);)*
             $(let $opt_field = $make_opt_v(subs);)*
             let fields = vec![
                 $( (stringify!($field).into(), RecordField::Required($field)) ,)*
                 $( (stringify!($opt_field).into(), RecordField::Optional($opt_field)) ,)*
             ];
             let fields = RecordFields::insert_into_subs(subs, fields);

             #[allow(unused_mut, unused)]
             let mut ext = Variable::EMPTY_RECORD;
             $( ext = $crate::v!($($ext)+)(subs); )?

             roc_derive::synth_var(subs, Content::Structure(FlatType::Record(fields, ext)))
         }
     }};
     ([ $($tag:ident $($payload:expr)*),* ] as $rec_var:ident) => {{
         use roc_types::subs::{Subs, SubsIndex, Variable, Content, FlatType, UnionTags};
         use roc_module::ident::TagName;
         |subs: &mut Subs| {
             let $rec_var = subs.fresh_unnamed_flex_var();
             let rec_name_index =
                 SubsIndex::push_new(&mut subs.field_names, stringify!($rec).into());

             $(
             let $tag = vec![ $( $payload(subs), )* ];
             )*
             let tags = UnionTags::insert_into_subs::<_, Vec<Variable>>(subs, vec![ $( (TagName(stringify!($tag).into()), $tag) ,)* ]);
             let tag_union_var = roc_derive::synth_var(subs, Content::Structure(FlatType::RecursiveTagUnion($rec_var, tags, Variable::EMPTY_TAG_UNION)));

             subs.set_content(
                 $rec_var,
                 Content::RecursionVar {
                     structure: tag_union_var,
                     opt_name: Some(rec_name_index),
                 },
             );
             tag_union_var
         }
     }};
     ([ $($tag:ident $($payload:expr)*),* ]$( $($ext:tt)+ )?) => {{
         #[allow(unused)]
         use roc_types::subs::{Subs, UnionTags, Content, FlatType, Variable};
         #[allow(unused)]
         use roc_module::ident::TagName;
         |subs: &mut Subs| {
             $(
             let $tag = vec![ $( $payload(subs), )* ];
             )*
             let tags = UnionTags::insert_into_subs::<_, Vec<Variable>>(subs, vec![ $( (TagName(stringify!($tag).into()), $tag) ,)* ]);

             #[allow(unused_mut, unused)]
             let mut ext = Variable::EMPTY_TAG_UNION;
             $( ext = $crate::v!($($ext)+)(subs); )?

             roc_derive::synth_var(subs, Content::Structure(FlatType::TagUnion(tags, ext)))
         }
     }};
     (Symbol::$sym:ident $($arg:expr)*) => {{
         use roc_types::subs::{Subs, SubsSlice, Content, FlatType};
         use roc_module::symbol::Symbol;
         |subs: &mut Subs| {
             let $sym = vec![ $( $arg(subs) ,)* ];
             let var_slice = SubsSlice::insert_into_subs(subs, $sym);
             roc_derive::synth_var(subs, Content::Structure(FlatType::Apply(Symbol::$sym, var_slice)))
         }
     }};
     (Symbol::$alias:ident $($arg:expr)* => $real_var:expr) => {{
         use roc_types::subs::{Subs, AliasVariables, Content};
         use roc_types::types::AliasKind;
         use roc_module::symbol::Symbol;
         |subs: &mut Subs| {
             let args = vec![$( $arg(subs) )*];
             let alias_variables = AliasVariables::insert_into_subs::<Vec<_>, Vec<_>, _>(subs, args, vec![], vec![]);
             let real_var = $real_var(subs);
             roc_derive::synth_var(subs, Content::Alias(Symbol::$alias, alias_variables, real_var, AliasKind::Structural))
         }
     }};
     (@Symbol::$alias:ident $($arg:expr)* => $real_var:expr) => {{
         use roc_types::subs::{Subs, AliasVariables, Content};
         use roc_types::types::AliasKind;
         use roc_module::symbol::Symbol;
         |subs: &mut Subs| {
             let args = vec![$( $arg(subs) )*];
             let alias_variables = AliasVariables::insert_into_subs::<Vec<_>, Vec<_>, _>(subs, args, vec![], vec![]);
             let real_var = $real_var(subs);
             roc_derive::synth_var(subs, Content::Alias(Symbol::$alias, alias_variables, real_var, AliasKind::Opaque))
         }
     }};
     (*) => {{
         use roc_types::subs::{Subs, Content};
         |subs: &mut Subs| { roc_derive::synth_var(subs, Content::FlexVar(None)) }
     }};
     ($name:ident has $ability:path) => {{
         use roc_types::subs::{Subs, SubsIndex, SubsSlice, Content};
         |subs: &mut Subs| {
             let name_index =
                 SubsIndex::push_new(&mut subs.field_names, stringify!($name).into());

             let abilities_slice = SubsSlice::extend_new(&mut subs.symbol_names, [$ability]);

             roc_derive::synth_var(subs, Content::FlexAbleVar(Some(name_index), abilities_slice))
         }
     }};
     (^$rec_var:ident) => {{
         use roc_types::subs::{Subs};
         |_: &mut Subs| { $rec_var }
     }};
     ($var:ident) => {{
         use roc_types::subs::{Subs};
         |_: &mut Subs| { Variable::$var }
     }};
 }

pub(crate) fn check_key<S1, S2>(builtin: DeriveBuiltin, eq: bool, synth1: S1, synth2: S2)
where
    S1: FnOnce(&mut Subs) -> Variable,
    S2: FnOnce(&mut Subs) -> Variable,
{
    let mut subs = Subs::new();
    let var1 = synth1(&mut subs);
    let var2 = synth2(&mut subs);

    let key1 = Derived::builtin(builtin, &subs, var1);
    let key2 = Derived::builtin(builtin, &subs, var2);

    if eq {
        assert_eq!(key1, key2);
    } else {
        assert_ne!(key1, key2);
    }
}

#[macro_export]
macro_rules! test_key_eq {
    ($builtin:expr, $($name:ident: $synth1:expr, $synth2:expr)*) => {$(
        #[test]
        fn $name() {
            $crate::util::check_key($builtin,true, $synth1, $synth2)
        }
    )*};
}

#[macro_export]
macro_rules! test_key_neq {
    ($builtin:expr, $($name:ident: $synth1:expr, $synth2:expr)*) => {$(
        #[test]
        fn $name() {
            $crate::util::check_key($builtin, false, $synth1, $synth2)
        }
    )*};
}

pub(crate) fn check_derivable<Sy>(builtin: DeriveBuiltin, synth: Sy, key: DeriveKey)
where
    Sy: FnOnce(&mut Subs) -> Variable,
{
    let mut subs = Subs::new();
    let var = synth(&mut subs);

    let derived = Derived::builtin(builtin, &subs, var);

    assert_eq!(derived, Ok(Derived::Key(key)));
}

pub(crate) fn check_underivable<Sy>(builtin: DeriveBuiltin, synth: Sy, err: DeriveError)
where
    Sy: FnOnce(&mut Subs) -> Variable,
{
    let mut subs = Subs::new();
    let var = synth(&mut subs);

    let key = Derived::builtin(builtin, &subs, var);

    assert_eq!(key, Err(err));
}

pub(crate) fn check_immediate<S>(builtin: DeriveBuiltin, synth: S, immediate: Symbol)
where
    S: FnOnce(&mut Subs) -> Variable,
{
    let mut subs = Subs::new();
    let var = synth(&mut subs);

    let key = Derived::builtin(builtin, &subs, var);

    assert_eq!(key, Ok(Derived::Immediate(immediate)));
}

pub(crate) fn check_single_lset_immediate<S>(builtin: DeriveBuiltin, synth: S, immediate: Symbol)
where
    S: FnOnce(&mut Subs) -> Variable,
{
    let mut subs = Subs::new();
    let var = synth(&mut subs);

    let key = Derived::builtin(builtin, &subs, var);

    assert_eq!(key, Ok(Derived::SingleLambdaSetImmediate(immediate)));
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
                ..DebugPrint::NOTHING
            },
        );
        subs.rollback_to(snapshot);
        pretty_type
    };

    let mut pretty_buf = String::new();

    // ignore returned result, writeln can not fail as it is used here
    let _ = writeln!(pretty_buf, "# derived for {}", print_var(source_var, false));

    let pretty_type = print_var(typ, false);
    let _ = writeln!(pretty_buf, "# {}", &pretty_type);

    let pretty_type_under_aliases = print_var(typ, true);
    let _ = writeln!(pretty_buf, "# {}", &pretty_type_under_aliases);

    pretty_buf.push_str("# Specialization lambda sets:\n");
    let mut specialization_lsets = specialization_lsets.into_iter().collect::<Vec<_>>();
    specialization_lsets.sort_by_key(|(region, _)| *region);
    for (region, var) in specialization_lsets {
        let pretty_lset = print_var(var, false);
        let _ = writeln!(pretty_buf, "#   @<{}>: {}", region, pretty_lset);
    }

    pretty_buf.push_str(derived_source);

    pretty_buf
}

/// The environment of the module containing the builtin ability we're deriving for a type.
struct DeriveBuiltinEnv {
    module_id: ModuleId,
    exposed_types: ExposedTypesStorageSubs,
    abilities_store: AbilitiesStore,
}

#[allow(clippy::too_many_arguments)]
fn check_derived_typechecks_and_golden(
    derived_def: Def,
    test_module: ModuleId,
    mut test_subs: Subs,
    interns: &Interns,
    derive_builtin_env: DeriveBuiltinEnv,
    source_var: Variable,
    derived_program: &str,
    specialization_lsets: SpecializationLambdaSets,
    check_golden: impl Fn(&str),
) {
    // constrain the derived
    let mut types = Types::new();
    let mut constraints = Constraints::new();
    let def_var = derived_def.expr_var;
    let mut decls = Declarations::new();
    decls.push_def(derived_def);
    let constr = constrain_decls(&mut types, &mut constraints, test_module, &decls);

    // the derived implementation on stuff from the builtin module, so
    //   - we need to add those dependencies as imported on the constraint
    //   - we need to add the builtin ability info to a local abilities store
    let values_to_import_from_builtin_module = derive_builtin_env
        .exposed_types
        .stored_vars_by_symbol
        .keys()
        .copied()
        .collect::<VecSet<_>>();
    let pending_abilities = derive_builtin_env
        .abilities_store
        .closure_from_imported(&values_to_import_from_builtin_module);
    let mut exposed_by_module = ExposedByModule::default();
    exposed_by_module.insert(
        derive_builtin_env.module_id,
        ExposedModuleTypes {
            exposed_types_storage_subs: derive_builtin_env.exposed_types,
            resolved_implementations: ResolvedImplementations::default(),
        },
    );
    let exposed_for_module = ExposedForModule::new(
        values_to_import_from_builtin_module.iter(),
        exposed_by_module,
    );
    let mut def_types = Default::default();
    let mut rigid_vars = Default::default();
    let (import_variables, abilities_store) = add_imports(
        test_module,
        &mut constraints,
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
        std::env::set_var(roc_debug_flags::ROC_PRINT_UNIFICATIONS, "1")
    );
    let (mut solved_subs, _, problems, _) = roc_solve::module::run_solve(
        test_module,
        types,
        &constraints,
        constr,
        RigidVariables::default(),
        test_subs,
        Default::default(),
        abilities_store,
        Default::default(),
        &exposed_for_module.exposed_by_module,
        Default::default(),
    );
    dbg_do!(
        roc_debug_flags::ROC_PRINT_UNIFICATIONS_DERIVED,
        std::env::set_var(roc_debug_flags::ROC_PRINT_UNIFICATIONS, "0")
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

fn get_key(builtin: DeriveBuiltin, subs: &Subs, var: Variable) -> DeriveKey {
    match Derived::builtin(builtin, subs, var) {
        Ok(Derived::Key(key)) => key,
        _ => unreachable!(),
    }
}

pub(crate) fn derive_test<S>(builtin: DeriveBuiltin, synth_input: S, check_golden: impl Fn(&str))
where
    S: FnOnce(&mut Subs) -> Variable,
{
    let arena = Bump::new();
    let (builtin_module, source, path) = module_source_and_path(builtin);
    let target_info = roc_target::TargetInfo::default_x86_64();

    let LoadedModule {
        mut interns,
        exposed_types_storage,
        abilities_store,
        resolved_implementations,
        ..
    } = roc_load_internal::file::load_and_typecheck_str(
        &arena,
        path.file_name().unwrap().into(),
        source,
        path.parent().unwrap().to_path_buf(),
        Default::default(),
        target_info,
        roc_reporting::report::RenderTarget::ColorTerminal,
        roc_reporting::report::DEFAULT_PALETTE,
        RocCacheDir::Disallowed,
        Threading::AllAvailable,
    )
    .unwrap();

    let mut subs = Subs::new();
    let ident_ids = IdentIds::default();
    let source_var = synth_input(&mut subs);
    let key = get_key(builtin, &subs, source_var);

    let mut derived_module = unsafe { DerivedModule::from_components(subs, ident_ids) };

    let mut exposed_by_module = ExposedByModule::default();
    exposed_by_module.insert(
        builtin_module,
        ExposedModuleTypes {
            exposed_types_storage_subs: exposed_types_storage.clone(),
            resolved_implementations,
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
        DeriveBuiltinEnv {
            module_id: builtin_module,
            exposed_types: exposed_types_storage,
            abilities_store,
        },
        source_var,
        &derived_program,
        specialization_lsets,
        check_golden,
    );
}
