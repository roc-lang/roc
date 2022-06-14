#![cfg(test)]

use std::path::PathBuf;

use bumpalo::Bump;
use roc_can::{
    abilities::{AbilitiesStore, ResolvedSpecializations},
    constraint::Constraints,
    expected::Expected,
    expr::Expr,
    module::RigidVariables,
};
use roc_collections::VecSet;
use roc_constrain::{
    expr::constrain_expr,
    module::{ExposedByModule, ExposedForModule, ExposedModuleTypes},
};
use roc_load_internal::file::{add_imports, default_aliases, LoadedModule, Threading};
use roc_module::{
    ident::ModuleName,
    symbol::{IdentIds, Interns, ModuleId},
};
use roc_mono::derivers::encoding::Env;
use roc_region::all::{LineInfo, Region};
use roc_reporting::report::{type_problem, RocDocAllocator};
use roc_types::{
    subs::{Content, ExposedTypesStorageSubs, FlatType, RecordFields, Subs, Variable},
    types::{RecordField, Type},
};
use ven_pretty::DocAllocator;

use roc_mono::derivers::{encoding, synth_var};

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
) {
    let mut constraints = Constraints::new();
    let mut env = roc_constrain::expr::Env {
        rigids: Default::default(),
        resolutions_to_make: Default::default(),
        home: test_module,
    };
    let constr = constrain_expr(
        &mut constraints,
        &mut env,
        Region::zero(),
        &derived,
        Expected::NoExpectation(Type::Variable(test_subs.fresh_unnamed_flex_var())),
    );
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

    let (_solved_subs, _, problems, _) = roc_solve::module::run_solve(
        &constraints,
        constr,
        RigidVariables::default(),
        test_subs,
        default_aliases(),
        abilities_store,
        Default::default(),
    );

    if !problems.is_empty() {
        let filename = PathBuf::from("Test.roc");
        let lines = LineInfo::new(" ");
        let src_lines = vec![" "];
        let mut reports = Vec::new();
        let alloc = RocDocAllocator::new(&src_lines, test_module, &interns);

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
}

fn derive_test<S>(synth_input: S)
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
        &encode_path().parent().unwrap(),
        Default::default(),
        target_info,
        roc_reporting::report::RenderTarget::ColorTerminal,
        Threading::AllAvailable,
    )
    .unwrap();

    let test_module = interns.module_id(&ModuleName::from("Test"));
    let mut test_subs = Subs::new();
    let mut test_ident_ids = IdentIds::default();

    let mut env = Env {
        home: test_module,
        arena: &arena,
        subs: &mut test_subs,
        ident_ids: &mut test_ident_ids,
        exposed_encode_types: &mut exposed_encode_types,
    };

    let signature_var = synth_input(env.subs);

    let derived = encoding::derive_to_encoder(&mut env, signature_var);

    check_derived_typechecks(
        derived,
        test_module,
        test_subs,
        &interns,
        exposed_encode_types,
        abilities_store,
    );
}

macro_rules! synth {
    (rcd{ $($field:literal: $typ:expr),* }) => {
        |subs| {
            let fields = RecordFields::insert_into_subs(subs, vec![ $( ($field.into(), RecordField::Required($typ)) ,)* ]);
            synth_var(subs, Content::Structure(FlatType::Record(fields, Variable::EMPTY_RECORD)))
        }
    }
}

#[test]
fn one_field_record() {
    derive_test(synth!(rcd{ "a": Variable::U8 }))
}
