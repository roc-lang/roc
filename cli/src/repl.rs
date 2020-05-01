use bumpalo::Bump;
use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::passes::PassManager;
use inkwell::types::BasicType;
use inkwell::OptimizationLevel;
use roc_builtins::unique::uniq_stdlib;
use roc_can::constraint::Constraint;
use roc_can::env::Env;
use roc_can::expected::Expected;
use roc_can::expr::{canonicalize_expr, Output};
use roc_can::operator;
use roc_can::scope::Scope;
use roc_collections::all::{ImMap, ImSet, MutMap, SendMap, SendSet};
use roc_constrain::expr::constrain_expr;
use roc_constrain::module::{constrain_imported_values, load_builtin_aliases, Import};
use roc_gen::llvm::build::{build_proc, build_proc_header, OptLevel};
use roc_gen::llvm::convert::basic_type_from_layout;
use roc_module::ident::Ident;
use roc_module::symbol::{IdentIds, Interns, ModuleId, ModuleIds, Symbol};
use roc_mono::expr::Procs;
use roc_mono::layout::Layout;
use roc_parse::ast::{self, Attempting};
use roc_parse::blankspace::space0_before;
use roc_parse::parser::{loc, Fail, FailReason, Parser, State};
use roc_problem::can::Problem;
use roc_region::all::{Located, Region};
use roc_solve::solve;
use roc_types::pretty_print::{content_to_string, name_all_type_vars};
use roc_types::subs::{Content, Subs, VarStore, Variable};
use roc_types::types::Type;
use std::hash::Hash;
use std::io::{self, Write};
use std::path::PathBuf;
use target_lexicon::Triple;

pub fn main() -> io::Result<()> {
    use std::io::BufRead;

    println!(
        "\n  The rockin’ \u{001b}[36mroc repl\u{001b}[0m\n\u{001b}[35m────────────────────────\u{001b}[0m\n\n{}",
        WELCOME_MESSAGE
    );

    // Loop

    let mut pending_src = String::new();
    let mut prev_line_blank = false;

    loop {
        if pending_src.is_empty() {
            print!("\n\u{001b}[36m▶\u{001b}[0m ");
        } else {
            print!("\u{001b}[36m…\u{001b}[0m ");
        }

        io::stdout().flush().unwrap();

        let stdin = io::stdin();
        let line = stdin
            .lock()
            .lines()
            .next()
            .expect("there was no next line")
            .expect("the line could not be read");

        match line.trim() {
            ":help" => {
                println!("Use :exit to exit.");
            }
            "" => {
                if pending_src.is_empty() {
                    println!("\n{}", WELCOME_MESSAGE);
                } else if prev_line_blank {
                    // After two blank lines in a row, give up and try parsing it
                    // even though it's going to fail. This way you don't get stuck.
                    match print_output(pending_src.as_str()) {
                        Ok(output) => {
                            println!("{}", output);
                        }
                        Err(fail) => {
                            report_parse_error(fail);
                        }
                    }

                    pending_src.clear();
                } else {
                    pending_src.push('\n');

                    prev_line_blank = true;
                    continue; // Skip the part where we reset prev_line_blank to false
                }
            }
            ":exit" => {
                break;
            }
            line => {
                let result = if pending_src.is_empty() {
                    print_output(line)
                } else {
                    pending_src.push('\n');
                    pending_src.push_str(line);

                    print_output(pending_src.as_str())
                };

                match result {
                    Ok(output) => {
                        println!("{}", output);
                        pending_src.clear();
                    }
                    Err(Fail {
                        reason: FailReason::Eof(_),
                        ..
                    }) => {
                        // If we hit an eof, and we're allowed to keep going,
                        // append the str to the src we're building up and continue.
                        // (We only need to append it here if it was empty before;
                        // otherwise, we already appended it before calling print_output.)

                        if pending_src.is_empty() {
                            pending_src.push_str(line);
                        }
                    }
                    Err(fail) => {
                        report_parse_error(fail);
                        pending_src.clear();
                    }
                }
            }
        }

        prev_line_blank = false;
    }

    Ok(())
}

const WELCOME_MESSAGE: &str =
    "Enter an expression, or :help for a list of commands, or :exit to exit.";

fn report_parse_error(fail: Fail) {
    println!("TODO Gracefully report parse error in repl: {:?}", fail);
}

fn print_output(src: &str) -> Result<String, Fail> {
    gen(src, Triple::host(), OptLevel::Normal).map(|(answer, answer_type)| {
        format!("\n{} \u{001b}[35m:\u{001b}[0m {}", answer, answer_type)
    })
}

pub fn repl_home() -> ModuleId {
    ModuleIds::default().get_or_insert(&"REPL".into())
}

pub fn gen(src: &str, target: Triple, opt_level: OptLevel) -> Result<(String, String), Fail> {
    use roc_reporting::report::{can_problem, type_problem, RocDocAllocator, DEFAULT_PALETTE};

    // Look up the types and expressions of the `provided` values
    let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;
    let arena = Bump::new();
    let CanExprOut {
        loc_expr,
        var_store,
        var,
        constraint,
        home,
        interns,
        problems: can_problems,
        ..
    } = can_expr(src)?;
    let subs = Subs::new(var_store.into());
    let mut type_problems = Vec::new();
    let (content, mut subs) = infer_expr(subs, &mut type_problems, &constraint, var);

    // Report problems
    let src_lines: Vec<&str> = src.split('\n').collect();
    let palette = DEFAULT_PALETTE;

    // Report parsing and canonicalization problems
    let alloc = RocDocAllocator::new(&src_lines, home, &interns);

    // Used for reporting where an error came from.
    //
    // TODO: maybe Reporting should have this be an Option?
    let path = PathBuf::new();

    for problem in can_problems.into_iter() {
        let report = can_problem(&alloc, path.clone(), problem);
        let mut buf = String::new();

        report.render_color_terminal(&mut buf, &alloc, &palette);

        println!("\n{}\n", buf);
    }

    for problem in type_problems.into_iter() {
        let report = type_problem(&alloc, path.clone(), problem);
        let mut buf = String::new();

        report.render_color_terminal(&mut buf, &alloc, &palette);

        println!("\n{}\n", buf);
    }

    let context = Context::create();
    let module = roc_gen::llvm::build::module_from_builtins(&context, "app");
    let builder = context.create_builder();
    let fpm = PassManager::create(&module);

    roc_gen::llvm::build::add_passes(&fpm, opt_level);

    fpm.initialize();

    // pretty-print the expr type string for later.
    name_all_type_vars(var, &mut subs);

    let expr_type_str = content_to_string(content.clone(), &subs, home, &interns);

    // Compute main_fn_type before moving subs to Env
    let layout = Layout::from_content(&arena, content, &subs, ptr_bytes).unwrap_or_else(|err| {
        panic!(
            "Code gen error in test: could not convert to layout. Err was {:?} and Subs were {:?}",
            err, subs
        )
    });
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("Error creating JIT execution engine for test");

    let main_fn_type =
        basic_type_from_layout(&arena, &context, &layout, ptr_bytes).fn_type(&[], false);
    let main_fn_name = "$Test.main";

    // Compile and add all the Procs before adding main
    let mut env = roc_gen::llvm::build::Env {
        arena: &arena,
        builder: &builder,
        context: &context,
        interns,
        module: arena.alloc(module),
        ptr_bytes,
    };
    let mut procs = Procs::default();
    let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();

    // Populate Procs and get the low-level Expr from the canonical Expr
    let mut mono_problems = Vec::new();
    let mut mono_env = roc_mono::expr::Env {
        arena: &arena,
        subs: &mut subs,
        problems: &mut mono_problems,
        home,
        ident_ids: &mut ident_ids,
        pointer_size: ptr_bytes,
        symbol_counter: 0,
        jump_counter: arena.alloc(0),
    };
    let main_body = roc_mono::expr::Expr::new(&mut mono_env, loc_expr.value, &mut procs);

    // Put this module's ident_ids back in the interns, so we can use them in Env.
    env.interns.all_ident_ids.insert(home, ident_ids);

    let mut headers = Vec::with_capacity(procs.len());

    // Add all the Proc headers to the module.
    // We have to do this in a separate pass first,
    // because their bodies may reference each other.
    for (symbol, opt_proc) in procs.as_map().into_iter() {
        if let Some(proc) = opt_proc {
            let (fn_val, arg_basic_types) = build_proc_header(&env, symbol, &proc);

            headers.push((proc, fn_val, arg_basic_types));
        }
    }

    // Build each proc using its header info.
    for (proc, fn_val, arg_basic_types) in headers {
        // NOTE: This is here to be uncommented in case verification fails.
        // (This approach means we don't have to defensively clone name here.)
        //
        // println!("\n\nBuilding and then verifying function {}\n\n", name);
        build_proc(&env, proc, &procs, fn_val, arg_basic_types);

        if fn_val.verify(true) {
            fpm.run_on(&fn_val);
        } else {
            // NOTE: If this fails, uncomment the above println to debug.
            panic!(
                "Non-main function failed LLVM verification. Uncomment the above println to debug!"
            );
        }
    }

    // Add main to the module.
    let main_fn = env.module.add_function(main_fn_name, main_fn_type, None);
    let cc =
        roc_gen::llvm::build::get_call_conventions(target.default_calling_convention().unwrap());

    main_fn.set_call_conventions(cc);

    // Add main's body
    let basic_block = context.append_basic_block(main_fn, "entry");

    builder.position_at_end(basic_block);

    let ret = roc_gen::llvm::build::build_expr(
        &env,
        &ImMap::default(),
        main_fn,
        &main_body,
        &Procs::default(),
    );

    builder.build_return(Some(&ret));

    // Uncomment this to see the module's un-optimized LLVM instruction output:
    // env.module.print_to_stderr();

    if main_fn.verify(true) {
        fpm.run_on(&main_fn);
    } else {
        panic!("Function {} failed LLVM verification.", main_fn_name);
    }

    // Verify the module
    if let Err(errors) = env.module.verify() {
        panic!("Errors defining module: {:?}", errors);
    }

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    unsafe {
        let main: JitFunction<
            unsafe extern "C" fn() -> i64, /* TODO have this return Str, and in the generated code make sure to call the appropriate string conversion function on the return val based on its type! */
        > = execution_engine
            .get_function(main_fn_name)
            .ok()
            .ok_or(format!("Unable to JIT compile `{}`", main_fn_name))
            .expect("errored");

        Ok((format!("{}", main.call()), expr_type_str))
    }
}

pub fn infer_expr(
    subs: Subs,
    problems: &mut Vec<solve::TypeError>,
    constraint: &Constraint,
    expr_var: Variable,
) -> (Content, Subs) {
    let env = solve::Env {
        aliases: MutMap::default(),
        vars_by_symbol: SendMap::default(),
    };
    let (solved, _) = solve::run(&env, problems, subs, constraint);

    let content = solved.inner().get_without_compacting(expr_var).content;

    (content, solved.into_inner())
}

pub fn parse_loc_with<'a>(arena: &'a Bump, input: &'a str) -> Result<Located<ast::Expr<'a>>, Fail> {
    let state = State::new(&input, Attempting::Module);
    let parser = space0_before(loc(roc_parse::expr::expr(0)), 0);
    let answer = parser.parse(&arena, state);

    answer
        .map(|(loc_expr, _)| loc_expr)
        .map_err(|(fail, _)| fail)
}

pub fn can_expr(expr_str: &str) -> Result<CanExprOut, Fail> {
    can_expr_with(&Bump::new(), repl_home(), expr_str)
}

// TODO make this return a named struct instead of a big tuple
#[allow(clippy::type_complexity)]
pub fn uniq_expr(
    expr_str: &str,
) -> Result<
    (
        Located<roc_can::expr::Expr>,
        Output,
        Vec<Problem>,
        Subs,
        Variable,
        Constraint,
        ModuleId,
        Interns,
    ),
    Fail,
> {
    let declared_idents: &ImMap<Ident, (Symbol, Region)> = &ImMap::default();

    uniq_expr_with(&Bump::new(), expr_str, declared_idents)
}

// TODO make this return a named struct instead of a big tuple
#[allow(clippy::type_complexity)]
pub fn uniq_expr_with(
    arena: &Bump,
    expr_str: &str,
    declared_idents: &ImMap<Ident, (Symbol, Region)>,
) -> Result<
    (
        Located<roc_can::expr::Expr>,
        Output,
        Vec<Problem>,
        Subs,
        Variable,
        Constraint,
        ModuleId,
        Interns,
    ),
    Fail,
> {
    let home = repl_home();
    let CanExprOut {
        loc_expr,
        output,
        problems,
        var_store: old_var_store,
        var,
        interns,
        ..
    } = can_expr_with(arena, home, expr_str)?;

    // double check
    let var_store = VarStore::new(old_var_store.fresh());

    let expected2 = Expected::NoExpectation(Type::Variable(var));
    let constraint = roc_constrain::uniq::constrain_declaration(
        home,
        &var_store,
        Region::zero(),
        &loc_expr,
        declared_idents,
        expected2,
    );

    let stdlib = uniq_stdlib();

    let types = stdlib.types;
    let imports: Vec<_> = types
        .iter()
        .map(|(symbol, (solved_type, region))| Import {
            loc_symbol: Located::at(*region, *symbol),
            solved_type,
        })
        .collect();

    // load builtin values

    // TODO what to do with those rigids?
    let (_introduced_rigids, constraint) =
        constrain_imported_values(imports, constraint, &var_store);

    // load builtin types
    let mut constraint = load_builtin_aliases(&stdlib.aliases, constraint, &var_store);

    constraint.instantiate_aliases(&var_store);

    let subs2 = Subs::new(var_store.into());

    Ok((
        loc_expr, output, problems, subs2, var, constraint, home, interns,
    ))
}

pub struct CanExprOut {
    pub loc_expr: Located<roc_can::expr::Expr>,
    pub output: Output,
    pub problems: Vec<Problem>,
    pub home: ModuleId,
    pub interns: Interns,
    pub var_store: VarStore,
    pub var: Variable,
    pub constraint: Constraint,
}

pub fn can_expr_with(arena: &Bump, home: ModuleId, expr_str: &str) -> Result<CanExprOut, Fail> {
    let loc_expr = parse_loc_with(&arena, expr_str)?;
    let var_store = VarStore::default();
    let var = var_store.fresh();
    let expected = Expected::NoExpectation(Type::Variable(var));
    let module_ids = ModuleIds::default();

    // Desugar operators (convert them to Apply calls, taking into account
    // operator precedence and associativity rules), before doing other canonicalization.
    //
    // If we did this *during* canonicalization, then each time we
    // visited a BinOp node we'd recursively try to apply this to each of its nested
    // operators, and then again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.
    let loc_expr = operator::desugar_expr(arena, &loc_expr);

    let mut scope = Scope::new(home);
    let dep_idents = IdentIds::exposed_builtins(0);
    let mut env = Env::new(home, dep_idents, &module_ids, IdentIds::default());
    let (loc_expr, output) = canonicalize_expr(
        &mut env,
        &var_store,
        &mut scope,
        Region::zero(),
        &loc_expr.value,
    );

    let constraint = constrain_expr(
        &roc_constrain::expr::Env {
            rigids: ImMap::default(),
            home,
        },
        loc_expr.region,
        &loc_expr.value,
        expected,
    );

    let types = roc_builtins::std::types();

    let imports: Vec<_> = types
        .iter()
        .map(|(symbol, (solved_type, region))| Import {
            loc_symbol: Located::at(*region, *symbol),
            solved_type,
        })
        .collect();

    //load builtin values
    let (_introduced_rigids, constraint) =
        constrain_imported_values(imports, constraint, &var_store);

    // TODO determine what to do with those rigids
    //    for var in introduced_rigids {
    //        output.ftv.insert(var, format!("internal_{:?}", var).into());
    //    }

    //load builtin types
    let mut constraint =
        load_builtin_aliases(&roc_builtins::std::aliases(), constraint, &var_store);

    constraint.instantiate_aliases(&var_store);

    let mut all_ident_ids = MutMap::default();

    // When pretty printing types, we may need the exposed builtins,
    // so include them in the Interns we'll ultimately return.
    for (module_id, ident_ids) in IdentIds::exposed_builtins(0) {
        all_ident_ids.insert(module_id, ident_ids);
    }

    all_ident_ids.insert(home, env.ident_ids);

    let interns = Interns {
        module_ids: env.module_ids.clone(),
        all_ident_ids,
    };

    Ok(CanExprOut {
        loc_expr,
        output,
        problems: env.problems,
        home: env.home,
        var_store,
        interns,
        var,
        constraint,
    })
}

pub fn mut_map_from_pairs<K, V, I>(pairs: I) -> MutMap<K, V>
where
    I: IntoIterator<Item = (K, V)>,
    K: Hash + Eq,
{
    let mut answer = MutMap::default();

    for (key, value) in pairs {
        answer.insert(key, value);
    }

    answer
}

pub fn im_map_from_pairs<K, V, I>(pairs: I) -> ImMap<K, V>
where
    I: IntoIterator<Item = (K, V)>,
    K: Hash + Eq + Clone,
    V: Clone,
{
    let mut answer = ImMap::default();

    for (key, value) in pairs {
        answer.insert(key, value);
    }

    answer
}

pub fn send_set_from<V, I>(elems: I) -> SendSet<V>
where
    I: IntoIterator<Item = V>,
    V: Hash + Eq + Clone,
{
    let mut answer = SendSet::default();

    for elem in elems {
        answer.insert(elem);
    }

    answer
}

// Check constraints
//
// Keep track of the used (in types or expectations) variables, and the declared variables (in
// flex_vars or rigid_vars fields of LetConstraint. These roc_collections should match: no duplicates
// and no variables that are used but not declared are allowed.
//
// There is one exception: the initial variable (that stores the type of the whole expression) is
// never declared, but is used.
pub fn assert_correct_variable_usage(constraint: &Constraint) {
    // variables declared in constraint (flex_vars or rigid_vars)
    // and variables actually used in constraints
    let (declared, used) = variable_usage(constraint);

    let used: ImSet<Variable> = used.into();
    let mut decl: ImSet<Variable> = declared.rigid_vars.clone().into();

    for var in declared.flex_vars.clone() {
        decl.insert(var);
    }

    let diff = used.clone().relative_complement(decl);

    // NOTE: this checks whether we're using variables that are not declared. For recursive type
    // definitions,  their rigid types are declared twice, which is correct!
    if !diff.is_empty() {
        println!("VARIABLE USAGE PROBLEM");

        println!("used: {:?}", &used);
        println!("rigids: {:?}", &declared.rigid_vars);
        println!("flexs: {:?}", &declared.flex_vars);

        println!("difference: {:?}", &diff);

        panic!("variable usage problem (see stdout for details)");
    }
}

#[derive(Default)]
pub struct SeenVariables {
    pub rigid_vars: Vec<Variable>,
    pub flex_vars: Vec<Variable>,
}

pub fn variable_usage(con: &Constraint) -> (SeenVariables, Vec<Variable>) {
    let mut declared = SeenVariables::default();
    let mut used = ImSet::default();
    variable_usage_help(con, &mut declared, &mut used);

    used.remove(unsafe { &Variable::unsafe_test_debug_variable(1) });

    let mut used_vec: Vec<Variable> = used.into_iter().collect();
    used_vec.sort();

    declared.rigid_vars.sort();
    declared.flex_vars.sort();

    (declared, used_vec)
}

fn variable_usage_help(con: &Constraint, declared: &mut SeenVariables, used: &mut ImSet<Variable>) {
    use Constraint::*;

    match con {
        True | SaveTheEnvironment => (),
        Eq(tipe, expectation, _, _) => {
            for v in tipe.variables() {
                used.insert(v);
            }

            for v in expectation.get_type_ref().variables() {
                used.insert(v);
            }
        }
        Lookup(_, expectation, _) => {
            for v in expectation.get_type_ref().variables() {
                used.insert(v);
            }
        }
        Pattern(_, _, tipe, pexpectation) => {
            for v in tipe.variables() {
                used.insert(v);
            }

            for v in pexpectation.get_type_ref().variables() {
                used.insert(v);
            }
        }
        Let(letcon) => {
            declared.rigid_vars.extend(letcon.rigid_vars.clone());
            declared.flex_vars.extend(letcon.flex_vars.clone());

            variable_usage_help(&letcon.defs_constraint, declared, used);
            variable_usage_help(&letcon.ret_constraint, declared, used);
        }
        And(constraints) => {
            for sub in constraints {
                variable_usage_help(sub, declared, used);
            }
        }
    }
}
