use bumpalo::Bump;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::OptimizationLevel;
use roc_builtins::unique::uniq_stdlib;
use roc_can::constraint::Constraint;
use roc_can::expected::Expected;
use roc_can::expr::{canonicalize_expr, Expr, Output};
use roc_can::operator;
use roc_can::scope::Scope;
use roc_collections::all::{ImMap, ImSet, MutMap, MutSet, SendMap, SendSet};
use roc_constrain::expr::constrain_expr;
use roc_constrain::module::{constrain_imported_values, load_builtin_aliases, Import};
use roc_fmt::annotation::{Formattable, Newlines, Parens};
use roc_gen::layout_id::LayoutIds;
use roc_gen::llvm::build::{build_proc, build_proc_header, OptLevel};
use roc_module::ident::Ident;
use roc_module::symbol::{IdentIds, Interns, ModuleId, ModuleIds, Symbol};
use roc_mono::ir::Procs;
use roc_mono::layout::{Layout, LayoutCache};
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
use std::path::{Path, PathBuf};
use std::str::from_utf8_unchecked;
use target_lexicon::Triple;

pub const WELCOME_MESSAGE: &str = "\n  The rockin’ \u{001b}[36mroc repl\u{001b}[0m\n\u{001b}[35m────────────────────────\u{001b}[0m\n\n";
pub const INSTRUCTIONS: &str = "Enter an expression, or :help, or :exit.\n";
pub const PROMPT: &str = "\n\u{001b}[36m»\u{001b}[0m ";
pub const ELLIPSIS: &str = "\u{001b}[36m…\u{001b}[0m ";

mod eval;

pub fn main() -> io::Result<()> {
    use std::io::BufRead;

    print!("{}{}", WELCOME_MESSAGE, INSTRUCTIONS);

    // Loop

    let mut pending_src = String::new();
    let mut prev_line_blank = false;

    loop {
        if pending_src.is_empty() {
            print!("{}", PROMPT);
        } else {
            print!("{}", ELLIPSIS);
        }

        io::stdout().flush().unwrap();

        let stdin = io::stdin();
        let line = stdin
            .lock()
            .lines()
            .next()
            .expect("there was no next line")
            .expect("the line could not be read");

        let line = line.trim();

        match line.to_lowercase().as_str() {
            ":help" => {
                println!("Use :exit to exit.");
            }
            "" => {
                if pending_src.is_empty() {
                    print!("\n{}", INSTRUCTIONS);
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
            _ => {
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

fn report_parse_error(fail: Fail) {
    println!("TODO Gracefully report parse error in repl: {:?}", fail);
}

fn print_output(src: &str) -> Result<String, Fail> {
    gen(src.as_bytes(), Triple::host(), OptLevel::Normal).map(|output| match output {
        ReplOutput::NoProblems { expr, expr_type } => {
            format!("\n{} \u{001b}[35m:\u{001b}[0m {}", expr, expr_type)
        }
        ReplOutput::Problems(lines) => format!("\n{}\n", lines.join("\n\n")),
    })
}

pub fn repl_home() -> ModuleId {
    ModuleIds::default().get_or_insert(&"REPL".into())
}

fn promote_expr_to_module(src: &str) -> String {
    let mut buffer = String::from("app Repl provides [ replOutput ] imports []\n\nreplOutput =\n");

    for line in src.lines() {
        // indent the body!
        buffer.push_str("    ");
        buffer.push_str(line);
        buffer.push('\n');
    }

    buffer
}

fn gen(src: &[u8], target: Triple, opt_level: OptLevel) -> Result<ReplOutput, Fail> {
    use roc_reporting::report::{can_problem, type_problem, RocDocAllocator, DEFAULT_PALETTE};

    let arena = Bump::new();

    // SAFETY: we've already verified that this is valid UTF-8 during parsing.
    let src_str: &str = unsafe { from_utf8_unchecked(src) };

    let stdlib = roc_builtins::std::standard_stdlib();
    let stdlib_mode = stdlib.mode;
    let filename = PathBuf::from("REPL.roc");
    let src_dir = Path::new("fake/test/path");

    let module_src = promote_expr_to_module(src_str);

    let exposed_types = MutMap::default();
    let loaded = roc_load::file::load_and_monomorphize_from_str(
        &arena,
        filename,
        &module_src,
        stdlib,
        src_dir,
        exposed_types,
    );

    let loaded = loaded.expect("failed to load module");

    use roc_load::file::MonomorphizedModule;
    let MonomorphizedModule {
        can_problems,
        type_problems,
        mono_problems,
        mut procedures,
        interns,
        exposed_to_host,
        mut subs,
        module_id: home,
        ..
    } = loaded;

    let error_count = can_problems.len() + type_problems.len() + mono_problems.len();

    if error_count > 0 {
        // There were problems; report them and return.
        let src_lines: Vec<&str> = src_str.split('\n').collect();

        // Used for reporting where an error came from.
        //
        // TODO: maybe Reporting should have this be an Option?
        let path = PathBuf::new();

        // Report problems
        let palette = DEFAULT_PALETTE;

        // Report parsing and canonicalization problems
        let alloc = RocDocAllocator::new(&src_lines, home, &interns);

        let mut lines = Vec::with_capacity(error_count);

        for problem in can_problems.into_iter() {
            let report = can_problem(&alloc, path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            lines.push(buf);
        }

        for problem in type_problems.into_iter() {
            let report = type_problem(&alloc, path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            lines.push(buf);
        }

        Ok(ReplOutput::Problems(lines))
    } else {
        let context = Context::create();
        let module = arena.alloc(roc_gen::llvm::build::module_from_builtins(&context, "app"));
        let builder = context.create_builder();

        debug_assert_eq!(exposed_to_host.len(), 1);
        let (main_fn_symbol, main_fn_var) = exposed_to_host.iter().nth(0).unwrap();
        let main_fn_symbol = *main_fn_symbol;
        let main_fn_var = *main_fn_var;

        // pretty-print the expr type string for later.
        name_all_type_vars(main_fn_var, &mut subs);
        let content = subs.get(main_fn_var).content;
        let expr_type_str = content_to_string(content.clone(), &subs, home, &interns);

        let (_, main_fn_layout) = procedures
            .keys()
            .find(|(s, _)| *s == main_fn_symbol)
            .unwrap()
            .clone();

        let target = target_lexicon::Triple::host();
        let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;

        let opt_level = if cfg!(debug_assertions) {
            roc_gen::llvm::build::OptLevel::Normal
        } else {
            roc_gen::llvm::build::OptLevel::Optimize
        };

        let module = arena.alloc(module);
        let (module_pass, function_pass) =
            roc_gen::llvm::build::construct_optimization_passes(module, opt_level);

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("Error creating JIT execution engine for test");

        // Compile and add all the Procs before adding main
        let env = roc_gen::llvm::build::Env {
            arena: &arena,
            builder: &builder,
            context: &context,
            interns,
            module,
            ptr_bytes,
            leak: false,
            // important! we don't want any procedures to get the C calling convention
            exposed_to_host: MutSet::default(),
        };

        let mut layout_ids = roc_gen::layout_id::LayoutIds::default();
        let mut headers = Vec::with_capacity(procedures.len());

        // Add all the Proc headers to the module.
        // We have to do this in a separate pass first,
        // because their bodies may reference each other.
        let mut scope = roc_gen::llvm::build::Scope::default();
        for ((symbol, layout), proc) in procedures.drain() {
            let fn_val = build_proc_header(&env, &mut layout_ids, symbol, &layout, &proc);

            if proc.args.is_empty() {
                // this is a 0-argument thunk, i.e. a top-level constant definition
                // it must be in-scope everywhere in the module!
                scope.insert_top_level_thunk(symbol, layout, fn_val);
            }

            headers.push((proc, fn_val));
        }

        // Build each proc using its header info.
        for (proc, fn_val) in headers {
            let mut current_scope = scope.clone();

            // only have top-level thunks for this proc's module in scope
            // this retain is not needed for correctness, but will cause less confusion when debugging
            let home = proc.name.module_id();
            current_scope.retain_top_level_thunks_for_module(home);

            build_proc(&env, &mut layout_ids, scope.clone(), proc, fn_val);

            if fn_val.verify(true) {
                function_pass.run_on(&fn_val);
            } else {
                use roc_builtins::std::Mode;

                let mode = match stdlib_mode {
                    Mode::Uniqueness => "OPTIMIZED",
                    Mode::Standard => "NON-OPTIMIZED",
                };

                eprintln!(
                    "\n\nFunction {:?} failed LLVM verification in {} build. Its content was:\n",
                    fn_val.get_name().to_str().unwrap(),
                    mode,
                );

                fn_val.print_to_stderr();

                panic!(
                    "The preceding code was from {:?}, which failed LLVM verification in {} build.",
                    fn_val.get_name().to_str().unwrap(),
                    mode,
                );
            }
        }
        let (main_fn_name, main_fn) = roc_gen::llvm::build::promote_to_main_function(
            &env,
            &mut layout_ids,
            main_fn_symbol,
            &main_fn_layout,
        );

        // Uncomment this to see the module's un-optimized LLVM instruction output:
        // env.module.print_to_stderr();

        if main_fn.verify(true) {
            function_pass.run_on(&main_fn);
        } else {
            panic!("Main function {} failed LLVM verification in build. Uncomment things nearby to see more details.", main_fn_name);
        }

        module_pass.run_on(env.module);

        // Verify the module
        if let Err(errors) = env.module.verify() {
            panic!("Errors defining module: {:?}", errors);
        }

        // Uncomment this to see the module's optimized LLVM instruction output:
        // env.module.print_to_stderr();

        let answer = unsafe {
            eval::jit_to_ast(
                &arena,
                execution_engine,
                main_fn_name,
                &main_fn_layout,
                &content,
                &env.interns,
                home,
                &subs,
                ptr_bytes,
            )
        };
        let mut expr = bumpalo::collections::String::new_in(&arena);

        answer.format_with_options(&mut expr, Parens::NotNeeded, Newlines::Yes, 0);

        Ok(ReplOutput::NoProblems {
            expr: expr.into_bump_str().to_string(),
            expr_type: expr_type_str,
        })
    }
}

enum ReplOutput {
    Problems(Vec<String>),
    NoProblems { expr: String, expr_type: String },
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

pub fn parse_loc_with<'a>(
    arena: &'a Bump,
    bytes: &'a [u8],
) -> Result<Located<ast::Expr<'a>>, Fail> {
    let state = State::new(&bytes, Attempting::Module);
    let parser = space0_before(loc(roc_parse::expr::expr(0)), 0);
    let answer = parser.parse(&arena, state);

    answer
        .map(|(loc_expr, _)| loc_expr)
        .map_err(|(fail, _)| fail)
}

pub fn can_expr(expr_bytes: &[u8]) -> Result<CanExprOut, Fail> {
    can_expr_with(&Bump::new(), repl_home(), expr_bytes)
}

// TODO make this return a named struct instead of a big tuple
#[allow(clippy::type_complexity)]
pub fn uniq_expr(
    expr_bytes: &[u8],
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

    uniq_expr_with(&Bump::new(), expr_bytes, declared_idents)
}

// TODO make this return a named struct instead of a big tuple
#[allow(clippy::type_complexity)]
pub fn uniq_expr_with(
    arena: &Bump,
    expr_bytes: &[u8],
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
        var_store: mut old_var_store,
        var,
        interns,
        ..
    } = can_expr_with(arena, home, expr_bytes)?;

    // double check
    let mut var_store = VarStore::new(old_var_store.fresh());

    let expected2 = Expected::NoExpectation(Type::Variable(var));
    let constraint = roc_constrain::uniq::constrain_declaration(
        home,
        &mut var_store,
        Region::zero(),
        &loc_expr,
        declared_idents,
        expected2,
    );

    let stdlib = uniq_stdlib();

    let types = stdlib.types;
    let imports: Vec<_> = types
        .into_iter()
        .map(|(symbol, (solved_type, region))| Import {
            loc_symbol: Located::at(region, symbol),
            solved_type,
        })
        .collect();

    // load builtin values

    // TODO what to do with those rigids?
    let (_introduced_rigids, constraint) =
        constrain_imported_values(imports, constraint, &mut var_store);

    // load builtin types
    let mut constraint = load_builtin_aliases(stdlib.aliases, constraint, &mut var_store);

    constraint.instantiate_aliases(&mut var_store);

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

pub fn can_expr_with(arena: &Bump, home: ModuleId, expr_bytes: &[u8]) -> Result<CanExprOut, Fail> {
    let loc_expr = parse_loc_with(&arena, expr_bytes)?;
    let mut var_store = VarStore::default();
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
    let mut env = roc_can::env::Env::new(home, dep_idents, &module_ids, IdentIds::default());
    let (loc_expr, output) = canonicalize_expr(
        &mut env,
        &mut var_store,
        &mut scope,
        Region::zero(),
        &loc_expr.value,
    );

    // Add builtin defs (e.g. List.get) directly to the canonical Expr,
    // since we aren't using modules here.
    let mut with_builtins = loc_expr.value;
    let builtin_defs = roc_can::builtins::builtin_defs(&mut var_store);

    for (symbol, def) in builtin_defs {
        if output.references.lookups.contains(&symbol) || output.references.calls.contains(&symbol)
        {
            with_builtins = Expr::LetNonRec(
                Box::new(def),
                Box::new(Located {
                    region: Region::zero(),
                    value: with_builtins,
                }),
                var_store.fresh(),
                SendMap::default(),
            );
        }
    }

    let loc_expr = Located {
        region: loc_expr.region,
        value: with_builtins,
    };

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
        .into_iter()
        .map(|(symbol, (solved_type, region))| Import {
            loc_symbol: Located::at(region, symbol),
            solved_type,
        })
        .collect();

    //load builtin values
    let (_introduced_rigids, constraint) =
        constrain_imported_values(imports, constraint, &mut var_store);

    // TODO determine what to do with those rigids
    //    for var in introduced_rigids {
    //        output.ftv.insert(var, format!("internal_{:?}", var).into());
    //    }

    //load builtin types
    let mut constraint =
        load_builtin_aliases(roc_builtins::std::aliases(), constraint, &mut var_store);

    constraint.instantiate_aliases(&mut var_store);

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
