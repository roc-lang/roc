use crate::annotation::IntroducedVariables;
use crate::builtins::builtin_defs;
use crate::def::{can_defs_with_return, Def};
use crate::env::Env;
use crate::num::{
    finish_parsing_base, finish_parsing_float, finish_parsing_int, float_expr_from_result,
    int_expr_from_result, num_expr_from_result,
};
use crate::pattern::{canonicalize_pattern, Pattern};
use crate::procedure::References;
use crate::scope::Scope;
use inlinable_string::InlinableString;
use roc_collections::all::{ImSet, MutMap, MutSet, SendMap};
use roc_module::ident::{Lowercase, TagName};
use roc_module::low_level::LowLevel;
use roc_module::operator::CalledVia;
use roc_module::symbol::Symbol;
use roc_parse::ast::{self, EscapedChar, StrLiteral};
use roc_parse::pattern::PatternType::*;
use roc_problem::can::{PrecedenceProblem, Problem, RuntimeError};
use roc_region::all::{Located, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::Alias;
use std::fmt::Debug;
use std::{char, i64, u32};

#[derive(Clone, Default, Debug, PartialEq)]
pub struct Output {
    pub references: References,
    pub tail_call: Option<Symbol>,
    pub introduced_variables: IntroducedVariables,
    pub aliases: SendMap<Symbol, Alias>,
    pub non_closures: MutSet<Symbol>,
}

impl Output {
    pub fn union(&mut self, other: Self) {
        self.references.union_mut(other.references);

        if let (None, Some(later)) = (self.tail_call, other.tail_call) {
            self.tail_call = Some(later);
        }

        self.introduced_variables.union(&other.introduced_variables);
        self.aliases.extend(other.aliases);
        self.non_closures.extend(other.non_closures);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals

    // Num stores the `a` variable in `Num a`. Not the same as the variable
    // stored in Int and Float below, which is strictly for better error messages
    Num(Variable, i64),

    // Int and Float store a variable to generate better error messages
    Int(Variable, i64),
    Float(Variable, f64),
    Str(InlinableString),
    List {
        list_var: Variable, // required for uniqueness of the list
        elem_var: Variable,
        loc_elems: Vec<Located<Expr>>,
    },

    // Lookups
    Var(Symbol),
    // Branching
    When {
        cond_var: Variable,
        expr_var: Variable,
        region: Region,
        loc_cond: Box<Located<Expr>>,
        branches: Vec<WhenBranch>,
    },
    If {
        cond_var: Variable,
        branch_var: Variable,
        branches: Vec<(Located<Expr>, Located<Expr>)>,
        final_else: Box<Located<Expr>>,
    },

    // Let
    LetRec(Vec<Def>, Box<Located<Expr>>, Variable),
    LetNonRec(Box<Def>, Box<Located<Expr>>, Variable),

    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    Call(
        Box<(Variable, Located<Expr>, Variable, Variable)>,
        Vec<(Variable, Located<Expr>)>,
        CalledVia,
    ),
    RunLowLevel {
        op: LowLevel,
        args: Vec<(Variable, Expr)>,
        ret_var: Variable,
    },

    Closure {
        function_type: Variable,
        closure_type: Variable,
        closure_ext_var: Variable,
        return_type: Variable,
        name: Symbol,
        captured_symbols: Vec<(Symbol, Variable)>,
        recursive: Recursive,
        arguments: Vec<(Variable, Located<Pattern>)>,
        loc_body: Box<Located<Expr>>,
    },

    // Product Types
    Record {
        record_var: Variable,
        fields: SendMap<Lowercase, Field>,
    },

    /// Empty record constant
    EmptyRecord,

    /// Look up exactly one field on a record, e.g. (expr).foo.
    Access {
        record_var: Variable,
        ext_var: Variable,
        field_var: Variable,
        loc_expr: Box<Located<Expr>>,
        field: Lowercase,
    },
    /// field accessor as a function, e.g. (.foo) expr
    Accessor {
        function_var: Variable,
        record_var: Variable,
        closure_var: Variable,
        ext_var: Variable,
        field_var: Variable,
        field: Lowercase,
    },

    Update {
        record_var: Variable,
        ext_var: Variable,
        symbol: Symbol,
        updates: SendMap<Lowercase, Field>,
    },

    // Sum Types
    Tag {
        variant_var: Variable,
        ext_var: Variable,
        name: TagName,
        arguments: Vec<(Variable, Located<Expr>)>,
    },

    // Compiles, but will crash if reached
    RuntimeError(RuntimeError),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub var: Variable,
    // The region of the full `foo: f bar`, rather than just `f bar`
    pub region: Region,
    pub loc_expr: Box<Located<Expr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Recursive {
    Recursive,
    TailRecursive,
    NotRecursive,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhenBranch {
    pub patterns: Vec<Located<Pattern>>,
    pub value: Located<Expr>,
    pub guard: Option<Located<Expr>>,
}

pub fn canonicalize_expr<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    region: Region,
    expr: &'a ast::Expr<'a>,
) -> (Located<Expr>, Output) {
    use Expr::*;

    let (expr, output) = match expr {
        ast::Expr::Num(string) => {
            let answer = num_expr_from_result(var_store, finish_parsing_int(*string), region, env);

            (answer, Output::default())
        }
        ast::Expr::Float(string) => {
            let answer =
                float_expr_from_result(var_store, finish_parsing_float(string), region, env);

            (answer, Output::default())
        }
        ast::Expr::Record {
            fields,
            update: Some(loc_update),
        } => {
            let (can_update, update_out) =
                canonicalize_expr(env, var_store, scope, loc_update.region, &loc_update.value);
            if let Var(symbol) = &can_update.value {
                let (can_fields, mut output) =
                    canonicalize_fields(env, var_store, scope, region, fields);

                output.references = output.references.union(update_out.references);

                let answer = Update {
                    record_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    symbol: *symbol,
                    updates: can_fields,
                };

                (answer, output)
            } else {
                // only (optionally qualified) variables can be updated, not arbitrary expressions

                let error = roc_problem::can::RuntimeError::InvalidRecordUpdate {
                    region: can_update.region,
                };

                let answer = Expr::RuntimeError(error.clone());

                env.problems.push(Problem::RuntimeError(error));

                (answer, Output::default())
            }
        }
        ast::Expr::Record {
            fields,
            update: None,
        } => {
            if fields.is_empty() {
                (EmptyRecord, Output::default())
            } else {
                let (can_fields, output) =
                    canonicalize_fields(env, var_store, scope, region, fields);

                (
                    Record {
                        record_var: var_store.fresh(),
                        fields: can_fields,
                    },
                    output,
                )
            }
        }
        ast::Expr::Str(literal) => flatten_str_literal(env, var_store, scope, literal),
        ast::Expr::List(loc_elems) => {
            if loc_elems.is_empty() {
                (
                    List {
                        list_var: var_store.fresh(),
                        elem_var: var_store.fresh(),
                        loc_elems: Vec::new(),
                    },
                    Output::default(),
                )
            } else {
                let mut can_elems = Vec::with_capacity(loc_elems.len());
                let mut references = References::new();

                for loc_elem in loc_elems.iter() {
                    let (can_expr, elem_out) =
                        canonicalize_expr(env, var_store, scope, loc_elem.region, &loc_elem.value);

                    references = references.union(elem_out.references);

                    can_elems.push(can_expr);
                }

                let mut output = Output::default();

                output.references = references;

                // A list literal is never a tail call!
                output.tail_call = None;

                (
                    List {
                        list_var: var_store.fresh(),
                        elem_var: var_store.fresh(),
                        loc_elems: can_elems,
                    },
                    output,
                )
            }
        }
        ast::Expr::Apply(loc_fn, loc_args, application_style) => {
            // The expression that evaluates to the function being called, e.g. `foo` in
            // (foo) bar baz
            let fn_region = loc_fn.region;

            // Canonicalize the function expression and its arguments
            let (fn_expr, mut output) =
                canonicalize_expr(env, var_store, scope, fn_region, &loc_fn.value);

            // The function's return type
            let mut args = Vec::new();
            let mut outputs = Vec::new();

            for loc_arg in loc_args.iter() {
                let (arg_expr, arg_out) =
                    canonicalize_expr(env, var_store, scope, loc_arg.region, &loc_arg.value);

                args.push((var_store.fresh(), arg_expr));
                outputs.push(arg_out);
            }

            // Default: We're not tail-calling a symbol (by name), we're tail-calling a function value.
            output.tail_call = None;

            let expr = match fn_expr.value {
                Var(symbol) => {
                    output.references.calls.insert(symbol);

                    // we're tail-calling a symbol by name, check if it's the tail-callable symbol
                    output.tail_call = match &env.tailcallable_symbol {
                        Some(tc_sym) if *tc_sym == symbol => Some(symbol),
                        Some(_) | None => None,
                    };

                    Call(
                        Box::new((
                            var_store.fresh(),
                            fn_expr,
                            var_store.fresh(),
                            var_store.fresh(),
                        )),
                        args,
                        *application_style,
                    )
                }
                RuntimeError(_) => {
                    // We can't call a runtime error; bail out by propagating it!
                    return (fn_expr, output);
                }
                Tag {
                    variant_var,
                    ext_var,
                    name,
                    ..
                } => Tag {
                    variant_var,
                    ext_var,
                    name,
                    arguments: args,
                },
                _ => {
                    // This could be something like ((if True then fn1 else fn2) arg1 arg2).
                    Call(
                        Box::new((
                            var_store.fresh(),
                            fn_expr,
                            var_store.fresh(),
                            var_store.fresh(),
                        )),
                        args,
                        *application_style,
                    )
                }
            };

            for arg_out in outputs {
                output.references = output.references.union(arg_out.references);
            }

            (expr, output)
        }
        ast::Expr::Var { module_name, ident } => {
            canonicalize_lookup(env, scope, module_name, ident, region)
        } //ast::Expr::InterpolatedStr(pairs, suffix) => {
        //    let mut output = Output::new();
        //    let can_pairs: Vec<(String, Located<Expr>)> = pairs
        //        .into_iter()
        //        .map(|(string, loc_ident)| {
        //            // From a language design perspective, we only permit idents in interpolation.
        //            // However, in a canonical Expr we store it as a full Expr, not a Symbol.
        //            // This is so that we can resolve it to either Var or Unrecognized; if we
        //            // stored it as a Symbol, we couldn't record runtime errors here.
        //            let can_expr = match resolve_ident(
        //                &env,
        //                &scope,
        //                loc_ident.value,
        //                &mut output.references,
        //            ) {
        //                Ok(symbol) => Var(symbol),
        //                Err(ident) => {
        //                    let loc_ident = Located {
        //                        region: loc_ident.region,
        //                        value: ident,
        //                    };

        //                    env.problem(Problem::LookupNotInScope(loc_ident.clone()));

        //                    RuntimeError(LookupNotInScope(loc_ident))
        //                }
        //            };

        //            (
        //                string,
        //                Located {
        //                    region: loc_ident.region,
        //                    value: can_expr,
        //                },
        //            )
        //        })
        //        .collect();

        //    (InterpolatedStr(can_pairs, suffix), output)
        //}
        ast::Expr::Defs(loc_defs, loc_ret) => {
            can_defs_with_return(
                env,
                var_store,
                // The body expression gets a new scope for canonicalization,
                // so clone it.
                scope.clone(),
                loc_defs,
                loc_ret,
            )
        }
        ast::Expr::Closure(loc_arg_patterns, loc_body_expr) => {
            // The globally unique symbol that will refer to this closure once it gets converted
            // into a top-level procedure for code gen.
            //
            // In the Foo module, this will look something like Foo.$1 or Foo.$2.
            let symbol = env
                .closure_name_symbol
                .unwrap_or_else(|| env.gen_unique_symbol());
            env.closure_name_symbol = None;

            // The body expression gets a new scope for canonicalization.
            // Shadow `scope` to make sure we don't accidentally use the original one for the
            // rest of this block, but keep the original around for later diffing.
            let original_scope = scope;
            let mut scope = original_scope.clone();
            let mut can_args = Vec::with_capacity(loc_arg_patterns.len());
            let mut output = Output::default();

            let mut bound_by_argument_patterns = MutSet::default();

            for loc_pattern in loc_arg_patterns.iter() {
                let (new_output, can_arg) = canonicalize_pattern(
                    env,
                    var_store,
                    &mut scope,
                    FunctionArg,
                    &loc_pattern.value,
                    loc_pattern.region,
                );

                bound_by_argument_patterns
                    .extend(new_output.references.bound_symbols.iter().copied());

                output.union(new_output);

                can_args.push((var_store.fresh(), can_arg));
            }

            let (loc_body_expr, new_output) = canonicalize_expr(
                env,
                var_store,
                &mut scope,
                loc_body_expr.region,
                &loc_body_expr.value,
            );

            let mut captured_symbols: MutSet<Symbol> =
                new_output.references.lookups.iter().copied().collect();

            // filter out the closure's name itself
            captured_symbols.remove(&symbol);

            // symbols bound either in this pattern or deeper down are not captured!
            captured_symbols.retain(|s| !new_output.references.bound_symbols.contains(s));
            captured_symbols.retain(|s| !bound_by_argument_patterns.contains(s));

            // filter out top-level symbols
            // those will be globally available, and don't need to be captured
            captured_symbols.retain(|s| !env.top_level_symbols.contains(s));

            // filter out imported symbols
            // those will be globally available, and don't need to be captured
            captured_symbols.retain(|s| s.module_id() == env.home);

            // TODO any Closure that has an empty `captured_symbols` list could be excluded!

            output.union(new_output);

            // filter out aliases
            captured_symbols.retain(|s| !output.references.referenced_aliases.contains(s));

            // filter out functions that don't close over anything
            captured_symbols.retain(|s| !output.non_closures.contains(s));

            // Now that we've collected all the references, check to see if any of the args we defined
            // went unreferenced. If any did, report them as unused arguments.
            for (sub_symbol, region) in scope.symbols() {
                if !original_scope.contains_symbol(*sub_symbol) {
                    if !output.references.has_lookup(*sub_symbol) {
                        // The body never referenced this argument we declared. It's an unused argument!
                        env.problem(Problem::UnusedArgument(symbol, *sub_symbol, *region));
                    }

                    // We shouldn't ultimately count arguments as referenced locals. Otherwise,
                    // we end up with weird conclusions like the expression (\x -> x + 1)
                    // references the (nonexistant) local variable x!
                    output.references.lookups.remove(sub_symbol);
                }
            }

            env.register_closure(symbol, output.references.clone());

            let mut captured_symbols: Vec<_> = captured_symbols
                .into_iter()
                .map(|s| (s, var_store.fresh()))
                .collect();

            // sort symbols, so we know the order in which they're stored in the closure record
            captured_symbols.sort();

            // store that this function doesn't capture anything. It will be promoted to a
            // top-level function, and does not need to be captured by other surrounding functions.
            if captured_symbols.is_empty() {
                output.non_closures.insert(symbol);
            }

            (
                Closure {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    closure_ext_var: var_store.fresh(),
                    return_type: var_store.fresh(),
                    name: symbol,
                    captured_symbols,
                    recursive: Recursive::NotRecursive,
                    arguments: can_args,
                    loc_body: Box::new(loc_body_expr),
                },
                output,
            )
        }
        ast::Expr::When(loc_cond, branches) => {
            // Infer the condition expression's type.
            let cond_var = var_store.fresh();
            let (can_cond, mut output) =
                canonicalize_expr(env, var_store, scope, loc_cond.region, &loc_cond.value);

            // the condition can never be a tail-call
            output.tail_call = None;

            let mut can_branches = Vec::with_capacity(branches.len());

            for branch in branches.iter() {
                let (can_when_branch, branch_references) =
                    canonicalize_when_branch(env, var_store, scope, region, *branch, &mut output);

                output.references = output.references.union(branch_references);

                can_branches.push(can_when_branch);
            }

            // A "when" with no branches is a runtime error, but it will mess things up
            // if code gen mistakenly thinks this is a tail call just because its condition
            // happend to be one. (The condition gave us our initial output value.)
            if branches.is_empty() {
                output.tail_call = None;
            }

            // Incorporate all three expressions into a combined Output value.
            let expr = When {
                expr_var: var_store.fresh(),
                cond_var,
                region,
                loc_cond: Box::new(can_cond),
                branches: can_branches,
            };

            (expr, output)
        }
        ast::Expr::Access(record_expr, field) => {
            let (loc_expr, output) = canonicalize_expr(env, var_store, scope, region, record_expr);

            (
                Access {
                    record_var: var_store.fresh(),
                    field_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    loc_expr: Box::new(loc_expr),
                    field: Lowercase::from(*field),
                },
                output,
            )
        }
        ast::Expr::AccessorFunction(field) => (
            Accessor {
                function_var: var_store.fresh(),
                record_var: var_store.fresh(),
                ext_var: var_store.fresh(),
                closure_var: var_store.fresh(),
                field_var: var_store.fresh(),
                field: (*field).into(),
            },
            Output::default(),
        ),
        ast::Expr::GlobalTag(tag) => {
            let variant_var = var_store.fresh();
            let ext_var = var_store.fresh();

            (
                Tag {
                    name: TagName::Global((*tag).into()),
                    arguments: vec![],
                    variant_var,
                    ext_var,
                },
                Output::default(),
            )
        }
        ast::Expr::PrivateTag(tag) => {
            let variant_var = var_store.fresh();
            let ext_var = var_store.fresh();
            let tag_ident = env.ident_ids.get_or_insert(&(*tag).into());
            let symbol = Symbol::new(env.home, tag_ident);

            (
                Tag {
                    name: TagName::Private(symbol),
                    arguments: vec![],
                    variant_var,
                    ext_var,
                },
                Output::default(),
            )
        }
        ast::Expr::If(cond, then_branch, else_branch) => {
            let (loc_cond, mut output) =
                canonicalize_expr(env, var_store, scope, cond.region, &cond.value);
            let (loc_then, then_output) = canonicalize_expr(
                env,
                var_store,
                scope,
                then_branch.region,
                &then_branch.value,
            );
            let (loc_else, else_output) = canonicalize_expr(
                env,
                var_store,
                scope,
                else_branch.region,
                &else_branch.value,
            );

            output.references = output.references.union(then_output.references);
            output.references = output.references.union(else_output.references);

            (
                If {
                    cond_var: var_store.fresh(),
                    branch_var: var_store.fresh(),
                    branches: vec![(loc_cond, loc_then)],
                    final_else: Box::new(loc_else),
                },
                output,
            )
        }

        ast::Expr::PrecedenceConflict(whole_region, binop1, binop2, _expr) => {
            use roc_problem::can::RuntimeError::*;

            let problem = PrecedenceProblem::BothNonAssociative(
                *whole_region,
                binop1.clone(),
                binop2.clone(),
            );

            env.problem(Problem::PrecedenceProblem(problem.clone()));

            (
                RuntimeError(InvalidPrecedence(problem, region)),
                Output::default(),
            )
        }
        ast::Expr::MalformedClosure => {
            use roc_problem::can::RuntimeError::*;
            (RuntimeError(MalformedClosure(region)), Output::default())
        }
        ast::Expr::MalformedIdent(name) => {
            use roc_problem::can::RuntimeError::*;
            (
                RuntimeError(MalformedIdentifier((*name).into(), region)),
                Output::default(),
            )
        }
        ast::Expr::Nested(sub_expr) => {
            let (answer, output) = canonicalize_expr(env, var_store, scope, region, sub_expr);

            (answer.value, output)
        }
        ast::Expr::NonBase10Int {
            string,
            base,
            is_negative,
        } => {
            // the minus sign is added before parsing, to get correct overflow/underflow behavior
            let result = finish_parsing_base(string, *base, *is_negative);
            let answer = int_expr_from_result(var_store, result, region, *base, env);

            (answer, Output::default())
        }
        // Below this point, we shouln't see any of these nodes anymore because
        // operator desugaring should have removed them!
        ast::Expr::ParensAround(sub_expr) => {
            panic!(
                "A ParensAround did not get removed during operator desugaring somehow: {:?}",
                sub_expr
            );
        }
        ast::Expr::SpaceBefore(sub_expr, _spaces) => {
            panic!(
                "A SpaceBefore did not get removed during operator desugaring somehow: {:?}",
                sub_expr
            );
        }
        ast::Expr::SpaceAfter(sub_expr, _spaces) => {
            panic!(
                "A SpaceAfter did not get removed during operator desugaring somehow: {:?}",
                sub_expr
            );
        }
        ast::Expr::BinOp((_, loc_op, _)) => {
            panic!(
                "A binary operator did not get desugared somehow: {:?}",
                loc_op
            );
        }
        ast::Expr::UnaryOp(_, loc_op) => {
            panic!(
                "A unary operator did not get desugared somehow: {:?}",
                loc_op
            );
        }
    };

    if cfg!(debug_assertions) {
        env.home.register_debug_idents(&env.ident_ids);
    }

    // At the end, diff used_idents and defined_idents to see which were unused.
    // Add warnings for those!

    // In a later phase, unused top level declarations won't get monomorphized or code-genned.
    // We aren't going to bother with DCE at the level of local defs. It's going to be
    // a rounding error anyway (especially given that they'll be surfaced as warnings), LLVM will
    // DCE them in optimized builds, and it's not worth the bookkeeping for dev builds.
    (
        Located {
            region,
            value: expr,
        },
        output,
    )
}

#[inline(always)]
fn canonicalize_when_branch<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    _region: Region,
    branch: &'a ast::WhenBranch<'a>,
    output: &mut Output,
) -> (WhenBranch, References) {
    let mut patterns = Vec::with_capacity(branch.patterns.len());

    let original_scope = scope;
    let mut scope = original_scope.clone();

    // TODO report symbols not bound in all patterns
    for loc_pattern in branch.patterns.iter() {
        let (new_output, can_pattern) = canonicalize_pattern(
            env,
            var_store,
            &mut scope,
            WhenBranch,
            &loc_pattern.value,
            loc_pattern.region,
        );

        output.union(new_output);

        patterns.push(can_pattern);
    }

    let (value, mut branch_output) = canonicalize_expr(
        env,
        var_store,
        &mut scope,
        branch.value.region,
        &branch.value.value,
    );

    let guard = match &branch.guard {
        None => None,
        Some(loc_expr) => {
            let (can_guard, guard_branch_output) =
                canonicalize_expr(env, var_store, &mut scope, loc_expr.region, &loc_expr.value);

            branch_output.union(guard_branch_output);
            Some(can_guard)
        }
    };

    // Now that we've collected all the references for this branch, check to see if
    // any of the new idents it defined were unused. If any were, report it.
    for (symbol, region) in scope.symbols() {
        let symbol = *symbol;

        if !output.references.has_lookup(symbol)
            && !branch_output.references.has_lookup(symbol)
            && !original_scope.contains_symbol(symbol)
        {
            env.problem(Problem::UnusedDef(symbol, *region));
        }
    }

    let references = branch_output.references.clone();
    output.union(branch_output);

    (
        WhenBranch {
            patterns,
            value,
            guard,
        },
        references,
    )
}

pub fn local_successors<'a>(
    references: &'a References,
    closures: &'a MutMap<Symbol, References>,
) -> ImSet<Symbol> {
    let mut answer = im_rc::hashset::HashSet::clone(&references.lookups);

    for call_symbol in references.calls.iter() {
        answer = answer.union(call_successors(*call_symbol, closures));
    }

    answer
}

fn call_successors<'a>(
    call_symbol: Symbol,
    closures: &'a MutMap<Symbol, References>,
) -> ImSet<Symbol> {
    let mut answer = im_rc::hashset::HashSet::default();
    let mut seen = MutSet::default();
    let mut queue = vec![call_symbol];

    while let Some(symbol) = queue.pop() {
        if seen.contains(&symbol) {
            continue;
        }

        if let Some(references) = closures.get(&symbol) {
            answer.extend(references.lookups.iter().copied());
            queue.extend(references.calls.iter().copied());

            seen.insert(symbol);
        }
    }

    answer
}

pub fn references_from_local<'a, T>(
    defined_symbol: Symbol,
    visited: &'a mut MutSet<Symbol>,
    refs_by_def: &'a MutMap<Symbol, (T, References)>,
    closures: &'a MutMap<Symbol, References>,
) -> References
where
    T: Debug,
{
    let mut answer: References = References::new();

    match refs_by_def.get(&defined_symbol) {
        Some((_, refs)) => {
            visited.insert(defined_symbol);

            for local in refs.lookups.iter() {
                if !visited.contains(&local) {
                    let other_refs: References =
                        references_from_local(*local, visited, refs_by_def, closures);

                    answer = answer.union(other_refs);
                }

                answer.lookups.insert(*local);
            }

            for call in refs.calls.iter() {
                if !visited.contains(&call) {
                    let other_refs = references_from_call(*call, visited, refs_by_def, closures);

                    answer = answer.union(other_refs);
                }

                answer.calls.insert(*call);
            }

            answer
        }
        None => answer,
    }
}

pub fn references_from_call<'a, T>(
    call_symbol: Symbol,
    visited: &'a mut MutSet<Symbol>,
    refs_by_def: &'a MutMap<Symbol, (T, References)>,
    closures: &'a MutMap<Symbol, References>,
) -> References
where
    T: Debug,
{
    match closures.get(&call_symbol) {
        Some(references) => {
            let mut answer = references.clone();

            visited.insert(call_symbol);

            for closed_over_local in references.lookups.iter() {
                if !visited.contains(&closed_over_local) {
                    let other_refs =
                        references_from_local(*closed_over_local, visited, refs_by_def, closures);

                    answer = answer.union(other_refs);
                }

                answer.lookups.insert(*closed_over_local);
            }

            for call in references.calls.iter() {
                if !visited.contains(&call) {
                    let other_refs = references_from_call(*call, visited, refs_by_def, closures);

                    answer = answer.union(other_refs);
                }

                answer.calls.insert(*call);
            }

            answer
        }
        None => {
            // If the call symbol was not in the closure map, that means we're calling a non-function and
            // will get a type mismatch later. For now, assume no references as a result of the "call."
            References::new()
        }
    }
}

fn canonicalize_fields<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    region: Region,
    fields: &'a [Located<ast::AssignedField<'a, ast::Expr<'a>>>],
) -> (SendMap<Lowercase, Field>, Output) {
    let mut can_fields = SendMap::default();
    let mut output = Output::default();

    for loc_field in fields.iter() {
        let (label, field_expr, field_out, field_var) =
            canonicalize_field(env, var_store, scope, &loc_field.value, loc_field.region);

        let field = Field {
            var: field_var,
            region: loc_field.region,
            loc_expr: Box::new(field_expr),
        };

        let replaced = can_fields.insert(label.clone(), field);

        if let Some(old) = replaced {
            env.problems.push(Problem::DuplicateRecordFieldValue {
                field_name: label,
                field_region: loc_field.region,
                record_region: region,
                replaced_region: old.region,
            });
        }

        output.references = output.references.union(field_out.references);
    }

    (can_fields, output)
}

fn canonicalize_field<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    field: &'a ast::AssignedField<'a, ast::Expr<'a>>,
    region: Region,
) -> (Lowercase, Located<Expr>, Output, Variable) {
    use roc_parse::ast::AssignedField::*;

    match field {
        // Both a label and a value, e.g. `{ name: "blah" }`
        RequiredValue(label, _, loc_expr) => {
            let field_var = var_store.fresh();
            let (loc_can_expr, output) =
                canonicalize_expr(env, var_store, scope, loc_expr.region, &loc_expr.value);

            (
                Lowercase::from(label.value),
                loc_can_expr,
                output,
                field_var,
            )
        }

        OptionalValue(_, _, _) => {
            todo!("TODO gracefully handle an optional field being used in an Expr");
        }

        // A label with no value, e.g. `{ name }` (this is sugar for { name: name })
        LabelOnly(_) => {
            panic!("Somehow a LabelOnly record field was not desugared!");
        }

        SpaceBefore(sub_field, _) | SpaceAfter(sub_field, _) => {
            canonicalize_field(env, var_store, scope, sub_field, region)
        }

        Malformed(_string) => {
            panic!("TODO canonicalize malformed record field");
        }
    }
}

fn canonicalize_lookup(
    env: &mut Env<'_>,
    scope: &mut Scope,
    module_name: &str,
    ident: &str,
    region: Region,
) -> (Expr, Output) {
    use Expr::*;

    let mut output = Output::default();
    let can_expr = if module_name.is_empty() {
        // Since module_name was empty, this is an unqualified var.
        // Look it up in scope!
        match scope.lookup(&(*ident).into(), region) {
            Ok(symbol) => {
                output.references.lookups.insert(symbol);

                Var(symbol)
            }
            Err(problem) => {
                env.problem(Problem::RuntimeError(problem.clone()));

                RuntimeError(problem)
            }
        }
    } else {
        // Since module_name was nonempty, this is a qualified var.
        // Look it up in the env!
        match env.qualified_lookup(module_name, ident, region) {
            Ok(symbol) => {
                output.references.lookups.insert(symbol);

                Var(symbol)
            }
            Err(problem) => {
                // Either the module wasn't imported, or
                // it was imported but it doesn't expose this ident.
                env.problem(Problem::RuntimeError(problem.clone()));

                RuntimeError(problem)
            }
        }
    };

    // If it's valid, this ident should be in scope already.

    (can_expr, output)
}

/// Currently uses the heuristic of "only inline if it's a builtin"
pub fn inline_calls(var_store: &mut VarStore, scope: &mut Scope, expr: Expr) -> Expr {
    use Expr::*;

    match expr {
        // Num stores the `a` variable in `Num a`. Not the same as the variable
        // stored in Int and Float below, which is strictly for better error messages
        other @ Num(_, _)
        | other @ Int(_, _)
        | other @ Float(_, _)
        | other @ Str { .. }
        | other @ RuntimeError(_)
        | other @ EmptyRecord
        | other @ Accessor { .. }
        | other @ Update { .. }
        | other @ Var(_)
        | other @ RunLowLevel { .. } => other,

        List {
            list_var,
            elem_var,
            loc_elems,
        } => {
            let mut new_elems = Vec::with_capacity(loc_elems.len());

            for loc_elem in loc_elems {
                let value = inline_calls(var_store, scope, loc_elem.value);

                new_elems.push(Located {
                    value,
                    region: loc_elem.region,
                });
            }

            List {
                list_var,
                elem_var,
                loc_elems: new_elems,
            }
        }
        // Branching
        When {
            cond_var,
            expr_var,
            region,
            loc_cond,
            branches,
        } => {
            let loc_cond = Box::new(Located {
                region: loc_cond.region,
                value: inline_calls(var_store, scope, loc_cond.value),
            });

            let mut new_branches = Vec::with_capacity(branches.len());

            for branch in branches {
                let value = Located {
                    value: inline_calls(var_store, scope, branch.value.value),
                    region: branch.value.region,
                };
                let guard = match branch.guard {
                    Some(loc_expr) => Some(Located {
                        region: loc_expr.region,
                        value: inline_calls(var_store, scope, loc_expr.value),
                    }),
                    None => None,
                };
                let new_branch = WhenBranch {
                    patterns: branch.patterns,
                    value,
                    guard,
                };

                new_branches.push(new_branch);
            }

            When {
                cond_var,
                expr_var,
                region,
                loc_cond,
                branches: new_branches,
            }
        }
        If {
            cond_var,
            branch_var,
            branches,
            final_else,
        } => {
            let mut new_branches = Vec::with_capacity(branches.len());

            for (loc_cond, loc_expr) in branches {
                let loc_cond = Located {
                    value: inline_calls(var_store, scope, loc_cond.value),
                    region: loc_cond.region,
                };

                let loc_expr = Located {
                    value: inline_calls(var_store, scope, loc_expr.value),
                    region: loc_expr.region,
                };

                new_branches.push((loc_cond, loc_expr));
            }

            let final_else = Box::new(Located {
                region: final_else.region,
                value: inline_calls(var_store, scope, final_else.value),
            });

            If {
                cond_var,
                branch_var,
                branches: new_branches,
                final_else,
            }
        }

        LetRec(defs, loc_expr, var) => {
            let mut new_defs = Vec::with_capacity(defs.len());

            for def in defs {
                new_defs.push(Def {
                    loc_pattern: def.loc_pattern,
                    loc_expr: Located {
                        region: def.loc_expr.region,
                        value: inline_calls(var_store, scope, def.loc_expr.value),
                    },
                    expr_var: def.expr_var,
                    pattern_vars: def.pattern_vars,
                    annotation: def.annotation,
                });
            }

            let loc_expr = Located {
                region: loc_expr.region,
                value: inline_calls(var_store, scope, loc_expr.value),
            };

            LetRec(new_defs, Box::new(loc_expr), var)
        }

        LetNonRec(def, loc_expr, var) => {
            let def = Def {
                loc_pattern: def.loc_pattern,
                loc_expr: Located {
                    region: def.loc_expr.region,
                    value: inline_calls(var_store, scope, def.loc_expr.value),
                },
                expr_var: def.expr_var,
                pattern_vars: def.pattern_vars,
                annotation: def.annotation,
            };

            let loc_expr = Located {
                region: loc_expr.region,
                value: inline_calls(var_store, scope, loc_expr.value),
            };

            LetNonRec(Box::new(def), Box::new(loc_expr), var)
        }

        Closure {
            function_type,
            closure_type,
            closure_ext_var,
            return_type,
            recursive,
            name,
            captured_symbols,
            arguments,
            loc_body,
        } => {
            let loc_expr = *loc_body;
            let loc_expr = Located {
                value: inline_calls(var_store, scope, loc_expr.value),
                region: loc_expr.region,
            };

            Closure {
                function_type,
                closure_type,
                closure_ext_var,
                return_type,
                recursive,
                name,
                captured_symbols,
                arguments,
                loc_body: Box::new(loc_expr),
            }
        }

        Record { record_var, fields } => {
            todo!(
                "Inlining for Record with record_var {:?} and fields {:?}",
                record_var,
                fields
            );
        }

        Access {
            record_var,
            ext_var,
            field_var,
            loc_expr,
            field,
        } => {
            todo!("Inlining for Access with record_var {:?}, ext_var {:?}, field_var {:?}, loc_expr {:?}, field {:?}", record_var, ext_var, field_var, loc_expr, field);
        }

        Tag {
            variant_var,
            ext_var,
            name,
            arguments,
        } => {
            todo!(
                "Inlining for Tag with variant_var {:?}, ext_var {:?}, name {:?}, arguments {:?}",
                variant_var,
                ext_var,
                name,
                arguments
            );
        }

        Call(boxed_tuple, args, called_via) => {
            let (fn_var, loc_expr, closure_var, expr_var) = *boxed_tuple;

            match loc_expr.value {
                Var(symbol) if symbol.is_builtin() => match builtin_defs(var_store).get(&symbol) {
                    Some(Def {
                        loc_expr:
                            Located {
                                value:
                                    Closure {
                                        recursive,
                                        arguments: params,
                                        loc_body: boxed_body,
                                        ..
                                    },
                                ..
                            },
                        ..
                    }) => {
                        debug_assert_eq!(*recursive, Recursive::NotRecursive);

                        // Since this is a canonicalized Expr, we should have
                        // already detected any arity mismatches and replaced this
                        // with a RuntimeError if there was a mismatch.
                        debug_assert_eq!(params.len(), args.len());

                        // Start with the function's body as the answer.
                        let mut loc_answer = *boxed_body.clone();

                        // Wrap the body in one LetNonRec for each argument,
                        // such that at the end we have all the arguments in
                        // scope with the values the caller provided.
                        for ((_param_var, loc_pattern), (expr_var, loc_expr)) in
                            params.iter().cloned().zip(args.into_iter()).rev()
                        {
                            // TODO get the correct vars into here.
                            // Not sure if param_var should be involved.
                            let pattern_vars = SendMap::default();

                            let def = Def {
                                loc_pattern,
                                loc_expr,
                                expr_var,
                                pattern_vars,
                                annotation: None,
                            };

                            loc_answer = Located {
                                region: Region::zero(),
                                value: LetNonRec(
                                    Box::new(def),
                                    Box::new(loc_answer),
                                    var_store.fresh(),
                                ),
                            };
                        }

                        loc_answer.value
                    }
                    Some(_) => {
                        unreachable!("Tried to inline a non-function");
                    }
                    None => {
                        unreachable!(
                            "Tried to inline a builtin that wasn't registered: {:?}",
                            symbol
                        );
                    }
                },
                _ => {
                    // For now, we only inline calls to builtins. Leave this alone!
                    Call(
                        Box::new((fn_var, loc_expr, closure_var, expr_var)),
                        args,
                        called_via,
                    )
                }
            }
        }
    }
}

fn flatten_str_literal<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    literal: &StrLiteral<'a>,
) -> (Expr, Output) {
    use ast::StrLiteral::*;

    match literal {
        PlainLine(str_slice) => (Expr::Str((*str_slice).into()), Output::default()),
        Line(segments) => flatten_str_lines(env, var_store, scope, &[segments]),
        Block(lines) => flatten_str_lines(env, var_store, scope, lines),
    }
}

fn is_valid_interpolation(expr: &ast::Expr<'_>) -> bool {
    match expr {
        ast::Expr::Var { .. } => true,
        ast::Expr::Access(sub_expr, _) => is_valid_interpolation(sub_expr),
        _ => false,
    }
}

enum StrSegment {
    Interpolation(Located<Expr>),
    Plaintext(InlinableString),
}

fn flatten_str_lines<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    lines: &[&[ast::StrSegment<'a>]],
) -> (Expr, Output) {
    use ast::StrSegment::*;

    let mut buf = String::new();
    let mut segments = Vec::new();
    let mut output = Output::default();

    for line in lines {
        for segment in line.iter() {
            match segment {
                Plaintext(string) => {
                    buf.push_str(string);
                }
                Unicode(loc_hex_digits) => match u32::from_str_radix(loc_hex_digits.value, 16) {
                    Ok(code_pt) => match char::from_u32(code_pt) {
                        Some(ch) => {
                            buf.push(ch);
                        }
                        None => {
                            env.problem(Problem::InvalidUnicodeCodePoint(loc_hex_digits.region));

                            return (
                                Expr::RuntimeError(RuntimeError::InvalidUnicodeCodePoint(
                                    loc_hex_digits.region,
                                )),
                                output,
                            );
                        }
                    },
                    Err(_) => {
                        env.problem(Problem::InvalidHexadecimal(loc_hex_digits.region));

                        return (
                            Expr::RuntimeError(RuntimeError::InvalidHexadecimal(
                                loc_hex_digits.region,
                            )),
                            output,
                        );
                    }
                },
                Interpolated(loc_expr) => {
                    if is_valid_interpolation(loc_expr.value) {
                        // Interpolations desugar to Str.concat calls
                        output.references.calls.insert(Symbol::STR_CONCAT);

                        if !buf.is_empty() {
                            segments.push(StrSegment::Plaintext(buf.into()));

                            buf = String::new();
                        }

                        let (loc_expr, new_output) = canonicalize_expr(
                            env,
                            var_store,
                            scope,
                            loc_expr.region,
                            loc_expr.value,
                        );

                        output.union(new_output);

                        segments.push(StrSegment::Interpolation(loc_expr));
                    } else {
                        env.problem(Problem::InvalidInterpolation(loc_expr.region));

                        return (
                            Expr::RuntimeError(RuntimeError::InvalidInterpolation(loc_expr.region)),
                            output,
                        );
                    }
                }
                EscapedChar(escaped) => buf.push(unescape_char(escaped)),
            }
        }
    }

    if !buf.is_empty() {
        segments.push(StrSegment::Plaintext(buf.into()));
    }

    (desugar_str_segments(var_store, segments), output)
}

/// Resolve stirng interpolations by desugaring a sequence of StrSegments
/// into nested calls to Str.concat
fn desugar_str_segments(var_store: &mut VarStore, segments: Vec<StrSegment>) -> Expr {
    use StrSegment::*;

    let mut iter = segments.into_iter().rev();
    let mut loc_expr = match iter.next() {
        Some(Plaintext(string)) => Located::new(0, 0, 0, 0, Expr::Str(string)),
        Some(Interpolation(loc_expr)) => loc_expr,
        None => {
            // No segments? Empty string!

            Located::new(0, 0, 0, 0, Expr::Str("".into()))
        }
    };

    for seg in iter {
        let loc_new_expr = match seg {
            Plaintext(string) => Located::new(0, 0, 0, 0, Expr::Str(string)),
            Interpolation(loc_interpolated_expr) => loc_interpolated_expr,
        };

        let fn_expr = Located::new(0, 0, 0, 0, Expr::Var(Symbol::STR_CONCAT));
        let expr = Expr::Call(
            Box::new((
                var_store.fresh(),
                fn_expr,
                var_store.fresh(),
                var_store.fresh(),
            )),
            vec![
                (var_store.fresh(), loc_new_expr),
                (var_store.fresh(), loc_expr),
            ],
            CalledVia::Space,
        );

        loc_expr = Located::new(0, 0, 0, 0, expr);
    }

    loc_expr.value
}

/// Returns the char that would have been originally parsed to
pub fn unescape_char(escaped: &EscapedChar) -> char {
    use EscapedChar::*;

    match escaped {
        Backslash => '\\',
        Quote => '"',
        CarriageReturn => '\r',
        Tab => '\t',
        Newline => '\n',
    }
}
