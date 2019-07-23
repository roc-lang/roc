use region::{Located, Region};
use operator::Operator;
use operator::Operator::Pizza;
use operator::Associativity::*;
use collections::{ImSet, ImMap};
use std::cmp::Ordering;
use expr::{Ident, VariantName, Path};
use expr;


#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    Frac(i64, i64),
    Approx(f64),
    EmptyStr,
    Str(String),
    Char(char),

    // Lookups
    Var(Resolved<Symbol>),
    CallByName(Resolved<Symbol>, Vec<Located<Expr>>),
    InterpolatedStr(Vec<(String, Resolved<Symbol>)>, String),

    // Pattern Matching
    Case(Box<Located<Expr>>, Vec<(Located<Pattern>, Located<Expr>)>),
    Assign(Vec<(Located<Pattern>, Located<Expr>)>, Box<Located<Expr>>),
    AnonymousClosure(Option<Vec<LocalSymbol>>, Vec<Located<Pattern>>, Box<Located<Expr>>),
    NamedClosure(Symbol, Option<Vec<LocalSymbol>>, Vec<Located<Pattern>>, Box<Located<Expr>>),
    TailRecursiveClosure(Symbol, Option<Vec<LocalSymbol>>, Vec<Located<Pattern>>, Box<Located<Expr>>),

    // Application
    Apply(Box<Located<Expr>>, Vec<Located<Expr>>),
    ApplyVariant(Resolved<GlobalSymbol>, Option<Vec<Located<Expr>>>),

    // Product Types
    EmptyRecord,

    // Sugar
    If(Box<Located<Expr>>, Box<Located<Expr>>, Box<Located<Expr>>),
    Operator(Box<Located<Expr>>, Located<Operator>, Box<Located<Expr>>),

    // Runtime Error
    InvalidPrecedence(PrecedenceProblem, Box<Located<Expr>>)
}

/// Problems that can occur in the course of canonicalization.
#[derive(Clone, Debug, PartialEq)]
pub enum Problem {
    Shadowing(Located<expr::Ident>),
    UnrecognizedFunctionName(Located<expr::Ident>),
    UnrecognizedConstant(Located<expr::Ident>),
    UnrecognizedVariant(Located<expr::VariantName>),
    UnusedAssignment(Located<expr::Ident>),
    PrecedenceProblem(PrecedenceProblem),
}

/// An ident or variant name, possibly unrecognized, possibly referring to either a toplevel or local symbol.
#[derive(Clone, Debug, PartialEq)]
pub enum Symbol {
    /// An ident or variant name referencing a toplevel declaration.
    Global(GlobalSymbol),

    /// An ident referencing a local assignment, not a toplevel declaration.
    Local(LocalSymbol),
}

/// An ident or variant name in the globlal scope; that is, something defined in the toplevel of some module.
#[derive(Clone, Debug, PartialEq)]
pub struct GlobalSymbol(String);

/// An ident referencing a local assignment - *not* something defined in the toplevel.
#[derive(Clone, Debug, PartialEq)]
pub struct LocalSymbol(String);

#[derive(Clone, Debug, PartialEq)]
pub enum Resolved<T> {
    /// This is the unique symbol we'll use in codegen.
    Recognized(T),

    /// These will codegen to runtime errors.
    UnrecognizedFunctionName(Located<expr::Ident>),
    UnrecognizedConstant(Located<expr::Ident>),
    UnrecognizedVariant(Located<expr::VariantName>),
}

impl GlobalSymbol {
    pub fn new((path, name): (Path, String)) -> GlobalSymbol {
        GlobalSymbol(format!("{}.{}", path.into_string(), name))
    }

    pub fn recognized(info: (Path, String)) -> Resolved<GlobalSymbol> {
        Resolved::Recognized(GlobalSymbol::new(info))
    }
}

impl LocalSymbol {
    pub fn new(name: String) -> LocalSymbol {
        LocalSymbol(name)
    }

    pub fn recognized(name: String) -> Resolved<LocalSymbol> {
        Resolved::Recognized(LocalSymbol::new(name))
    }
}

impl Symbol {
    pub fn resolved_global(info: (Path, String)) -> Resolved<Symbol> {
        Resolved::Recognized(Symbol::Global(GlobalSymbol::new(info)))
    }

    pub fn resolved_local(name: String) -> Resolved<Symbol> {
        Resolved::Recognized(Symbol::Local(LocalSymbol::new(name)))
    }

    pub fn global(info: (Path, String)) -> Symbol {
        Symbol::Global(GlobalSymbol::new(info))
    }

    pub fn local(name: String) -> Symbol {
        Symbol::Local(LocalSymbol::new(name))
    }
}

/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Identifier(Symbol),
    Variant(Resolved<Symbol>, Option<Vec<Located<Pattern>>>),
    Integer(i64),
    Fraction(i64, i64),
    ExactString(String),
    EmptyRecordLiteral,
    Underscore,
    Shadowed(Located<expr::Ident>)
}

/// The canonicalization environment for a particular module.
struct Env {
    /// The module's path. Unqualified references to identifiers and variant names are assumed
    /// to be relative to this Path, and will be turned into Qualified references accordingly.
    home: Path,

    /// Problems we've encountered along the way, which will be reported to the user at the end.
    problems: Vec<Problem>,

    /// Variants either declared in this module, or imported.
    declared_variants: ImMap<(Path, String), Located<expr::VariantName>>,
}

impl Env {
    pub fn new(home: Path, declared_variants: ImMap<(Path, String), Located<expr::VariantName>>) -> Env {
        Env { home, declared_variants, problems: Vec::new() }
    }

    pub fn problem(&mut self, problem: Problem) -> () {
        self.problems.push(problem)
    }
}

pub fn canonicalize_declarations(
    home: Path,
    decls: Vec<(Located<Pattern>, Located<expr::Expr>)>,
    declared_idents: &ImMap<(Option<Path>, String), Located<expr::Ident>>,
    declared_variants: &ImMap<(Path, String), Located<expr::VariantName>>,
) {
    panic!("TODO: handle decls like assignments; check for shadowing, tail recursion, named closures functions, etc");
}

pub fn canonicalize_declaration(
    home: Path,
    loc_expr: Located<expr::Expr>,
    declared_idents: &ImMap<(Option<Path>, String), Located<expr::Ident>>,
    declared_variants: &ImMap<(Path, String), Located<expr::VariantName>>,
) -> (Located<Expr>, Output, Vec<Problem>) {
    let mut env = Env::new(home, declared_variants.clone());
    let (mut new_loc_expr, output) = canonicalize(&mut env, loc_expr, declared_idents);

    // Apply operator precedence and associativity rules once, after canonicalization is
    // otherwise complete. If we did this *during* canonicalization, then each time we
    // visited an Operator node we'd recursively try to apply this to each of its nested
    // operators, and thena again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.
    new_loc_expr = apply_precedence_and_associativity(&mut env, new_loc_expr);

    (new_loc_expr, output, env.problems)
}

pub struct Output {
    pub referenced_idents: ImSet<(Option<Path>, String)>,
    pub applied_variants: ImSet<(Path, String)>,
    pub tail_call: Option<Symbol>,
}

impl Output {
    pub fn new() -> Output {
        Output {
            referenced_idents: ImSet::default(),
            applied_variants: ImSet::default(),
            tail_call: None,
        }
    }

    pub fn union_usages(mut self, other: Output) -> Self {
        self.referenced_idents = self.referenced_idents.union(other.referenced_idents);
        self.applied_variants = self.applied_variants.union(other.applied_variants);

        self
    }
}

fn canonicalize(
    env: &mut Env,
    loc_expr: Located<expr::Expr>,
    idents_in_scope: &ImMap<(Option<Path>, String), Located<expr::Ident>>,
) -> (Located<Expr>, Output) {
    use self::Expr::*;

    let region = loc_expr.region;
    let (expr, output) = match loc_expr.value {
        expr::Expr::Int(num) => ( Int(num), Output::new() ),
        expr::Expr::Frac(numerator, denominator) => ( Frac(numerator, denominator), Output::new()),
        expr::Expr::Approx(num) => ( Approx(num), Output::new()),
        expr::Expr::EmptyRecord => ( EmptyRecord, Output::new()),
        expr::Expr::Str(string) => ( Str(string), Output::new()),
        expr::Expr::Char(ch) => ( Char(ch), Output::new()),
        expr::Expr::EmptyStr => ( EmptyStr, Output::new()),

        expr::Expr::If(loc_cond, loc_true, loc_false) => {
            // Canonicalize the nested expressions
            let (cond_expr, cond_out) = canonicalize(env, *loc_cond, idents_in_scope);
            let (true_expr, true_out) = canonicalize(env, *loc_true, idents_in_scope);
            let (false_expr, false_out) = canonicalize(env, *loc_false, idents_in_scope);

            // Incorporate all three expressions into a combined Output value.
            let expr = If(Box::new(cond_expr), Box::new(true_expr), Box::new(false_expr));
            let mut output = cond_out;

            // If both branches are tail calling the same symbol, then so is the conditional as a whole.
            // Also, if both branches are not tail calls (tail_call == None), then so is the conditional.
            // If the branches are different, we leave the default of None as-is.
            if true_out.tail_call == false_out.tail_call {
                output.tail_call = true_out.tail_call;
            }

            output.referenced_idents = output.referenced_idents.union(true_out.referenced_idents);
            output.referenced_idents = output.referenced_idents.union(false_out.referenced_idents);
            output.applied_variants = output.applied_variants.union(true_out.applied_variants);
            output.applied_variants = output.applied_variants.union(false_out.applied_variants);

            (expr, output)
        },

        expr::Expr::Apply(loc_fn, loc_args) => {
            // Canonicalize the function expression and its arguments
            let (fn_expr, fn_out) = canonicalize(env, *loc_fn, idents_in_scope);
            let mut args = Vec::new();
            let mut outputs = Vec::new();

            for loc_arg in loc_args {
                let (arg_expr, arg_out) = canonicalize(env, loc_arg, idents_in_scope);

                args.push(arg_expr);
                outputs.push(arg_out);
            }

            let expr = Apply(Box::new(fn_expr), args);
            let mut output = fn_out;

            for arg_out in outputs {
                output = output.union_usages(arg_out);
            }

            // We're not tail-calling a symbol (by name), we're tail-calling a function value.
            output.tail_call = None;

            (expr, output)
        },

        expr::Expr::Operator(loc_left, op, loc_right) => {
            // Canonicalize the nested expressions
            let (left_expr, left_out) = canonicalize(&mut *env, *loc_left, idents_in_scope);
            let (right_expr, right_out) = canonicalize(&mut *env, *loc_right, idents_in_scope);

            // Incorporate both expressions into a combined Output value.
            let mut output = left_out.union_usages(right_out);

            // The pizza operator is the only one that can be a tail call,
            // because it's the only one that can call a function by name.
            output.tail_call = match op.value {
                Pizza => {
                    match &right_expr.value {
                        Var(Resolved::Recognized(sym)) => Some(sym.clone()),
                        CallByName(Resolved::Recognized(sym), _) => Some(sym.clone()),
                        _ => None
                    }
                },
                _ => None
            };

            let expr = Operator(Box::new(left_expr), op, Box::new(right_expr));

            (expr, output)
        },

        expr::Expr::Var(ident) => {
            // check if present in idents_in_scope...
            //     if not, NAMING PROBLEM
            //     if so, include it in referenced_idents
            let mut referenced_idents = ImSet::default();
            let symbol =
                resolve_ident(&env, ident, &mut referenced_idents, idents_in_scope)
                    .unwrap_or_else(|ident| {
                        let loc_ident = Located {region, value: ident};

                        env.problem(Problem::UnrecognizedConstant(loc_ident.clone()));

                        Resolved::UnrecognizedConstant(loc_ident)
                    });

            (
                Var(symbol),
                Output {referenced_idents, applied_variants: ImSet::default(), tail_call: None}
            )
        },

        expr::Expr::CallByName(ident, args) => {
            // check if function name is present in idents_in_scope...
            //     if not, NAMING PROBLEM
            //     if so, include it in referenced_idents
            let mut referenced_idents = ImSet::default();
            let mut applied_variants = ImSet::default();

            let symbol =
                resolve_ident(&env, ident, &mut referenced_idents, idents_in_scope)
                    .unwrap_or_else(|ident| {
                        let loc_ident = Located {region, value: ident};

                        env.problem(Problem::UnrecognizedFunctionName(loc_ident.clone()));

                        Resolved::UnrecognizedFunctionName(loc_ident)
                    });

            let mut new_args = Vec::with_capacity(args.len());

            for arg in args {
                let (loc_expr, output) = canonicalize(&mut *env, arg, &idents_in_scope);

                referenced_idents = referenced_idents.clone().union(output.referenced_idents);
                applied_variants = applied_variants.clone().union(output.applied_variants);

                new_args.push(loc_expr);
            }

            // If we recognized the symbol, this is a tail call!
            let tail_call = match &symbol {
                &Resolved::Recognized(ref sym) => Some(sym.clone()),
                _ => None
            };

            let output = Output {referenced_idents, applied_variants, tail_call};

            (CallByName(symbol, new_args), output)
        },

        expr::Expr::InterpolatedStr(pairs, suffix) => {
            let mut referenced_idents = ImSet::default();
            let applied_variants = ImSet::default();

            let new_pairs: Vec<(String, Resolved<Symbol>)> = pairs.into_iter().map(|(string, ident)| {
                // check if present in idents_in_scope...
                //     if not, NAMING PROBLEM
                //     if so, include it in referenced_idents
                let ident_region = ident.region;
                let symbol =
                    resolve_ident(&env, ident.value, &mut referenced_idents, idents_in_scope)
                        .unwrap_or_else(|value| {
                            let loc_ident = Located {region: ident_region, value};

                            env.problem(Problem::UnrecognizedConstant(loc_ident.clone()));

                            Resolved::UnrecognizedConstant(loc_ident)
                        });

                (string, symbol)
            }).collect();

            let output = Output {referenced_idents, applied_variants, tail_call: None};

            (InterpolatedStr(new_pairs, suffix), output)
        }

        expr::Expr::ApplyVariant(variant_name, opt_args) => {
            // check if present in declared_variants...
            //     if not, NAMING ERROR
            //     if so, return applied_variants which includes it
            let mut referenced_idents = ImSet::default();
            let mut applied_variants = ImSet::default();

            let symbol =
                resolve_variant_name(&env, variant_name, &mut applied_variants)
                    .unwrap_or_else(|value| {
                        let loc_variant = Located {region, value};

                        env.problem(Problem::UnrecognizedVariant(loc_variant.clone()));

                        Resolved::UnrecognizedVariant(loc_variant)
                    });

            let new_opt_args = opt_args.map(|args| {
                let mut new_args = Vec::with_capacity(args.len());

                for arg in args {
                    let (loc_expr, output) = canonicalize(&mut *env, arg, &idents_in_scope);

                    referenced_idents = referenced_idents.clone().union(output.referenced_idents);
                    applied_variants = applied_variants.clone().union(output.applied_variants);

                    new_args.push(loc_expr);
                }

                new_args
            });

            let output = Output {referenced_idents, applied_variants, tail_call: None};

            (ApplyVariant(symbol, new_opt_args), output)
        }

        expr::Expr::Assign(assignments, box_loc_returned) => {
            use self::Pattern::*;

            let mut referenced_idents = ImSet::default();
            let mut applied_variants = ImSet::default();
            let mut new_idents_in_scope = ImMap::default();

            // Record all the new idents we're adding to scope
            for (loc_pattern, _) in assignments.iter() {
                add_idents_to_scope(loc_pattern.clone(), &mut new_idents_in_scope);
            }

            // Add the new_idents_in_scope to idents_in_scope. If there's a collision,
            // it means there was shadowing, so keep the original mapping from ident_in_scope.
            // Shadowing means the mapping from new_idents_in_scope will be removed later.
            let mut combined_idents_in_scope = idents_in_scope.clone().union(new_idents_in_scope.clone());

            let can_assignments = assignments.into_iter().map(|(loc_pattern, expr)| {
                // Each assignment gets to have all the idents in scope that are assigned in this
                // block. Order of assignments doesn't matter, thanks to referential transparency!
                let (loc_can_expr, can_output) = canonicalize(env, expr, &combined_idents_in_scope);

                referenced_idents = referenced_idents.clone().union(can_output.referenced_idents);
                applied_variants = applied_variants.clone().union(can_output.applied_variants);

                // Exclude the current ident from shadowable_idents; you can't shadow yourself!
                // (However, still include it in scope, because you *can* recursively refer to yourself.)
                let mut shadowable_idents = combined_idents_in_scope.clone();
                remove_idents(loc_pattern.value.clone(), &mut shadowable_idents);

                let can_pattern = canonicalize_pattern(env, loc_pattern, &mut combined_idents_in_scope, &mut shadowable_idents);

                // Give closures names (and tail-recursive status) where appropriate
                let region = loc_can_expr.region;
                let can_expr =
                    match &can_pattern.value {
                        // First, make sure we are actually assigning an identifier instead of (for example) a variant.
                        // If we're assigning (UserId userId) = ... then this is by definition not a self tail call!
                        // (We could theoretically support certain scenarios like that, but it doesn't seem worthwhile;
                        // all we'd be saving anyone is the step of refactoring the closure out to have its own name.)
                        // By our definition, only assignments of the form (foo = ...) can be self tail calls.
                        &Identifier(ref closure_symbol_ref) => {
                            match loc_can_expr.value {
                                AnonymousClosure(closed_over, args, body) => {
                                    let closure_symbol = closure_symbol_ref.clone();
                                    let is_self_tail_recursive = match can_output.tail_call {
                                        None => false,
                                        Some(symbol) => symbol == closure_symbol
                                    };

                                    if is_self_tail_recursive {
                                        TailRecursiveClosure(closure_symbol, closed_over, args, body)
                                    } else {
                                        NamedClosure(closure_symbol, closed_over, args, body)
                                    }
                                },
                                non_closure => non_closure
                            }
                        },
                        &Shadowed(_) | &Variant(_, _) | &Integer(_) | &Fraction(_, _)
                            | &ExactString(_) | &EmptyRecordLiteral | &Underscore => loc_can_expr.value
                    };

                (can_pattern, Located {region, value: can_expr})
            }).collect();

            // The assignment as a whole is a tail call iff its return expression is a tail call.
            // We use its output as a starting point because its tail_call already has the right answer!
            let (ret_expr, mut output) = canonicalize(env, *box_loc_returned, &combined_idents_in_scope);

            output.referenced_idents = output.referenced_idents.clone().union(referenced_idents);
            output.applied_variants = output.applied_variants.clone().union(applied_variants);

            // Now that we've collected all the references, check to see if any of the new idents
            // we defined were unused. If any were, report it.
            for (ident, loc_expr) in new_idents_in_scope {
                if !output.referenced_idents.contains(&ident) {
                    env.problem(Problem::UnusedAssignment(loc_expr));
                }
            }

            (Assign(can_assignments, Box::new(ret_expr)), output)
        },

        expr::Expr::Closure(loc_arg_patterns, box_loc_body_expr) => {
            let mut new_idents_in_scope = ImMap::default();

            // Record all the new idents we're adding to scope
            for loc_pattern in loc_arg_patterns.iter() {
                add_idents_to_scope(loc_pattern.clone(), &mut new_idents_in_scope);
            }

            // Add the new_idents_in_scope to idents_in_scope. If there's a collision,
            // it means there was shadowing, so keep the original mapping from ident_in_scope.
            // Shadowing means the mapping from new_idents_in_scope will be removed later.
            let mut combined_idents_in_scope = idents_in_scope.clone().union(new_idents_in_scope.clone());

            let can_args = loc_arg_patterns.into_iter().map(|loc_pattern| {
                canonicalize_pattern(env, loc_pattern, &mut combined_idents_in_scope.clone(), &mut combined_idents_in_scope)
            }).collect();

            let (body_expr, output) = canonicalize(env, *box_loc_body_expr, &combined_idents_in_scope);

            // Now that we've collected all the references, check to see if any of the new idents
            // we defined were unused. If any were, report it.
            for (ident, loc_expr) in new_idents_in_scope {
                if !output.referenced_idents.contains(&ident) {
                    env.problem(Problem::UnusedAssignment(loc_expr));
                }
            }

            // While we still have the info, determine what locals we will need to close over in code gen.
            let closed_over_locals = if output.referenced_idents.is_empty() {
                None
            } else {
                let locals = output.referenced_idents.iter().filter_map(|(opt_path, name)| {
                    // Only close over locally assigned idents; globals are always available.
                    if opt_path.is_none()
                        // If it's not in scope, it'll be a NAMING ERROR at runtime, and
                        // attempting to close over it will fail. Leave it out!
                        && combined_idents_in_scope.contains_key(&(None, name.clone()))
                    {
                        Some(LocalSymbol::new(name.clone()))
                    } else {
                        None
                    }
                }).collect::<Vec<_>>();

                if locals.is_empty() {
                    None
                } else {
                    Some(locals)
                }
            };

            (AnonymousClosure(closed_over_locals, can_args, Box::new(body_expr)), output)
        },

        expr::Expr::Case(loc_cond, branches) => {
            // Canonicalize the nested expressions
            let (can_cond, mut output) = canonicalize(env, *loc_cond, idents_in_scope);
            let mut recorded_tail_call = false;

            // Clear the initial tail_call, since it depends only on the branches.
            // The branches should overwrite this, so it will only come up if
            // there are no branches, but that is possible! A case with no branches
            // is a runtime error, but code gen thinking this is actually tail recursive
            // could have more serious consequences than a runtime error.
            output.tail_call = None;

            let can_branches = branches.into_iter().map(|(loc_pattern, loc_expr)| {
                let can_pattern = canonicalize_pattern(env, loc_pattern.clone(), &mut idents_in_scope.clone(), &mut idents_in_scope.clone());

                // Patterns introduce new idents to the scope!
                let mut new_idents_in_scope = ImMap::default();

                add_idents_to_scope(loc_pattern, &mut new_idents_in_scope);

                let branch_idents_in_scope = idents_in_scope.clone().union(new_idents_in_scope.clone());
                let (can_expr, branch_out) = canonicalize(env, loc_expr, &branch_idents_in_scope);

                output.applied_variants = output.applied_variants.clone().union(branch_out.applied_variants);
                output.referenced_idents = output.referenced_idents.clone().union(branch_out.referenced_idents);

                // If all branches are tail calling the same symbol, then so is the conditional as a whole.
                if !recorded_tail_call {
                    // If we haven't recorded output.tail_call yet, record it.
                    output.tail_call = branch_out.tail_call;
                    recorded_tail_call = true;
                } else if branch_out.tail_call != output.tail_call {
                    // If we recorded output.tail_call, but what we recorded is
                    // different from what we just saw, then game over. This isn't
                    // a potential self tail call!
                    output.tail_call = None;
                }

                // Now that we've collected all the references for this branch, check to see if
                // any of the new idents it defined were unused. If any were, report it.
                for (ident, loc_expr) in new_idents_in_scope {
                    if !output.referenced_idents.contains(&ident) {
                        env.problem(Problem::UnusedAssignment(loc_expr));
                    }
                }

                (can_pattern, can_expr)
            }).collect();

            // Incorporate all three expressions into a combined Output value.
            let expr = Case(Box::new(can_cond), can_branches);

            (expr, output)
        }
    };

    // At the end, diff used_idents and assigned_idents to see which were unused.
    // Add warnings for those!

    // In a later phase, unused top level declarations won't get monomorphized or code-genned.
    // We aren't going to bother with DCE at the level of local assignments. It's going to be
    // a rounding error anyway (especially given that they'll be surfaced as warnings), LLVM will
    // DCE them in optimized builds, and it's not worth the bookkeeping for dev builds.
    (Located {region, value: expr}, output)
}

fn add_idents_to_scope(
    pattern: Located<expr::Pattern>,
    idents_in_scope: &mut ImMap<(Option<Path>, String), Located<expr::Ident>>
) {
    use expr::Pattern::*;

    match pattern.value {
        Identifier(name) => {
            let loc_ident = Located {
                region: pattern.region.clone(),
                value: Ident::Unqualified(name.clone())
            };

            idents_in_scope.insert((None, name), loc_ident);
        },
        Variant(_, Some(loc_args)) => {
            for loc_arg in loc_args {
                add_idents_to_scope(loc_arg, idents_in_scope);
            }
        },
        Variant(_, None) | Integer(_) | Fraction(_, _) | ExactString(_)
            | EmptyRecordLiteral | Underscore => {}
    }
}

fn remove_idents(
    pattern: expr::Pattern,
    idents: &mut ImMap<(Option<Path>, String), Located<expr::Ident>>
) {
    use expr::Pattern::*;

    match pattern {
        Identifier(name) => { idents.remove(&(None, name)); },
        Variant(_, Some(loc_args)) => {
            for loc_arg in loc_args {
                remove_idents(loc_arg.value, idents);
            }
        },
        Variant(_, None) | Integer(_) | Fraction(_, _) | ExactString(_)
            | EmptyRecordLiteral | Underscore => {}
    }
}

/// If it could not be found, return it unchanged as an Err.
#[inline(always)] // This is shared code between Var() and CallByName(); it was inlined when handwritten
fn resolve_ident(
    env: &Env,
    ident: Ident,
    referenced_idents: &mut ImSet<(Option<Path>, String)>,
    idents_in_scope: &ImMap<(Option<Path>, String), Located<expr::Ident>>,
) -> Result<Resolved<Symbol>, Ident> {
    match ident {
        Ident::Unqualified(name) => {
            let unqualified = ( None, name );

            if idents_in_scope.contains_key(&unqualified) {
                referenced_idents.insert(unqualified.clone());

                Ok(Symbol::resolved_local(unqualified.1))
            } else {
                let qualified = ( Some(env.home.clone()), unqualified.1 );

                if idents_in_scope.contains_key(&qualified) {
                    referenced_idents.insert(qualified.clone());

                    Ok(Symbol::resolved_global((qualified.0.unwrap(), qualified.1)))
                } else {
                    // We couldn't find the unqualified ident in scope. NAMING PROBLEM!
                    Err(Ident::Unqualified(qualified.1))
                }
            }
        },
        Ident::Qualified((path, name)) => {
            let qualified = (Some(path), name);

            if idents_in_scope.contains_key(&qualified) {
                referenced_idents.insert(qualified.clone());

                Ok(Symbol::resolved_global((qualified.0.unwrap(), qualified.1)))
            } else {
                // We couldn't find the qualified ident in scope. NAMING PROBLEM!
                Err(Ident::Qualified((qualified.0.unwrap(), qualified.1)))
            }
        }
    }
}

/// Translate a VariantName into a resolved symbol if it's found in env.declared_variants.
/// If it could not be found, return it unchanged as an Err.
#[inline(always)]
fn resolve_variant_name(
    env: &Env,
    variant_name: VariantName,
    applied_variants: &mut ImSet<(Path, String)>,
) -> Result<Resolved<GlobalSymbol>, VariantName> {
    let qualified = match variant_name {
        VariantName::Unqualified(name) => ( env.home.clone(), name ),
        VariantName::Qualified(qualified) => qualified
    };

    if env.declared_variants.contains_key(&qualified) {
        applied_variants.insert(qualified.clone());

        Ok(GlobalSymbol::recognized(qualified))
    } else {
        // We couldn't find the qualified variant name in scope. NAMING PROBLEM!
        Err(VariantName::Qualified(qualified))
    }
}

fn canonicalize_pattern(
    env: &mut Env,
    loc_pattern: Located<expr::Pattern>,
    idents_in_scope: &mut ImMap<(Option<Path>, String), Located<expr::Ident>>,
    shadowable_idents: &mut ImMap<(Option<Path>, String), Located<expr::Ident>>,
) -> Located<Pattern> {
    use self::Pattern::*;

    let region = loc_pattern.region;
    let new_pattern = match loc_pattern.value {
        expr::Pattern::Integer(num) => Integer(num),
        expr::Pattern::Fraction(numerator, denominator) => Fraction(numerator, denominator),
        expr::Pattern::EmptyRecordLiteral => EmptyRecordLiteral,
        expr::Pattern::ExactString(string) => ExactString(string),
        expr::Pattern::Underscore => Underscore,

        expr::Pattern::Identifier(name) => {
            let unqualified_ident = ( None, name );

            // We use shadowable_idents for this, and not idents_in_scope, because for assignments
            // they are different. When canonicalizing a particular assignment, that new
            // ident is in scope (for recursion) but not shadowable.
            //
            // For example, when canonicalizing (fibonacci = ...), `fibonacci` should be in scope
            // so that it can refer to itself without getting a naming problem, but it should not
            // be in the collection of shadowable idents because you can't shadow yourself!
            match shadowable_idents.get(&unqualified_ident) {
                Some(shadowed_ident) => {
                    // This is already in scope, meaning it's about to be shadowed.
                    // Shadowing is not allowed!
                    env.problem(Problem::Shadowing(shadowed_ident.clone()));

                    // Change this Pattern to a Shadowed variant, so that
                    // codegen knows to generate a runtime exception here.
                    Shadowed(shadowed_ident.clone())
                },
                None => {
                    let qualified_ident = ( Some(env.home.clone()), unqualified_ident.1 );

                    match idents_in_scope.get(&qualified_ident) {
                        Some(shadowed_ident) => {
                            // This is already in scope, meaning it's about to be shadowed.
                            // Shadowing is not allowed!
                            env.problem(Problem::Shadowing(shadowed_ident.clone()));

                            // Change this Pattern to a Shadowed variant, so that
                            // codegen knows to generate a runtime exception here.
                            Shadowed(shadowed_ident.clone())
                        },
                        None => {
                            let new_name = qualified_ident.1.clone();

                            // This is a fresh identifier that wasn't already in scope.
                            // Add it to scope!
                            let located = Located {region, value: expr::Ident::Unqualified(new_name.clone())};

                            // Add this to both idents_in_scope *and* shadowable_idents.
                            // The latter is relevant when recursively canonicalizing Variant patterns,
                            // which can bring multiple new idents into scope. For example, it's important
                            // that we catch (Blah foo foo) as being an example of shadowing.
                            idents_in_scope.insert(qualified_ident.clone(), located.clone());
                            shadowable_idents.insert(qualified_ident, located);

                            Identifier(Symbol::local(new_name))
                        }
                    }
                }
            }
        },

        expr::Pattern::Variant(loc_name, opt_args) => {
            // Canonicalize the variant's name.
            let can_name = canonicalize_variant_name(env, loc_name);

            // Canonicalize the variant's arguments, if it has any.
            let opt_can_args: Option<Vec<Located<Pattern>>> = match opt_args {
                None => None,
                Some(loc_args) => {
                    let mut can_args:Vec<Located<Pattern>> = Vec::new();

                    for loc_arg in loc_args {
                        let can_arg = canonicalize_pattern(env, loc_arg, idents_in_scope, shadowable_idents);

                        can_args.push(can_arg);
                    }

                    Some(can_args)
                }
            };

            Variant(can_name, opt_can_args)
        }
    };

    Located {region, value: new_pattern}
}

fn canonicalize_variant_name(
    env: &mut Env,
    loc_name: Located<VariantName>
) -> Resolved<Symbol> {
    let qualified_name = match &loc_name.value {
        &VariantName::Unqualified(ref name) => ( env.home.clone(), name.clone() ),
        &VariantName::Qualified(ref qualified) => qualified.clone()
    };

    if env.declared_variants.contains_key(&qualified_name) {
        // No problems; the qualified variant name was in scope!
        Symbol::resolved_global(qualified_name)
    } else {
        // We couldn't find the variant name in scope. NAMING PROBLEM!
        env.problem(Problem::UnrecognizedVariant(loc_name.clone()));

        Resolved::UnrecognizedVariant(loc_name)
    }
}

// OPERATOR PRECEDENCE

// Precedence logic adapted from Gluon by Markus Westerlind, MIT licensed
// https://github.com/gluon-lang/gluon
#[derive(Clone, Debug, PartialEq)]
pub enum PrecedenceProblem {
    BothNonAssociative(Located<Operator>, Located<Operator>),
}

fn new_op_expr(left: Box<Located<Expr>>, op: Located<Operator>, right: Box<Located<Expr>>)
    -> Located<Expr> {
    let new_region = Region {
        start_line: left.region.start_line,
        start_col: left.region.start_col,

        end_line: right.region.end_line,
        end_col: right.region.end_col
    };
    let new_expr = Expr::Operator(left, op, right);

    Located::new(new_expr, new_region)
}

/// Reorder the expression tree based on operator precedence and associativity rules.
/// In many languages, this can fail due to (for example) <| and |> having the same
/// precedence but different associativity. Languages which support custom operators with
/// user-defined precedence and associativity (e.g. Haskell) can have many such errors.
///
/// By design, Roc neither allows custom operators nor has any built-in operators with
/// the same precedence and different associativity, so this operation always succeeds
/// and can never produce any user-facing errors.
fn apply_precedence_and_associativity(env: &mut Env, expr: Located<Expr>)
    -> Located<Expr> {
    use self::PrecedenceProblem::*;

    // NOTE: A potentially nice performance optimization here would be to use
    // arena bump allocation for Infixes, arg_stack, and op_stack. As long as we
    // allocate each element inside arg_stack outside the arena, this should end
    // up being a decent bit more efficient.
    let mut infixes = Infixes::new(expr);
    let mut arg_stack: Vec<Box<Located<Expr>>> = Vec::new();
    let mut op_stack: Vec<Located<Operator>> = Vec::new();

    while let Some(token) = infixes.next() {
        match token {
            InfixToken::Arg(next_expr) => arg_stack.push(next_expr),
            InfixToken::Op(next_op) => {
                match op_stack.pop() {
                    Some(stack_op) => {
                        match next_op.value.cmp(&stack_op.value) {
                            Ordering::Less => {
                                // Inline
                                let right = arg_stack.pop().unwrap();
                                let left = arg_stack.pop().unwrap();

                                infixes.next_op = Some(next_op);
                                arg_stack.push(Box::new(new_op_expr(left, stack_op, right)));
                            }

                            Ordering::Greater => {
                                // Swap
                                op_stack.push(stack_op);
                                op_stack.push(next_op);
                            }

                            Ordering::Equal => {
                                match (next_op.value.associativity(), stack_op.value.associativity()) {
                                    ( LeftAssociative, LeftAssociative ) => {
                                        // Inline
                                        let right = arg_stack.pop().unwrap();
                                        let left = arg_stack.pop().unwrap();

                                        infixes.next_op = Some(next_op);
                                        arg_stack.push(Box::new(new_op_expr(left, stack_op, right)));
                                    },

                                    ( RightAssociative, RightAssociative ) => {
                                        // Swap
                                        op_stack.push(stack_op);
                                        op_stack.push(next_op);
                                    },

                                    ( NonAssociative, NonAssociative ) => {
                                        // Both operators were non-associative, e.g. (True == False == False).
                                        // We should tell the author to disambiguate by grouping them with parens.
                                        let problem = BothNonAssociative(next_op.clone(), stack_op);

                                        env.problem(Problem::PrecedenceProblem(problem.clone()));

                                        let right = arg_stack.pop().unwrap();
                                        let left = arg_stack.pop().unwrap();
                                        let broken_expr = new_op_expr(left, next_op, right);
                                        let region = broken_expr.region.clone();
                                        let value = Expr::InvalidPrecedence(problem, Box::new(broken_expr));

                                        return Located {region, value};
                                    }

                                    _ => {
                                        // The operators had the same precedence but different associativity.
                                        //
                                        // In many languages, this case can happen due to (for example) <| and |> having the same
                                        // precedence but different associativity. Languages which support custom operators with
                                        // (e.g. Haskell) can potentially have arbitrarily many of these cases.
                                        //
                                        // By design, Roc neither allows custom operators nor has any built-in operators with
                                        // the same precedence and different associativity, so this should never happen!
                                        panic!("Operators had the same associativity, but different precedence. This should never happen!");
                                    }
                                }
                            }
                        }
                    },
                    None => op_stack.push(next_op)
                };
            }
        }
    }

    for op in op_stack.into_iter().rev() {
        let right = arg_stack.pop().unwrap();
        let left = arg_stack.pop().unwrap();

        arg_stack.push(Box::new(new_op_expr(left, op, right)));
    }

    assert_eq!(arg_stack.len(), 1);

    *arg_stack.pop().unwrap()
}

#[derive(Debug, Clone, PartialEq)]
enum InfixToken {
    Arg(Box<Located<Expr>>),
    Op(Located<Operator>),
}

/// An iterator that takes an expression that has had its operators grouped
/// with _right associativity_, and yeilds a sequence of `InfixToken`s. This
/// is useful for reparsing the operators with their correct associativies
/// and precedences.
///
/// For example, the expression:
///
/// ```text
/// (1 + (2 ^ (4 * (6 - 8))))
/// ```
///
/// Will result in the following iterations:
///
/// ```text
/// Arg:  1
/// Op:   +
/// Arg:  2
/// Op:   ^
/// Arg:  4
/// Op:   *
/// Arg:  6
/// Op:   -
/// Arg:  8
/// ```
struct Infixes {
    /// The next part of the expression that we need to flatten
    remaining_expr: Option<Box<Located<Expr>>>,
    /// Cached operator from a previous iteration
    next_op: Option<Located<Operator>>,
}

impl Infixes {
    fn new(expr: Located<Expr>) -> Infixes {
        Infixes {
            remaining_expr: Some(Box::new(expr)),
            next_op: None,
        }
    }
}

impl Iterator for Infixes {
    type Item = InfixToken;

    fn next(&mut self) -> Option<InfixToken> {
        match self.next_op.take() {
            Some(op) => Some(InfixToken::Op(op)),
            None => {
                self.remaining_expr.take().map(|boxed_expr| {
                    let expr = *boxed_expr;

                    match expr.value {
                        Expr::Operator(left, op, right) => {
                            self.remaining_expr = Some(right);
                            self.next_op = Some(op);

                            InfixToken::Arg(left)
                        }
                        _ => InfixToken::Arg(Box::new(expr)),
                    }
                })
            }
        }
    }
}