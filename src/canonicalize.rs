use region::{Located, Region};
use operator::Operator;
use operator::Operator::Pizza;
use operator::Associativity::*;
use collections::{ImSet, ImMap, MutMap};
use std::cmp::Ordering;
use expr::{Ident, VariantName};
use expr;
use self::PatternType::*;


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
    Var(Symbol),
    FunctionPointer(Symbol),
    CallByName(Symbol, Vec<Located<Expr>>),
    InterpolatedStr(Vec<(String, Expr)>, String),

    // Pattern Matching
    Case(Box<Located<Expr>>, Vec<(Pattern, Located<Expr>)>),
    Assign(Vec<(Pattern, Located<Expr>)>, Box<Located<Expr>>),

    // Application
    Apply(Box<Located<Expr>>, Vec<Located<Expr>>),
    ApplyVariant(Symbol, Option<Vec<Located<Expr>>>),

    // Product Types
    EmptyRecord,

    // Sugar
    If(Box<Located<Expr>>, Box<Located<Expr>>, Box<Located<Expr>>),
    Operator(Box<Located<Expr>>, Located<Operator>, Box<Located<Expr>>),

    // Runtime Errors
    InvalidPrecedence(PrecedenceProblem, Box<Located<Expr>>),
    UnrecognizedFunctionName(Located<expr::Ident>),
    UnrecognizedConstant(Located<expr::Ident>),
    UnrecognizedVariant(Located<expr::VariantName>),
}

/// Problems that can occur in the course of canonicalization.
#[derive(Clone, Debug, PartialEq)]
pub enum Problem {
    Shadowing(Located<expr::Ident>),
    UnrecognizedFunctionName(Located<Ident>),
    UnrecognizedConstant(Located<Ident>),
    UnrecognizedVariant(Located<VariantName>),
    UnusedAssignment(Located<Ident>),
    UnusedArgument(Located<Ident>),
    PrecedenceProblem(PrecedenceProblem),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(PatternType, Located<expr::Pattern>)
}

/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Identifier(Symbol),
    Variant(Symbol, Option<Vec<Pattern>>),
    Integer(i64),
    Fraction(i64, i64),
    ExactString(String),
    EmptyRecordLiteral,
    Underscore,

    // Runtime Exceptions
    Shadowed(Located<Ident>),
    UnrecognizedVariant(Located<expr::VariantName>),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Located<expr::Pattern>)
}

/// A globally unique identifier, used for both vars and variants.
/// It will be used directly in code gen.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Symbol(String);

impl Symbol {
    pub fn new(prefix: &str, name: &str) -> Symbol {
        Symbol(format!("{}{}", prefix, name))
    }

    pub fn from_variant(variant_name: &VariantName, home: &str) -> Symbol {
        match &variant_name {
            &VariantName::Unqualified(ref name) =>
                Symbol::new(home, name),

            &VariantName::Qualified(ref path, ref name) =>
                Symbol::new(path, name),
        }
    }
}

impl Into<String> for Symbol {
    fn into(self) -> String {
        let Symbol(string) = self;

        string
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Scope {
    pub idents: ImMap<Ident, (Symbol, Region)>,
    symbol_prefix: String,
    next_unique_id: u64,
}

impl Scope {
    pub fn new(symbol_prefix: String, declared_idents: ImMap<Ident, (Symbol, Region)>) -> Scope {
        Scope {
            symbol_prefix,

            // This is used to generate unique names for anonymous closures.
            // It always begins at 0.
            next_unique_id: 0,

            idents: declared_idents
        }
    }

    pub fn symbol(&self, name: &str) -> Symbol {
        Symbol::new(&self.symbol_prefix, name)
    }

    pub fn gen_unique_symbol(&mut self) -> Symbol {
        self.next_unique_id = self.next_unique_id + 1;

        Symbol::new(&self.symbol_prefix, &self.next_unique_id.to_string())
    }
}

struct Procedure {
    name: Option<String>,
    closes_over: ImSet<Symbol>,
    is_self_tail_recursive: bool,
    definition: Region,
    args: Vec<Pattern>,
    body: Expr
}

impl Procedure {
    pub fn new(definition: Region, closes_over: ImSet<Symbol>, args: Vec<Pattern>, body: Expr) -> Procedure {
        Procedure {
            name: None,
            closes_over,
            is_self_tail_recursive: false,
            definition,
            args,
            body
        }
    }
}

/// The canonicalization environment for a particular module.
struct Env {
    /// The module's path. Unqualified references to identifiers and variant names are assumed
    /// to be relative to this path.
    home: String,

    /// Problems we've encountered along the way, which will be reported to the user at the end.
    problems: Vec<Problem>,

    /// Variants either declared in this module, or imported.
    variants: ImMap<Symbol, Located<expr::VariantName>>,

    /// Former closures converted to top-level procedures.
    procedures: MutMap<Symbol, Procedure>,
}

impl Env {
    pub fn new(home: String, declared_variants: ImMap<Symbol, Located<expr::VariantName>>) -> Env {
        Env {
            home,
            variants: declared_variants,
            problems: Vec::new(),
            procedures: MutMap::default(),
        }
    }

    pub fn problem(&mut self, problem: Problem) -> () {
        self.problems.push(problem)
    }

    pub fn register_closure(
        &mut self,
        symbol: Symbol,
        closes_over: ImSet<Symbol>,
        args: Vec<Pattern>,
        body: Expr,
        definition: Region
    ) -> () {
        // We can't if the closure is self tail recursive yet, because it doesn't know its final name yet.
        // (Assign sets that.) Assume this is false, and let Assign change it to true after it sets final name.
        let is_self_tail_recursive = false;
        let name = None; // The Assign logic is also responsible for setting names after the fact.
        let procedure = Procedure {closes_over, args, name, body, is_self_tail_recursive, definition};

        self.procedures.insert(symbol, procedure);
    }
}

pub fn canonicalize_declaration(
    home: String,
    name: &str,
    loc_expr: Located<expr::Expr>,
    declared_idents: &ImMap<Ident, (Symbol, Region)>,
    declared_variants: &ImMap<Symbol, Located<expr::VariantName>>,
) -> (Located<Expr>, Output, Vec<Problem>) {
    // If we're canonicalizing the declaration `foo = ...` inside the `Main` module,
    // scope_prefix will be "Main$foo$" and its first closure will be named "Main$foo$0"
    let scope_prefix = format!("{}${}$", home, name);
    let mut scope = Scope::new(scope_prefix, declared_idents.clone());
    let mut env = Env::new(home, declared_variants.clone());
    let (mut new_loc_expr, output) = canonicalize(&mut env, &mut scope, loc_expr);

    // Apply operator precedence and associativity rules once, after canonicalization is
    // otherwise complete. If we did this *during* canonicalization, then each time we
    // visited an Operator node we'd recursively try to apply this to each of its nested
    // operators, and thena again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.
    new_loc_expr = apply_precedence_and_associativity(&mut env, new_loc_expr);

    (new_loc_expr, output, env.problems)
}

#[derive(Clone, Debug, PartialEq)]
pub struct Output {
    pub references: References,
    pub tail_call: Option<Symbol>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct References {
    pub locals: ImSet<Symbol>,
    pub globals: ImSet<Symbol>,
    pub variants: ImSet<Symbol>,
}

impl References {
    pub fn new() -> References {
        References {
            locals: ImSet::default(),
            globals: ImSet::default(),
            variants: ImSet::default(),
        }
    }

    pub fn union(mut self, other: References) -> Self {
        self.locals = self.locals.union(other.locals);
        self.globals = self.globals.union(other.globals);
        self.variants = self.variants.union(other.variants);

        self
    }

    pub fn has_local(&self, symbol: &Symbol) -> bool {
        self.locals.contains(symbol)
    }

    pub fn has_variant(&self, symbol: &Symbol) -> bool {
        self.variants.contains(symbol)
    }
}

impl Output {
    pub fn new() -> Output {
        Output {
            references: References::new(),
            tail_call: None,
        }
    }
}

fn canonicalize(
    env: &mut Env,
    scope: &mut Scope,
    loc_expr: Located<expr::Expr>,
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
            let (cond_expr, cond_out) = canonicalize(env, scope, *loc_cond);
            let (true_expr, true_out) = canonicalize(env, scope, *loc_true);
            let (false_expr, false_out) = canonicalize(env, scope, *loc_false);

            // Incorporate all three expressions into a combined Output value.
            let expr = If(Box::new(cond_expr), Box::new(true_expr), Box::new(false_expr));
            let mut output = cond_out;

            // If both branches are tail calling the same symbol, then so is the conditional as a whole.
            // Also, if both branches are not tail calls (tail_call == None), then so is the conditional.
            // If the branches are different, we leave the default of None as-is.
            if true_out.tail_call == false_out.tail_call {
                output.tail_call = true_out.tail_call;
            }

            // To evaluate the whole if-expression, we depend on all the values that both branches depend on.
            output.references = output.references.union(true_out.references);
            output.references = output.references.union(false_out.references);

            (expr, output)
        },

        expr::Expr::Apply(loc_fn, loc_args) => {
            // Canonicalize the function expression and its arguments
            let (fn_expr, mut output) = canonicalize(env, scope, *loc_fn);
            let mut args = Vec::new();
            let mut outputs = Vec::new();

            for loc_arg in loc_args {
                let (arg_expr, arg_out) = canonicalize(env, scope, loc_arg);

                args.push(arg_expr);
                outputs.push(arg_out);
            }

            let expr = Apply(Box::new(fn_expr), args);

            for arg_out in outputs {
                output.references = output.references.union(arg_out.references);
            }

            // We're not tail-calling a symbol (by name), we're tail-calling a function value.
            output.tail_call = None;

            (expr, output)
        },

        expr::Expr::Operator(loc_left, op, loc_right) => {
            // Canonicalize the nested expressions
            let (left_expr, left_out) = canonicalize(env, scope, *loc_left);
            let (right_expr, mut output) = canonicalize(env, scope, *loc_right);

            // Incorporate both expressions into a combined Output value.
            output.references = output.references.union(left_out.references);

            // The pizza operator is the only one that can be a tail call,
            // because it's the only one that can call a function by name.
            output.tail_call = match op.value {
                Pizza => {
                    match &right_expr.value {
                        &Var(ref sym) => Some(sym.clone()),
                        &CallByName(ref sym, _) => Some(sym.clone()),
                        _ => None
                    }
                },
                _ => None
            };

            let expr = Operator(Box::new(left_expr), op, Box::new(right_expr));

            (expr, output)
        },

        expr::Expr::Var(ident) => {
            let mut output = Output::new();
            let can_expr =
                match resolve_ident(&env, &scope, ident, &mut output.references) {
                    Ok(symbol) => Var(symbol),
                    Err(ident) => {
                        let loc_ident = Located {region, value: ident};

                        env.problem(Problem::UnrecognizedConstant(loc_ident.clone()));

                        UnrecognizedConstant(loc_ident)
                    }
                };

            (can_expr, output)
        },

        expr::Expr::CallByName(ident, args) => {
            // Canonicalize the arguments and union their references into our output.
            // We'll do this even if the function name isn't recognized, since we still
            // want to report canonicalization problems with the function's arguments,
            // and their references still matter for purposes of detecting unused things.
            let mut output = Output::new();
            let mut can_args = Vec::with_capacity(args.len());

            for arg in args {
                let (loc_expr, arg_output) = canonicalize(env, scope, arg);

                output.references = output.references.union(arg_output.references);

                can_args.push(loc_expr);
            }

            let can_expr =
                match resolve_ident(&env, &scope, ident, &mut output.references) {
                    Ok(symbol) => {
                        // CallByName expressions are considered tail calls,
                        // so that their parents in the expression tree will
                        // correctly inherit tail-call-ness from them.
                        output.tail_call = Some(symbol.clone());

                        CallByName(symbol, can_args)
                    }
                    Err(ident) => {
                        let loc_ident = Located {region, value: ident};

                        env.problem(Problem::UnrecognizedFunctionName(loc_ident.clone()));

                        UnrecognizedFunctionName(loc_ident)
                    }
                };

            (can_expr, output)
        },

        expr::Expr::InterpolatedStr(pairs, suffix) => {
            let mut output = Output::new();
            let can_pairs: Vec<(String, Expr)> = pairs.into_iter().map(|(string, loc_ident)| {
                // From a language design perspective, we only permit idents in interpolation.
                // However, in a canonical Expr we store it as a full Expr, not a Symbol.
                // This is so that we can resolve it to either Var or Unrecognized; if we
                // stored it as a Symbol, we couldn't record runtime errors here.
                let can_expr =
                    match resolve_ident(&env, &scope, loc_ident.value, &mut output.references) {
                        Ok(symbol) => Var(symbol),
                        Err(ident) => {
                            let loc_ident = Located {region: loc_ident.region, value: ident};

                            env.problem(Problem::UnrecognizedConstant(loc_ident.clone()));

                            UnrecognizedConstant(loc_ident)
                        }
                    };

                (string, can_expr)
            }).collect();

            (InterpolatedStr(can_pairs, suffix), output)
        }

        expr::Expr::ApplyVariant(variant_name, opt_args) => {
            // Canonicalize the arguments and union their references into our output.
            // We'll do this even if the variant name isn't recognized, since we still
            // want to report canonicalization problems with the variant's arguments,
            // and their references still matter for purposes of detecting unused things.
            let mut output = Output::new();

            let opt_can_args = match opt_args {
                Some(args) => {
                    let mut can_args = Vec::with_capacity(args.len());

                    for arg in args {
                        let (loc_expr, arg_output) = canonicalize(env, scope, arg);

                        output.references = output.references.union(arg_output.references);

                        can_args.push(loc_expr);
                    }

                    Some(can_args)
                }
                None => None
            };

            let can_expr =
                match resolve_variant_name(&env, &scope, variant_name, &mut output.references) {
                    Ok(symbol) => ApplyVariant(symbol, opt_can_args),
                    Err(variant_name) => {
                        let loc_variant = Located {region, value: variant_name};

                        env.problem(Problem::UnrecognizedVariant(loc_variant.clone()));

                        UnrecognizedVariant(loc_variant)
                    }
                };

            (can_expr, output)
        }

        expr::Expr::Assign(assignments, box_loc_returned) => {
            // The body expression gets a new scope for canonicalization.
            // Shadow `scope` to make sure we don't accidentally use the original one for the
            // rest of this block.
            let mut scope = scope.clone();

            // Add the assigned identifiers to scope. If there's a collision, it means there
            // was shadowing, which will be handled later.
            let assigned_idents: ImMap<Ident, (Symbol, Region)> =
                idents_from_patterns(assignments.clone().into_iter().map(|(loc_pattern, _)| loc_pattern), &scope);

            scope.idents = scope.idents.union(assigned_idents.clone());

            let mut refs_by_assignment: MutMap<Symbol, (Region, References)> = MutMap::default();

            let can_assignments: Vec<(Pattern, Located<Expr>)> = assignments.into_iter().map(|(loc_pattern, expr)| {
                // Each assignment gets to have all the idents in scope that are assigned in this
                // block. Order of assignments doesn't matter, thanks to referential transparency!
                let (loc_can_expr, can_output) = canonicalize(env, &mut scope, expr);

                // Exclude the current ident from shadowable_idents; you can't shadow yourself!
                // (However, still include it in scope, because you *can* recursively refer to yourself.)
                let mut shadowable_idents = scope.idents.clone();
                remove_idents(loc_pattern.value.clone(), &mut shadowable_idents);

                let can_pattern = canonicalize_pattern(env, &mut scope, &Assignment, &loc_pattern, &mut shadowable_idents);

                // Store the referenced locals in the refs_by_assignment map, so we can later figure out
                // which assigned names reference each other.
                for (symbol, region) in idents_from_patterns(std::iter::once(loc_pattern.clone()), &scope).values() {
                    let refs = can_output.references.clone();

                    refs_by_assignment.insert(symbol.clone(), (*region, refs));
                }

                // Give closures names (and tail-recursive status) where appropriate.
                let can_expr = match (&loc_pattern.value, &can_pattern) {
                    // First, make sure we are actually assigning an identifier instead of (for example) a variant.
                    //
                    // If we're assigning (UserId userId) = ... then this is certainly not a closure declaration,
                    // which also implies it's not a self tail call!
                    //
                    // Only assignments of the form (foo = ...) can be closure declarations or self tail calls.
                    (&expr::Pattern::Identifier(ref name), &Pattern::Identifier(ref assigned_symbol)) => {
                        match loc_can_expr.value {
                            FunctionPointer(anonymous_closure_symbol) => {
                                // Since everywhere in the code it'll be referred to by its assigned name,
                                // remove its generated name from the procedure map. (We'll re-insert it later.)
                                let mut procedure = env.procedures.remove(&anonymous_closure_symbol).unwrap();

                                // The original ident name will be used for debugging and stack traces.
                                procedure.name = Some(name.clone());

                                // The closure is self tail recursive iff it tail calls itself (by assigned name).
                                procedure.is_self_tail_recursive = match &can_output.tail_call {
                                    &None => false,
                                    &Some(ref symbol) => symbol == assigned_symbol
                                };

                                // Re-insert the procedure into the map, under its assigned name. This way,
                                // when code elsewhere calls it by assigned name, it'll resolve properly.
                                env.procedures.insert(assigned_symbol.clone(), procedure);

                                // Return a pointer to the assigned symbol, since the auto-generated one no
                                // longer references any entry in the procedure map!
                                FunctionPointer(assigned_symbol.clone())
                            },
                            non_closure => non_closure
                        }
                    },
                    _ => loc_can_expr.value
                };

                (can_pattern, Located {region: loc_can_expr.region, value: can_expr})
            }).collect();

            // The assignment as a whole is a tail call iff its return expression is a tail call.
            // Use its output as a starting point because its tail_call already has the right answer!
            let (ret_expr, mut output) = canonicalize(env, &mut scope, *box_loc_returned);

            // Determine the full set of references by traversing the graph.
            let mut visited_symbols = ImSet::default();

            // Start with the return expression's referenced locals. They are the only ones that count!
            //
            // If I have two assignments which reference each other, but neither of them
            // is referenced in the return expression, I don't want either of them (or their references)
            // to end up in the final output.references. They were unused, and so were their references!
            //
            // The reason we need a graph here is so we don't overlook transitive dependencies.
            // For example, if I have `a = b + 1` and the assignment returns `a + 1`, then the
            // assignment as a whole references both `a` *and* `b`, even though it doesn't
            // directly mention `b` - because `a` depends on `b`. If we didn't traverse a graph here,
            // we'd erroneously give a warning that `b` was unused since it wasn't directly referenced.
            for symbol in output.references.locals.clone().into_iter() {
                // Traverse the graph and look up *all* the references for this local symbol.
                let refs = get_all_referenced(symbol, &mut visited_symbols, &refs_by_assignment);

                output.references = output.references.union(refs);
            }

            // Now that we've collected all the references, check to see if any of the new idents
            // we defined went unused by the return expression. If any were unused, report it.
            for (ident, (symbol, region)) in assigned_idents {
                if !output.references.has_local(&symbol) {
                    let loc_ident = Located {region: region.clone(), value: ident.clone()};

                    env.problem(Problem::UnusedAssignment(loc_ident));
                }
            }

            (Assign(can_assignments, Box::new(ret_expr)), output)
        },

        expr::Expr::Closure(loc_arg_patterns, box_loc_body_expr) => {
            // The globally unique symbol that will refer to this closure once it gets converted
            // into a top-level procedure for code gen.
            //
            // The symbol includes the module name, the top-level declaration name, and the
            // index (0-based) of the closure within that declaration.
            //
            // Example: "MyModule$main$3" if this is the 4th closure in MyModule.main.
            let symbol = scope.gen_unique_symbol();

            // The body expression gets a new scope for canonicalization.
            // Shadow `scope` to make sure we don't accidentally use the original one for the
            // rest of this block.
            let mut scope = scope.clone();

            // Add the arguments' idents to scope.idents. If there's a collision,
            // it means there was shadowing, which will be handled later.
            let arg_idents: ImMap<Ident, (Symbol, Region)> =
                idents_from_patterns(loc_arg_patterns.clone().into_iter(), &scope);

            scope.idents = scope.idents.union(arg_idents.clone());

            let can_args: Vec<Pattern> = loc_arg_patterns.into_iter().map(|loc_pattern| {
                // Exclude the current ident from shadowable_idents; you can't shadow yourself!
                // (However, still include it in scope, because you *can* recursively refer to yourself.)
                let mut shadowable_idents = scope.idents.clone();
                remove_idents(loc_pattern.value.clone(), &mut shadowable_idents);

                canonicalize_pattern(env, &mut scope, &FunctionArg, &loc_pattern, &mut shadowable_idents)
            }).collect();
            let (loc_body_expr, output) = canonicalize(env, &mut scope, *box_loc_body_expr);

            // We only ever need to close over locals. Globals are always available!
            let mut closes_over: ImSet<Symbol> = output.references.locals.clone();

            // Now that we've collected all the references, check to see if any of the args we defined
            // went unreferenced. If any did, report them as unused arguments.
            for (ident, (arg_symbol, region)) in arg_idents {
                if !output.references.has_local(&arg_symbol) {
                    // The body never referenced this argument we declared. It's an unused argument!
                    env.problem(Problem::UnusedArgument(Located {region, value: ident}));
                }

                // If it's an argument, we shouldn't close over it.
                // (We need to explicitly remove these because we start by
                // closing over *all* referenced locals, including args.)
                closes_over.remove(&arg_symbol);
            }

            // We've finished analyzing the closure. Register it as a top-level procedure in the Env!
            env.register_closure(symbol.clone(), closes_over, can_args, loc_body_expr.value, region);

            // Always return a function pointer, in case that's how the closure is being used (e.g. with Apply).
            // It's possible that Assign will rewrite this. In that case, Assign will need to know the symbol we
            // used here, to look up the closure's info before renaming it. This pointer gives Assign that symbol.
            (FunctionPointer(symbol), output)
        },

        expr::Expr::Case(loc_cond, branches) => {
            // Canonicalize the conditional
            let (can_cond, mut output) = canonicalize(env, scope, *loc_cond);
            let mut can_branches = Vec::with_capacity(branches.len());
            let mut recorded_tail_call = false;

            for (loc_pattern, loc_expr) in branches {
                // Each case branch gets a new scope for canonicalization.
                // Shadow `scope` to make sure we don't accidentally use the original one for the
                // rest of this block.
                let mut scope = scope.clone();

                // Exclude the current ident from shadowable_idents; you can't shadow yourself!
                // (However, still include it in scope, because you *can* recursively refer to yourself.)
                let mut shadowable_idents = scope.idents.clone();
                remove_idents(loc_pattern.value.clone(), &mut shadowable_idents);

                let can_pattern = canonicalize_pattern(env, &mut scope, &CaseBranch, &loc_pattern, &mut shadowable_idents);

                // Patterns introduce new idents to the scope!
                // Add the assigned identifiers to scope. If there's a collision, it means there
                // was shadowing, which will be handled later.
                let assigned_idents: ImMap<Ident, (Symbol, Region)> =
                    idents_from_patterns(std::iter::once(loc_pattern), &scope);

                scope.idents = scope.idents.union(assigned_idents.clone());

                let (can_expr, branch_output) = canonicalize(env, &mut scope, loc_expr);

                output.references = output.references.union(branch_output.references);

                // If all branches are tail calling the same symbol, then so is the conditional as a whole.
                if !recorded_tail_call {
                    // If we haven't recorded output.tail_call yet, record it.
                    output.tail_call = branch_output.tail_call;
                    recorded_tail_call = true;
                } else if branch_output.tail_call != output.tail_call {
                    // If we recorded output.tail_call, but what we recorded differs from what we just saw,
                    // then game over. This can't possibly be a self tail call!
                    output.tail_call = None;
                }

                // Now that we've collected all the references for this branch, check to see if
                // any of the new idents it defined were unused. If any were, report it.
                for (ident, (symbol, region)) in assigned_idents {
                    if !output.references.has_local(&symbol) {
                        let loc_ident = Located {region: region.clone(), value: ident.clone()};

                        env.problem(Problem::UnusedAssignment(loc_ident));
                    }
                }

                can_branches.push((can_pattern, can_expr));
            }

            // One of the branches should have flipped this, so this should only happen
            // in the situation where the case had no branches. That can come up, though!
            // A case with no branches is a runtime error, but it will mess things up
            // if code gen mistakenly thinks this is a tail call just because its condition
            // happend to be one. (The condition gave us our initial output value.)
            if !recorded_tail_call {
                output.tail_call = None;
            }

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

fn get_all_referenced(
    assigned_symbol: Symbol,
    visited: &mut ImSet<Symbol>,
    refs_by_assignment: &MutMap<Symbol, (Region, References)>
) -> References {
    match refs_by_assignment.get(&assigned_symbol) {
        Some((_, refs)) => {
            let mut answer = References::new();

            visited.insert(assigned_symbol);

            for local in refs.locals.clone() {
                if !visited.contains(&local) {
                    let other_refs = get_all_referenced(local.clone(), visited, refs_by_assignment);

                    answer = answer.union(other_refs);
                }

                answer.locals.insert(local);
            }

            answer
        },
        None => References::new()
    }
}

fn idents_from_patterns<I>(loc_patterns: I, scope: &Scope) -> ImMap<Ident, (Symbol, Region)>
where I: Iterator<Item = Located<expr::Pattern>>
{
    let mut answer = ImMap::default();

    for loc_pattern in loc_patterns {
        add_idents_from_pattern(loc_pattern, scope, &mut answer);
    }

    answer
}

/// helper function for idents_from_patterns
fn add_idents_from_pattern(
    loc_pattern: Located<expr::Pattern>,
    scope: &Scope,
    answer: &mut ImMap<Ident, (Symbol, Region)>
) {
    use expr::Pattern::*;

    match loc_pattern.value {
        Identifier(name) => {
            let symbol = scope.symbol(&name);

            answer.insert(Ident::Unqualified(name), (symbol, loc_pattern.region));
        },
        Variant(_, Some(loc_args)) => {
            for loc_arg in loc_args {
                add_idents_from_pattern(loc_arg, scope, answer);
            }
        },
        Variant(_, None) | Integer(_) | Fraction(_, _) | ExactString(_)
            | EmptyRecordLiteral | Underscore => ()
    }
}

fn remove_idents(
    pattern: expr::Pattern,
    idents: &mut ImMap<Ident, (Symbol, Region)>
) {
    use expr::Pattern::*;

    match pattern {
        Identifier(name) => { idents.remove(&(Ident::Unqualified(name))); },
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
    scope: &Scope,
    ident: Ident,
    references: &mut References
) -> Result<Symbol, Ident> {
    if scope.idents.contains_key(&ident) {
        let recognized = match ident {
            Ident::Unqualified(name) => {
                let symbol = scope.symbol(&name);

                references.locals.insert(symbol.clone());

                symbol
            }
            Ident::Qualified(path, name) => {
                let symbol = Symbol::new(&path, &name);

                references.globals.insert(symbol.clone());

                symbol
            }
        };

        Ok(recognized)
    } else {
        match ident {
            Ident::Unqualified(name) => {
                // Try again, this time using the current module as the path.
                let qualified = Ident::Qualified(env.home.clone(), name.clone());

                if scope.idents.contains_key(&qualified) {
                    let symbol = Symbol::new(&env.home, &name);

                    references.globals.insert(symbol.clone());

                    Ok(symbol)
                } else {
                    // We couldn't find the unqualified ident in scope. NAMING PROBLEM!
                    Err(Ident::Unqualified(name))
                }

            },
            qualified @ Ident::Qualified(_, _) => {
                // We couldn't find the qualified ident in scope. NAMING PROBLEM!
                Err(qualified)
            }
        }
    }
}

/// Translate a VariantName into a resolved symbol if it's found in env.declared_variants.
/// If it could not be found, return it unchanged as an Err.
#[inline(always)]
fn resolve_variant_name(
    env: &Env,
    scope: &Scope,
    variant_name: VariantName,
    references: &mut References
) -> Result<Symbol, VariantName> {
    let symbol = Symbol::from_variant(&variant_name, &env.home);

    if env.variants.contains_key(&symbol) {
        references.variants.insert(symbol.clone());

        Ok(symbol)
    } else {
        // We couldn't find the qualified variant name in scope. NAMING PROBLEM!
        Err(variant_name)
    }
}

/// Different patterns are supported in different circumstances.
/// For example, case branches can pattern match on number literals, but
/// assignments and function args can't. Underscore is supported in function
/// arg patterns and in case branch patterns, but not in assignments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PatternType {
    Assignment,
    FunctionArg,
    CaseBranch
}

fn canonicalize_pattern(
    env: &mut Env,
    scope: &mut Scope,
    pattern_type: &PatternType,
    loc_pattern: &Located<expr::Pattern>,
    shadowable_idents: &mut ImMap<Ident, (Symbol, Region)>,
) -> Pattern {
    use expr::Pattern::*;

    let region = loc_pattern.region;
    match &loc_pattern.value {
        &Identifier(ref name) => {
            let unqualified_ident = Ident::Unqualified(name.clone());

            // We use shadowable_idents for this, and not scope, because for assignments
            // they are different. When canonicalizing a particular assignment, that new
            // ident is in scope (for recursion) but not shadowable.
            //
            // For example, when canonicalizing (fibonacci = ...), `fibonacci` should be in scope
            // so that it can refer to itself without getting a naming problem, but it should not
            // be in the collection of shadowable idents because you can't shadow yourself!
            match shadowable_idents.get(&unqualified_ident) {
                Some((_, region)) => {
                    let loc_shadowed_ident = Located {region: region.clone(), value: unqualified_ident};

                    // This is already in scope, meaning it's about to be shadowed.
                    // Shadowing is not allowed!
                    env.problem(Problem::Shadowing(loc_shadowed_ident.clone()));

                    // Change this Pattern to a Shadowed variant, so that
                    // codegen knows to generate a runtime exception here.
                    Pattern::Shadowed(loc_shadowed_ident)
                },
                None => {
                    // Make sure we aren't shadowing something in the home module's scope.
                    let qualified_ident = Ident::Qualified(env.home.clone(), unqualified_ident.name());

                    match scope.idents.get(&qualified_ident) {
                        Some((_, region)) => {
                            let loc_shadowed_ident = Located {region: region.clone(), value: qualified_ident};

                            // This is already in scope, meaning it's about to be shadowed.
                            // Shadowing is not allowed!
                            env.problem(Problem::Shadowing(loc_shadowed_ident.clone()));

                            // Change this Pattern to a Shadowed variant, so that
                            // codegen knows to generate a runtime exception here.
                            Pattern::Shadowed(loc_shadowed_ident)
                        },
                        None => {
                            let new_ident = qualified_ident.clone();
                            let new_name = qualified_ident.name();
                            let symbol = scope.symbol(&new_name);

                            // This is a fresh identifier that wasn't already in scope.
                            // Add it to scope!
                            let symbol_and_region = (symbol.clone(), region);

                            // Add this to both scope.idents *and* shadowable_idents.
                            // The latter is relevant when recursively canonicalizing Variant patterns,
                            // which can bring multiple new idents into scope. For example, it's important
                            // that we catch (Blah foo foo) as being an example of shadowing.
                            scope.idents.insert(new_ident.clone(), symbol_and_region.clone());
                            shadowable_idents.insert(new_ident, symbol_and_region);

                            Pattern::Identifier(symbol)
                        }
                    }
                }
            }
        },

        &Variant(ref loc_name, ref opt_args) => {
            // Canonicalize the variant's arguments, if it has any.
            let opt_can_args: Option<Vec<Pattern>> = match opt_args {
                None => None,
                Some(loc_args) => {
                    let mut can_args:Vec<Pattern> = Vec::new();

                    for loc_arg in loc_args {
                        let can_arg = canonicalize_pattern(env, scope, pattern_type, &loc_arg, shadowable_idents);

                        can_args.push(can_arg);
                    }

                    Some(can_args)
                }
            };

            // Canonicalize the variant's name.
            let symbol = Symbol::from_variant(&loc_name.value, &env.home);

            if env.variants.contains_key(&symbol) {
                // No problems; the qualified variant name was in scope!
                Pattern::Variant(symbol, opt_can_args)
            } else {
                // We couldn't find the variant name in scope. NAMING PROBLEM!
                env.problem(Problem::UnrecognizedVariant(loc_name.clone()));

                Pattern::UnrecognizedVariant(loc_name.clone())
            }
        },

        &Integer(ref num) => {
            match pattern_type {
                CaseBranch => Pattern::Integer(*num),
                ptype @ Assignment | ptype @ FunctionArg => unsupported_pattern(env, *ptype, &region, &loc_pattern.value)
            }
        },

        &Fraction(ref numerator, ref denominator) => {
            match pattern_type {
                CaseBranch => Pattern::Fraction(*numerator, *denominator),
                ptype @ Assignment | ptype @ FunctionArg => unsupported_pattern(env, *ptype, &region, &loc_pattern.value)
            }
        },

        &ExactString(ref string) => {
            match pattern_type {
                CaseBranch => Pattern::ExactString(string.clone()),
                ptype @ Assignment | ptype @ FunctionArg => unsupported_pattern(env, *ptype, &region, &loc_pattern.value)
            }
        },

        &Underscore => {
            match pattern_type {
                CaseBranch | FunctionArg => Pattern::Underscore,
                Assignment => unsupported_pattern(env, Assignment, &region, &loc_pattern.value)
            }
        },

        &EmptyRecordLiteral => Pattern::EmptyRecordLiteral,
    }
}

/// When we detect an unsupported pattern type (e.g. 5 = 1 + 2 is unsupported because you can't
/// assign to Int patterns), report it to Env and return an UnsupportedPattern runtime error pattern.
fn unsupported_pattern(env: &mut Env, pattern_type: PatternType, region: &Region, pattern: &expr::Pattern) -> Pattern {
    let loc_problem_pattern = Located {region: region.clone(), value: pattern.clone()};

    env.problem(Problem::UnsupportedPattern(pattern_type, loc_problem_pattern.clone()));

    Pattern::UnsupportedPattern(loc_problem_pattern)
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