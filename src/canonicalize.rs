use region::{Located, Region};
use operator::Operator;
use operator::Operator::Pizza;
use operator::Associativity::*;
use collections::{ImSet, ImMap, MutMap, MutSet};
use std::cmp::Ordering;
use expr::{Ident, VariantName};
use expr;
use graph::{topological_sort, strongly_connected_component};
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
    FunctionPointer(Symbol), // TODO do we actually need this, or is it completely redundant with Var?
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
    CircularAssignment(Vec<Located<expr::Ident>>, Vec<(Pattern, Located<Expr>)>, Box<Located<Expr>>),
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
    UnsupportedPattern(PatternType, Located<expr::Pattern>),
    CircularAssignment(Vec<Located<expr::Ident>>),
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
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

#[derive(Clone, Debug, PartialEq)]
pub struct Procedure {
    pub name: Option<String>,
    pub is_self_tail_recursive: bool,
    pub definition: Region,
    pub args: Vec<Pattern>,
    pub body: Expr,
    pub references: References,
}

impl Procedure {
    pub fn new(definition: Region, args: Vec<Pattern>, body: Expr, references: References) -> Procedure {
        Procedure {
            name: None,
            is_self_tail_recursive: false,
            definition,
            args,
            body,
            references,
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
        args: Vec<Pattern>,
        body: Expr,
        definition: Region,
        references: References
    ) -> () {
        // We can't if the closure is self tail recursive yet, because it doesn't know its final name yet.
        // (Assign sets that.) Assume this is false, and let Assign change it to true after it sets final name.
        let is_self_tail_recursive = false;
        let name = None; // The Assign logic is also responsible for setting names after the fact.
        let procedure = Procedure {args, name, body, is_self_tail_recursive, definition, references};

        self.procedures.insert(symbol, procedure);
    }
}

pub fn canonicalize_declaration(
    home: String,
    name: &str,
    loc_expr: Located<expr::Expr>,
    declared_idents: &ImMap<Ident, (Symbol, Region)>,
    declared_variants: &ImMap<Symbol, Located<expr::VariantName>>,
) -> (Located<Expr>, Output, Vec<Problem>, MutMap<Symbol, Procedure>) {
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

    (new_loc_expr, output, env.problems, env.procedures)
}

#[derive(Clone, Debug, PartialEq)]
pub struct Output {
    pub references: References,
    pub tail_call: Option<Symbol>,
}

/// These are all ordered sets because they end up getting traversed in a graph search
/// to determine how assignments shuold be ordered. We want builds to be reproducible,
/// so it's important that building the same code gives the same order every time!
#[derive(Clone, Debug, PartialEq)]
pub struct References {
    pub locals: ImSet<Symbol>,
    pub globals: ImSet<Symbol>,
    pub variants: ImSet<Symbol>,
    pub calls: ImSet<Symbol>,
}

impl References {
    pub fn new() -> References {
        References {
            locals: ImSet::default(),
            globals: ImSet::default(),
            variants: ImSet::default(),
            calls: ImSet::default(),
        }
    }

    pub fn union(mut self, other: References) -> Self {
        self.locals = self.locals.union(other.locals);
        self.globals = self.globals.union(other.globals);
        self.variants = self.variants.union(other.variants);
        self.calls = self.calls.union(other.calls);

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

            match &fn_expr.value {
                &Var(ref sym) => {
                    output.references.calls.insert(sym.clone());
                },
                _ => ()
            };

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
                        &Apply(ref loc_boxed_expr, _) => {
                            match (*loc_boxed_expr.clone()).value {
                                Var(sym) => Some(sym),
                                _ => None
                            }
                        }
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
                        let loc_ident = Located {region: loc_expr.region.clone(), value: ident};

                        env.problem(Problem::UnrecognizedConstant(loc_ident.clone()));

                        UnrecognizedConstant(loc_ident)
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
                match resolve_variant_name(&env, variant_name, &mut output.references) {
                    Ok(symbol) => ApplyVariant(symbol, opt_can_args),
                    Err(variant_name) => {
                        let loc_variant = Located {region: loc_expr.region.clone(), value: variant_name};

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
            let assigned_idents: Vec<(Ident, (Symbol, Region))> =
                idents_from_patterns(assignments.clone().iter().map(|(loc_pattern, _)| loc_pattern), &scope);

            scope.idents = union_pairs(scope.idents, assigned_idents.iter());

            let mut refs_by_assignment: MutMap<Symbol, (Located<Ident>, References)> = MutMap::default();
            let mut can_assignments_by_symbol: MutMap<Symbol, (Pattern, Located<Expr>)> = MutMap::default();

            for (loc_pattern, expr) in assignments {
                // Each assignment gets to have all the idents in scope that are assigned in this
                // block. Order of assignments doesn't matter, thanks to referential transparency!
                let (loc_can_expr, can_output) = canonicalize(env, &mut scope, expr);

                // Exclude the current ident from shadowable_idents; you can't shadow yourself!
                // (However, still include it in scope, because you *can* recursively refer to yourself.)
                let mut shadowable_idents = scope.idents.clone();
                remove_idents(loc_pattern.value.clone(), &mut shadowable_idents);

                let can_pattern = canonicalize_pattern(env, &mut scope, &Assignment, &loc_pattern, &mut shadowable_idents);
                let mut assigned_symbols = Vec::new();

                // Store the referenced locals in the refs_by_assignment map, so we can later figure out
                // which assigned names reference each other.
                for (ident, (symbol, region)) in idents_from_patterns(std::iter::once(&loc_pattern), &scope) {
                    let refs = can_output.references.clone();

                    refs_by_assignment.insert(symbol.clone(), (Located {value: ident, region}, refs));

                    assigned_symbols.push(symbol.clone());
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

                                // Recursion doesn't count as referencing. (If it did, all recursive functions
                                // would result in circular assignment errors!)
                                refs_by_assignment
                                    .entry(assigned_symbol.clone())
                                    .and_modify(|(_, refs)| {
                                        refs.locals = refs.locals.without(assigned_symbol);
                                    });

                                // Return a pointer to the assigned symbol, since the auto-generated one no
                                // longer references any entry in the procedure map!
                                FunctionPointer(assigned_symbol.clone())
                            },
                            non_closure => non_closure
                        }
                    },
                    _ => loc_can_expr.value
                };

                for symbol in assigned_symbols {
                    can_assignments_by_symbol.insert(
                        symbol,
                        (can_pattern.clone(), Located {region: loc_can_expr.region.clone(), value: can_expr.clone()})
                    );
                }
            }

            // The assignment as a whole is a tail call iff its return expression is a tail call.
            // Use its output as a starting point because its tail_call already has the right answer!
            let (ret_expr, mut output) = canonicalize(env, &mut scope, *box_loc_returned);

            // Determine the full set of references by traversing the graph.
            let mut visited_symbols = MutSet::default();

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
                let refs = references_from_local(symbol, &mut visited_symbols, &refs_by_assignment, &env.procedures);

                output.references = output.references.union(refs);
            }

            for symbol in output.references.calls.clone().into_iter() {
                // Traverse the graph and look up *all* the references for this call.
                // Reuse the same visited_symbols as before; if we already visited it, we
                // won't learn anything new from visiting it again!
                let refs = references_from_call(symbol, &mut visited_symbols, &refs_by_assignment, &env.procedures);

                output.references = output.references.union(refs);
            }

            // Now that we've collected all the references, check to see if any of the new idents
            // we defined went unused by the return expression. If any were unused, report it.
            for (ident, (symbol, region)) in assigned_idents.clone() {
                if !output.references.has_local(&symbol) {
                    let loc_ident = Located {region: region.clone(), value: ident.clone()};

                    env.problem(Problem::UnusedAssignment(loc_ident));
                }
            }

            // Use topological sort to reorder the assignments based on their dependencies to one another.
            // This way, during code gen, no assignment will refer to a value that hasn't been initialized yet.
            // As a bonus, the topological sort also reveals any cycles between the assignments, allowing
            // us to give a CircularAssignment error.
            let successors = |symbol: &Symbol| -> ImSet<Symbol> {
                let (_, references) = refs_by_assignment.get(symbol).unwrap();

                local_successors(&references, &env.procedures)
            };

            let assigned_symbols: Vec<Symbol> =
                can_assignments_by_symbol.keys().into_iter().map(Symbol::clone).collect();

            match topological_sort(assigned_symbols.as_slice(), successors) {
                Ok(sorted_symbols) => {
                    let can_assignments =
                        sorted_symbols
                            .into_iter()
                            .rev() // Topological sort gives us the reverse of the sorting we want!
                            .map(|symbol| can_assignments_by_symbol.get(&symbol).unwrap().clone())
                            .collect();

                    (Assign(can_assignments, Box::new(ret_expr)), output)
                },
                Err(node_in_cycle) => {
                    // We have one node we know is in the cycle.
                    // We want to show the entire cycle in the error message, so expand it out.
                    let mut loc_idents_in_cycle: Vec<Located<expr::Ident>> =
                        strongly_connected_component(&node_in_cycle, successors)
                            .into_iter()
                            .rev() // Strongly connected component gives us the reverse of the sorting we want!
                            .map(|symbol| refs_by_assignment.get(&symbol).unwrap().0.clone())
                            .collect();

                    loc_idents_in_cycle = sort_cyclic_idents(loc_idents_in_cycle, &mut assigned_idents.iter().map(|(ident, _)| ident));

                    env.problem(Problem::CircularAssignment(loc_idents_in_cycle.clone()));

                    let can_assignments = can_assignments_by_symbol.values().map(|tuple| tuple.clone()).collect();

                    (CircularAssignment(loc_idents_in_cycle, can_assignments, Box::new(ret_expr)), output)
                }
            }
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

            let arg_idents: Vec<(Ident, (Symbol, Region))> =
                idents_from_patterns(loc_arg_patterns.iter(), &scope);

            // Add the arguments' idents to scope.idents. If there's a collision,
            // it means there was shadowing, which will be handled later.
            scope.idents = union_pairs(scope.idents, arg_idents.iter());

            let can_args: Vec<Pattern> = loc_arg_patterns.into_iter().map(|loc_pattern| {
                // Exclude the current ident from shadowable_idents; you can't shadow yourself!
                // (However, still include it in scope, because you *can* recursively refer to yourself.)
                let mut shadowable_idents = scope.idents.clone();
                remove_idents(loc_pattern.value.clone(), &mut shadowable_idents);

                canonicalize_pattern(env, &mut scope, &FunctionArg, &loc_pattern, &mut shadowable_idents)
            }).collect();
            let (loc_body_expr, mut output) = canonicalize(env, &mut scope, *box_loc_body_expr);

            // Now that we've collected all the references, check to see if any of the args we defined
            // went unreferenced. If any did, report them as unused arguments.
            for (ident, (arg_symbol, region)) in arg_idents {
                if !output.references.has_local(&arg_symbol) {
                    // The body never referenced this argument we declared. It's an unused argument!
                    env.problem(Problem::UnusedArgument(Located {region, value: ident}));
                }

                // We shouldn't ultimately count arguments as referenced locals. Otherwise,
                // we end up with weird conclusions like the expression (\x -> x + 1)
                // references the (nonexistant) local variable x!
                output.references.locals.remove(&arg_symbol);
            }

            // We've finished analyzing the closure. Its references.locals are now the values it closes over,
            // since we removed the only locals it shouldn't close over (its arguments).
            // Register it as a top-level procedure in the Env!
            env.register_closure(symbol.clone(), can_args, loc_body_expr.value, loc_expr.region.clone(), output.references);

            // Having now registered the closure's references, the function pointer that remains has
            // no references. The references we registered will be used only if this symbol gets called!
            output.references = References::new();

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
                let assigned_idents: Vec<(Ident, (Symbol, Region))> =
                    idents_from_patterns(std::iter::once(&loc_pattern), &scope);

                scope.idents = union_pairs(scope.idents, assigned_idents.iter());

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
    (Located {region: loc_expr.region.clone(), value: expr}, output)
}

fn union_pairs<'a, K, V, I>(mut map: ImMap<K, V>, pairs: I) -> ImMap<K, V>
where I: Iterator<Item=&'a (K, V)>,
    K: std::hash::Hash + Eq + Clone,
    K: 'a,
    V: Clone,
    V: 'a
{
    for (ref k, ref v) in pairs {
        map.insert(k.clone(), v.clone());
    }

    map
}

fn local_successors(
    references: &References,
    procedures: &MutMap<Symbol, Procedure>,
) -> ImSet<Symbol> {
    let mut answer = references.locals.clone();

    for call_symbol in references.calls.iter() {
        answer = answer.union(call_successors(call_symbol, procedures));
    }

    answer
}

fn call_successors(
    call_symbol: &Symbol,
    procedures: &MutMap<Symbol, Procedure>,
) -> ImSet<Symbol> {
    // TODO (this comment should be moved to a GH issue) this may cause an infinite loop if 2 procedures reference each other; may need to track visited procedures!
    match procedures.get(call_symbol) {
        Some(procedure) => {
            let mut answer = local_successors(&procedure.references, procedures);

            answer.insert(call_symbol.clone());

            answer
        },
        None => ImSet::default()
    }
}

fn references_from_local<T>(
    assigned_symbol: Symbol,
    visited: &mut MutSet<Symbol>,
    refs_by_assignment: &MutMap<Symbol, (T, References)>,
    procedures: &MutMap<Symbol, Procedure>,
) -> References {
    match refs_by_assignment.get(&assigned_symbol) {
        Some((_, refs)) => {
            let mut answer = References::new();

            visited.insert(assigned_symbol);

            for local in refs.locals.iter() {
                if !visited.contains(&local) {
                    let other_refs = references_from_local(local.clone(), visited, refs_by_assignment, procedures);

                    answer = answer.union(other_refs);
                }

                answer.locals.insert(local.clone());
            }

            for call in refs.calls.iter() {
                if !visited.contains(&call) {
                    let other_refs = references_from_call(call.clone(), visited, refs_by_assignment, procedures);

                    answer = answer.union(other_refs);
                }

                answer.calls.insert(call.clone());
            }

            answer
        },
        None => {
            // This should never happen! If the local was not recognized, it should not have been
            // added to the local references.
            unreachable!();
        }
    }
}

/// When we get a list of cyclic idents, the first node listed is a matter of chance.
/// This reorders the list such that the first node listed is always alphabetically the lowest,
/// while preserving the overall order of the cycle.
///
/// Example: the cycle  (c ---> a ---> b)  becomes  (a ---> b ---> c)
pub fn sort_cyclic_idents<'a, I>(loc_idents: Vec<Located<Ident>>, ordered_idents: &mut I) -> Vec<Located<Ident>>
where I: Iterator<Item=&'a Ident>
{
    // Find the first ident in ordered_idents that also appears in loc_idents.
    let first_ident = ordered_idents.find(|ident| loc_idents.iter().any(|loc_ident| &&loc_ident.value == ident)).unwrap();

    let mut answer = Vec::with_capacity(loc_idents.len());
    let mut end = Vec::with_capacity(loc_idents.len());
    let mut encountered_first_ident = false;

    for loc_ident in loc_idents {
        if encountered_first_ident {
            answer.push(loc_ident);
        } else if &loc_ident.value == first_ident {
            encountered_first_ident = true;

            answer.push(loc_ident);
        } else {
            end.push(loc_ident);
        }
    }

    // Add the contents of `end` to the end of the answer.
    answer.extend_from_slice(end.as_slice());

    answer
}

fn references_from_call<T>(
    call_symbol: Symbol,
    visited: &mut MutSet<Symbol>,
    refs_by_assignment: &MutMap<Symbol, (T, References)>,
    procedures: &MutMap<Symbol, Procedure>,
) -> References {
    match procedures.get(&call_symbol) {
        Some(procedure) => {
            let mut answer = procedure.references.clone();

            visited.insert(call_symbol);

            for closed_over_local in procedure.references.locals.iter() {
                if !visited.contains(&closed_over_local) {
                    let other_refs = references_from_local(closed_over_local.clone(), visited, refs_by_assignment, procedures);

                    answer = answer.union(other_refs);
                }

                answer.locals.insert(closed_over_local.clone());
            }

            for call in procedure.references.calls.iter() {
                if !visited.contains(&call) {
                    let other_refs = references_from_call(call.clone(), visited, refs_by_assignment, procedures);

                    answer = answer.union(other_refs);
                }

                answer.calls.insert(call.clone());
            }

            answer
        },
        None => {
            // If the call symbol was not in the procedures map, that means we're calling a non-function and
            // will get a type mismatch later. For now, assume no references as a result of the "call."
            References::new()
        }
    }
}


fn idents_from_patterns<'a, I>(loc_patterns: I, scope: &Scope) -> Vec<(Ident, (Symbol, Region))>
where I: Iterator<Item = &'a Located<expr::Pattern>>
{
    let mut answer = Vec::new();

    for loc_pattern in loc_patterns {
        add_idents_from_pattern(loc_pattern, scope, &mut answer);
    }

    answer
}

/// helper function for idents_from_patterns
fn add_idents_from_pattern(
    loc_pattern: &Located<expr::Pattern>,
    scope: &Scope,
    answer: &mut Vec<(Ident, (Symbol, Region))>
) {
    use expr::Pattern::*;

    match &loc_pattern.value {
        &Identifier(ref name) => {
            let symbol = scope.symbol(&name);

            answer.push((Ident::Unqualified(name.clone()), (symbol, loc_pattern.region.clone())));
        },
        &Variant(_, ref opt_loc_args) => {
            match opt_loc_args {
                &None => (),
                &Some(ref loc_args) => {
                    for loc_arg in loc_args.iter() {
                        add_idents_from_pattern(loc_arg, scope, answer);
                    }
                }
            }
        },
        &Integer(_) | &Fraction(_, _) | &ExactString(_)
            | &EmptyRecordLiteral | &Underscore => ()
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
#[inline(always)] // This is shared code between Var and InterpolatedStr; it was inlined when handwritten
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

    let region = loc_pattern.region.clone();
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
// Thank you, Markus!
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
