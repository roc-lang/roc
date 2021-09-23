#![allow(clippy::all)]
#![allow(dead_code)]
#![allow(unused_imports)]
use bumpalo::{collections::Vec as BumpVec, Bump};
use std::collections::HashMap;
use std::iter::FromIterator;

use crate::lang::ast::{
    expr2_to_string, value_def_to_string, ClosureExtra, Def2, Expr2, ExprId, FloatVal, IntStyle,
    IntVal, RecordField, ValueDef, WhenBranch,
};
use crate::lang::def::{
    canonicalize_defs, sort_can_defs, CanDefs, Declaration, Def, PendingDef, References,
};
use crate::lang::pattern::{to_pattern2, Pattern2, PatternId};
use crate::lang::pool::{NodeId, Pool, PoolStr, PoolVec, ShallowClone};
use crate::lang::scope::Scope;
use crate::lang::types::{Alias, Annotation2, Type2, TypeId};

use roc_can::expr::Recursive;
use roc_can::num::{finish_parsing_base, finish_parsing_float, finish_parsing_int};
use roc_can::operator::desugar_expr;
use roc_collections::all::default_hasher;
use roc_collections::all::{MutMap, MutSet};
use roc_module::ident::{Ident, Lowercase, ModuleName};
use roc_module::low_level::LowLevel;
use roc_module::operator::CalledVia;
use roc_module::symbol::{IdentIds, ModuleId, ModuleIds, Symbol};
use roc_parse::ast;
use roc_parse::ast::Expr;
use roc_parse::ast::StrLiteral;
use roc_parse::parser::{loc, Parser, State, SyntaxError};
use roc_parse::pattern::PatternType;
use roc_problem::can::{Problem, RuntimeError};
use roc_region::all::{Located, Region};
use roc_types::subs::{VarStore, Variable};

#[derive(Clone, Debug, PartialEq, Default)]
pub struct IntroducedVariables {
    // Rigids must be unique within a type annoation.
    // E.g. in `identity : a -> a`, there should only be one
    // variable (a rigid one, with name "a").
    // Hence `rigids : Map<Lowercase, Variable>`
    //
    // But then between annotations, the same name can occur multiple times,
    // but a variable can only have one name. Therefore
    // `ftv : Map<Variable, Lowercase>`.
    pub wildcards: Vec<Variable>,
    pub var_by_name: MutMap<Lowercase, Variable>,
    pub name_by_var: MutMap<Variable, Lowercase>,
    pub host_exposed_aliases: MutMap<Symbol, Variable>,
}

impl IntroducedVariables {
    pub fn insert_named(&mut self, name: Lowercase, var: Variable) {
        self.var_by_name.insert(name.clone(), var);
        self.name_by_var.insert(var, name);
    }

    pub fn insert_wildcard(&mut self, var: Variable) {
        self.wildcards.push(var);
    }

    pub fn insert_host_exposed_alias(&mut self, symbol: Symbol, var: Variable) {
        self.host_exposed_aliases.insert(symbol, var);
    }

    pub fn union(&mut self, other: &Self) {
        self.wildcards.extend(other.wildcards.iter().cloned());
        self.var_by_name.extend(other.var_by_name.clone());
        self.name_by_var.extend(other.name_by_var.clone());
        self.host_exposed_aliases
            .extend(other.host_exposed_aliases.clone());
    }

    pub fn var_by_name(&self, name: &Lowercase) -> Option<&Variable> {
        self.var_by_name.get(name)
    }

    pub fn name_by_var(&self, var: Variable) -> Option<&Lowercase> {
        self.name_by_var.get(&var)
    }
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct Output {
    pub references: References,
    pub tail_call: Option<Symbol>,
    pub introduced_variables: IntroducedVariables,
    pub aliases: MutMap<Symbol, NodeId<Alias>>,
    pub non_closures: MutSet<Symbol>,
}

impl Output {
    pub fn union(&mut self, other: Self) {
        self.references.union_mut(other.references);

        if let (None, Some(later)) = (self.tail_call, other.tail_call) {
            self.tail_call = Some(later);
        }

        self.aliases.extend(other.aliases);
        self.non_closures.extend(other.non_closures);
    }
}

#[derive(Debug)]
pub struct Env<'a> {
    pub home: ModuleId,
    pub var_store: &'a mut VarStore,
    pub pool: &'a mut Pool,
    pub arena: &'a Bump,

    pub problems: BumpVec<'a, Problem>,

    pub dep_idents: MutMap<ModuleId, IdentIds>,
    pub module_ids: &'a ModuleIds,
    pub ident_ids: IdentIds,
    pub exposed_ident_ids: IdentIds,

    pub closures: MutMap<Symbol, References>,
    /// Symbols which were referenced by qualified lookups.
    pub qualified_lookups: MutSet<Symbol>,

    pub top_level_symbols: MutSet<Symbol>,

    pub closure_name_symbol: Option<Symbol>,
    pub tailcallable_symbol: Option<Symbol>,
}

impl<'a> Env<'a> {
    pub fn new(
        home: ModuleId,
        arena: &'a Bump,
        pool: &'a mut Pool,
        var_store: &'a mut VarStore,
        dep_idents: MutMap<ModuleId, IdentIds>,
        module_ids: &'a ModuleIds,
        exposed_ident_ids: IdentIds,
    ) -> Env<'a> {
        Env {
            home,
            arena,
            pool,
            problems: BumpVec::new_in(arena),
            var_store,
            dep_idents,
            module_ids,
            ident_ids: exposed_ident_ids.clone(), // we start with these, but will add more later
            exposed_ident_ids,
            closures: MutMap::default(),
            qualified_lookups: MutSet::default(),
            tailcallable_symbol: None,
            closure_name_symbol: None,
            top_level_symbols: MutSet::default(),
        }
    }

    pub fn add<T>(&mut self, item: T, region: Region) -> NodeId<T> {
        let id = self.pool.add(item);
        self.set_region(id, region);

        id
    }

    pub fn problem(&mut self, problem: Problem) {
        self.problems.push(problem);
    }

    pub fn set_region<T>(&mut self, _node_id: NodeId<T>, _region: Region) {
        dbg!("Don't Forget to set the region eventually");
    }

    pub fn register_closure(&mut self, symbol: Symbol, references: References) {
        self.closures.insert(symbol, references);
    }

    /// Generates a unique, new symbol like "$1" or "$5",
    /// using the home module as the module_id.
    ///
    /// This is used, for example, during canonicalization of an Expr::Closure
    /// to generate a unique symbol to refer to that closure.
    pub fn gen_unique_symbol(&mut self) -> Symbol {
        let ident_id = self.ident_ids.gen_unique();

        Symbol::new(self.home, ident_id)
    }

    /// Returns Err if the symbol resolved, but it was not exposed by the given module
    pub fn qualified_lookup(
        &mut self,
        module_name: &str,
        ident: &str,
        region: Region,
    ) -> Result<Symbol, RuntimeError> {
        debug_assert!(
            !module_name.is_empty(),
            "Called env.qualified_lookup with an unqualified ident: {:?}",
            ident
        );

        let module_name: ModuleName = module_name.into();

        match self.module_ids.get_id(&module_name) {
            Some(&module_id) => {
                let ident: Ident = ident.into();

                // You can do qualified lookups on your own module, e.g.
                // if I'm in the Foo module, I can do a `Foo.bar` lookup.
                if module_id == self.home {
                    match self.ident_ids.get_id(&ident) {
                        Some(ident_id) => {
                            let symbol = Symbol::new(module_id, *ident_id);

                            self.qualified_lookups.insert(symbol);

                            Ok(symbol)
                        }
                        None => Err(RuntimeError::LookupNotInScope(
                            Located {
                                value: ident,
                                region,
                            },
                            self.ident_ids
                                .idents()
                                .map(|(_, string)| string.as_ref().into())
                                .collect(),
                        )),
                    }
                } else {
                    match self
                        .dep_idents
                        .get(&module_id)
                        .and_then(|exposed_ids| exposed_ids.get_id(&ident))
                    {
                        Some(ident_id) => {
                            let symbol = Symbol::new(module_id, *ident_id);

                            self.qualified_lookups.insert(symbol);

                            Ok(symbol)
                        }
                        None => Err(RuntimeError::ValueNotExposed {
                            module_name: ModuleName::from(module_name),
                            ident,
                            region,
                        }),
                    }
                }
            }
            None => Err(RuntimeError::ModuleNotImported {
                module_name,
                imported_modules: self
                    .module_ids
                    .available_modules()
                    .map(|string| string.as_ref().into())
                    .collect(),
                region,
            }),
        }
    }
}

const ZERO: Region = Region::zero();

pub fn as_expr_id<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    expr_id: ExprId,
    parse_expr: &'a roc_parse::ast::Expr<'a>,
    region: Region,
) -> Output {
    let (expr, output) = to_expr2(env, scope, parse_expr, region);

    env.pool[expr_id] = expr;
    env.set_region(expr_id, region);

    output
}

pub fn to_expr_id<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    parse_expr: &'a roc_parse::ast::Expr<'a>,
    region: Region,
) -> (ExprId, Output) {
    let (expr, output) = to_expr2(env, scope, parse_expr, region);

    (env.add(expr, region), output)
}

pub fn str_to_def2<'a>(
    arena: &'a Bump,
    input: &'a str,
    env: &mut Env<'a>,
    scope: &mut Scope,
    region: Region,
) -> Result<Vec<Def2>, SyntaxError<'a>> {
    match roc_parse::test_helpers::parse_defs_with(arena, input.trim()) {
        Ok(vec_loc_def) => Ok(defs_to_defs2(
            arena,
            env,
            scope,
            arena.alloc(vec_loc_def),
            region,
        )),
        Err(fail) => Err(fail),
    }
}

pub fn str_to_expr2<'a>(
    arena: &'a Bump,
    input: &'a str,
    env: &mut Env<'a>,
    scope: &mut Scope,
    region: Region,
) -> Result<(Expr2, self::Output), SyntaxError<'a>> {
    match roc_parse::test_helpers::parse_loc_with(arena, input.trim()) {
        Ok(loc_expr) => Ok(loc_expr_to_expr2(arena, loc_expr, env, scope, region)),
        Err(fail) => Err(fail),
    }
}

fn loc_expr_to_expr2<'a>(
    arena: &'a Bump,
    loc_expr: Located<Expr<'a>>,
    env: &mut Env<'a>,
    scope: &mut Scope,
    region: Region,
) -> (Expr2, self::Output) {
    let desugared_loc_expr = desugar_expr(arena, arena.alloc(loc_expr));

    to_expr2(env, scope, arena.alloc(desugared_loc_expr.value), region)
}

pub fn to_expr2<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    parse_expr: &'a roc_parse::ast::Expr<'a>,
    region: Region,
) -> (Expr2, self::Output) {
    use roc_parse::ast::Expr::*;

    match parse_expr {
        Float(string) => {
            match finish_parsing_float(string) {
                Ok(float) => {
                    let expr = Expr2::Float {
                        number: FloatVal::F64(float),
                        var: env.var_store.fresh(),
                        text: PoolStr::new(string, &mut env.pool),
                    };

                    (expr, Output::default())
                }
                Err((raw, error)) => {
                    // emit runtime error
                    let runtime_error = RuntimeError::InvalidFloat(error, ZERO, raw.into());

                    env.problem(Problem::RuntimeError(runtime_error.clone()));
                    //
                    //                    Expr::RuntimeError(runtime_error)
                    todo!()
                }
            }
        }
        Num(string) => {
            match finish_parsing_int(string) {
                Ok(int) => {
                    let expr = Expr2::SmallInt {
                        number: IntVal::I64(int),
                        var: env.var_store.fresh(),
                        // TODO non-hardcode
                        style: IntStyle::Decimal,
                        text: PoolStr::new(string, &mut env.pool),
                    };

                    (expr, Output::default())
                }
                Err((raw, error)) => {
                    // emit runtime error
                    let runtime_error = RuntimeError::InvalidInt(
                        error,
                        roc_parse::ast::Base::Decimal,
                        ZERO,
                        raw.into(),
                    );

                    env.problem(Problem::RuntimeError(runtime_error.clone()));
                    //
                    //                    Expr::RuntimeError(runtime_error)
                    todo!()
                }
            }
        }
        NonBase10Int {
            string,
            base,
            is_negative,
        } => {
            match finish_parsing_base(string, *base, *is_negative) {
                Ok(int) => {
                    let expr = Expr2::SmallInt {
                        number: IntVal::I64(int),
                        var: env.var_store.fresh(),
                        // TODO non-hardcode
                        style: IntStyle::from_base(*base),
                        text: PoolStr::new(string, &mut env.pool),
                    };

                    (expr, Output::default())
                }
                Err((raw, error)) => {
                    // emit runtime error
                    let runtime_error = RuntimeError::InvalidInt(error, *base, ZERO, raw.into());

                    env.problem(Problem::RuntimeError(runtime_error.clone()));
                    //
                    //                    Expr::RuntimeError(runtime_error)
                    todo!()
                }
            }
        }

        Str(literal) => flatten_str_literal(env, scope, &literal),

        List { items, .. } => {
            let mut output = Output::default();
            let output_ref = &mut output;

            let elems: PoolVec<ExprId> = PoolVec::with_capacity(items.len() as u32, env.pool);

            for (node_id, item) in elems.iter_node_ids().zip(items.iter()) {
                let (expr, sub_output) = to_expr2(env, scope, &item.value, item.region);

                output_ref.union(sub_output);

                let expr_id = env.pool.add(expr);
                env.pool[node_id] = expr_id;
            }

            let expr = Expr2::List {
                elem_var: env.var_store.fresh(),
                elems,
            };

            (expr, output)
        }

        GlobalTag(tag) => {
            // a global tag without any arguments
            (
                Expr2::GlobalTag {
                    name: PoolStr::new(tag, env.pool),
                    variant_var: env.var_store.fresh(),
                    ext_var: env.var_store.fresh(),
                    arguments: PoolVec::empty(env.pool),
                },
                Output::default(),
            )
        }
        PrivateTag(name) => {
            // a private tag without any arguments
            let ident_id = env.ident_ids.get_or_insert(&(*name).into());
            let name = Symbol::new(env.home, ident_id);
            (
                Expr2::PrivateTag {
                    name,
                    variant_var: env.var_store.fresh(),
                    ext_var: env.var_store.fresh(),
                    arguments: PoolVec::empty(env.pool),
                },
                Output::default(),
            )
        }

        RecordUpdate {
            fields,
            update: loc_update,
            final_comments: _,
        } => {
            let (can_update, update_out) =
                to_expr2(env, scope, &loc_update.value, loc_update.region);

            if let Expr2::Var(symbol) = &can_update {
                match canonicalize_fields(env, scope, fields) {
                    Ok((can_fields, mut output)) => {
                        output.references.union_mut(update_out.references);

                        let answer = Expr2::Update {
                            record_var: env.var_store.fresh(),
                            ext_var: env.var_store.fresh(),
                            symbol: *symbol,
                            updates: can_fields,
                        };

                        (answer, output)
                    }
                    Err(CanonicalizeRecordProblem::InvalidOptionalValue {
                        field_name: _,
                        field_region: _,
                        record_region: _,
                    }) => {
                        //                        let runtime_error = roc_problem::can::RuntimeError::InvalidOptionalValue {
                        //                            field_name,
                        //                            field_region,
                        //                            record_region,
                        //                        };
                        //
                        //                        env.problem(Problem::RuntimeError(runtime_error));

                        todo!()
                    }
                }
            } else {
                // only (optionally qualified) variables can be updated, not arbitrary expressions

                //                let error = roc_problem::can::RuntimeError::InvalidRecordUpdate {
                //                    region: can_update.region,
                //                };
                //
                //                let answer = Expr::RuntimeError(error.clone());
                //
                //                env.problems.push(Problem::RuntimeError(error));
                //
                //                (answer, Output::default())
                todo!()
            }
        }

        Record {
            fields,
            final_comments: _,
        } => {
            if fields.is_empty() {
                (Expr2::EmptyRecord, Output::default())
            } else {
                match canonicalize_fields(env, scope, fields) {
                    Ok((can_fields, output)) => (
                        Expr2::Record {
                            record_var: env.var_store.fresh(),
                            fields: can_fields,
                        },
                        output,
                    ),
                    Err(CanonicalizeRecordProblem::InvalidOptionalValue {
                        field_name: _,
                        field_region: _,
                        record_region: _,
                    }) => {
                        //                        let runtime_error = RuntimeError::InvalidOptionalValue {
                        //                            field_name,
                        //                            field_region,
                        //                            record_region,
                        //                        };
                        //
                        //                        env.problem(runtime_error);
                        //                        (
                        //                            Expr::RuntimeError(
                        //                            ),
                        //                            Output::default(),
                        //
                        //                        )
                        todo!()
                    }
                }
            }
        }

        Access(record_expr, field) => {
            // TODO
            let region = ZERO;
            let (record_expr_id, output) = to_expr_id(env, scope, record_expr, region);

            (
                Expr2::Access {
                    record_var: env.var_store.fresh(),
                    field_var: env.var_store.fresh(),
                    ext_var: env.var_store.fresh(),
                    expr: record_expr_id,
                    field: PoolStr::new(field, env.pool),
                },
                output,
            )
        }

        AccessorFunction(field) => (
            Expr2::Accessor {
                function_var: env.var_store.fresh(),
                record_var: env.var_store.fresh(),
                ext_var: env.var_store.fresh(),
                closure_var: env.var_store.fresh(),
                field_var: env.var_store.fresh(),
                field: PoolStr::new(field, env.pool),
            },
            Output::default(),
        ),

        If(branches, final_else) => {
            let mut new_branches = Vec::with_capacity(branches.len());
            let mut output = Output::default();

            for (condition, then_branch) in branches.iter() {
                let (cond, cond_output) = to_expr2(env, scope, &condition.value, condition.region);

                let (then_expr, then_output) =
                    to_expr2(env, scope, &then_branch.value, then_branch.region);

                output.references.union_mut(cond_output.references);
                output.references.union_mut(then_output.references);

                new_branches.push((env.pool.add(cond), env.pool.add(then_expr)));
            }

            let (else_expr, else_output) =
                to_expr2(env, scope, &final_else.value, final_else.region);

            output.references.union_mut(else_output.references);

            let expr = Expr2::If {
                cond_var: env.var_store.fresh(),
                expr_var: env.var_store.fresh(),
                branches: PoolVec::new(new_branches.into_iter(), env.pool),
                final_else: env.pool.add(else_expr),
            };

            (expr, output)
        }

        When(loc_cond, branches) => {
            // Infer the condition expression's type.
            let cond_var = env.var_store.fresh();
            let (can_cond, mut output) = to_expr2(env, scope, &loc_cond.value, loc_cond.region);

            // the condition can never be a tail-call
            output.tail_call = None;

            let can_branches = PoolVec::with_capacity(branches.len() as u32, env.pool);

            for (node_id, branch) in can_branches.iter_node_ids().zip(branches.iter()) {
                let (can_when_branch, branch_references) =
                    canonicalize_when_branch(env, scope, *branch, &mut output);

                output.references.union_mut(branch_references);

                env.pool[node_id] = can_when_branch;
            }

            // A "when" with no branches is a runtime error, but it will mess things up
            // if code gen mistakenly thinks this is a tail call just because its condition
            // happened to be one. (The condition gave us our initial output value.)
            if branches.is_empty() {
                output.tail_call = None;
            }

            // Incorporate all three expressions into a combined Output value.
            let expr = Expr2::When {
                expr_var: env.var_store.fresh(),
                cond_var,
                cond: env.pool.add(can_cond),
                branches: can_branches,
            };

            (expr, output)
        }

        Closure(loc_arg_patterns, loc_body_expr) => {
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
            let mut scope = original_scope.shallow_clone();
            let can_args = PoolVec::with_capacity(loc_arg_patterns.len() as u32, env.pool);
            let mut output = Output::default();

            let mut bound_by_argument_patterns = MutSet::default();

            for (node_id, loc_pattern) in can_args.iter_node_ids().zip(loc_arg_patterns.iter()) {
                let (new_output, can_arg) = to_pattern2(
                    env,
                    &mut scope,
                    roc_parse::pattern::PatternType::FunctionArg,
                    &loc_pattern.value,
                    loc_pattern.region,
                );

                bound_by_argument_patterns
                    .extend(new_output.references.bound_symbols.iter().copied());

                output.union(new_output);

                let pattern_id = env.add(can_arg, loc_pattern.region);
                env.pool[node_id] = (env.var_store.fresh(), pattern_id);
            }

            let (body_expr, new_output) =
                to_expr2(env, &mut scope, &loc_body_expr.value, loc_body_expr.region);

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
                if !original_scope.contains_symbol(sub_symbol) {
                    if !output.references.has_lookup(sub_symbol) {
                        // The body never referenced this argument we declared. It's an unused argument!
                        env.problem(Problem::UnusedArgument(symbol, sub_symbol, region));
                    }

                    // We shouldn't ultimately count arguments as referenced locals. Otherwise,
                    // we end up with weird conclusions like the expression (\x -> x + 1)
                    // references the (nonexistant) local variable x!
                    output.references.lookups.remove(&sub_symbol);
                }
            }

            env.register_closure(symbol, output.references.clone());

            let mut captured_symbols: Vec<_> = captured_symbols
                .into_iter()
                .map(|s| (s, env.var_store.fresh()))
                .collect();

            // sort symbols, so we know the order in which they're stored in the closure record
            captured_symbols.sort();

            // store that this function doesn't capture anything. It will be promoted to a
            // top-level function, and does not need to be captured by other surrounding functions.
            if captured_symbols.is_empty() {
                output.non_closures.insert(symbol);
            }

            let captured_symbols = PoolVec::new(captured_symbols.into_iter(), env.pool);

            let extra = ClosureExtra {
                return_type: env.var_store.fresh(),     // 4B
                captured_symbols,                       // 8B
                closure_type: env.var_store.fresh(),    // 4B
                closure_ext_var: env.var_store.fresh(), // 4B
            };

            (
                Expr2::Closure {
                    function_type: env.var_store.fresh(),
                    name: symbol,
                    recursive: Recursive::NotRecursive,
                    args: can_args,
                    body: env.add(body_expr, loc_body_expr.region),
                    extra: env.pool.add(extra),
                },
                output,
            )
        }

        Apply(loc_fn, loc_args, application_style) => {
            // The expression that evaluates to the function being called, e.g. `foo` in
            // (foo) bar baz
            let fn_region = loc_fn.region;

            // Canonicalize the function expression and its arguments
            let (fn_expr, mut output) = to_expr2(env, scope, &loc_fn.value, fn_region);

            // The function's return type
            let args = PoolVec::with_capacity(loc_args.len() as u32, env.pool);

            for (node_id, loc_arg) in args.iter_node_ids().zip(loc_args.iter()) {
                let (arg_expr_id, arg_out) = to_expr_id(env, scope, &loc_arg.value, loc_arg.region);

                env.pool[node_id] = (env.var_store.fresh(), arg_expr_id);

                output.references.union_mut(arg_out.references);
            }

            // Default: We're not tail-calling a symbol (by name), we're tail-calling a function value.
            output.tail_call = None;

            let expr = match fn_expr {
                Expr2::Var(ref symbol) => {
                    output.references.calls.insert(*symbol);

                    // we're tail-calling a symbol by name, check if it's the tail-callable symbol
                    output.tail_call = match &env.tailcallable_symbol {
                        Some(tc_sym) if *tc_sym == *symbol => Some(*symbol),
                        Some(_) | None => None,
                    };

                    // IDEA: Expr2::CallByName?
                    let fn_expr_id = env.add(fn_expr, fn_region);
                    Expr2::Call {
                        args,
                        expr: fn_expr_id,
                        expr_var: env.var_store.fresh(),
                        fn_var: env.var_store.fresh(),
                        closure_var: env.var_store.fresh(),
                        called_via: *application_style,
                    }
                }
                Expr2::RuntimeError() => {
                    // We can't call a runtime error; bail out by propagating it!
                    return (fn_expr, output);
                }
                Expr2::GlobalTag {
                    variant_var,
                    ext_var,
                    name,
                    ..
                } => Expr2::GlobalTag {
                    variant_var,
                    ext_var,
                    name,
                    arguments: args,
                },
                Expr2::PrivateTag {
                    variant_var,
                    ext_var,
                    name,
                    ..
                } => Expr2::PrivateTag {
                    variant_var,
                    ext_var,
                    name,
                    arguments: args,
                },
                _ => {
                    // This could be something like ((if True then fn1 else fn2) arg1 arg2).
                    let fn_expr_id = env.add(fn_expr, fn_region);
                    Expr2::Call {
                        args,
                        expr: fn_expr_id,
                        expr_var: env.var_store.fresh(),
                        fn_var: env.var_store.fresh(),
                        closure_var: env.var_store.fresh(),
                        called_via: *application_style,
                    }
                }
            };

            (expr, output)
        }

        Defs(loc_defs, loc_ret) => {
            let (unsorted, mut scope, defs_output, symbols_introduced) = canonicalize_defs(
                env,
                Output::default(),
                &scope,
                loc_defs,
                PatternType::DefExpr,
            );

            // The def as a whole is a tail call iff its return expression is a tail call.
            // Use its output as a starting point because its tail_call already has the right answer!
            let (ret_expr, mut output) = to_expr2(env, &mut scope, &loc_ret.value, loc_ret.region);

            output
                .introduced_variables
                .union(&defs_output.introduced_variables);

            output.references.union_mut(defs_output.references);

            // Now that we've collected all the references, check to see if any of the new idents
            // we defined went unused by the return expression. If any were unused, report it.
            for (symbol, region) in symbols_introduced {
                if !output.references.has_lookup(symbol) {
                    env.problem(Problem::UnusedDef(symbol, region));
                }
            }

            let (can_defs, output) = sort_can_defs(env, unsorted, output);

            match can_defs {
                Ok(decls) => {
                    let mut expr = ret_expr;

                    for declaration in decls.into_iter().rev() {
                        expr = decl_to_let(env.pool, env.var_store, declaration, expr);
                    }

                    (expr, output)
                }
                Err(_err) => {
                    // TODO: fix this to be something from Expr2
                    // (RuntimeError(err), output)
                    todo!()
                }
            }
        }

        PrecedenceConflict { .. } => {
            //            use roc_problem::can::RuntimeError::*;
            //
            //            let problem = PrecedenceProblem::BothNonAssociative(
            //                *whole_region,
            //                binop1.clone(),
            //                binop2.clone(),
            //            );
            //
            //            env.problem(Problem::PrecedenceProblem(problem.clone()));
            //
            //            (
            //                RuntimeError(InvalidPrecedence(problem, region)),
            //                Output::default(),
            //            )
            todo!()
        }
        MalformedClosure => {
            //            use roc_problem::can::RuntimeError::*;
            //            (RuntimeError(MalformedClosure(region)), Output::default())
            todo!()
        }
        MalformedIdent(_name, _problem) => {
            //            use roc_problem::can::RuntimeError::*;
            //
            //            let problem = MalformedIdentifier((*name).into(), region);
            //            env.problem(Problem::RuntimeError(problem.clone()));
            //
            //            (RuntimeError(problem), Output::default())
            todo!()
        }
        Var { module_name, ident } => canonicalize_lookup(env, scope, module_name, ident, region),

        // Below this point, we shouln't see any of these nodes anymore because
        // operator desugaring should have removed them!
        bad_expr @ ParensAround(_) => {
            panic!(
                "A ParensAround did not get removed during operator desugaring somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ SpaceBefore(_, _) => {
            panic!(
                "A SpaceBefore did not get removed during operator desugaring somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ SpaceAfter(_, _) => {
            panic!(
                "A SpaceAfter did not get removed during operator desugaring somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ BinOps { .. } => {
            panic!(
                "A binary operator chain did not get desugared somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ UnaryOp(_, _) => {
            panic!(
                "A unary operator did not get desugared somehow: {:#?}",
                bad_expr
            );
        }

        rest => todo!("not yet implemented {:?}", rest),
    }
}

pub fn defs_to_defs2<'a>(
    arena: &'a Bump,
    env: &mut Env<'a>,
    scope: &mut Scope,
    parsed_defs: &'a BumpVec<roc_region::all::Loc<roc_parse::ast::Def<'a>>>,
    region: Region,
) -> Vec<Def2> {
    use roc_parse::ast::Expr::*;

    parsed_defs
        .iter()
        .map(|loc| to_def2_from_def(arena, env, scope, &loc.value, region))
        .collect()
}

pub fn to_def2_from_def<'a>(
    arena: &'a Bump,
    env: &mut Env<'a>,
    scope: &mut Scope,
    parsed_def: &'a roc_parse::ast::Def<'a>,
    region: Region,
) -> Def2 {
    use roc_parse::ast::Def::*;

    match parsed_def {
        SpaceBefore(inner_def, _) => to_def2_from_def(arena, env, scope, inner_def, region),
        SpaceAfter(inner_def, _) => to_def2_from_def(arena, env, scope, inner_def, region),
        Body(&loc_pattern, &loc_expr) => {
            // TODO loc_pattern use identifier
            let expr2 = loc_expr_to_expr2(arena, loc_expr, env, scope, region).0;
            let expr_id = env.pool.add(expr2);

            use roc_parse::ast::Pattern::*;

            match loc_pattern.value {
                Identifier(_) => {
                    let (_, pattern2) = to_pattern2(
                        env,
                        scope,
                        PatternType::TopLevelDef,
                        &loc_pattern.value,
                        region,
                    );
                    let pattern_id = env.pool.add(pattern2);

                    // TODO support with annotation
                    Def2::ValueDef {
                        identifier_id: pattern_id,
                        expr_id,
                    }
                }
                other => {
                    unimplemented!(
                        "I don't yet know how to convert the pattern {:?} into an expr2",
                        other
                    )
                }
            }
        }
        other => {
            unimplemented!(
                "I don't know how to make an expr2 from this def yet: {:?}",
                other
            )
        }
    }
}

fn flatten_str_literal<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    literal: &StrLiteral<'a>,
) -> (Expr2, Output) {
    use roc_parse::ast::StrLiteral::*;

    match literal {
        PlainLine(str_slice) => {
            // TODO use smallstr
            let expr = Expr2::Str(PoolStr::new(str_slice, &mut env.pool));

            (expr, Output::default())
        }
        Line(segments) => flatten_str_lines(env, scope, &[segments]),
        Block(lines) => flatten_str_lines(env, scope, lines),
    }
}

enum StrSegment {
    Interpolation(Expr2),
    Plaintext(PoolStr),
}

fn flatten_str_lines<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    lines: &[&[roc_parse::ast::StrSegment<'a>]],
) -> (Expr2, Output) {
    use roc_parse::ast::StrSegment::*;

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
                    Ok(code_pt) => match std::char::from_u32(code_pt) {
                        Some(ch) => {
                            buf.push(ch);
                        }
                        None => {
                            //                            env.problem(Problem::InvalidUnicodeCodePt(loc_hex_digits.region));
                            //
                            //                            return (
                            //                                Expr::RuntimeError(RuntimeError::InvalidUnicodeCodePt(
                            //                                    loc_hex_digits.region,
                            //                                )),
                            //                                output,
                            //                            );
                            todo!()
                        }
                    },
                    Err(_) => {
                        //                        env.problem(Problem::InvalidHexadecimal(loc_hex_digits.region));
                        //
                        //                        return (
                        //                            Expr::RuntimeError(RuntimeError::InvalidHexadecimal(
                        //                                loc_hex_digits.region,
                        //                            )),
                        //                            output,
                        //                        );
                        todo!()
                    }
                },
                Interpolated(loc_expr) => {
                    if roc_can::expr::is_valid_interpolation(loc_expr.value) {
                        // Interpolations desugar to Str.concat calls
                        output.references.calls.insert(Symbol::STR_CONCAT);

                        if !buf.is_empty() {
                            segments.push(StrSegment::Plaintext(PoolStr::new(&buf, &mut env.pool)));

                            buf = String::new();
                        }

                        let (loc_expr, new_output) =
                            to_expr2(env, scope, loc_expr.value, loc_expr.region);

                        output.union(new_output);

                        segments.push(StrSegment::Interpolation(loc_expr));
                    } else {
                        //                        env.problem(Problem::InvalidInterpolation(loc_expr.region));
                        //
                        //                        return (
                        //                            Expr::RuntimeError(RuntimeError::InvalidInterpolation(loc_expr.region)),
                        //                            output,
                        //                        );
                        todo!()
                    }
                }
                EscapedChar(escaped) => buf.push(roc_can::expr::unescape_char(escaped)),
            }
        }
    }

    if !buf.is_empty() {
        segments.push(StrSegment::Plaintext(PoolStr::new(&buf, &mut env.pool)));
    }

    (desugar_str_segments(env, segments), output)
}

/// Resolve string interpolations by desugaring a sequence of StrSegments
/// into nested calls to Str.concat
fn desugar_str_segments<'a>(env: &mut Env<'a>, segments: Vec<StrSegment>) -> Expr2 {
    use StrSegment::*;

    let pool = &mut env.pool;
    let var_store = &mut env.var_store;

    let mut iter = segments.into_iter().rev();
    let mut expr = match iter.next() {
        Some(Plaintext(pool_str)) => Expr2::Str(pool_str),
        Some(Interpolation(expr_id)) => expr_id,
        None => {
            // No segments? Empty string!

            let pool_str = PoolStr::new("", pool);
            Expr2::Str(pool_str)
        }
    };

    for seg in iter {
        let new_expr = match seg {
            Plaintext(string) => Expr2::Str(string),
            Interpolation(expr_id) => expr_id,
        };

        let concat_expr_id = pool.add(Expr2::Var(Symbol::STR_CONCAT));

        let args = vec![
            (var_store.fresh(), pool.add(new_expr)),
            (var_store.fresh(), pool.add(expr)),
        ];
        let args = PoolVec::new(args.into_iter(), pool);

        let new_call = Expr2::Call {
            args,
            expr: concat_expr_id,
            expr_var: var_store.fresh(),
            fn_var: var_store.fresh(),
            closure_var: var_store.fresh(),
            called_via: CalledVia::Space,
        };

        expr = new_call
    }

    expr
}

enum CanonicalizeRecordProblem {
    InvalidOptionalValue {
        field_name: PoolStr,
        field_region: Region,
        record_region: Region,
    },
}

enum FieldVar {
    VarAndExprId(Variable, ExprId),
    OnlyVar(Variable),
}

fn canonicalize_fields<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    fields: &'a [Located<roc_parse::ast::AssignedField<'a, roc_parse::ast::Expr<'a>>>],
) -> Result<(PoolVec<RecordField>, Output), CanonicalizeRecordProblem> {
    let mut can_fields: MutMap<&'a str, FieldVar> = MutMap::default();
    let mut output = Output::default();

    for loc_field in fields.iter() {
        match canonicalize_field(env, scope, &loc_field.value) {
            Ok(can_field) => {
                match can_field {
                    CanonicalField::LabelAndValue {
                        label,
                        value_expr,
                        value_output,
                        var,
                    } => {
                        let expr_id = env.pool.add(value_expr);

                        let replaced =
                            can_fields.insert(label, FieldVar::VarAndExprId(var, expr_id));

                        if let Some(_old) = replaced {
                            //                    env.problems.push(Problem::DuplicateRecordFieldValue {
                            //                        field_name: label,
                            //                        field_region: loc_field.region,
                            //                        record_region: region,
                            //                        replaced_region: old.region,
                            //                    });
                            todo!()
                        }

                        output.references.union_mut(value_output.references);
                    }
                    CanonicalField::InvalidLabelOnly { label, var } => {
                        let replaced = can_fields.insert(label, FieldVar::OnlyVar(var));

                        if let Some(_old) = replaced {
                            todo!()
                        }
                    }
                }
            }

            Err(CanonicalizeFieldProblem::InvalidOptionalValue {
                field_name: _,
                field_region: _,
            }) => {
                //                env.problem(Problem::InvalidOptionalValue {
                //                    field_name: field_name.clone(),
                //                    field_region,
                //                    record_region: region,
                //                });
                //                return Err(CanonicalizeRecordProblem::InvalidOptionalValue {
                //                    field_name,
                //                    field_region,
                //                    record_region: region,
                //                });
                todo!()
            }
        }
    }

    let pool_vec = PoolVec::with_capacity(can_fields.len() as u32, env.pool);

    for (node_id, (string, field_var)) in pool_vec.iter_node_ids().zip(can_fields.into_iter()) {
        let name = PoolStr::new(string, env.pool);

        match field_var {
            FieldVar::VarAndExprId(var, expr_id) => {
                env.pool[node_id] = RecordField::LabeledValue(name, var, expr_id);
            }
            FieldVar::OnlyVar(var) => {
                env.pool[node_id] = RecordField::InvalidLabelOnly(name, var);
            } // TODO RecordField::LabelOnly
        }
    }

    Ok((pool_vec, output))
}

enum CanonicalizeFieldProblem {
    InvalidOptionalValue {
        field_name: PoolStr,
        field_region: Region,
    },
}
enum CanonicalField<'a> {
    LabelAndValue {
        label: &'a str,
        value_expr: Expr2,
        value_output: Output,
        var: Variable,
    },
    InvalidLabelOnly {
        label: &'a str,
        var: Variable,
    }, // TODO make ValidLabelOnly
}
fn canonicalize_field<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    field: &'a roc_parse::ast::AssignedField<'a, roc_parse::ast::Expr<'a>>,
) -> Result<CanonicalField<'a>, CanonicalizeFieldProblem> {
    use roc_parse::ast::AssignedField::*;

    match field {
        // Both a label and a value, e.g. `{ name: "blah" }`
        RequiredValue(label, _, loc_expr) => {
            let field_var = env.var_store.fresh();
            let (loc_can_expr, output) = to_expr2(env, scope, &loc_expr.value, loc_expr.region);

            Ok(CanonicalField::LabelAndValue {
                label: label.value,
                value_expr: loc_can_expr,
                value_output: output,
                var: field_var,
            })
        }

        OptionalValue(label, _, loc_expr) => Err(CanonicalizeFieldProblem::InvalidOptionalValue {
            field_name: PoolStr::new(label.value, env.pool),
            field_region: Region::span_across(&label.region, &loc_expr.region),
        }),

        // A label with no value, e.g. `{ name }` (this is sugar for { name: name })
        LabelOnly(label) => {
            let field_var = env.var_store.fresh();
            // TODO return ValidLabel if label points to in scope variable
            Ok(CanonicalField::InvalidLabelOnly {
                label: label.value,
                var: field_var,
            })
        }

        SpaceBefore(sub_field, _) | SpaceAfter(sub_field, _) => {
            canonicalize_field(env, scope, sub_field)
        }

        Malformed(_string) => {
            panic!("TODO canonicalize malformed record field");
        }
    }
}

#[inline(always)]
fn canonicalize_when_branch<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    branch: &'a roc_parse::ast::WhenBranch<'a>,
    output: &mut Output,
) -> (WhenBranch, References) {
    let patterns = PoolVec::with_capacity(branch.patterns.len() as u32, env.pool);

    let original_scope = scope;
    let mut scope = original_scope.shallow_clone();

    // TODO report symbols not bound in all patterns
    for (node_id, loc_pattern) in patterns.iter_node_ids().zip(branch.patterns.iter()) {
        let (new_output, can_pattern) = to_pattern2(
            env,
            &mut scope,
            roc_parse::pattern::PatternType::WhenBranch,
            &loc_pattern.value,
            loc_pattern.region,
        );

        output.union(new_output);

        env.set_region(node_id, loc_pattern.region);
        env.pool[node_id] = can_pattern;
    }

    let (value, mut branch_output) =
        to_expr2(env, &mut scope, &branch.value.value, branch.value.region);
    let value_id = env.pool.add(value);
    env.set_region(value_id, branch.value.region);

    let guard = match &branch.guard {
        None => None,
        Some(loc_expr) => {
            let (can_guard, guard_branch_output) =
                to_expr2(env, &mut scope, &loc_expr.value, loc_expr.region);

            let expr_id = env.pool.add(can_guard);
            env.set_region(expr_id, loc_expr.region);

            branch_output.union(guard_branch_output);
            Some(expr_id)
        }
    };

    // Now that we've collected all the references for this branch, check to see if
    // any of the new idents it defined were unused. If any were, report it.
    for (symbol, region) in scope.symbols() {
        let symbol = symbol;

        if !output.references.has_lookup(symbol)
            && !branch_output.references.has_lookup(symbol)
            && !original_scope.contains_symbol(symbol)
        {
            env.problem(Problem::UnusedDef(symbol, region));
        }
    }

    let references = branch_output.references.clone();
    output.union(branch_output);

    (
        WhenBranch {
            patterns,
            body: value_id,
            guard,
        },
        references,
    )
}

fn canonicalize_lookup(
    env: &mut Env<'_>,
    scope: &mut Scope,
    module_name: &str,
    ident: &str,
    region: Region,
) -> (Expr2, Output) {
    use Expr2::*;

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

                RuntimeError()
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

                RuntimeError()
            }
        }
    };

    // If it's valid, this ident should be in scope already.

    (can_expr, output)
}

fn decl_to_let(pool: &mut Pool, var_store: &mut VarStore, decl: Declaration, ret: Expr2) -> Expr2 {
    match decl {
        Declaration::Declare(def) => match def {
            Def::AnnotationOnly { .. } => todo!(),
            Def::Value(value_def) => {
                let def_id = pool.add(value_def);

                let body_id = pool.add(ret);

                Expr2::LetValue {
                    def_id,
                    body_id,
                    body_var: var_store.fresh(),
                }
            }
            Def::Function(function_def) => {
                let def_id = pool.add(function_def);
                let body_id = pool.add(ret);

                Expr2::LetFunction {
                    def_id,
                    body_id,
                    body_var: var_store.fresh(),
                }
            }
        },
        Declaration::DeclareRec(defs) => {
            let mut function_defs = vec![];

            for def in defs {
                match def {
                    Def::AnnotationOnly { .. } => todo!(),
                    Def::Function(function_def) => function_defs.push(function_def),
                    Def::Value(_) => unreachable!(),
                }
            }

            let body_id = pool.add(ret);

            Expr2::LetRec {
                defs: PoolVec::new(function_defs.into_iter(), pool),
                body_var: var_store.fresh(),
                body_id,
            }
        }
        Declaration::InvalidCycle(_entries, _) => {
            // TODO: replace with something from Expr2
            // Expr::RuntimeError(RuntimeError::CircularDef(entries))
            todo!()
        }
        Declaration::Builtin(_) => {
            // Builtins should only be added to top-level decls, not to let-exprs!
            unreachable!()
        }
    }
}
