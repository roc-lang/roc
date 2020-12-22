#![allow(clippy::all)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
// use crate::annotation::canonicalize_annotation;
// use crate::annotation::IntroducedVariables;
// use crate::env::Env;
// use crate::expr::Expr::{self, *};
// use crate::expr::{
//     canonicalize_expr, local_successors, references_from_call, references_from_local, Output,
//     Recursive,
// };
// use crate::pattern::{bindings_from_patterns, canonicalize_pattern, Pattern};
// use crate::procedure::References;
use crate::ast::{Expr2, FunctionDef, Rigids, ValueDef};
use crate::expr::Output;
use crate::expr::{to_expr2, to_expr_id, Env};
use crate::pattern::{
    symbols_and_variables_from_pattern, symbols_from_pattern, to_pattern_id, Pattern2, PatternId,
};
use crate::pool::{NodeId, Pool, PoolStr, PoolVec, ShallowClone};
use crate::scope::Scope;
use crate::types::{to_annotation2, Alias, Annotation2, Signature, Type2, TypeId};
use roc_collections::all::{default_hasher, ImMap, ImSet, MutMap, MutSet, SendMap};
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_parse::ast;
use roc_parse::pattern::PatternType;
use roc_problem::can::{Problem, RuntimeError};
use roc_region::all::{Located, Region};
use roc_types::subs::{VarStore, Variable};
use std::collections::HashMap;
use std::fmt::Debug;
use ven_graph::{strongly_connected_components, topological_sort_into_groups};

#[derive(Debug)]
pub enum Def {
    AnnotationOnly {
        rigids: crate::ast::Rigids,
        annotation: TypeId,
    },
    Value(ValueDef),
    Function(FunctionDef),
}

impl ShallowClone for Def {
    fn shallow_clone(&self) -> Self {
        match self {
            Self::AnnotationOnly { rigids, annotation } => Self::AnnotationOnly {
                rigids: rigids.shallow_clone(),
                annotation: *annotation,
            },
            Self::Value(def) => Self::Value(def.shallow_clone()),
            Self::Function(def) => Self::Function(def.shallow_clone()),
        }
    }
}

/// A Def that has had patterns and type annnotations canonicalized,
/// but no Expr canonicalization has happened yet. Also, it has had spaces
/// and nesting resolved, and knows whether annotations are standalone or not.
#[derive(Debug)]
enum PendingDef<'a> {
    /// A standalone annotation with no body
    AnnotationOnly(
        &'a Located<ast::Pattern<'a>>,
        PatternId,
        &'a Located<ast::TypeAnnotation<'a>>,
    ),
    /// A body with no type annotation
    Body(
        &'a Located<ast::Pattern<'a>>,
        PatternId,
        &'a Located<ast::Expr<'a>>,
    ),
    /// A body with a type annotation
    TypedBody(
        &'a Located<ast::Pattern<'a>>,
        PatternId,
        &'a Located<ast::TypeAnnotation<'a>>,
        &'a Located<ast::Expr<'a>>,
    ),

    /// A type alias, e.g. `Ints : List Int`
    Alias {
        name: Located<Symbol>,
        vars: Vec<Located<Lowercase>>,
        ann: &'a Located<ast::TypeAnnotation<'a>>,
    },

    /// An invalid alias, that is ignored in the rest of the pipeline
    /// e.g. a shadowed alias, or a definition like `MyAlias 1 : Int`
    /// with an incorrect pattern
    InvalidAlias,
}

fn to_pending_def<'a>(
    env: &mut Env<'a>,
    def: &'a ast::Def<'a>,
    scope: &mut Scope,
    pattern_type: PatternType,
) -> Option<(Output, PendingDef<'a>)> {
    use roc_parse::ast::Def::*;

    match def {
        Annotation(loc_pattern, loc_ann) => {
            // This takes care of checking for shadowing and adding idents to scope.
            let (output, loc_can_pattern) = crate::pattern::to_pattern_id(
                env,
                scope,
                pattern_type,
                &loc_pattern.value,
                loc_pattern.region,
            );

            Some((
                output,
                PendingDef::AnnotationOnly(loc_pattern, loc_can_pattern, loc_ann),
            ))
        }
        Body(loc_pattern, loc_expr) => {
            // This takes care of checking for shadowing and adding idents to scope.
            let (output, loc_can_pattern) = crate::pattern::to_pattern_id(
                env,
                scope,
                pattern_type,
                &loc_pattern.value,
                loc_pattern.region,
            );

            Some((
                output,
                PendingDef::Body(loc_pattern, loc_can_pattern, loc_expr),
            ))
        }

        AnnotatedBody {
            ann_pattern,
            ann_type,
            comment: _,
            body_pattern,
            body_expr,
        } => {
            if ann_pattern.value.equivalent(&body_pattern.value) {
                // NOTE: Pick the body pattern, picking the annotation one is
                // incorrect in the presence of optional record fields!
                //
                // { x, y } : { x : Int, y ? Bool }*
                // { x, y ? False } = rec
                Some(pending_typed_body(
                    env,
                    body_pattern,
                    ann_type,
                    body_expr,
                    scope,
                    pattern_type,
                ))
            } else {
                // the pattern of the annotation does not match the pattern of the body direc
                env.problem(Problem::SignatureDefMismatch {
                    annotation_pattern: ann_pattern.region,
                    def_pattern: body_pattern.region,
                });

                // TODO: Should we instead build some PendingDef::InvalidAnnotatedBody ? This would
                // remove the `Option` on this function (and be probably more reliable for further
                // problem/error reporting)
                None
            }
        }

        roc_parse::ast::Def::Alias { name, vars, ann } => {
            let region = Region::span_across(&name.region, &ann.region);

            match scope.introduce(
                name.value.into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                region,
            ) {
                Ok(symbol) => {
                    let mut can_rigids: Vec<Located<Lowercase>> = Vec::with_capacity(vars.len());

                    for loc_var in vars.iter() {
                        match loc_var.value {
                            ast::Pattern::Identifier(name)
                                if name.chars().next().unwrap().is_lowercase() =>
                            {
                                let lowercase = Lowercase::from(name);
                                can_rigids.push(Located {
                                    value: lowercase,
                                    region: loc_var.region,
                                });
                            }
                            _ => {
                                // any other pattern in this position is a syntax error.
                                env.problem(Problem::InvalidAliasRigid {
                                    alias_name: symbol,
                                    region: loc_var.region,
                                });

                                return Some((Output::default(), PendingDef::InvalidAlias));
                            }
                        }
                    }

                    Some((
                        Output::default(),
                        PendingDef::Alias {
                            name: Located {
                                region: name.region,
                                value: symbol,
                            },
                            vars: can_rigids,
                            ann,
                        },
                    ))
                }

                Err((original_region, loc_shadowed_symbol)) => {
                    env.problem(Problem::ShadowingInAnnotation {
                        original_region,
                        shadow: loc_shadowed_symbol,
                    });

                    Some((Output::default(), PendingDef::InvalidAlias))
                }
            }
        }

        SpaceBefore(sub_def, _) | SpaceAfter(sub_def, _) | Nested(sub_def) => {
            to_pending_def(env, sub_def, scope, pattern_type)
        }

        NotYetImplemented(s) => todo!("{}", s),
    }
}

fn pending_typed_body<'a>(
    env: &mut Env<'a>,
    loc_pattern: &'a Located<ast::Pattern<'a>>,
    loc_ann: &'a Located<ast::TypeAnnotation<'a>>,
    loc_expr: &'a Located<ast::Expr<'a>>,
    scope: &mut Scope,
    pattern_type: PatternType,
) -> (Output, PendingDef<'a>) {
    // This takes care of checking for shadowing and adding idents to scope.
    let (output, loc_can_pattern) = to_pattern_id(
        env,
        scope,
        pattern_type,
        &loc_pattern.value,
        loc_pattern.region,
    );

    (
        output,
        PendingDef::TypedBody(loc_pattern, loc_can_pattern, loc_ann, loc_expr),
    )
}

fn from_pending_alias<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    name: Located<Symbol>,
    vars: Vec<Located<Lowercase>>,
    ann: &'a Located<ast::TypeAnnotation<'a>>,
    mut output: Output,
) -> Output {
    let symbol = name.value;

    match to_annotation2(env, scope, &ann.value, ann.region) {
        Annotation2::Erroneous(_) => todo!(),
        Annotation2::Annotation {
            named_rigids,
            unnamed_rigids,
            symbols,
            signature,
        } => {
            // Record all the annotation's references in output.references.lookups

            for symbol in symbols {
                output.references.lookups.insert(symbol);
                output.references.referenced_aliases.insert(symbol);
            }

            for loc_lowercase in vars {
                if !named_rigids.contains_key(loc_lowercase.value.as_str()) {
                    env.problem(Problem::PhantomTypeArgument {
                        alias: symbol,
                        variable_region: loc_lowercase.region,
                        variable_name: loc_lowercase.value.clone(),
                    });
                }
            }

            let named = PoolVec::with_capacity(named_rigids.len() as u32, env.pool);
            let unnamed = PoolVec::with_capacity(unnamed_rigids.len() as u32, env.pool);

            for (node_id, (name, variable)) in named.iter_node_ids().zip(named_rigids) {
                let poolstr = PoolStr::new(name, env.pool);

                env.pool[node_id] = (poolstr, variable);
            }

            for (node_id, rigid) in unnamed.iter_node_ids().zip(unnamed_rigids) {
                env.pool[node_id] = rigid;
            }

            let rigids = Rigids {
                named: named.shallow_clone(),
                unnamed,
            };

            let annotation = match signature {
                Signature::Value { annotation } => annotation,
                Signature::Function {
                    arguments,
                    closure_type_id,
                    return_type_id,
                } => Type2::Function(arguments, closure_type_id, return_type_id),
                Signature::FunctionWithAliases { annotation, .. } => annotation,
            };

            if annotation.contains_symbol(env.pool, symbol) {
                // the alias is recursive. If it's a tag union, we attempt to fix this
                if let Type2::TagUnion(tags, ext) = annotation {
                    // re-canonicalize the alias with the alias already in scope
                    let rec_var = env.var_store.fresh();
                    let rec_type_union = Type2::RecursiveTagUnion(rec_var, tags, ext);

                    // NOTE this only looks at the symbol, and just assumes that the
                    // recursion is not polymorphic
                    rec_type_union.substitute_alias(env.pool, symbol, Type2::Variable(rec_var));

                    let annotation_id = env.add(rec_type_union, ann.region);
                    scope.add_alias(env.pool, symbol, named, annotation_id);
                } else {
                    env.problem(Problem::CyclicAlias(symbol, name.region, vec![]));
                    return output;
                }
            } else {
                let annotation_id = env.add(annotation, ann.region);
                scope.add_alias(env.pool, symbol, named, annotation_id);
            }

            output
        }
    }
}

// TODO trim down these arguments!
#[allow(clippy::too_many_arguments)]
#[allow(clippy::cognitive_complexity)]
fn canonicalize_pending_def<'a>(
    env: &mut Env<'a>,
    pending_def: PendingDef<'a>,
    mut output: Output,
    scope: &mut Scope,
    can_defs_by_symbol: &mut MutMap<Symbol, Def>,
    refs_by_symbol: &mut MutMap<Symbol, (Region, References)>,
    aliases: &mut MutMap<Symbol, Alias>,
) -> Output {
    use PendingDef::*;

    // Make types for the body expr, even if we won't end up having a body.
    let expr_var = env.var_store.fresh();

    match pending_def {
        AnnotationOnly(_, loc_can_pattern, loc_ann) => {
            // annotation sans body cannot introduce new rigids that are visible in other annotations
            // but the rigids can show up in type error messages, so still register them

            match to_annotation2(env, scope, &loc_ann.value, loc_ann.region) {
                Annotation2::Erroneous(_) => todo!(),
                Annotation2::Annotation {
                    named_rigids,
                    unnamed_rigids,
                    symbols,
                    signature,
                } => {
                    // Record all the annotation's references in output.references.lookups

                    for symbol in symbols {
                        output.references.lookups.insert(symbol);
                        output.references.referenced_aliases.insert(symbol);
                    }

                    let rigids = {
                        let named = PoolVec::with_capacity(named_rigids.len() as u32, env.pool);
                        let unnamed = PoolVec::with_capacity(unnamed_rigids.len() as u32, env.pool);

                        for (node_id, (name, variable)) in named.iter_node_ids().zip(named_rigids) {
                            let poolstr = PoolStr::new(name, env.pool);
                            env.pool[node_id] = (poolstr, variable);
                        }

                        for (node_id, rigid) in unnamed.iter_node_ids().zip(unnamed_rigids) {
                            env.pool[node_id] = rigid;
                        }

                        Rigids { named, unnamed }
                    };

                    let annotation = match signature {
                        Signature::Value { annotation } => annotation,
                        Signature::Function {
                            arguments,
                            closure_type_id,
                            return_type_id,
                        } => Type2::Function(arguments, closure_type_id, return_type_id),
                        Signature::FunctionWithAliases { annotation, .. } => annotation,
                    };
                    let annotation = env.add(annotation, loc_ann.region);

                    let def = Def::AnnotationOnly { rigids, annotation };

                    for symbol in symbols_from_pattern(env.pool, env.pool.get(loc_can_pattern)) {
                        can_defs_by_symbol.insert(symbol, def.shallow_clone());
                    }

                    output
                }
            }
        }

        PendingDef::Alias { name, ann, vars } => {
            from_pending_alias(env, scope, name, vars, ann, output)
        }

        InvalidAlias => {
            // invalid aliases (shadowed, incorrect patterns )
            todo!()
        }

        TypedBody(loc_pattern, loc_can_pattern, loc_ann, loc_expr) => {
            match to_annotation2(env, scope, &loc_ann.value, loc_ann.region) {
                Annotation2::Erroneous(_) => todo!(),
                Annotation2::Annotation {
                    named_rigids,
                    unnamed_rigids,
                    symbols,
                    signature,
                } => {
                    // Record all the annotation's references in output.references.lookups

                    for symbol in symbols {
                        output.references.lookups.insert(symbol);
                        output.references.referenced_aliases.insert(symbol);
                    }

                    let rigids = {
                        let named = PoolVec::with_capacity(named_rigids.len() as u32, env.pool);
                        let unnamed = PoolVec::with_capacity(unnamed_rigids.len() as u32, env.pool);

                        for (node_id, (name, variable)) in named.iter_node_ids().zip(named_rigids) {
                            let poolstr = PoolStr::new(name, env.pool);
                            env.pool[node_id] = (poolstr, variable);
                        }

                        for (node_id, rigid) in unnamed.iter_node_ids().zip(unnamed_rigids) {
                            env.pool[node_id] = rigid;
                        }

                        Rigids { named, unnamed }
                    };

                    // bookkeeping for tail-call detection. If we're assigning to an
                    // identifier (e.g. `f = \x -> ...`), then this symbol can be tail-called.
                    let outer_identifier = env.tailcallable_symbol;

                    if let Pattern2::Identifier(ref defined_symbol) = env.pool[loc_can_pattern] {
                        env.tailcallable_symbol = Some(*defined_symbol);
                    };

                    // regiser the name of this closure, to make sure the closure won't capture it's own name
                    if let (Pattern2::Identifier(ref defined_symbol), &ast::Expr::Closure(_, _)) =
                        (&env.pool[loc_can_pattern], &loc_expr.value)
                    {
                        env.closure_name_symbol = Some(*defined_symbol);
                    };

                    let (loc_can_expr, can_output) =
                        to_expr2(env, scope, &loc_expr.value, loc_expr.region);

                    output.references.union_mut(can_output.references.clone());

                    // reset the tailcallable_symbol
                    env.tailcallable_symbol = outer_identifier;

                    // First, make sure we are actually assigning an identifier instead of (for example) a tag.
                    //
                    // If we're assigning (UserId userId) = ... then this is certainly not a closure declaration,
                    // which also implies it's not a self tail call!
                    //
                    // Only defs of the form (foo = ...) can be closure declarations or self tail calls.
                    match loc_can_expr {
                        Expr2::Closure {
                            args: closure_args,
                            body,
                            extra,
                            name: closure_symbol,
                            ..
                        } => {
                            let symbol = match env.pool[loc_can_pattern] {
                                Pattern2::Identifier(ref s) => *s,
                                _ => todo!(
                                    r"this is an error; functions must be bound with an identifier pattern!"
                                ),
                            };

                            // Since everywhere in the code it'll be referred to by its defined name,
                            // remove its generated name from the closure map.
                            let references =
                                env.closures.remove(&closure_symbol).unwrap_or_else(|| {
                            panic!( r"Tried to remove symbol {:?} from procedures, but it was not found: {:?}", closure_symbol, env.closures) 
                            });

                            // TODO should we re-insert this function into env.closures?
                            env.closures.insert(symbol, references);

                            // Recursion doesn't count as referencing. (If it did, all recursive functions
                            // would result in circular def errors!)
                            refs_by_symbol.entry(symbol).and_modify(|(_, refs)| {
                                refs.lookups.remove(&symbol);
                            });

                            // Functions' references don't count in defs.
                            // See 3d5a2560057d7f25813112dfa5309956c0f9e6a9 and its
                            // parent commit for the bug this fixed!
                            let refs = References::new();

                            let arguments: PoolVec<(Pattern2, Type2)> =
                                PoolVec::with_capacity(closure_args.len() as u32, env.pool);

                            let return_type: TypeId;

                            let annotation = match signature {
                                Signature::Value { .. } => {
                                    todo!("type annotation says 0 arguments, but it's a function")
                                }
                                Signature::Function {
                                    arguments: type_arguments,
                                    closure_type_id,
                                    return_type_id,
                                }
                                | Signature::FunctionWithAliases {
                                    arguments: type_arguments,
                                    closure_type_id,
                                    return_type_id,
                                    ..
                                } => {
                                    if arguments.len() != type_arguments.len() {
                                        panic!("argument number mismatch");
                                    }

                                    let it: Vec<_> = closure_args
                                        .iter(env.pool)
                                        .map(|(x, y)| (*x, *y))
                                        .zip(
                                            type_arguments
                                                .iter(env.pool)
                                                .map(|t| t.shallow_clone()),
                                        )
                                        .collect();

                                    for (node_id, ((_, pattern_id), typ)) in
                                        arguments.iter_node_ids().zip(it.into_iter())
                                    {
                                        let pattern = &env.pool[pattern_id];

                                        env.pool[node_id] = (pattern.shallow_clone(), typ);
                                    }

                                    return_type = return_type_id;
                                }
                            };

                            let function_def = FunctionDef::WithAnnotation {
                                name: symbol,
                                arguments,
                                rigids: env.pool.add(rigids),
                                return_type,
                                body,
                            };

                            let def = Def::Function(function_def);

                            can_defs_by_symbol.insert(symbol, def);

                            output
                        }

                        _ => {
                            let annotation = match signature {
                                Signature::Value { annotation } => annotation,
                                Signature::Function {
                                    arguments,
                                    closure_type_id,
                                    return_type_id,
                                } => Type2::Function(arguments, closure_type_id, return_type_id),
                                Signature::FunctionWithAliases { annotation, .. } => annotation,
                            };
                            let annotation = env.add(annotation, loc_ann.region);

                            let value_def = ValueDef {
                                pattern: loc_can_pattern,
                                expr_type: Some((annotation, rigids)),
                                expr_var: env.var_store.fresh(),
                            };

                            let def = Def::Value(value_def);

                            for symbol in
                                symbols_from_pattern(env.pool, env.pool.get(loc_can_pattern))
                            {
                                can_defs_by_symbol.insert(symbol, def.shallow_clone());
                            }

                            for (_, (symbol, region)) in scope.idents() {
                                // if !vars_by_symbol.contains_key(&symbol) {
                                //     continue;
                                // }
                                let refs = can_output.references.clone();
                                refs_by_symbol.insert(*symbol, (*region, refs));
                            }

                            output
                        }
                    }
                }
            }
        }

        Body(loc_pattern, loc_can_pattern, loc_expr) => {
            // bookkeeping for tail-call detection. If we're assigning to an
            // identifier (e.g. `f = \x -> ...`), then this symbol can be tail-called.
            let outer_identifier = env.tailcallable_symbol;

            if let Pattern2::Identifier(ref defined_symbol) = env.pool[loc_can_pattern] {
                env.tailcallable_symbol = Some(*defined_symbol);
            };

            // regiser the name of this closure, to make sure the closure won't capture it's own name
            if let (Pattern2::Identifier(ref defined_symbol), &ast::Expr::Closure(_, _)) =
                (&env.pool[loc_can_pattern], &loc_expr.value)
            {
                env.closure_name_symbol = Some(*defined_symbol);
            };

            let (loc_can_expr, can_output) = to_expr2(env, scope, &loc_expr.value, loc_expr.region);

            output.references.union_mut(can_output.references.clone());

            // reset the tailcallable_symbol
            env.tailcallable_symbol = outer_identifier;

            // First, make sure we are actually assigning an identifier instead of (for example) a tag.
            //
            // If we're assigning (UserId userId) = ... then this is certainly not a closure declaration,
            // which also implies it's not a self tail call!
            //
            // Only defs of the form (foo = ...) can be closure declarations or self tail calls.
            match loc_can_expr {
                Expr2::Closure {
                    args: closure_args,
                    body,
                    extra,
                    name: closure_symbol,
                    ..
                } => {
                    let symbol = match env.pool[loc_can_pattern] {
                        Pattern2::Identifier(ref s) => *s,
                        _ => todo!(
                            r"this is an error; functions must be bound with an identifier pattern!"
                        ),
                    };

                    // Since everywhere in the code it'll be referred to by its defined name,
                    // remove its generated name from the closure map.
                    let references =
                        env.closures.remove(&closure_symbol).unwrap_or_else(|| {
                            panic!( r"Tried to remove symbol {:?} from procedures, but it was not found: {:?}", closure_symbol, env.closures) 
                        });

                    // TODO should we re-insert this function into env.closures?
                    env.closures.insert(symbol, references);

                    // Recursion doesn't count as referencing. (If it did, all recursive functions
                    // would result in circular def errors!)
                    refs_by_symbol.entry(symbol).and_modify(|(_, refs)| {
                        refs.lookups.remove(&symbol);
                    });

                    // Functions' references don't count in defs.
                    // See 3d5a2560057d7f25813112dfa5309956c0f9e6a9 and its
                    // parent commit for the bug this fixed!
                    let refs = References::new();

                    let arguments: PoolVec<(Pattern2, Variable)> =
                        PoolVec::with_capacity(closure_args.len() as u32, env.pool);

                    let it: Vec<_> = closure_args.iter(env.pool).map(|(x, y)| (*x, *y)).collect();

                    for (node_id, (_, pattern_id)) in arguments.iter_node_ids().zip(it.into_iter())
                    {
                        let pattern = &env.pool[pattern_id];

                        env.pool[node_id] = (pattern.shallow_clone(), env.var_store.fresh());
                    }

                    let function_def = FunctionDef::NoAnnotation {
                        name: symbol,
                        arguments,
                        return_var: env.var_store.fresh(),
                        body,
                    };

                    let def = Def::Function(function_def);

                    can_defs_by_symbol.insert(symbol, def);

                    output
                }

                _ => {
                    let value_def = ValueDef {
                        pattern: loc_can_pattern,
                        expr_type: None,
                        expr_var: env.var_store.fresh(),
                    };

                    let def = Def::Value(value_def);

                    for symbol in symbols_from_pattern(env.pool, env.pool.get(loc_can_pattern)) {
                        can_defs_by_symbol.insert(symbol, def.shallow_clone());
                    }

                    for (_, (symbol, region)) in scope.idents() {
                        // if !vars_by_symbol.contains_key(&symbol) {
                        //     continue;
                        // }
                        let refs = can_output.references.clone();
                        refs_by_symbol.insert(*symbol, (*region, refs));
                    }

                    output
                }
            }
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct References {
    pub bound_symbols: MutSet<Symbol>,
    pub lookups: MutSet<Symbol>,
    pub referenced_aliases: MutSet<Symbol>,
    pub calls: MutSet<Symbol>,
}

impl References {
    pub fn new() -> References {
        Self::default()
    }

    pub fn union_mut(&mut self, other: References) {
        self.lookups.extend(other.lookups);
        self.calls.extend(other.calls);
        self.bound_symbols.extend(other.bound_symbols);
        self.referenced_aliases.extend(other.referenced_aliases);
    }

    pub fn has_lookup(&self, symbol: Symbol) -> bool {
        self.lookups.contains(&symbol)
    }
}

#[derive(Debug)]
pub struct CanDefs {
    pub refs_by_symbol: MutMap<Symbol, (Region, References)>,
    pub can_defs_by_symbol: MutMap<Symbol, Def>,
    pub aliases: MutMap<Symbol, Alias>,
}

#[inline(always)]
pub fn canonicalize_defs<'a>(
    env: &mut Env<'a>,
    mut output: Output,
    var_store: &mut VarStore,
    original_scope: &Scope,
    loc_defs: &'a [&'a Located<ast::Def<'a>>],
    pattern_type: PatternType,
) -> (CanDefs, Scope, Output, MutMap<Symbol, Region>) {
    // Canonicalizing defs while detecting shadowing involves a multi-step process:
    //
    // 1. Go through each of the patterns.
    // 2. For each identifier pattern, get the scope.symbol() for the ident. (That symbol will use the home module for its module.)
    // 3. If that symbol is already in scope, then we're about to shadow it. Error!
    // 4. Otherwise, add it to the scope immediately, so we can detect shadowing within the same
    //    pattern (e.g. (Foo a a) = ...)
    // 5. Add this canonicalized pattern and its corresponding ast::Expr to pending_exprs.
    // 5. Once every pattern has been processed and added to scope, go back and canonicalize the exprs from
    //    pending_exprs, this time building up a canonical def for each one.
    //
    // This way, whenever any expr is doing lookups, it knows everything that's in scope -
    // even defs that appear after it in the source.
    //
    // This naturally handles recursion too, because a given expr which refers
    // to itself won't be processed until after its def has been added to scope.

    // Record both the original and final idents from the scope,
    // so we can diff them while detecting unused defs.
    let mut scope = original_scope.shallow_clone();
    let num_defs = loc_defs.len();
    let mut refs_by_symbol = MutMap::default();
    let mut can_defs_by_symbol = HashMap::with_capacity_and_hasher(num_defs, default_hasher());
    let mut pending = Vec::with_capacity(num_defs); // TODO bump allocate this!

    // Canonicalize all the patterns, record shadowing problems, and store
    // the ast::Expr values in pending_exprs for further canonicalization
    // once we've finished assembling the entire scope.
    for loc_def in loc_defs {
        match to_pending_def(env, &loc_def.value, &mut scope, pattern_type) {
            None => (),
            Some((new_output, pending_def)) => {
                // store the top-level defs, used to ensure that closures won't capture them
                if let PatternType::TopLevelDef = pattern_type {
                    match &pending_def {
                        PendingDef::AnnotationOnly(_, loc_can_pattern, _)
                        | PendingDef::Body(_, loc_can_pattern, _)
                        | PendingDef::TypedBody(_, loc_can_pattern, _, _) => {
                            env.top_level_symbols.extend(symbols_from_pattern(
                                env.pool,
                                env.pool.get(*loc_can_pattern),
                            ))
                        }
                        PendingDef::Alias { .. } | PendingDef::InvalidAlias => {}
                    }
                }
                // Record the ast::Expr for later. We'll do another pass through these
                // once we have the entire scope assembled. If we were to canonicalize
                // the exprs right now, they wouldn't have symbols in scope from defs
                // that get would have gotten added later in the defs list!
                pending.push(pending_def);
                output.union(new_output);
            }
        }
    }

    if cfg!(debug_assertions) {
        env.home.register_debug_idents(&env.ident_ids);
    }

    // TODO what to do here? aliases are already in the scope!
    let mut aliases = MutMap::default();
    let mut value_defs = Vec::new();

    for pending_def in pending.into_iter() {
        match pending_def {
            PendingDef::Alias { name, vars, ann } => {
                output = from_pending_alias(env, &mut scope, name, vars, ann, output);
            }
            other => value_defs.push(other),
        }
    }

    // TODO
    // correct_mutual_recursive_type_alias(env, &mut aliases, var_store);

    // Now that we have the scope completely assembled, and shadowing resolved,
    // we're ready to canonicalize any body exprs.
    for pending_def in value_defs.into_iter() {
        output = canonicalize_pending_def(
            env,
            pending_def,
            output,
            &mut scope,
            &mut can_defs_by_symbol,
            &mut refs_by_symbol,
            &mut aliases,
        );

        // TODO we should do something with these references; they include
        // things like type annotations.
    }

    // Determine which idents we introduced in the course of this process.
    let mut symbols_introduced = MutMap::default();

    for (symbol, region) in scope.symbols() {
        if !original_scope.contains_symbol(symbol) {
            symbols_introduced.insert(symbol, region);
        }
    }

    // This returns both the defs info as well as the new scope.
    //
    // We have to return the new scope because we added defs to it
    // (and those lookups shouldn't fail later, e.g. when canonicalizing
    // the return expr), but we didn't want to mutate the original scope
    // directly because we wanted to keep a clone of it around to diff
    // when looking for unused idents.
    //
    // We have to return the scope separately from the defs, because the
    // defs need to get moved later.
    (
        CanDefs {
            refs_by_symbol,
            can_defs_by_symbol,
            aliases,
        },
        scope,
        output,
        symbols_introduced,
    )
}
