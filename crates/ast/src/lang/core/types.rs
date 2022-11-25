#![allow(clippy::all)]
#![allow(dead_code)]
#![allow(unused_imports)]
// use roc_can::expr::Output;
use roc_collections::all::{MutMap, MutSet};
use roc_error_macros::todo_abilities;
use roc_module::ident::{Ident, Lowercase, TagName, Uppercase};
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::types::{AliasKind, RecordField};
use roc_types::{subs::Variable, types::ErrorType};

use crate::lang::env::Env;
use crate::lang::scope::Scope;
use crate::mem_pool::pool::{NodeId, Pool};
use crate::mem_pool::pool_str::PoolStr;
use crate::mem_pool::pool_vec::PoolVec;
use crate::mem_pool::shallow_clone::ShallowClone;

pub type TypeId = NodeId<Type2>;

const TYPE2_SIZE: () = assert!(std::mem::size_of::<Type2>() == 3 * 8 + 4);

#[derive(Debug)]
pub enum Type2 {
    Variable(Variable), // 4B

    Alias(Symbol, PoolVec<TypeId>, TypeId), // 24B = 8B + 8B + 4B + pad
    Opaque(Symbol, PoolVec<TypeId>, TypeId), // 24B = 8B + 8B + 4B + pad
    AsAlias(Symbol, PoolVec<(PoolStr, TypeId)>, TypeId), // 24B = 8B + 8B + 4B + pad

    // 24B
    HostExposedAlias {
        name: Symbol,                          // 8B
        arguments: PoolVec<(PoolStr, TypeId)>, // 8B
        actual_var: Variable,                  // 4B
        actual: TypeId,                        // 4B
    },
    EmptyTagUnion,
    TagUnion(PoolVec<(TagName, PoolVec<Type2>)>, TypeId), // 12B = 8B + 4B
    RecursiveTagUnion(Variable, PoolVec<(TagName, PoolVec<Type2>)>, TypeId), // 16B = 4B + 8B + 4B

    EmptyRec,
    Record(PoolVec<(PoolStr, RecordField<TypeId>)>, TypeId), // 12B = 8B + 4B

    Function(PoolVec<Type2>, TypeId, TypeId), // 16B = 8B + 4B + 4B
    Apply(Symbol, PoolVec<Type2>),            // 16B = 8B + 8B

    Erroneous(Problem2), // 24B
}

#[derive(Debug)]
pub enum Problem2 {
    CanonicalizationProblem,
    CircularType(Symbol, NodeId<ErrorType>), // 12B = 8B + 4B
    CyclicAlias(Symbol, PoolVec<Symbol>),    // 20B = 8B + 12B
    UnrecognizedIdent(PoolStr),              // 8B
    Shadowed(Loc<PoolStr>),
    BadTypeArguments {
        symbol: Symbol,  // 8B
        type_got: u8,    // 1B
        alias_needs: u8, // 1B
    },
    InvalidModule,
    SolvedTypeError,
}

impl ShallowClone for Type2 {
    fn shallow_clone(&self) -> Self {
        match self {
            Self::Variable(var) => Self::Variable(*var),
            Self::Alias(symbol, args, alias_type_id) => {
                Self::Alias(*symbol, args.shallow_clone(), alias_type_id.clone())
            }
            Self::Opaque(symbol, args, alias_type_id) => {
                Self::Opaque(*symbol, args.shallow_clone(), alias_type_id.clone())
            }
            Self::Record(fields, ext_id) => Self::Record(fields.shallow_clone(), ext_id.clone()),
            Self::Function(args, closure_type_id, ret_type_id) => Self::Function(
                args.shallow_clone(),
                closure_type_id.clone(),
                ret_type_id.clone(),
            ),
            rest => todo!("{:?}", rest),
        }
    }
}

impl Type2 {
    fn substitute(_pool: &mut Pool, _subs: &MutMap<Variable, TypeId>, _type_id: TypeId) {
        todo!()
    }

    pub fn variables(&self, pool: &mut Pool) -> MutSet<Variable> {
        use Type2::*;

        let mut stack = vec![self];
        let mut result = MutSet::default();

        while let Some(this) = stack.pop() {
            match this {
                Variable(v) => {
                    result.insert(*v);
                }
                Alias(_, _, actual) | AsAlias(_, _, actual) | Opaque(_, _, actual) => {
                    stack.push(pool.get(*actual));
                }
                HostExposedAlias {
                    actual_var, actual, ..
                } => {
                    result.insert(*actual_var);
                    stack.push(pool.get(*actual));
                }
                EmptyTagUnion | EmptyRec | Erroneous(_) => {}
                TagUnion(tags, ext) => {
                    for (_, args) in tags.iter(pool) {
                        stack.extend(args.iter(pool));
                    }
                    stack.push(pool.get(*ext));
                }
                RecursiveTagUnion(rec, tags, ext) => {
                    for (_, args) in tags.iter(pool) {
                        stack.extend(args.iter(pool));
                    }
                    stack.push(pool.get(*ext));
                    result.insert(*rec);
                }
                Record(fields, ext) => {
                    for (_, field) in fields.iter(pool) {
                        stack.push(pool.get(*field.as_inner()));
                    }
                    stack.push(pool.get(*ext));
                }
                Function(args, closure, result) => {
                    stack.extend(args.iter(pool));
                    stack.push(pool.get(*closure));
                    stack.push(pool.get(*result));
                }
                Apply(_, args) => {
                    stack.extend(args.iter(pool));
                }
            }
        }

        result
    }

    pub fn contains_symbol(&self, _pool: &mut Pool, _needle: Symbol) -> bool {
        todo!()
    }

    pub fn substitute_alias(&self, _pool: &mut Pool, _needle: Symbol, _actual: Self) {
        todo!()
    }
}

impl NodeId<Type2> {
    pub fn variables(&self, _pool: &mut Pool) -> MutSet<Variable> {
        todo!()
    }
}

/// A temporary data structure to return a bunch of values to Def construction
pub enum Signature {
    FunctionWithAliases {
        annotation: Type2,
        arguments: PoolVec<Type2>,
        closure_type_id: TypeId,
        return_type_id: TypeId,
    },
    Function {
        arguments: PoolVec<Type2>,
        closure_type_id: TypeId,
        return_type_id: TypeId,
    },
    Value {
        annotation: Type2,
    },
}

pub enum Annotation2 {
    Annotation {
        named_rigids: MutMap<Lowercase, Variable>,
        unnamed_rigids: MutSet<Variable>,
        symbols: MutSet<Symbol>,
        signature: Signature,
    },
    Erroneous,
}

pub fn to_annotation2<'a>(
    env: &mut Env,
    scope: &mut Scope,
    annotation: &'a roc_parse::ast::TypeAnnotation<'a>,
    region: Region,
) -> Annotation2 {
    let mut references = References::default();

    let annotation = to_type2(env, scope, &mut references, annotation, region);

    // we dealias until we hit a non-alias, then we either hit a function type (and produce a
    // function annotation) or anything else (and produce a value annotation)
    match annotation {
        Type2::Function(arguments, closure_type_id, return_type_id) => {
            let References {
                named,
                unnamed,
                symbols,
                ..
            } = references;

            let signature = Signature::Function {
                arguments,
                closure_type_id,
                return_type_id,
            };

            Annotation2::Annotation {
                named_rigids: named,
                unnamed_rigids: unnamed,
                symbols,
                signature,
            }
        }
        Type2::Alias(_, _, _) => {
            // most of the time, the annotation is not an alias, so this case is comparatively
            // less efficient
            shallow_dealias(env, references, annotation)
        }
        _ => {
            let References {
                named,
                unnamed,
                symbols,
                ..
            } = references;

            let signature = Signature::Value { annotation };

            Annotation2::Annotation {
                named_rigids: named,
                unnamed_rigids: unnamed,
                symbols,
                signature,
            }
        }
    }
}

fn shallow_dealias<'a>(env: &mut Env, references: References, annotation: Type2) -> Annotation2 {
    let References {
        named,
        unnamed,
        symbols,
        ..
    } = references;
    let mut inner = &annotation;

    loop {
        match inner {
            Type2::Alias(_, _, actual) => {
                inner = env.pool.get(*actual);
            }
            Type2::Function(arguments, closure_type_id, return_type_id) => {
                let signature = Signature::FunctionWithAliases {
                    arguments: arguments.shallow_clone(),
                    closure_type_id: *closure_type_id,
                    return_type_id: *return_type_id,
                    annotation,
                };

                return Annotation2::Annotation {
                    named_rigids: named,
                    unnamed_rigids: unnamed,
                    symbols,
                    signature,
                };
            }
            _ => {
                let signature = Signature::Value { annotation };

                return Annotation2::Annotation {
                    named_rigids: named,
                    unnamed_rigids: unnamed,
                    symbols,
                    signature,
                };
            }
        }
    }
}

#[derive(Default)]
pub struct References {
    named: MutMap<Lowercase, Variable>,
    unnamed: MutSet<Variable>,
    hidden: MutSet<Variable>,
    symbols: MutSet<Symbol>,
}

pub fn to_type_id<'a>(
    env: &mut Env,
    scope: &mut Scope,
    rigids: &mut References,
    annotation: &roc_parse::ast::TypeAnnotation<'a>,
    region: Region,
) -> TypeId {
    let type2 = to_type2(env, scope, rigids, annotation, region);

    env.add(type2, region)
}

pub fn as_type_id<'a>(
    env: &mut Env,
    scope: &mut Scope,
    rigids: &mut References,
    type_id: TypeId,
    annotation: &roc_parse::ast::TypeAnnotation<'a>,
    region: Region,
) {
    let type2 = to_type2(env, scope, rigids, annotation, region);

    env.pool[type_id] = type2;
    env.set_region(type_id, region);
}

pub fn to_type2<'a>(
    env: &mut Env,
    scope: &mut Scope,
    references: &mut References,
    annotation: &roc_parse::ast::TypeAnnotation<'a>,
    region: Region,
) -> Type2 {
    use roc_parse::ast::Pattern;
    use roc_parse::ast::TypeAnnotation::*;
    use roc_parse::ast::TypeHeader;

    match annotation {
        Apply(module_name, ident, targs) => {
            match to_type_apply(env, scope, references, module_name, ident, targs, region) {
                TypeApply::Apply(symbol, args) => {
                    references.symbols.insert(symbol);
                    Type2::Apply(symbol, args)
                }
                TypeApply::Alias(symbol, args, actual) => {
                    references.symbols.insert(symbol);
                    Type2::Alias(symbol, args, actual)
                }
                TypeApply::Erroneous => {
                    // Type2::Erroneous
                    todo!()
                }
            }
        }
        Function(argument_types, return_type) => {
            let arguments = PoolVec::with_capacity(argument_types.len() as u32, env.pool);

            for (type_id, loc_arg) in arguments.iter_node_ids().zip(argument_types.iter()) {
                as_type_id(
                    env,
                    scope,
                    references,
                    type_id,
                    &loc_arg.value,
                    loc_arg.region,
                );
            }

            let return_type_id = to_type_id(
                env,
                scope,
                references,
                &return_type.value,
                return_type.region,
            );

            let closure_type = Type2::Variable(env.var_store.fresh());
            let closure_type_id = env.pool.add(closure_type);

            Type2::Function(arguments, closure_type_id, return_type_id)
        }
        BoundVariable(v) => {
            // A rigid type variable. The parser should have already ensured that the name is indeed a lowercase.
            let v = Lowercase::from(*v);
            match references.named.get(&v) {
                Some(var) => Type2::Variable(*var),
                None => {
                    let var = env.var_store.fresh();

                    references.named.insert(v, var);

                    Type2::Variable(var)
                }
            }
        }
        Inferred => {
            let var = env.var_store.fresh();

            Type2::Variable(var)
        }
        Wildcard | Malformed(_) => {
            let var = env.var_store.fresh();

            references.unnamed.insert(var);

            Type2::Variable(var)
        }
        Tuple { fields: _, ext: _ } => {
            todo!("tuple type");
        }
        Record { fields, ext, .. } => {
            let field_types_map =
                can_assigned_fields(env, scope, references, &fields.items, region);

            let field_types = PoolVec::with_capacity(field_types_map.len() as u32, env.pool);

            for (node_id, (label, field)) in field_types.iter_node_ids().zip(field_types_map) {
                let poolstr = PoolStr::new(label.as_str(), env.pool);

                let rec_field = field.map_owned(|field| env.pool.add(field));
                env.pool[node_id] = (poolstr, rec_field);
            }

            let ext_type = match ext {
                Some(loc_ann) => to_type_id(env, scope, references, &loc_ann.value, region),
                None => env.add(Type2::EmptyRec, region),
            };

            Type2::Record(field_types, ext_type)
        }
        TagUnion { tags, ext, .. } => {
            let tag_types_vec = can_tags(env, scope, references, tags.items, region);

            let tag_types = PoolVec::with_capacity(tag_types_vec.len() as u32, env.pool);

            for (node_id, (tag_name, field)) in tag_types.iter_node_ids().zip(tag_types_vec) {
                env.pool[node_id] = (tag_name, field);
            }

            let ext_type = match ext {
                Some(loc_ann) => to_type_id(env, scope, references, &loc_ann.value, region),
                None => env.add(Type2::EmptyTagUnion, region),
            };

            Type2::TagUnion(tag_types, ext_type)
        }
        As(
            loc_inner,
            _spaces,
            TypeHeader {
                name,
                vars: loc_vars,
            },
        ) => {
            // e.g. `{ x : Int, y : Int } as Point`
            let symbol = match scope.introduce(
                name.value.into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                region,
            ) {
                Ok(symbol) => symbol,

                Err((_original_region, _shadow)) => {
                    // let problem = Problem2::Shadowed(original_region, shadow.clone());

                    // env.problem(roc_problem::can::Problem::ShadowingInAnnotation {
                    //     original_region,
                    //     shadow,
                    // });

                    // return Type2::Erroneous(problem);
                    todo!();
                }
            };

            let inner_type = to_type2(env, scope, references, &loc_inner.value, region);
            let vars = PoolVec::with_capacity(loc_vars.len() as u32, env.pool);

            let lowercase_vars = PoolVec::with_capacity(loc_vars.len() as u32, env.pool);

            for ((loc_var, named_id), var_id) in loc_vars
                .iter()
                .zip(lowercase_vars.iter_node_ids())
                .zip(vars.iter_node_ids())
            {
                let var = match loc_var.value {
                    Pattern::Identifier(name) if name.chars().next().unwrap().is_lowercase() => {
                        name
                    }
                    _ => unreachable!("I thought this was validated during parsing"),
                };
                let var_name = Lowercase::from(var);

                if let Some(var) = references.named.get(&var_name) {
                    let poolstr = PoolStr::new(var_name.as_str(), env.pool);

                    let type_id = env.pool.add(Type2::Variable(*var));
                    env.pool[var_id] = (poolstr.shallow_clone(), type_id);

                    env.pool[named_id] = (poolstr, *var);
                    env.set_region(named_id, loc_var.region);
                } else {
                    let var = env.var_store.fresh();

                    references.named.insert(var_name.clone(), var);
                    let poolstr = PoolStr::new(var_name.as_str(), env.pool);

                    let type_id = env.pool.add(Type2::Variable(var));
                    env.pool[var_id] = (poolstr.shallow_clone(), type_id);

                    env.pool[named_id] = (poolstr, var);
                    env.set_region(named_id, loc_var.region);
                }
            }

            let alias_actual = inner_type;
            // TODO instantiate recursive tag union
            //                    let alias_actual = if let Type2::TagUnion(tags, ext) = inner_type {
            //                        let rec_var = env.var_store.fresh();
            //
            //                        let mut new_tags = Vec::with_capacity(tags.len());
            //                        for (tag_name, args) in tags {
            //                            let mut new_args = Vec::with_capacity(args.len());
            //                            for arg in args {
            //                                let mut new_arg = arg.clone();
            //                                new_arg.substitute_alias(symbol, &Type2::Variable(rec_var));
            //                                new_args.push(new_arg);
            //                            }
            //                            new_tags.push((tag_name.clone(), new_args));
            //                        }
            //                        Type2::RecursiveTagUnion(rec_var, new_tags, ext)
            //                    } else {
            //                        inner_type
            //                    };

            let mut hidden_variables = MutSet::default();
            hidden_variables.extend(alias_actual.variables(env.pool));

            for (_, var) in lowercase_vars.iter(env.pool) {
                hidden_variables.remove(var);
            }

            let alias_actual_id = env.pool.add(alias_actual);
            scope.add_alias(env.pool, symbol, lowercase_vars, alias_actual_id);

            let alias = scope.lookup_alias(symbol).unwrap();
            // local_aliases.insert(symbol, alias.clone());

            // TODO host-exposed
            //                    if vars.is_empty() && env.home == symbol.module_id() {
            //                        let actual_var = env.var_store.fresh();
            //                        rigids.host_exposed.insert(symbol, actual_var);
            //                        Type::HostExposedAlias {
            //                            name: symbol,
            //                            arguments: vars,
            //                            actual: Box::new(alias.typ.clone()),
            //                            actual_var,
            //                        }
            //                    } else {
            //                        Type::Alias(symbol, vars, Box::new(alias.typ.clone()))
            //                    }
            Type2::AsAlias(symbol, vars, alias.actual)
        }
        Where { .. } => todo_abilities!(),
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
            to_type2(env, scope, references, nested, region)
        }
    }
}

// TODO trim down these arguments!
#[allow(clippy::too_many_arguments)]
fn can_assigned_fields<'a>(
    env: &mut Env,
    scope: &mut Scope,
    rigids: &mut References,
    fields: &&[Loc<roc_parse::ast::AssignedField<'a, roc_parse::ast::TypeAnnotation<'a>>>],
    region: Region,
) -> MutMap<Lowercase, RecordField<Type2>> {
    use roc_parse::ast::AssignedField::*;
    use roc_types::types::RecordField::*;

    // SendMap doesn't have a `with_capacity`
    let mut field_types = MutMap::default();

    // field names we've seen so far in this record
    let mut seen = std::collections::HashMap::with_capacity(fields.len());

    'outer: for loc_field in fields.iter() {
        let mut field = &loc_field.value;

        // use this inner loop to unwrap the SpaceAfter/SpaceBefore
        // when we find the name of this field, break out of the loop
        // with that value, so we can check whether the field name is
        // a duplicate
        let new_name = 'inner: loop {
            match field {
                RequiredValue(field_name, _, annotation) => {
                    let field_type =
                        to_type2(env, scope, rigids, &annotation.value, annotation.region);

                    let label = Lowercase::from(field_name.value);
                    field_types.insert(label.clone(), Required(field_type));

                    break 'inner label;
                }
                OptionalValue(field_name, _, annotation) => {
                    let field_type =
                        to_type2(env, scope, rigids, &annotation.value, annotation.region);

                    let label = Lowercase::from(field_name.value);
                    field_types.insert(label.clone(), Optional(field_type));

                    break 'inner label;
                }
                LabelOnly(loc_field_name) => {
                    // Interpret { a, b } as { a : a, b : b }
                    let field_name = Lowercase::from(loc_field_name.value);
                    let field_type = {
                        if let Some(var) = rigids.named.get(&field_name) {
                            Type2::Variable(*var)
                        } else {
                            let field_var = env.var_store.fresh();
                            rigids.named.insert(field_name.clone(), field_var);
                            Type2::Variable(field_var)
                        }
                    };

                    field_types.insert(field_name.clone(), Required(field_type));

                    break 'inner field_name;
                }
                SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
                    // check the nested field instead
                    field = nested;
                    continue 'inner;
                }
                Malformed(_) => {
                    // TODO report this?
                    // completely skip this element, advance to the next tag
                    continue 'outer;
                }
            }
        };

        // ensure that the new name is not already in this record:
        // note that the right-most tag wins when there are two with the same name
        if let Some(replaced_region) = seen.insert(new_name.clone(), loc_field.region) {
            env.problem(roc_problem::can::Problem::DuplicateRecordFieldType {
                field_name: new_name.into(),
                record_region: region,
                field_region: loc_field.region,
                replaced_region,
            });
        }
    }

    field_types
}

fn can_tags<'a>(
    env: &mut Env,
    scope: &mut Scope,
    rigids: &mut References,
    tags: &'a [Loc<roc_parse::ast::Tag<'a>>],
    region: Region,
) -> Vec<(TagName, PoolVec<Type2>)> {
    use roc_parse::ast::Tag;
    let mut tag_types = Vec::with_capacity(tags.len());

    // tag names we've seen so far in this tag union
    let mut seen = std::collections::HashMap::with_capacity(tags.len());

    'outer: for loc_tag in tags.iter() {
        let mut tag = &loc_tag.value;

        // use this inner loop to unwrap the SpaceAfter/SpaceBefore
        // when we find the name of this tag, break out of the loop
        // with that value, so we can check whether the tag name is
        // a duplicate
        let new_name = 'inner: loop {
            match tag {
                Tag::Apply { name, args } => {
                    let arg_types = PoolVec::with_capacity(args.len() as u32, env.pool);

                    for (type_id, loc_arg) in arg_types.iter_node_ids().zip(args.iter()) {
                        as_type_id(env, scope, rigids, type_id, &loc_arg.value, loc_arg.region);
                    }

                    let tag_name = TagName(name.value.into());
                    tag_types.push((tag_name.clone(), arg_types));

                    break 'inner tag_name;
                }
                Tag::SpaceBefore(nested, _) | Tag::SpaceAfter(nested, _) => {
                    // check the nested tag instead
                    tag = nested;
                    continue 'inner;
                }
                Tag::Malformed(_) => {
                    // TODO report this?
                    // completely skip this element, advance to the next tag
                    continue 'outer;
                }
            }
        };

        // ensure that the new name is not already in this tag union:
        // note that the right-most tag wins when there are two with the same name
        if let Some(replaced_region) = seen.insert(new_name.clone(), loc_tag.region) {
            env.problem(roc_problem::can::Problem::DuplicateTag {
                tag_region: loc_tag.region,
                tag_union_region: region,
                replaced_region,
                tag_name: new_name,
            });
        }
    }

    tag_types
}

enum TypeApply {
    Apply(Symbol, PoolVec<Type2>),
    Alias(Symbol, PoolVec<TypeId>, TypeId),
    Erroneous,
}

#[inline(always)]
fn to_type_apply<'a>(
    env: &mut Env,
    scope: &mut Scope,
    rigids: &mut References,
    module_name: &str,
    ident: &str,
    type_arguments: &[Loc<roc_parse::ast::TypeAnnotation<'a>>],
    region: Region,
) -> TypeApply {
    let symbol = if module_name.is_empty() {
        // Since module_name was empty, this is an unqualified type.
        // Look it up in scope!
        let ident: Ident = (*ident).into();

        match scope.lookup(&ident, region) {
            Ok(symbol) => symbol,
            Err(problem) => {
                env.problem(roc_problem::can::Problem::RuntimeError(problem));

                return TypeApply::Erroneous;
            }
        }
    } else {
        match env.qualified_lookup(module_name, ident, region) {
            Ok(symbol) => symbol,
            Err(problem) => {
                // Either the module wasn't imported, or
                // it was imported but it doesn't expose this ident.
                env.problem(roc_problem::can::Problem::RuntimeError(problem));

                return TypeApply::Erroneous;
            }
        }
    };

    let argument_type_ids = PoolVec::with_capacity(type_arguments.len() as u32, env.pool);

    for (type_id, loc_arg) in argument_type_ids.iter_node_ids().zip(type_arguments.iter()) {
        as_type_id(env, scope, rigids, type_id, &loc_arg.value, loc_arg.region);
    }

    let args = type_arguments;
    let opt_alias = scope.lookup_alias(symbol);
    match opt_alias {
        Some(ref alias) => {
            // use a known alias
            let actual = alias.actual;
            let mut substitutions: MutMap<Variable, TypeId> = MutMap::default();

            if alias.targs.len() != args.len() {
                return TypeApply::Erroneous;
            }

            let arguments = PoolVec::with_capacity(type_arguments.len() as u32, env.pool);

            let it = arguments.iter_node_ids().zip(
                argument_type_ids
                    .iter_node_ids()
                    .zip(alias.targs.iter_node_ids()),
            );

            for (node_id, (type_id, loc_var_id)) in it {
                let loc_var = &env.pool[loc_var_id];
                let name = loc_var.0.shallow_clone();
                let var = loc_var.1;

                env.pool[node_id] = (name, type_id);

                substitutions.insert(var, type_id);
            }

            // make sure the recursion variable is freshly instantiated
            // have to allocate these outside of the if for lifetime reasons...
            let new = env.var_store.fresh();
            let fresh = env.pool.add(Type2::Variable(new));
            if let Type2::RecursiveTagUnion(rvar, ref tags, ext) = &mut env.pool[actual] {
                substitutions.insert(*rvar, fresh);

                env.pool[actual] = Type2::RecursiveTagUnion(new, tags.shallow_clone(), *ext);
            }

            // make sure hidden variables are freshly instantiated
            for var_id in alias.hidden_variables.iter_node_ids() {
                let var = env.pool[var_id];
                let fresh = env.pool.add(Type2::Variable(env.var_store.fresh()));
                substitutions.insert(var, fresh);
            }

            // instantiate variables
            Type2::substitute(env.pool, &substitutions, actual);

            let type_arguments = PoolVec::with_capacity(arguments.len() as u32, env.pool);

            for (node_id, type_id) in arguments
                .iter_node_ids()
                .zip(type_arguments.iter_node_ids())
            {
                let typ = env.pool[node_id].1;
                env.pool[type_id] = typ;
            }

            TypeApply::Alias(symbol, type_arguments, actual)
        }
        None => TypeApply::Apply(symbol, argument_type_ids),
    }
}

#[derive(Debug)]
pub struct Alias {
    pub targs: PoolVec<(PoolStr, Variable)>,
    pub actual: TypeId,

    /// hidden type variables, like the closure variable in `a -> b`
    pub hidden_variables: PoolVec<Variable>,
}

impl ShallowClone for Alias {
    fn shallow_clone(&self) -> Self {
        Self {
            targs: self.targs.shallow_clone(),
            hidden_variables: self.hidden_variables.shallow_clone(),
            actual: self.actual,
        }
    }
}
