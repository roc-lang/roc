use crate::expr::Env;
use crate::pool::{NodeId, Pool, PoolStr, PoolVec};
use crate::scope::Scope;
// use roc_can::expr::Output;
use roc_collections::all::{MutMap, MutSet};
use roc_module::ident::Ident;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::subs::Variable;
use roc_types::types::{Problem, RecordField};

pub type TypeId = NodeId<Type2>;

#[derive(Debug)]
pub enum Type2 {
    Variable(Variable),

    Alias(Symbol, PoolVec<(PoolStr, TypeId)>, TypeId), // 20B = 8B + 8B + 4B
    HostExposedAlias {
        name: Symbol,                          // 8B
        arguments: PoolVec<(PoolStr, TypeId)>, // 8B
        actual_var: Variable,                  // 4B
        actual: TypeId,                        // 4B
    },

    EmptyTagUnion,
    TagUnion(PoolVec<(PoolStr, PoolVec<Type2>)>, TypeId),
    RecursiveTagUnion(Variable, PoolVec<(PoolStr, PoolVec<Type2>)>, TypeId),

    EmptyRec,
    Record(PoolVec<(PoolStr, RecordField<Type2>)>, TypeId),

    Function(PoolVec<Type2>, TypeId, TypeId), // 16B = 8B + 4B + 4B
    Apply(Symbol, PoolVec<Type2>),            // 16B = 8B + 8B

    Erroneous(roc_types::types::Problem),
}

impl Type2 {
    fn substitute(_pool: &mut Pool, _subs: &MutMap<Variable, TypeId>, _type_id: TypeId) {
        todo!()
    }
    pub fn variables(&self, _pool: &mut Pool) -> MutSet<Variable> {
        todo!()
    }
}

impl NodeId<Type2> {
    pub fn variables(&self, _pool: &mut Pool) -> MutSet<Variable> {
        todo!()
    }
}

/// A temporary data structure to return a bunch of values to Def construction
pub enum Annotation2<'a> {
    FunctionWithAliases {
        annotation: Type2,
        arguments: PoolVec<Type2>,
        closure_type_id: TypeId,
        return_type_id: TypeId,
        named_rigids: MutMap<&'a str, Variable>,
        unnamed_rigids: MutSet<Variable>,
    },
    Function {
        arguments: PoolVec<Type2>,
        closure_type_id: TypeId,
        return_type_id: TypeId,
        named_rigids: MutMap<&'a str, Variable>,
        unnamed_rigids: MutSet<Variable>,
    },
    Value {
        annotation: Type2,
        named_rigids: MutMap<&'a str, Variable>,
        unnamed_rigids: MutSet<Variable>,
    },
    Erroneous(roc_types::types::Problem),
}

pub fn to_annotation2<'a>(
    env: &mut Env,
    scope: &mut Scope,
    annotation: &'a roc_parse::ast::TypeAnnotation<'a>,
    region: Region,
) -> Annotation2<'a> {
    let mut rigids = Rigids::default();

    let annotation = to_type2(env, scope, &mut rigids, annotation, region);

    // we dealias until we hit a non-alias, then we either hit a function type (and produce a
    // function annotation) or anything else (and produce a value annotation)
    match annotation {
        Type2::Function(arguments, closure_type_id, return_type_id) => {
            let Rigids { named, unnamed, .. } = rigids;

            Annotation2::Function {
                arguments,
                closure_type_id,
                return_type_id,
                named_rigids: named,
                unnamed_rigids: unnamed,
            }
        }
        Type2::Alias(_, _, _) => {
            // most of the time, the annotation is not an alias, so this case is comparatively
            // less efficient
            shallow_dealias(env, rigids, annotation)
        }
        _ => {
            let Rigids { named, unnamed, .. } = rigids;

            return Annotation2::Value {
                annotation,
                named_rigids: named,
                unnamed_rigids: unnamed,
            };
        }
    }
}

fn shallow_dealias<'a>(env: &mut Env, rigids: Rigids<'a>, annotation: Type2) -> Annotation2<'a> {
    let Rigids { named, unnamed, .. } = rigids;
    let mut inner = &annotation;

    loop {
        match inner {
            Type2::Alias(_, _, actual) => {
                inner = env.pool.get(*actual);
            }
            Type2::Function(arguments, closure_type_id, return_type_id) => {
                return Annotation2::FunctionWithAliases {
                    arguments: arguments.duplicate(),
                    closure_type_id: *closure_type_id,
                    return_type_id: *return_type_id,
                    named_rigids: named,
                    unnamed_rigids: unnamed,
                    annotation,
                };
            }
            _ => {
                return Annotation2::Value {
                    annotation,
                    named_rigids: named,
                    unnamed_rigids: unnamed,
                }
            }
        }
    }
}

#[derive(Default)]
struct Rigids<'a> {
    named: MutMap<&'a str, Variable>,
    unnamed: MutSet<Variable>,
    hidden: MutSet<Variable>,
}

pub fn to_type_id(
    env: &mut Env,
    scope: &mut Scope,
    rigids: &mut Rigids,
    annotation: &roc_parse::ast::TypeAnnotation,
    region: Region,
) -> TypeId {
    let type2 = to_type2(env, scope, rigids, annotation, region);

    env.add(type2, region)
}

pub fn as_type_id(
    env: &mut Env,
    scope: &mut Scope,
    rigids: &mut Rigids,
    type_id: TypeId,
    annotation: &roc_parse::ast::TypeAnnotation,
    region: Region,
) {
    let type2 = to_type2(env, scope, rigids, annotation, region);

    env.pool[type_id] = type2;
    env.set_region(type_id, region);
}

pub fn to_type2(
    env: &mut Env,
    scope: &mut Scope,
    rigids: &mut Rigids,
    annotation: &roc_parse::ast::TypeAnnotation,
    region: Region,
) -> Type2 {
    use roc_parse::ast::TypeAnnotation::*;

    match annotation {
        Apply(module_name, ident, targs) => {
            match to_type_apply(env, scope, rigids, module_name, ident, targs, region) {
                TypeApply::Apply(symbol, args) => Type2::Apply(symbol, args),
                TypeApply::Alias(symbol, args, actual) => Type2::Alias(symbol, args, actual),
                TypeApply::Erroneous(problem) => Type2::Erroneous(problem),
            }
        }
        Function(argument_types, return_type) => {
            let arguments = PoolVec::with_capacity(argument_types.len() as u32, env.pool);

            for (type_id, loc_arg) in arguments.iter_node_ids().zip(argument_types.iter()) {
                as_type_id(env, scope, rigids, type_id, &loc_arg.value, loc_arg.region);
            }

            let return_type_id =
                to_type_id(env, scope, rigids, &return_type.value, return_type.region);

            let closure_type = Type2::Variable(env.var_store.fresh());
            let closure_type_id = env.pool.add(closure_type);

            Type2::Function(arguments, closure_type_id, return_type_id)
        }
    }
}

enum TypeApply {
    Apply(Symbol, PoolVec<Type2>),
    Alias(Symbol, PoolVec<(PoolStr, TypeId)>, TypeId),
    Erroneous(roc_types::types::Problem),
}

#[inline(always)]
fn to_type_apply(
    env: &mut Env,
    scope: &mut Scope,
    rigids: &mut Rigids,
    module_name: &str,
    ident: &str,
    type_arguments: &[Located<roc_parse::ast::TypeAnnotation>],
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

                return TypeApply::Erroneous(Problem::UnrecognizedIdent(ident.into()));
            }
        }
    } else {
        match env.qualified_lookup(module_name, ident, region) {
            Ok(symbol) => symbol,
            Err(problem) => {
                // Either the module wasn't imported, or
                // it was imported but it doesn't expose this ident.
                env.problem(roc_problem::can::Problem::RuntimeError(problem));

                return TypeApply::Erroneous(Problem::UnrecognizedIdent((*ident).into()));
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
                let error = TypeApply::Erroneous(Problem::BadTypeArguments {
                    symbol,
                    region,
                    alias_needs: alias.targs.len() as u8,
                    type_got: args.len() as u8,
                });
                return error;
            }

            let arguments = PoolVec::with_capacity(type_arguments.len() as u32, env.pool);

            let it = arguments.iter_node_ids().zip(
                argument_type_ids
                    .iter_node_ids()
                    .zip(alias.targs.iter_node_ids()),
            );

            for (node_id, (type_id, loc_var_id)) in it {
                let loc_var = &env.pool[loc_var_id];
                let name = loc_var.0.duplicate();
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

                env.pool[actual] = Type2::RecursiveTagUnion(new, tags.duplicate(), *ext);
            }

            // make sure hidden variables are freshly instantiated
            for var_id in alias.hidden_variables.iter_node_ids() {
                let var = env.pool[var_id];
                let fresh = env.pool.add(Type2::Variable(env.var_store.fresh()));
                substitutions.insert(var, fresh);
            }

            // instantiate variables
            Type2::substitute(env.pool, &substitutions, actual);

            TypeApply::Alias(symbol, arguments, actual)
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
