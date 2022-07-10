use crate::subs::{VarId, VarStore, Variable};
use crate::types::{AliasKind, OptAbleType, Problem, RecordField, Type, TypeExtension};
use roc_collections::all::{ImMap, SendMap};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};

/// A marker that a given Subs has been solved.
/// The only way to obtain a Solved<Subs> is by running the solver on it.
#[derive(Clone, Debug)]
pub struct Solved<T>(pub T);

impl<T> Solved<T> {
    pub fn inner(&self) -> &'_ T {
        &self.0
    }

    pub fn inner_mut(&mut self) -> &'_ mut T {
        &mut self.0
    }

    pub fn into_inner(self) -> T {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct SolvedLambdaSet(pub SolvedType);

/// This is a fully solved type, with no Variables remaining in it.
#[derive(Debug, Clone)]
pub enum SolvedType {
    /// A function. The types of its arguments, then the type of its return value.
    Func(Vec<SolvedType>, Box<SolvedType>, Box<SolvedType>),
    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply(Symbol, Vec<SolvedType>),
    /// A bound type variable, e.g. `a` in `(a -> a)`
    Rigid(Lowercase),
    Flex(VarId),
    Wildcard,
    /// Inline type alias, e.g. `as List a` in `[Cons a (List a), Nil] as List a`
    Record {
        fields: Vec<(Lowercase, RecordField<SolvedType>)>,
        /// The row type variable in an open record, e.g. the `r` in `{ name: Str }r`.
        /// This is None if it's a closed record annotation like `{ name: Str }`.
        ext: Box<SolvedType>,
    },
    EmptyRecord,
    TagUnion(Vec<(TagName, Vec<SolvedType>)>, Box<SolvedType>),
    LambdaTag(Symbol, Vec<SolvedType>),
    FunctionOrTagUnion(TagName, Symbol, Box<SolvedType>),
    RecursiveTagUnion(VarId, Vec<(TagName, Vec<SolvedType>)>, Box<SolvedType>),
    EmptyTagUnion,
    /// A type from an Invalid module
    Erroneous(Problem),

    Alias(
        Symbol,
        Vec<SolvedType>,
        Vec<SolvedLambdaSet>,
        Box<SolvedType>,
        AliasKind,
    ),

    HostExposedAlias {
        name: Symbol,
        arguments: Vec<SolvedType>,
        lambda_set_variables: Vec<SolvedLambdaSet>,
        actual_var: VarId,
        actual: Box<SolvedType>,
    },

    /// A type error
    Error,
}

#[derive(Clone, Debug)]
pub struct BuiltinAlias {
    pub region: Region,
    pub vars: Vec<Loc<Lowercase>>,
    pub typ: SolvedType,
    pub kind: AliasKind,
}

#[derive(Debug, Clone, Default)]
pub struct FreeVars {
    pub named_vars: ImMap<Lowercase, Variable>,
    pub unnamed_vars: ImMap<VarId, Variable>,
    pub wildcards: Vec<Variable>,
}

pub fn to_type(
    solved_type: &SolvedType,
    free_vars: &mut FreeVars,
    var_store: &mut VarStore,
) -> Type {
    use SolvedType::*;

    match solved_type {
        Func(args, closure, ret) => {
            let mut new_args = Vec::with_capacity(args.len());

            for arg in args {
                new_args.push(to_type(arg, free_vars, var_store));
            }

            let new_ret = to_type(ret, free_vars, var_store);
            let new_closure = to_type(closure, free_vars, var_store);

            Type::Function(new_args, Box::new(new_closure), Box::new(new_ret))
        }
        Apply(symbol, args) => {
            let mut new_args = Vec::with_capacity(args.len());

            for arg in args {
                new_args.push(to_type(arg, free_vars, var_store));
            }

            Type::Apply(*symbol, new_args, Region::zero())
        }
        Rigid(lowercase) => {
            if let Some(var) = free_vars.named_vars.get(lowercase) {
                Type::Variable(*var)
            } else {
                let var = var_store.fresh();
                free_vars.named_vars.insert(lowercase.clone(), var);
                Type::Variable(var)
            }
        }
        Flex(var_id) => Type::Variable(var_id_to_flex_var(*var_id, free_vars, var_store)),
        Wildcard => {
            let var = var_store.fresh();
            free_vars.wildcards.push(var);
            Type::Variable(var)
        }
        Record { fields, ext } => {
            use RecordField::*;

            let mut new_fields = SendMap::default();

            for (label, field) in fields {
                let field_val = match field {
                    Required(typ) => Required(to_type(typ, free_vars, var_store)),
                    Optional(typ) => Optional(to_type(typ, free_vars, var_store)),
                    Demanded(typ) => Demanded(to_type(typ, free_vars, var_store)),
                };

                new_fields.insert(label.clone(), field_val);
            }

            let ext = match ext.as_ref() {
                SolvedType::EmptyRecord => TypeExtension::Closed,
                other => TypeExtension::Open(Box::new(to_type(other, free_vars, var_store))),
            };

            Type::Record(new_fields, ext)
        }
        EmptyRecord => Type::EmptyRec,
        EmptyTagUnion => Type::EmptyTagUnion,
        TagUnion(tags, ext) => {
            let mut new_tags = Vec::with_capacity(tags.len());

            for (tag_name, args) in tags {
                let mut new_args = Vec::with_capacity(args.len());

                for arg in args.iter() {
                    new_args.push(to_type(arg, free_vars, var_store));
                }

                new_tags.push((tag_name.clone(), new_args));
            }

            let ext = match ext.as_ref() {
                SolvedType::EmptyTagUnion => TypeExtension::Closed,
                other => TypeExtension::Open(Box::new(to_type(other, free_vars, var_store))),
            };

            Type::TagUnion(new_tags, ext)
        }
        LambdaTag(name, args) => {
            let mut new_args = Vec::with_capacity(args.len());

            for arg in args.iter() {
                new_args.push(to_type(arg, free_vars, var_store));
            }

            Type::ClosureTag {
                name: *name,
                captures: new_args,
                ambient_function: var_store.fresh(),
            }
        }
        FunctionOrTagUnion(tag_name, symbol, ext) => {
            let ext = match ext.as_ref() {
                SolvedType::EmptyTagUnion => TypeExtension::Closed,
                other => TypeExtension::Open(Box::new(to_type(other, free_vars, var_store))),
            };

            Type::FunctionOrTagUnion(tag_name.clone(), *symbol, ext)
        }
        RecursiveTagUnion(rec_var_id, tags, ext) => {
            let mut new_tags = Vec::with_capacity(tags.len());

            for (tag_name, args) in tags {
                let mut new_args = Vec::with_capacity(args.len());

                for arg in args.iter() {
                    new_args.push(to_type(arg, free_vars, var_store));
                }

                new_tags.push((tag_name.clone(), new_args));
            }

            let ext = match ext.as_ref() {
                SolvedType::EmptyTagUnion => TypeExtension::Closed,
                other => TypeExtension::Open(Box::new(to_type(other, free_vars, var_store))),
            };

            let rec_var = free_vars
                .unnamed_vars
                .get(rec_var_id)
                .expect("rec var not in unnamed vars");

            Type::RecursiveTagUnion(*rec_var, new_tags, ext)
        }
        Alias(symbol, solved_type_variables, solved_lambda_sets, solved_actual, kind) => {
            let mut type_variables = Vec::with_capacity(solved_type_variables.len());

            for solved_arg in solved_type_variables {
                type_variables.push(OptAbleType {
                    typ: to_type(solved_arg, free_vars, var_store),
                    // TODO: is this always correct?
                    opt_ability: None,
                });
            }

            let mut lambda_set_variables = Vec::with_capacity(solved_lambda_sets.len());
            for solved_set in solved_lambda_sets {
                lambda_set_variables.push(crate::types::LambdaSet(to_type(
                    &solved_set.0,
                    free_vars,
                    var_store,
                )))
            }

            let actual = to_type(solved_actual, free_vars, var_store);

            Type::Alias {
                symbol: *symbol,
                type_arguments: type_variables,
                lambda_set_variables,
                actual: Box::new(actual),
                kind: *kind,
            }
        }
        HostExposedAlias {
            name,
            arguments: solved_type_variables,
            lambda_set_variables: solved_lambda_sets,
            actual_var,
            actual: solved_actual,
        } => {
            let mut type_variables = Vec::with_capacity(solved_type_variables.len());

            for solved_arg in solved_type_variables {
                type_variables.push(to_type(solved_arg, free_vars, var_store));
            }

            let mut lambda_set_variables = Vec::with_capacity(solved_lambda_sets.len());
            for solved_set in solved_lambda_sets {
                lambda_set_variables.push(crate::types::LambdaSet(to_type(
                    &solved_set.0,
                    free_vars,
                    var_store,
                )))
            }

            let actual = to_type(solved_actual, free_vars, var_store);

            Type::HostExposedAlias {
                name: *name,
                type_arguments: type_variables,
                lambda_set_variables,
                actual_var: var_id_to_flex_var(*actual_var, free_vars, var_store),
                actual: Box::new(actual),
            }
        }
        Error => Type::Erroneous(Problem::SolvedTypeError),
        Erroneous(problem) => Type::Erroneous(problem.clone()),
    }
}

fn var_id_to_flex_var(
    var_id: VarId,
    free_vars: &mut FreeVars,
    var_store: &mut VarStore,
) -> Variable {
    if let Some(var) = free_vars.unnamed_vars.get(&var_id) {
        *var
    } else {
        let var = var_store.fresh();
        free_vars.unnamed_vars.insert(var_id, var);

        var
    }
}
