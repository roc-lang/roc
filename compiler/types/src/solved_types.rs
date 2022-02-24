use crate::subs::{FlatType, GetSubsSlice, Subs, VarId, VarStore, Variable};
use crate::types::{AliasKind, Problem, RecordField, Type};
use roc_collections::all::{ImMap, MutSet, SendMap};
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
    /// Inline type alias, e.g. `as List a` in `[ Cons a (List a), Nil ] as List a`
    Record {
        fields: Vec<(Lowercase, RecordField<SolvedType>)>,
        /// The row type variable in an open record, e.g. the `r` in `{ name: Str }r`.
        /// This is None if it's a closed record annotation like `{ name: Str }`.
        ext: Box<SolvedType>,
    },
    EmptyRecord,
    TagUnion(Vec<(TagName, Vec<SolvedType>)>, Box<SolvedType>),
    FunctionOrTagUnion(TagName, Symbol, Box<SolvedType>),
    RecursiveTagUnion(VarId, Vec<(TagName, Vec<SolvedType>)>, Box<SolvedType>),
    EmptyTagUnion,
    /// A type from an Invalid module
    Erroneous(Problem),

    /// A type alias
    /// TODO transmit lambda sets!
    Alias(
        Symbol,
        Vec<(Lowercase, SolvedType)>,
        Vec<SolvedLambdaSet>,
        Box<SolvedType>,
    ),

    HostExposedAlias {
        name: Symbol,
        arguments: Vec<(Lowercase, SolvedType)>,
        lambda_set_variables: Vec<SolvedLambdaSet>,
        actual_var: VarId,
        actual: Box<SolvedType>,
    },

    /// A type error
    Error,
}

impl SolvedType {
    pub fn new(solved_subs: &Solved<Subs>, var: Variable) -> Self {
        Self::from_var(solved_subs.inner(), var)
    }

    pub fn from_type(solved_subs: &Solved<Subs>, typ: &Type) -> Self {
        use crate::types::Type::*;

        match typ {
            EmptyRec => SolvedType::EmptyRecord,
            EmptyTagUnion => SolvedType::EmptyTagUnion,
            Apply(symbol, types, _) => {
                let mut solved_types = Vec::with_capacity(types.len());

                for typ in types {
                    let solved_type = Self::from_type(solved_subs, typ);

                    solved_types.push(solved_type);
                }

                SolvedType::Apply(*symbol, solved_types)
            }
            Function(args, box_closure, box_ret) => {
                let solved_ret = Self::from_type(solved_subs, box_ret);
                let solved_closure = Self::from_type(solved_subs, box_closure);
                let mut solved_args = Vec::with_capacity(args.len());

                for arg in args {
                    let solved_arg = Self::from_type(solved_subs, arg);

                    solved_args.push(solved_arg);
                }

                SolvedType::Func(solved_args, Box::new(solved_closure), Box::new(solved_ret))
            }
            Record(fields, box_ext) => {
                let solved_ext = Self::from_type(solved_subs, box_ext);
                let mut solved_fields = Vec::with_capacity(fields.len());

                for (label, field) in fields {
                    use crate::types::RecordField::*;

                    let solved_type = match field {
                        Optional(typ) => RecordField::Optional(Self::from_type(solved_subs, typ)),
                        Required(typ) => RecordField::Required(Self::from_type(solved_subs, typ)),
                        Demanded(typ) => RecordField::Demanded(Self::from_type(solved_subs, typ)),
                    };

                    solved_fields.push((label.clone(), solved_type));
                }

                SolvedType::Record {
                    fields: solved_fields,
                    ext: Box::new(solved_ext),
                }
            }
            ClosureTag { name, ext } => {
                let solved_ext = Self::from_type(solved_subs, &Type::Variable(*ext));
                let solved_tags = vec![(TagName::Closure(*name), vec![])];
                SolvedType::TagUnion(solved_tags, Box::new(solved_ext))
            }
            TagUnion(tags, box_ext) => {
                let solved_ext = Self::from_type(solved_subs, box_ext);
                let mut solved_tags = Vec::with_capacity(tags.len());
                for (tag_name, types) in tags {
                    let mut solved_types = Vec::with_capacity(types.len());

                    for typ in types {
                        let solved_type = Self::from_type(solved_subs, typ);
                        solved_types.push(solved_type);
                    }

                    solved_tags.push((tag_name.clone(), solved_types));
                }

                SolvedType::TagUnion(solved_tags, Box::new(solved_ext))
            }
            FunctionOrTagUnion(tag_name, symbol, box_ext) => {
                let solved_ext = Self::from_type(solved_subs, box_ext);
                SolvedType::FunctionOrTagUnion(tag_name.clone(), *symbol, Box::new(solved_ext))
            }
            RecursiveTagUnion(rec_var, tags, box_ext) => {
                let solved_ext = Self::from_type(solved_subs, box_ext);
                let mut solved_tags = Vec::with_capacity(tags.len());
                for (tag_name, types) in tags {
                    let mut solved_types = Vec::with_capacity(types.len());

                    for typ in types {
                        let solved_type = Self::from_type(solved_subs, typ);
                        solved_types.push(solved_type);
                    }

                    solved_tags.push((tag_name.clone(), solved_types));
                }

                SolvedType::RecursiveTagUnion(
                    VarId::from_var(*rec_var, solved_subs.inner()),
                    solved_tags,
                    Box::new(solved_ext),
                )
            }
            Erroneous(problem) => SolvedType::Erroneous(problem.clone()),
            Alias {
                symbol,
                type_arguments,
                lambda_set_variables,
                actual: box_type,
                ..
            } => {
                let solved_type = Self::from_type(solved_subs, box_type);
                let mut solved_args = Vec::with_capacity(type_arguments.len());

                for (name, var) in type_arguments {
                    solved_args.push((name.clone(), Self::from_type(solved_subs, var)));
                }

                let mut solved_lambda_sets = Vec::with_capacity(lambda_set_variables.len());

                for var in lambda_set_variables {
                    solved_lambda_sets.push(SolvedLambdaSet(Self::from_type(solved_subs, &var.0)));
                }

                SolvedType::Alias(
                    *symbol,
                    solved_args,
                    solved_lambda_sets,
                    Box::new(solved_type),
                )
            }
            HostExposedAlias {
                name,
                type_arguments: arguments,
                lambda_set_variables,
                actual_var,
                actual,
            } => {
                let solved_type = Self::from_type(solved_subs, actual);
                let mut solved_args = Vec::with_capacity(arguments.len());

                for (name, var) in arguments {
                    solved_args.push((name.clone(), Self::from_type(solved_subs, var)));
                }

                let mut solved_lambda_sets = Vec::with_capacity(lambda_set_variables.len());
                for var in lambda_set_variables {
                    solved_lambda_sets.push(SolvedLambdaSet(Self::from_type(solved_subs, &var.0)));
                }

                SolvedType::HostExposedAlias {
                    name: *name,
                    arguments: solved_args,
                    lambda_set_variables: solved_lambda_sets,
                    actual_var: VarId::from_var(*actual_var, solved_subs.inner()),
                    actual: Box::new(solved_type),
                }
            }
            Variable(var) => Self::from_var(solved_subs.inner(), *var),
            RangedNumber(typ, _) => Self::from_type(solved_subs, typ),
        }
    }

    fn from_var(subs: &Subs, var: Variable) -> Self {
        let mut seen = RecursionVars::default();
        Self::from_var_help(subs, &mut seen, var)
    }

    fn from_var_help(subs: &Subs, recursion_vars: &mut RecursionVars, var: Variable) -> Self {
        use crate::subs::Content::*;

        // if this is a recursion var we've seen before, just generate a Flex
        // (not doing so would have this function loop forever)
        if recursion_vars.contains(subs, var) {
            return SolvedType::Flex(VarId::from_var(var, subs));
        }

        match subs.get_content_without_compacting(var) {
            FlexVar(_) => SolvedType::Flex(VarId::from_var(var, subs)),
            RecursionVar { structure, .. } => {
                // TODO should there be a SolvedType RecursionVar variant?
                Self::from_var_help(subs, recursion_vars, *structure)
            }
            RigidVar(name) => SolvedType::Rigid(name.clone()),
            Structure(flat_type) => Self::from_flat_type(subs, recursion_vars, flat_type),
            Alias(symbol, args, actual_var) => {
                let mut new_args = Vec::with_capacity(args.len());

                for var_index in args.named_type_arguments() {
                    let arg_var = subs[var_index];

                    let node = Self::from_var_help(subs, recursion_vars, arg_var);

                    // NOTE we fake the lowercase here: the user will never get to see it anyway
                    new_args.push((Lowercase::default(), node));
                }

                let mut solved_lambda_sets = Vec::with_capacity(0);

                for var_index in args.unnamed_type_arguments() {
                    let var = subs[var_index];

                    solved_lambda_sets.push(SolvedLambdaSet(Self::from_var_help(
                        subs,
                        recursion_vars,
                        var,
                    )));
                }

                let aliased_to = Self::from_var_help(subs, recursion_vars, *actual_var);

                SolvedType::Alias(*symbol, new_args, solved_lambda_sets, Box::new(aliased_to))
            }
            RangedNumber(typ, _range_vars) => Self::from_var_help(subs, recursion_vars, *typ),
            Error => SolvedType::Error,
        }
    }

    fn from_flat_type(
        subs: &Subs,
        recursion_vars: &mut RecursionVars,
        flat_type: &FlatType,
    ) -> Self {
        use crate::subs::FlatType::*;

        match flat_type {
            Apply(symbol, args) => {
                let mut new_args = Vec::with_capacity(args.len());

                for var in subs.get_subs_slice(*args) {
                    new_args.push(Self::from_var_help(subs, recursion_vars, *var));
                }

                SolvedType::Apply(*symbol, new_args)
            }
            Func(args, closure, ret) => {
                let mut new_args = Vec::with_capacity(args.len());

                for var in subs.get_subs_slice(*args) {
                    new_args.push(Self::from_var_help(subs, recursion_vars, *var));
                }

                let ret = Self::from_var_help(subs, recursion_vars, *ret);
                let closure = Self::from_var_help(subs, recursion_vars, *closure);

                SolvedType::Func(new_args, Box::new(closure), Box::new(ret))
            }
            Record(fields, ext_var) => {
                let mut new_fields = Vec::with_capacity(fields.len());

                for (i1, i2, i3) in fields.iter_all() {
                    let field_name: Lowercase = subs[i1].clone();
                    let variable: Variable = subs[i2];
                    let record_field: RecordField<()> = subs[i3];

                    let solved_type =
                        record_field.map(|_| Self::from_var_help(subs, recursion_vars, variable));

                    new_fields.push((field_name, solved_type));
                }

                let ext = Self::from_var_help(subs, recursion_vars, *ext_var);

                SolvedType::Record {
                    fields: new_fields,
                    ext: Box::new(ext),
                }
            }
            TagUnion(tags, ext_var) => {
                let mut new_tags = Vec::with_capacity(tags.len());

                for (name_index, slice_index) in tags.iter_all() {
                    let slice = subs[slice_index];

                    let mut new_args = Vec::with_capacity(slice.len());

                    for var_index in slice {
                        let var = subs[var_index];
                        new_args.push(Self::from_var_help(subs, recursion_vars, var));
                    }
                    let tag_name = subs[name_index].clone();
                    new_tags.push((tag_name.clone(), new_args));
                }

                let ext = Self::from_var_help(subs, recursion_vars, *ext_var);

                SolvedType::TagUnion(new_tags, Box::new(ext))
            }
            FunctionOrTagUnion(tag_name, symbol, ext_var) => {
                let ext = Self::from_var_help(subs, recursion_vars, *ext_var);

                SolvedType::FunctionOrTagUnion(subs[*tag_name].clone(), *symbol, Box::new(ext))
            }
            RecursiveTagUnion(rec_var, tags, ext_var) => {
                recursion_vars.insert(subs, *rec_var);

                let mut new_tags = Vec::with_capacity(tags.len());

                for (name_index, slice_index) in tags.iter_all() {
                    let slice = subs[slice_index];

                    let mut new_args = Vec::with_capacity(slice.len());

                    for var_index in slice {
                        let var = subs[var_index];
                        new_args.push(Self::from_var_help(subs, recursion_vars, var));
                    }
                    let tag_name = subs[name_index].clone();
                    new_tags.push((tag_name.clone(), new_args));
                }

                let ext = Self::from_var_help(subs, recursion_vars, *ext_var);

                SolvedType::RecursiveTagUnion(
                    VarId::from_var(*rec_var, subs),
                    new_tags,
                    Box::new(ext),
                )
            }
            EmptyRecord => SolvedType::EmptyRecord,
            EmptyTagUnion => SolvedType::EmptyTagUnion,
            Erroneous(problem) => SolvedType::Erroneous(*problem.clone()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct BuiltinAlias {
    pub region: Region,
    pub vars: Vec<Loc<Lowercase>>,
    pub typ: SolvedType,
}

#[derive(Default)]
struct RecursionVars(MutSet<Variable>);

impl RecursionVars {
    fn contains(&self, subs: &Subs, var: Variable) -> bool {
        let var = subs.get_root_key_without_compacting(var);

        self.0.contains(&var)
    }

    fn insert(&mut self, subs: &Subs, var: Variable) {
        let var = subs.get_root_key_without_compacting(var);

        self.0.insert(var);
    }
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

            Type::Record(new_fields, Box::new(to_type(ext, free_vars, var_store)))
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

            Type::TagUnion(new_tags, Box::new(to_type(ext, free_vars, var_store)))
        }
        FunctionOrTagUnion(tag_name, symbol, ext) => Type::FunctionOrTagUnion(
            tag_name.clone(),
            *symbol,
            Box::new(to_type(ext, free_vars, var_store)),
        ),
        RecursiveTagUnion(rec_var_id, tags, ext) => {
            let mut new_tags = Vec::with_capacity(tags.len());

            for (tag_name, args) in tags {
                let mut new_args = Vec::with_capacity(args.len());

                for arg in args.iter() {
                    new_args.push(to_type(arg, free_vars, var_store));
                }

                new_tags.push((tag_name.clone(), new_args));
            }

            let rec_var = free_vars
                .unnamed_vars
                .get(rec_var_id)
                .expect("rec var not in unnamed vars");

            Type::RecursiveTagUnion(
                *rec_var,
                new_tags,
                Box::new(to_type(ext, free_vars, var_store)),
            )
        }
        Alias(symbol, solved_type_variables, solved_lambda_sets, solved_actual) => {
            let mut type_variables = Vec::with_capacity(solved_type_variables.len());

            for (lowercase, solved_arg) in solved_type_variables {
                type_variables.push((lowercase.clone(), to_type(solved_arg, free_vars, var_store)));
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
                // TODO(opaques): revisit when opaques are in the solver
                kind: AliasKind::Structural,
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

            for (lowercase, solved_arg) in solved_type_variables {
                type_variables.push((lowercase.clone(), to_type(solved_arg, free_vars, var_store)));
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
