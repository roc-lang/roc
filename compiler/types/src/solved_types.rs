use crate::boolean_algebra;
use crate::subs::{FlatType, Subs, VarId, Variable};
use crate::types::{Problem, RecordField, Type};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};

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

/// This is a fully solved type, with no Variables remaining in it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    RecursiveTagUnion(VarId, Vec<(TagName, Vec<SolvedType>)>, Box<SolvedType>),
    EmptyTagUnion,
    /// A type from an Invalid module
    Erroneous(Problem),

    /// A type alias
    Alias(Symbol, Vec<(Lowercase, SolvedType)>, Box<SolvedType>),

    /// a boolean algebra Bool
    Boolean(SolvedBool),

    /// A type error
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SolvedBool {
    SolvedShared,
    SolvedContainer(VarId, Vec<VarId>),
}

impl SolvedBool {
    pub fn from_bool(boolean: &boolean_algebra::Bool, subs: &Subs) -> Self {
        use boolean_algebra::Bool;

        match boolean {
            Bool::Shared => SolvedBool::SolvedShared,
            Bool::Container(cvar, mvars) => {
                debug_assert!(matches!(
                    subs.get_without_compacting(*cvar).content,
                    crate::subs::Content::FlexVar(_)
                ));

                SolvedBool::SolvedContainer(
                    VarId::from_var(*cvar, subs),
                    mvars
                        .iter()
                        .map(|mvar| VarId::from_var(*mvar, subs))
                        .collect(),
                )
            }
        }
    }
}

impl SolvedType {
    pub fn new(solved_subs: &Solved<Subs>, var: Variable) -> Self {
        Self::from_var(solved_subs.inner(), var)
    }

    pub fn from_type(solved_subs: &Solved<Subs>, typ: Type) -> Self {
        use crate::types::Type::*;

        match typ {
            EmptyRec => SolvedType::EmptyRecord,
            EmptyTagUnion => SolvedType::EmptyTagUnion,
            Apply(symbol, types) => {
                let mut solved_types = Vec::with_capacity(types.len());

                for typ in types {
                    let solved_type = Self::from_type(solved_subs, typ);

                    solved_types.push(solved_type);
                }

                SolvedType::Apply(symbol, solved_types)
            }
            Function(args, box_closure, box_ret) => {
                let solved_ret = Self::from_type(solved_subs, *box_ret);
                let solved_closure = Self::from_type(solved_subs, *box_closure);
                let mut solved_args = Vec::with_capacity(args.len());

                for arg in args.into_iter() {
                    let solved_arg = Self::from_type(solved_subs, arg);

                    solved_args.push(solved_arg);
                }

                SolvedType::Func(solved_args, Box::new(solved_closure), Box::new(solved_ret))
            }
            Record(fields, box_ext) => {
                let solved_ext = Self::from_type(solved_subs, *box_ext);
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
            TagUnion(tags, box_ext) => {
                let solved_ext = Self::from_type(solved_subs, *box_ext);
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
            RecursiveTagUnion(rec_var, tags, box_ext) => {
                let solved_ext = Self::from_type(solved_subs, *box_ext);
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
                    VarId::from_var(rec_var, solved_subs.inner()),
                    solved_tags,
                    Box::new(solved_ext),
                )
            }
            Erroneous(problem) => SolvedType::Erroneous(problem),
            Alias(symbol, args, box_type) => {
                let solved_type = Self::from_type(solved_subs, *box_type);
                let mut solved_args = Vec::with_capacity(args.len());

                for (name, var) in args {
                    solved_args.push((name.clone(), Self::from_type(solved_subs, var)));
                }

                SolvedType::Alias(symbol, solved_args, Box::new(solved_type))
            }
            Boolean(val) => SolvedType::Boolean(SolvedBool::from_bool(&val, solved_subs.inner())),
            Variable(var) => Self::from_var(solved_subs.inner(), var),
        }
    }

    pub fn from_var(subs: &Subs, var: Variable) -> Self {
        use crate::subs::Content::*;

        match subs.get_without_compacting(var).content {
            FlexVar(_) => SolvedType::Flex(VarId::from_var(var, subs)),
            RigidVar(name) => SolvedType::Rigid(name),
            Structure(flat_type) => Self::from_flat_type(subs, flat_type),
            Alias(symbol, args, actual_var) => {
                let mut new_args = Vec::with_capacity(args.len());

                for (arg_name, arg_var) in args {
                    new_args.push((arg_name, Self::from_var(subs, arg_var)));
                }

                let aliased_to = Self::from_var(subs, actual_var);

                SolvedType::Alias(symbol, new_args, Box::new(aliased_to))
            }
            Error => SolvedType::Error,
        }
    }

    fn from_flat_type(subs: &Subs, flat_type: FlatType) -> Self {
        use crate::subs::FlatType::*;

        match flat_type {
            Apply(symbol, args) => {
                let mut new_args = Vec::with_capacity(args.len());

                for var in args {
                    new_args.push(Self::from_var(subs, var));
                }

                SolvedType::Apply(symbol, new_args)
            }
            Func(args, closure, ret) => {
                let mut new_args = Vec::with_capacity(args.len());

                for var in args {
                    new_args.push(Self::from_var(subs, var));
                }

                let ret = Self::from_var(subs, ret);
                let closure = Self::from_var(subs, closure);

                SolvedType::Func(new_args, Box::new(closure), Box::new(ret))
            }
            Record(fields, ext_var) => {
                let mut new_fields = Vec::with_capacity(fields.len());

                for (label, field) in fields {
                    use RecordField::*;

                    let solved_type = match field {
                        Optional(var) => Optional(Self::from_var(subs, var)),
                        Required(var) => Required(Self::from_var(subs, var)),
                        Demanded(var) => Demanded(Self::from_var(subs, var)),
                    };

                    new_fields.push((label, solved_type));
                }

                let ext = Self::from_var(subs, ext_var);

                SolvedType::Record {
                    fields: new_fields,
                    ext: Box::new(ext),
                }
            }
            TagUnion(tags, ext_var) => {
                let mut new_tags = Vec::with_capacity(tags.len());

                for (tag_name, args) in tags {
                    let mut new_args = Vec::with_capacity(args.len());

                    for var in args {
                        new_args.push(Self::from_var(subs, var));
                    }

                    new_tags.push((tag_name, new_args));
                }

                let ext = Self::from_var(subs, ext_var);

                SolvedType::TagUnion(new_tags, Box::new(ext))
            }
            RecursiveTagUnion(rec_var, tags, ext_var) => {
                let mut new_tags = Vec::with_capacity(tags.len());

                for (tag_name, args) in tags {
                    let mut new_args = Vec::with_capacity(args.len());

                    for var in args {
                        new_args.push(Self::from_var(subs, var));
                    }

                    new_tags.push((tag_name, new_args));
                }

                let ext = Self::from_var(subs, ext_var);

                SolvedType::RecursiveTagUnion(
                    VarId::from_var(rec_var, subs),
                    new_tags,
                    Box::new(ext),
                )
            }
            EmptyRecord => SolvedType::EmptyRecord,
            EmptyTagUnion => SolvedType::EmptyTagUnion,
            Boolean(val) => SolvedType::Boolean(SolvedBool::from_bool(&val, subs)),
            Erroneous(problem) => SolvedType::Erroneous(problem),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinAlias {
    pub region: Region,
    pub vars: Vec<Located<Lowercase>>,
    pub typ: SolvedType,
}
