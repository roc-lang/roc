use crate::can::ident::{Ident, Lowercase, TagName};
use crate::can::pattern::Pattern;
use crate::collections::{ImMap, ImSet, MutSet, SendMap};
use crate::module::symbol::Symbol;
use crate::operator::{ArgSide, BinOp};
use crate::region::Located;
use crate::region::Region;
use crate::subs::{VarStore, Variable};
use crate::uniqueness::boolean_algebra;
use inlinable_string::InlinableString;
use std::fmt;

pub const TYPE_NUM: &str = "Num";
pub const TYPE_INTEGER: &str = "Integer";
pub const TYPE_FLOATINGPOINT: &str = "FloatingPoint";

#[derive(PartialEq, Eq, Clone)]
pub enum Type {
    EmptyRec,
    EmptyTagUnion,
    /// A function. The types of its arguments, then the type of its return value.
    Function(Vec<Type>, Box<Type>),
    Record(SendMap<Lowercase, Type>, Box<Type>),
    TagUnion(Vec<(TagName, Vec<Type>)>, Box<Type>),
    Alias(Symbol, Vec<(Lowercase, Type)>, Box<Type>),
    RecursiveTagUnion(Variable, Vec<(TagName, Vec<Type>)>, Box<Type>),
    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply(Symbol, Vec<Type>),
    /// Boolean type used in uniqueness inference
    Boolean(boolean_algebra::Bool),
    Variable(Variable),
    /// A type error, which will code gen to a runtime error
    Erroneous(Problem),
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::EmptyRec => write!(f, "{{}}"),
            Type::EmptyTagUnion => write!(f, "[]"),
            Type::Function(args, ret) => {
                write!(f, "Fn(")?;

                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        ", ".fmt(f)?;
                    }

                    arg.fmt(f)?;
                }

                write!(f, " -> ")?;

                ret.fmt(f)?;

                write!(f, ")")
            }
            Type::Variable(var) => write!(f, "<{:?}>", var),

            Type::Apply(symbol, args) => {
                write!(f, "({:?}", symbol)?;

                for arg in args {
                    write!(f, " {:?}", arg)?;
                }

                write!(f, ")")
            }
            Type::Erroneous(problem) => {
                write!(f, "Erroneous(")?;

                problem.fmt(f)?;

                write!(f, ")")
            }
            Type::Alias(symbol, args, _actual) => {
                write!(f, "Alias {:?}", symbol)?;

                for (_, arg) in args {
                    write!(f, " {:?}", arg)?;
                }

                // Sometimes it's useful to see the expansion of the alias
                // write!(f, "[ but actually {:?} ]", _actual)?;

                Ok(())
            }
            Type::Record(fields, ext) => {
                write!(f, "{{")?;

                if !fields.is_empty() {
                    write!(f, " ")?;
                }

                let mut any_written_yet = false;

                for (label, field_type) in fields {
                    write!(f, "{:?} : {:?}", label, field_type)?;

                    if any_written_yet {
                        write!(f, ", ")?;
                    } else {
                        any_written_yet = true;
                    }
                }

                if !fields.is_empty() {
                    write!(f, " ")?;
                }

                write!(f, "}}")?;

                match *ext.clone() {
                    Type::EmptyRec => {
                        // This is a closed record. We're done!
                        Ok(())
                    }
                    other => {
                        // This is an open record, so print the variable
                        // right after the '}'
                        //
                        // e.g. the "*" at the end of `{ x: Int }*`
                        // or the "r" at the end of `{ x: Int }r`
                        other.fmt(f)
                    }
                }
            }
            Type::TagUnion(tags, ext) => {
                write!(f, "[")?;

                if !tags.is_empty() {
                    write!(f, " ")?;
                }

                let mut any_written_yet = false;

                for (label, arguments) in tags {
                    if any_written_yet {
                        write!(f, ", ")?;
                    } else {
                        any_written_yet = true;
                    }

                    write!(f, "{:?}", label)?;

                    for argument in arguments {
                        write!(f, " {:?}", argument)?;
                    }
                }

                if !tags.is_empty() {
                    write!(f, " ")?;
                }

                write!(f, "]")?;

                match *ext.clone() {
                    Type::EmptyTagUnion => {
                        // This is a closed variant. We're done!
                        Ok(())
                    }
                    other => {
                        // This is an open tag union, so print the variable
                        // right after the ']'
                        //
                        // e.g. the "*" at the end of `[ Foo ]*`
                        // or the "r" at the end of `[ DivByZero ]r`
                        other.fmt(f)
                    }
                }
            }
            Type::RecursiveTagUnion(rec, tags, ext) => {
                write!(f, "[")?;

                if !tags.is_empty() {
                    write!(f, " ")?;
                }

                let mut any_written_yet = false;

                for (label, arguments) in tags {
                    if any_written_yet {
                        write!(f, ", ")?;
                    } else {
                        any_written_yet = true;
                    }

                    write!(f, "{:?}", label)?;

                    for argument in arguments {
                        write!(f, " {:?}", argument)?;
                    }
                }

                if !tags.is_empty() {
                    write!(f, " ")?;
                }

                write!(f, "]")?;

                match *ext.clone() {
                    Type::EmptyTagUnion => {
                        // This is a closed variant. We're done!
                    }
                    other => {
                        // This is an open tag union, so print the variable
                        // right after the ']'
                        //
                        // e.g. the "*" at the end of `[ Foo ]*`
                        // or the "r" at the end of `[ DivByZero ]r`
                        other.fmt(f)?;
                    }
                }

                write!(f, " as <{:?}>", rec)
            }
            Type::Boolean(b) => write!(f, "{:?}", b),
        }
    }
}

impl Type {
    pub fn arity(&self) -> usize {
        if let Type::Function(args, _) = self {
            args.len()
        } else {
            0
        }
    }

    pub fn variables(&self) -> ImSet<Variable> {
        let mut result = ImSet::default();
        variables_help(self, &mut result);

        result
    }

    pub fn substitute(&mut self, substitutions: &ImMap<Variable, Type>) {
        use Type::*;

        match self {
            Variable(v) => {
                if let Some(replacement) = substitutions.get(&v) {
                    *self = replacement.clone();
                }
            }
            Function(args, ret) => {
                for arg in args {
                    arg.substitute(substitutions);
                }
                ret.substitute(substitutions);
            }
            TagUnion(tags, ext) => {
                for (_, args) in tags {
                    for x in args {
                        x.substitute(substitutions);
                    }
                }
                ext.substitute(substitutions);
            }
            RecursiveTagUnion(_, tags, ext) => {
                for (_, args) in tags {
                    for x in args {
                        x.substitute(substitutions);
                    }
                }
                ext.substitute(substitutions);
            }
            Record(fields, ext) => {
                for x in fields.iter_mut() {
                    x.substitute(substitutions);
                }
                ext.substitute(substitutions);
            }
            Alias(_, zipped, actual_type) => {
                for (_, value) in zipped.iter_mut() {
                    value.substitute(substitutions);
                }
                actual_type.substitute(substitutions);
            }
            Apply(_, args) => {
                for arg in args {
                    arg.substitute(substitutions);
                }
            }
            EmptyRec | EmptyTagUnion | Erroneous(_) | Boolean(_) => {}
        }
    }

    // swap Apply with Alias if their module and tag match
    pub fn substitute_alias(&mut self, rep_symbol: Symbol, actual: &Type) {
        use Type::*;

        match self {
            Function(args, ret) => {
                for arg in args {
                    arg.substitute_alias(rep_symbol, actual);
                }
                ret.substitute_alias(rep_symbol, actual);
            }
            RecursiveTagUnion(_, tags, ext) | TagUnion(tags, ext) => {
                for (_, args) in tags {
                    for x in args {
                        x.substitute_alias(rep_symbol, actual);
                    }
                }
                ext.substitute_alias(rep_symbol, actual);
            }
            Record(fields, ext) => {
                for x in fields.iter_mut() {
                    x.substitute_alias(rep_symbol, actual);
                }
                ext.substitute_alias(rep_symbol, actual);
            }
            Alias(_, _, actual_type) => {
                actual_type.substitute_alias(rep_symbol, actual);
            }
            Apply(symbol, _) if *symbol == rep_symbol => {
                *self = actual.clone();

                if let Apply(_, args) = self {
                    for arg in args {
                        arg.substitute_alias(rep_symbol, actual);
                    }
                }
            }
            Apply(_, args) => {
                for arg in args {
                    arg.substitute_alias(rep_symbol, actual);
                }
            }
            EmptyRec | EmptyTagUnion | Erroneous(_) | Variable(_) | Boolean(_) => {}
        }
    }

    pub fn contains_symbol(&self, rep_symbol: Symbol) -> bool {
        use Type::*;

        match self {
            Function(args, ret) => {
                ret.contains_symbol(rep_symbol)
                    || args.iter().any(|arg| arg.contains_symbol(rep_symbol))
            }
            RecursiveTagUnion(_, tags, ext) | TagUnion(tags, ext) => {
                ext.contains_symbol(rep_symbol)
                    || tags
                        .iter()
                        .map(|v| v.1.iter())
                        .flatten()
                        .any(|arg| arg.contains_symbol(rep_symbol))
            }

            Record(fields, ext) => {
                ext.contains_symbol(rep_symbol)
                    || fields.values().any(|arg| arg.contains_symbol(rep_symbol))
            }
            Alias(alias_symbol, _, actual_type) => {
                alias_symbol == &rep_symbol || actual_type.contains_symbol(rep_symbol)
            }
            Apply(symbol, _) if *symbol == rep_symbol => true,
            Apply(_, args) => args.iter().any(|arg| arg.contains_symbol(rep_symbol)),
            EmptyRec | EmptyTagUnion | Erroneous(_) | Variable(_) | Boolean(_) => false,
        }
    }

    pub fn symbols(&self) -> ImSet<Symbol> {
        let mut found_symbols = ImSet::default();
        symbols_help(self, &mut found_symbols);

        found_symbols
    }

    /// a shallow dealias, continue until the first constructor is not an alias.
    pub fn shallow_dealias(&self) -> &Self {
        match self {
            Type::Alias(_, _, actual) => actual.shallow_dealias(),
            _ => self,
        }
    }

    pub fn instantiate_aliases(&mut self, aliases: &ImMap<Symbol, Alias>, var_store: &VarStore) {
        use Type::*;

        match self {
            Function(args, ret) => {
                for arg in args {
                    arg.instantiate_aliases(aliases, var_store);
                }
                ret.instantiate_aliases(aliases, var_store);
            }
            RecursiveTagUnion(_, tags, ext) | TagUnion(tags, ext) => {
                for (_, args) in tags {
                    for x in args {
                        x.instantiate_aliases(aliases, var_store);
                    }
                }
                ext.instantiate_aliases(aliases, var_store);
            }
            Record(fields, ext) => {
                for x in fields.iter_mut() {
                    x.instantiate_aliases(aliases, var_store);
                }
                ext.instantiate_aliases(aliases, var_store);
            }
            Alias(_, _, actual_type) => {
                actual_type.instantiate_aliases(aliases, var_store);
            }
            Apply(symbol, args) => {
                if let Some(alias) = aliases.get(symbol) {
                    debug_assert!(args.len() == alias.vars.len());
                    let mut actual = alias.typ.clone();

                    let mut named_args = Vec::with_capacity(args.len());
                    let mut substitution = ImMap::default();

                    // TODO substitute further in args
                    for (
                        Located {
                            value: (lowercase, placeholder),
                            ..
                        },
                        filler,
                    ) in alias.vars.iter().zip(args.iter())
                    {
                        let mut filler = filler.clone();
                        filler.instantiate_aliases(aliases, var_store);
                        named_args.push((lowercase.clone(), filler.clone()));
                        substitution.insert(*placeholder, filler);
                    }

                    actual.substitute(&substitution);
                    actual.instantiate_aliases(aliases, var_store);

                    // instantiate recursion variable!
                    if let Type::RecursiveTagUnion(rec_var, mut tags, mut ext) = actual {
                        let new_rec_var = var_store.fresh();
                        substitution.clear();
                        substitution.insert(rec_var, Type::Variable(new_rec_var));

                        for typ in tags.iter_mut().map(|v| v.1.iter_mut()).flatten() {
                            typ.substitute(&substitution);
                        }
                        ext.substitute(&substitution);

                        *self = Type::Alias(
                            *symbol,
                            named_args,
                            Box::new(Type::RecursiveTagUnion(new_rec_var, tags, ext)),
                        );
                    } else {
                        *self = Type::Alias(*symbol, named_args, Box::new(actual));
                    }
                } else {
                    // do nothing, maybe this alias gets instantiated later?
                }
            }
            EmptyRec | EmptyTagUnion | Erroneous(_) | Variable(_) | Boolean(_) => {}
        }
    }
}

fn symbols_help(tipe: &Type, accum: &mut ImSet<Symbol>) {
    use Type::*;

    match tipe {
        Function(args, ret) => {
            symbols_help(&ret, accum);
            args.iter().for_each(|arg| symbols_help(arg, accum));
        }
        RecursiveTagUnion(_, tags, ext) | TagUnion(tags, ext) => {
            symbols_help(&ext, accum);
            tags.iter()
                .map(|v| v.1.iter())
                .flatten()
                .for_each(|arg| symbols_help(arg, accum));
        }

        Record(fields, ext) => {
            symbols_help(&ext, accum);
            fields.values().for_each(|arg| symbols_help(arg, accum));
        }
        Alias(alias_symbol, _, actual_type) => {
            accum.insert(*alias_symbol);
            symbols_help(&actual_type, accum);
        }
        Apply(symbol, args) => {
            accum.insert(*symbol);
            args.iter().for_each(|arg| symbols_help(arg, accum));
        }
        EmptyRec | EmptyTagUnion | Erroneous(_) | Variable(_) | Boolean(_) => {}
    }
}

fn variables_help(tipe: &Type, accum: &mut ImSet<Variable>) {
    use Type::*;
    match tipe {
        EmptyRec | EmptyTagUnion | Erroneous(_) => (),
        Boolean(b) => {
            for v in b.variables() {
                accum.insert(v);
            }
        }
        Variable(v) => {
            accum.insert(*v);
        }

        Function(args, ret) => {
            for arg in args {
                variables_help(arg, accum);
            }
            variables_help(ret, accum);
        }
        Record(fields, ext) => {
            for (_, x) in fields {
                variables_help(x, accum);
            }
            variables_help(ext, accum);
        }
        TagUnion(tags, ext) => {
            for (_, args) in tags {
                for x in args {
                    variables_help(x, accum);
                }
            }
            variables_help(ext, accum);
        }
        RecursiveTagUnion(rec, tags, ext) => {
            for (_, args) in tags {
                for x in args {
                    variables_help(x, accum);
                }
            }
            variables_help(ext, accum);

            // just check that this is actually a recursive type
            debug_assert!(accum.contains(rec));

            // this rec var doesn't need to be in flex_vars or rigid_vars
            accum.remove(rec);
        }
        Alias(_, args, actual) => {
            for (_, x) in args {
                variables_help(x, accum);
            }
            variables_help(actual, accum);
        }
        Apply(_, args) => {
            for x in args {
                variables_help(x, accum);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expected<T> {
    NoExpectation(T),
    FromAnnotation(Located<Pattern>, usize, AnnotationSource, T),
    ForReason(Reason, T, Region),
}

/// Like Expected, but for Patterns.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PExpected<T> {
    NoExpectation(T),
    ForReason(PReason, T, Region),
}

impl<T> PExpected<T> {
    pub fn get_type(self) -> T {
        match self {
            PExpected::NoExpectation(val) => val,
            PExpected::ForReason(_, val, _) => val,
        }
    }

    pub fn get_type_ref(&self) -> &T {
        match self {
            PExpected::NoExpectation(val) => val,
            PExpected::ForReason(_, val, _) => val,
        }
    }

    pub fn get_type_mut_ref(&mut self) -> &mut T {
        match self {
            PExpected::NoExpectation(val) => val,
            PExpected::ForReason(_, val, _) => val,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PReason {
    TypedArg { name: Box<str>, index: usize },
    WhenMatch { index: usize },
    CtorArg { name: Box<str>, index: usize },
    ListEntry { index: usize },
    Tail,
}

impl<T> Expected<T> {
    pub fn get_type(self) -> T {
        match self {
            Expected::NoExpectation(val) => val,
            Expected::ForReason(_, val, _) => val,
            Expected::FromAnnotation(_, _, _, val) => val,
        }
    }

    pub fn get_type_ref(&self) -> &T {
        match self {
            Expected::NoExpectation(val) => val,
            Expected::ForReason(_, val, _) => val,
            Expected::FromAnnotation(_, _, _, val) => val,
        }
    }

    pub fn get_type_mut_ref(&mut self) -> &mut T {
        match self {
            Expected::NoExpectation(val) => val,
            Expected::ForReason(_, val, _) => val,
            Expected::FromAnnotation(_, _, _, val) => val,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnnotationSource {
    TypedIfBranch(usize /* index */),
    TypedWhenBranch(usize /* index */),
    TypedBody,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Reason {
    AnonymousFnArg { arg_index: u8 },
    NamedFnArg(String /* function name */, u8 /* arg index */),
    AnonymousFnCall { arity: u8 },
    NamedFnCall(String /* function name */, u8 /* arity */),
    BinOpArg(BinOp, ArgSide),
    BinOpRet(BinOp),
    FloatLiteral,
    IntLiteral,
    InterpolatedStringVar,
    WhenBranch { index: usize },
    IfCondition,
    IfBranch { index: usize },
    ElemInList,
    RecordUpdateValue(Lowercase),
    RecordUpdateKeys(Symbol, SendMap<Lowercase, Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Eq(Type, Expected<Type>, Region),
    Lookup(Symbol, Expected<Type>, Region),
    Pattern(Region, PatternCategory, Type, PExpected<Type>),
    True, // Used for things that always unify, e.g. blanks and runtime errors
    SaveTheEnvironment,
    Let(Box<LetConstraint>),
    And(Vec<Constraint>),
}

impl Constraint {
    pub fn instantiate_aliases(&mut self, var_store: &VarStore) {
        Self::instantiate_aliases_help(self, &ImMap::default(), var_store);
    }

    fn instantiate_aliases_help(&mut self, aliases: &ImMap<Symbol, Alias>, var_store: &VarStore) {
        use Constraint::*;

        match self {
            True | SaveTheEnvironment => {}

            Eq(typ, expected, _) => {
                expected
                    .get_type_mut_ref()
                    .instantiate_aliases(aliases, var_store);
                typ.instantiate_aliases(aliases, var_store);
            }

            Lookup(_, expected, _) => {
                expected
                    .get_type_mut_ref()
                    .instantiate_aliases(aliases, var_store);
            }

            Pattern(_, _, typ, pexpected) => {
                pexpected
                    .get_type_mut_ref()
                    .instantiate_aliases(aliases, var_store);
                typ.instantiate_aliases(aliases, var_store);
            }

            And(nested) => {
                for c in nested.iter_mut() {
                    c.instantiate_aliases_help(aliases, var_store);
                }
            }

            Let(letcon) => {
                let mut new_aliases = aliases.clone();
                for (k, v) in letcon.def_aliases.iter() {
                    new_aliases.insert(*k, v.clone());
                }

                for Located { value: typ, .. } in letcon.def_types.iter_mut() {
                    typ.instantiate_aliases(&new_aliases, var_store);
                }

                letcon
                    .defs_constraint
                    .instantiate_aliases_help(&new_aliases, var_store);
                letcon
                    .ret_constraint
                    .instantiate_aliases_help(&new_aliases, var_store);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternCategory {
    Record,
    EmptyRecord,
    List,
    Set,
    Map,
    Ctor(TagName),
    Int,
    Str,
    Float,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetConstraint {
    pub rigid_vars: Vec<Variable>,
    pub flex_vars: Vec<Variable>,
    pub def_types: SendMap<Symbol, Located<Type>>,
    pub def_aliases: SendMap<Symbol, Alias>,
    pub defs_constraint: Constraint,
    pub ret_constraint: Constraint,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Alias {
    pub region: Region,
    pub vars: Vec<Located<(Lowercase, Variable)>>,
    pub typ: Type,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Problem {
    CanonicalizationProblem,
    Mismatch(Mismatch, ErrorType, ErrorType),
    CircularType(Symbol, ErrorType, Region),
    UnrecognizedIdent(InlinableString),
    Shadowed(Region, Located<Ident>),
    InvalidModule,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Mismatch {
    TypeMismatch,
    IfConditionNotBool,
    InconsistentIfElse,
    InconsistentWhenBranches,
    CanonicalizationProblem,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ErrorType {
    Infinite,
    Type(Symbol, Vec<ErrorType>),
    FlexVar(Lowercase),
    RigidVar(Lowercase),
    Record(SendMap<Lowercase, ErrorType>, TypeExt),
    TagUnion(SendMap<TagName, Vec<ErrorType>>, TypeExt),
    RecursiveTagUnion(Variable, SendMap<TagName, Vec<ErrorType>>, TypeExt),
    Function(Vec<ErrorType>, Box<ErrorType>),
    Alias(Symbol, Vec<(Lowercase, ErrorType)>, Box<ErrorType>),
    Boolean(boolean_algebra::Bool),
    Error,
}

impl ErrorType {
    pub fn unwrap_alias(self) -> ErrorType {
        match self {
            ErrorType::Alias(_, _, real) => real.unwrap_alias(),
            real => real,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TypeExt {
    Closed,
    FlexOpen(Lowercase),
    RigidOpen(Lowercase),
}

static THE_LETTER_A: u32 = 'a' as u32;

pub fn name_type_var(letters_used: u32, taken: &mut MutSet<Lowercase>) -> (Lowercase, u32) {
    // TODO we should arena-allocate this String,
    // so all the strings in the entire pass only require ~1 allocation.
    let generated_name = if letters_used < 26 {
        // This should generate "a", then "b", etc.
        std::char::from_u32(THE_LETTER_A + letters_used)
            .unwrap_or_else(|| panic!("Tried to convert {} to a char", THE_LETTER_A + letters_used))
            .to_string()
            .into()
    } else {
        panic!("TODO generate aa, ab, ac, ...");
    };

    if taken.contains(&generated_name) {
        // If the generated name is already taken, try again.
        name_type_var(letters_used + 1, taken)
    } else {
        taken.insert(generated_name.clone());

        (generated_name, letters_used + 1)
    }
}
