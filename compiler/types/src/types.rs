use crate::pretty_print::Parens;
use crate::subs::{
    GetSubsSlice, RecordFields, Subs, UnionTags, VarStore, Variable, VariableSubsSlice,
};
use roc_collections::all::{ImMap, ImSet, Index, MutSet, SendMap};
use roc_error_macros::internal_error;
use roc_module::called_via::CalledVia;
use roc_module::ident::{ForeignSymbol, Ident, Lowercase, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_region::all::{Loc, Region};
use std::fmt;

pub const TYPE_NUM: &str = "Num";
pub const TYPE_INTEGER: &str = "Integer";
pub const TYPE_FLOATINGPOINT: &str = "FloatingPoint";

const GREEK_LETTERS: &[char] = &[
    'α', 'ν', 'β', 'ξ', 'γ', 'ο', 'δ', 'π', 'ε', 'ρ', 'ζ', 'σ', 'η', 'τ', 'θ', 'υ', 'ι', 'φ', 'κ',
    'χ', 'λ', 'ψ', 'μ', 'ω', 'ς',
];

///
/// Intuitively
///
/// - Demanded: only introduced by pattern matches, e.g. { x } ->
///     Cannot unify with an Optional field, but can unify with a Required field
/// - Required: introduced by record literals and type annotations.
///     Can unify with Optional and Demanded
/// - Optional: introduced by pattern matches and annotations.
///     Can unify with Required, but not with Demanded
#[derive(PartialEq, Eq, Clone, Hash)]
pub enum RecordField<T> {
    Optional(T),
    Required(T),
    Demanded(T),
}

impl<T: Copy> Copy for RecordField<T> {}

impl<T: fmt::Debug> fmt::Debug for RecordField<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use RecordField::*;

        match self {
            Optional(typ) => write!(f, "Optional({:?})", typ),
            Required(typ) => write!(f, "Required({:?})", typ),
            Demanded(typ) => write!(f, "Demanded({:?})", typ),
        }
    }
}

impl<T> RecordField<T> {
    pub fn into_inner(self) -> T {
        use RecordField::*;

        match self {
            Optional(t) => t,
            Required(t) => t,
            Demanded(t) => t,
        }
    }

    pub fn as_inner(&self) -> &T {
        use RecordField::*;

        match self {
            Optional(t) => t,
            Required(t) => t,
            Demanded(t) => t,
        }
    }

    pub fn map<F, U>(&self, mut f: F) -> RecordField<U>
    where
        F: FnMut(&T) -> U,
    {
        use RecordField::*;
        match self {
            Optional(t) => Optional(f(t)),
            Required(t) => Required(f(t)),
            Demanded(t) => Demanded(f(t)),
        }
    }
}

impl RecordField<Type> {
    pub fn substitute(&mut self, substitutions: &ImMap<Variable, Type>) {
        use RecordField::*;

        match self {
            Optional(typ) => typ.substitute(substitutions),
            Required(typ) => typ.substitute(substitutions),
            Demanded(typ) => typ.substitute(substitutions),
        }
    }

    pub fn substitute_alias(
        &mut self,
        rep_symbol: Symbol,
        rep_args: &[Type],
        actual: &Type,
    ) -> Result<(), Region> {
        use RecordField::*;

        match self {
            Optional(typ) => typ.substitute_alias(rep_symbol, rep_args, actual),
            Required(typ) => typ.substitute_alias(rep_symbol, rep_args, actual),
            Demanded(typ) => typ.substitute_alias(rep_symbol, rep_args, actual),
        }
    }

    pub fn instantiate_aliases(
        &mut self,
        region: Region,
        aliases: &ImMap<Symbol, Alias>,
        var_store: &mut VarStore,
        introduced: &mut ImSet<Variable>,
    ) {
        use RecordField::*;

        match self {
            Optional(typ) => typ.instantiate_aliases(region, aliases, var_store, introduced),
            Required(typ) => typ.instantiate_aliases(region, aliases, var_store, introduced),
            Demanded(typ) => typ.instantiate_aliases(region, aliases, var_store, introduced),
        }
    }

    pub fn contains_symbol(&self, rep_symbol: Symbol) -> bool {
        use RecordField::*;

        match self {
            Optional(typ) => typ.contains_symbol(rep_symbol),
            Required(typ) => typ.contains_symbol(rep_symbol),
            Demanded(typ) => typ.contains_symbol(rep_symbol),
        }
    }
    pub fn contains_variable(&self, rep_variable: Variable) -> bool {
        use RecordField::*;

        match self {
            Optional(typ) => typ.contains_variable(rep_variable),
            Required(typ) => typ.contains_variable(rep_variable),
            Demanded(typ) => typ.contains_variable(rep_variable),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LambdaSet(pub Type);

impl LambdaSet {
    fn substitute(&mut self, substitutions: &ImMap<Variable, Type>) {
        self.0.substitute(substitutions);
    }

    fn instantiate_aliases(
        &mut self,
        region: Region,
        aliases: &ImMap<Symbol, Alias>,
        var_store: &mut VarStore,
        introduced: &mut ImSet<Variable>,
    ) {
        self.0
            .instantiate_aliases(region, aliases, var_store, introduced)
    }
}

#[derive(PartialEq, Eq, Clone)]
pub enum Type {
    EmptyRec,
    EmptyTagUnion,
    /// A function. The types of its arguments, size of its closure, then the type of its return value.
    Function(Vec<Type>, Box<Type>, Box<Type>),
    Record(SendMap<Lowercase, RecordField<Type>>, Box<Type>),
    TagUnion(Vec<(TagName, Vec<Type>)>, Box<Type>),
    FunctionOrTagUnion(TagName, Symbol, Box<Type>),
    /// A function name that is used in our defunctionalization algorithm
    ClosureTag {
        name: Symbol,
        ext: Variable,
    },
    Alias {
        symbol: Symbol,
        type_arguments: Vec<(Lowercase, Type)>,
        lambda_set_variables: Vec<LambdaSet>,
        actual: Box<Type>,
        kind: AliasKind,
    },
    HostExposedAlias {
        name: Symbol,
        type_arguments: Vec<(Lowercase, Type)>,
        lambda_set_variables: Vec<LambdaSet>,
        actual_var: Variable,
        actual: Box<Type>,
    },
    RecursiveTagUnion(Variable, Vec<(TagName, Vec<Type>)>, Box<Type>),
    /// Applying a type to some arguments (e.g. Dict.Dict String Int)
    Apply(Symbol, Vec<Type>, Region),
    Variable(Variable),
    RangedNumber(Box<Type>, Vec<Variable>),
    /// A type error, which will code gen to a runtime error
    Erroneous(Problem),
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::EmptyRec => write!(f, "{{}}"),
            Type::EmptyTagUnion => write!(f, "[]"),
            Type::Function(args, closure, ret) => {
                write!(f, "Fn(")?;

                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{:?}", arg)?;
                }

                write!(f, " |{:?}|", closure)?;
                write!(f, " -> ")?;

                ret.fmt(f)?;

                write!(f, ")")
            }
            Type::Variable(var) => write!(f, "<{:?}>", var),

            Type::Apply(symbol, args, _) => {
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
            Type::Alias {
                symbol,
                type_arguments,
                lambda_set_variables,
                actual: _actual,
                ..
            } => {
                write!(f, "(Alias {:?}", symbol)?;

                for (_, arg) in type_arguments {
                    write!(f, " {:?}", arg)?;
                }

                for (lambda_set, greek_letter) in
                    lambda_set_variables.iter().zip(GREEK_LETTERS.iter())
                {
                    write!(f, " {}@{:?}", greek_letter, lambda_set.0)?;
                }

                // Sometimes it's useful to see the expansion of the alias
                write!(f, "[ but actually {:?} ]", _actual)?;

                write!(f, ")")?;

                Ok(())
            }
            Type::HostExposedAlias {
                name,
                type_arguments: arguments,
                ..
            } => {
                write!(f, "HostExposedAlias {:?}", name)?;

                for (_, arg) in arguments {
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
                    match field_type {
                        RecordField::Optional(_) => write!(f, "{:?} ? {:?}", label, field_type)?,
                        RecordField::Required(_) => write!(f, "{:?} : {:?}", label, field_type)?,
                        RecordField::Demanded(_) => write!(f, "{:?} : {:?}", label, field_type)?,
                    }

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

                let mut it = tags.iter().peekable();
                while let Some((label, arguments)) = it.next() {
                    write!(f, "{:?}", label)?;

                    for argument in arguments {
                        write!(f, " {:?}", argument)?;
                    }

                    if it.peek().is_some() {
                        write!(f, ", ")?;
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
            Type::FunctionOrTagUnion(tag_name, _, ext) => {
                write!(f, "[")?;
                write!(f, "{:?}", tag_name)?;
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
            Type::ClosureTag { name, ext } => {
                write!(f, "ClosureTag(")?;

                name.fmt(f)?;
                write!(f, ", ")?;
                ext.fmt(f)?;

                write!(f, ")")
            }
            Type::RecursiveTagUnion(rec, tags, ext) => {
                write!(f, "[")?;

                if !tags.is_empty() {
                    write!(f, " ")?;
                }

                let mut it = tags.iter().peekable();
                while let Some((label, arguments)) = it.next() {
                    write!(f, "{:?}", label)?;

                    for argument in arguments {
                        write!(f, " {:?}", argument)?;
                    }

                    if it.peek().is_some() {
                        write!(f, ", ")?;
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
            Type::RangedNumber(typ, range_vars) => {
                write!(f, "Ranged({:?}, {:?})", typ, range_vars)
            }
        }
    }
}

impl Type {
    pub fn arity(&self) -> usize {
        if let Type::Function(args, _, _) = self {
            args.len()
        } else {
            0
        }
    }
    pub fn is_recursive(&self) -> bool {
        matches!(self, Type::RecursiveTagUnion(_, _, _))
    }

    pub fn is_empty_tag_union(&self) -> bool {
        matches!(self, Type::EmptyTagUnion)
    }

    pub fn is_empty_record(&self) -> bool {
        matches!(self, Type::EmptyRec)
    }

    pub fn variables(&self) -> ImSet<Variable> {
        let mut result = ImSet::default();
        variables_help(self, &mut result);

        result
    }

    pub fn variables_detail(&self) -> VariableDetail {
        let mut result = Default::default();
        variables_help_detailed(self, &mut result);

        result
    }

    pub fn substitute(&mut self, substitutions: &ImMap<Variable, Type>) {
        use Type::*;

        match self {
            ClosureTag { ext: v, .. } | Variable(v) => {
                if let Some(replacement) = substitutions.get(v) {
                    *self = replacement.clone();
                }
            }
            Function(args, closure, ret) => {
                for arg in args {
                    arg.substitute(substitutions);
                }
                closure.substitute(substitutions);
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
            FunctionOrTagUnion(_, _, ext) => {
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
                for (_, x) in fields.iter_mut() {
                    x.substitute(substitutions);
                }
                ext.substitute(substitutions);
            }
            Alias {
                type_arguments,
                lambda_set_variables,
                actual,
                ..
            } => {
                for (_, value) in type_arguments.iter_mut() {
                    value.substitute(substitutions);
                }

                for lambda_set in lambda_set_variables.iter_mut() {
                    lambda_set.substitute(substitutions);
                }

                actual.substitute(substitutions);
            }
            HostExposedAlias {
                type_arguments: arguments,
                actual: actual_type,
                ..
            } => {
                for (_, value) in arguments.iter_mut() {
                    value.substitute(substitutions);
                }
                actual_type.substitute(substitutions);
            }
            Apply(_, args, _) => {
                for arg in args {
                    arg.substitute(substitutions);
                }
            }
            RangedNumber(typ, _) => {
                typ.substitute(substitutions);
            }

            EmptyRec | EmptyTagUnion | Erroneous(_) => {}
        }
    }

    /// Swap Apply(rep_symbol, rep_args) with `actual`. Returns `Err` if there is an
    /// `Apply(rep_symbol, _)`, but the args don't match.
    pub fn substitute_alias(
        &mut self,
        rep_symbol: Symbol,
        rep_args: &[Type],
        actual: &Type,
    ) -> Result<(), Region> {
        use Type::*;

        match self {
            Function(args, closure, ret) => {
                for arg in args {
                    arg.substitute_alias(rep_symbol, rep_args, actual)?;
                }
                closure.substitute_alias(rep_symbol, rep_args, actual)?;
                ret.substitute_alias(rep_symbol, rep_args, actual)
            }
            FunctionOrTagUnion(_, _, ext) => ext.substitute_alias(rep_symbol, rep_args, actual),
            RecursiveTagUnion(_, tags, ext) | TagUnion(tags, ext) => {
                for (_, args) in tags {
                    for x in args {
                        x.substitute_alias(rep_symbol, rep_args, actual)?;
                    }
                }
                ext.substitute_alias(rep_symbol, rep_args, actual)
            }
            Record(fields, ext) => {
                for (_, x) in fields.iter_mut() {
                    x.substitute_alias(rep_symbol, rep_args, actual)?;
                }
                ext.substitute_alias(rep_symbol, rep_args, actual)
            }
            Alias {
                type_arguments,
                actual: alias_actual,
                ..
            } => {
                for (_, ta) in type_arguments {
                    ta.substitute_alias(rep_symbol, rep_args, actual)?;
                }
                alias_actual.substitute_alias(rep_symbol, rep_args, actual)
            }
            HostExposedAlias {
                actual: actual_type,
                ..
            } => actual_type.substitute_alias(rep_symbol, rep_args, actual),
            Apply(symbol, args, region) if *symbol == rep_symbol => {
                if args.len() == rep_args.len()
                    && args.iter().zip(rep_args.iter()).all(|(t1, t2)| t1 == t2)
                {
                    *self = actual.clone();

                    if let Apply(_, args, _) = self {
                        for arg in args {
                            arg.substitute_alias(rep_symbol, rep_args, actual)?;
                        }
                    }
                    return Ok(());
                }
                Err(*region)
            }
            Apply(_, args, _) => {
                for arg in args {
                    arg.substitute_alias(rep_symbol, rep_args, actual)?;
                }
                Ok(())
            }
            RangedNumber(typ, _) => typ.substitute_alias(rep_symbol, rep_args, actual),
            EmptyRec | EmptyTagUnion | ClosureTag { .. } | Erroneous(_) | Variable(_) => Ok(()),
        }
    }

    pub fn contains_symbol(&self, rep_symbol: Symbol) -> bool {
        use Type::*;

        match self {
            Function(args, closure, ret) => {
                ret.contains_symbol(rep_symbol)
                    || closure.contains_symbol(rep_symbol)
                    || args.iter().any(|arg| arg.contains_symbol(rep_symbol))
            }
            FunctionOrTagUnion(_, _, ext) => ext.contains_symbol(rep_symbol),
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
            Alias {
                symbol: alias_symbol,
                actual: actual_type,
                ..
            } => alias_symbol == &rep_symbol || actual_type.contains_symbol(rep_symbol),
            HostExposedAlias { name, actual, .. } => {
                name == &rep_symbol || actual.contains_symbol(rep_symbol)
            }
            Apply(symbol, _, _) if *symbol == rep_symbol => true,
            Apply(_, args, _) => args.iter().any(|arg| arg.contains_symbol(rep_symbol)),
            RangedNumber(typ, _) => typ.contains_symbol(rep_symbol),
            EmptyRec | EmptyTagUnion | ClosureTag { .. } | Erroneous(_) | Variable(_) => false,
        }
    }

    pub fn contains_variable(&self, rep_variable: Variable) -> bool {
        use Type::*;

        match self {
            ClosureTag { ext: v, .. } | Variable(v) => *v == rep_variable,
            Function(args, closure, ret) => {
                ret.contains_variable(rep_variable)
                    || closure.contains_variable(rep_variable)
                    || args.iter().any(|arg| arg.contains_variable(rep_variable))
            }
            FunctionOrTagUnion(_, _, ext) => ext.contains_variable(rep_variable),
            RecursiveTagUnion(_, tags, ext) | TagUnion(tags, ext) => {
                ext.contains_variable(rep_variable)
                    || tags
                        .iter()
                        .map(|v| v.1.iter())
                        .flatten()
                        .any(|arg| arg.contains_variable(rep_variable))
            }

            Record(fields, ext) => {
                ext.contains_variable(rep_variable)
                    || fields
                        .values()
                        .any(|arg| arg.contains_variable(rep_variable))
            }
            Alias {
                actual: actual_type,
                ..
            } => actual_type.contains_variable(rep_variable),
            HostExposedAlias { actual, .. } => actual.contains_variable(rep_variable),
            Apply(_, args, _) => args.iter().any(|arg| arg.contains_variable(rep_variable)),
            RangedNumber(typ, vars) => {
                typ.contains_variable(rep_variable) || vars.iter().any(|&v| v == rep_variable)
            }
            EmptyRec | EmptyTagUnion | Erroneous(_) => false,
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
            Type::Alias { actual, .. } => actual.shallow_dealias(),
            _ => self,
        }
    }

    pub fn instantiate_aliases(
        &mut self,
        region: Region,
        aliases: &ImMap<Symbol, Alias>,
        var_store: &mut VarStore,
        introduced: &mut ImSet<Variable>,
    ) {
        use Type::*;

        match self {
            Function(args, closure, ret) => {
                for arg in args {
                    arg.instantiate_aliases(region, aliases, var_store, introduced);
                }
                closure.instantiate_aliases(region, aliases, var_store, introduced);
                ret.instantiate_aliases(region, aliases, var_store, introduced);
            }
            FunctionOrTagUnion(_, _, ext) => {
                ext.instantiate_aliases(region, aliases, var_store, introduced);
            }
            RecursiveTagUnion(_, tags, ext) | TagUnion(tags, ext) => {
                for (_, args) in tags {
                    for x in args {
                        x.instantiate_aliases(region, aliases, var_store, introduced);
                    }
                }
                ext.instantiate_aliases(region, aliases, var_store, introduced);
            }
            Record(fields, ext) => {
                for (_, x) in fields.iter_mut() {
                    x.instantiate_aliases(region, aliases, var_store, introduced);
                }
                ext.instantiate_aliases(region, aliases, var_store, introduced);
            }
            HostExposedAlias {
                type_arguments: type_args,
                lambda_set_variables,
                actual: actual_type,
                ..
            }
            | Alias {
                type_arguments: type_args,
                lambda_set_variables,
                actual: actual_type,
                ..
            } => {
                for arg in type_args {
                    arg.1
                        .instantiate_aliases(region, aliases, var_store, introduced);
                }

                for arg in lambda_set_variables {
                    arg.instantiate_aliases(region, aliases, var_store, introduced);
                }

                actual_type.instantiate_aliases(region, aliases, var_store, introduced);
            }
            Apply(symbol, args, _) => {
                if let Some(alias) = aliases.get(symbol) {
                    if args.len() != alias.type_variables.len() {
                        *self = Type::Erroneous(Problem::BadTypeArguments {
                            symbol: *symbol,
                            region,
                            type_got: args.len() as u8,
                            alias_needs: alias.type_variables.len() as u8,
                        });
                        return;
                    }

                    let mut actual = alias.typ.clone();

                    let mut named_args = Vec::with_capacity(args.len());
                    let mut substitution = ImMap::default();

                    // TODO substitute further in args
                    for (
                        Loc {
                            value: (lowercase, placeholder),
                            ..
                        },
                        filler,
                    ) in alias.type_variables.iter().zip(args.iter())
                    {
                        let mut filler = filler.clone();
                        filler.instantiate_aliases(region, aliases, var_store, introduced);
                        named_args.push((lowercase.clone(), filler.clone()));
                        substitution.insert(*placeholder, filler);
                    }

                    // make sure hidden variables are freshly instantiated
                    let mut lambda_set_variables =
                        Vec::with_capacity(alias.lambda_set_variables.len());
                    for typ in alias.lambda_set_variables.iter() {
                        if let Type::Variable(var) = typ.0 {
                            let fresh = var_store.fresh();
                            introduced.insert(fresh);
                            substitution.insert(var, Type::Variable(fresh));
                            lambda_set_variables.push(LambdaSet(Type::Variable(fresh)));
                        } else {
                            unreachable!("at this point there should be only vars in there");
                        }
                    }

                    actual.instantiate_aliases(region, aliases, var_store, introduced);

                    actual.substitute(&substitution);

                    // instantiate recursion variable!
                    if let Type::RecursiveTagUnion(rec_var, mut tags, mut ext) = actual {
                        let new_rec_var = var_store.fresh();
                        substitution.clear();
                        substitution.insert(rec_var, Type::Variable(new_rec_var));

                        for typ in tags.iter_mut().map(|v| v.1.iter_mut()).flatten() {
                            typ.substitute(&substitution);
                        }
                        ext.substitute(&substitution);

                        actual = Type::RecursiveTagUnion(new_rec_var, tags, ext);
                    }

                    *self = Type::Alias {
                        symbol: *symbol,
                        type_arguments: named_args,
                        lambda_set_variables,
                        actual: Box::new(actual),
                        kind: alias.kind,
                    };
                } else {
                    // one of the special-cased Apply types.
                    for x in args {
                        x.instantiate_aliases(region, aliases, var_store, introduced);
                    }
                }
            }
            RangedNumber(typ, _) => {
                typ.instantiate_aliases(region, aliases, var_store, introduced);
            }
            EmptyRec | EmptyTagUnion | ClosureTag { .. } | Erroneous(_) | Variable(_) => {}
        }
    }

    pub fn is_tag_union_like(&self) -> bool {
        matches!(
            self,
            Type::TagUnion(..)
                | Type::RecursiveTagUnion(..)
                | Type::FunctionOrTagUnion(..)
                | Type::EmptyTagUnion
        )
    }

    /// We say a type is "narrow" if no type composing it is a proper sum; that is, no type
    /// composing it is a tag union with more than one variant.
    ///
    /// The types checked here must have all of their non-builtin `Apply`s instantiated, as a
    /// non-instantiated `Apply` would be ambiguous.
    ///
    /// The following are narrow:
    ///
    /// ```roc
    /// U8
    /// [ A I8 ]
    /// [ A [ B [ C U8 ] ] ]
    /// [ A (R a) ] as R a
    /// ```
    ///
    /// The following are not:
    ///
    /// ```roc
    /// [ A I8, B U8 ]
    /// [ A [ B [ Result U8 {} ] ] ]         (Result U8 {} is actually [ Ok U8, Err {} ])
    /// [ A { lst: List (R a) } ] as R a     (List a is morally [ Cons (List a), Nil ] as List a)
    /// ```
    pub fn is_narrow(&self) -> bool {
        match self.shallow_dealias() {
            Type::TagUnion(tags, ext) | Type::RecursiveTagUnion(_, tags, ext) => {
                ext.is_empty_tag_union()
                    && tags.len() == 1
                    && tags[0].1.len() == 1
                    && tags[0].1[0].is_narrow()
            }
            Type::Record(fields, ext) => {
                fields.values().all(|field| field.as_inner().is_narrow()) && ext.is_narrow()
            }
            Type::Function(args, clos, ret) => {
                args.iter().all(|a| a.is_narrow()) && clos.is_narrow() && ret.is_narrow()
            }
            // Lists and sets are morally two-tagged unions, as they can be empty
            Type::Apply(Symbol::LIST_LIST | Symbol::SET_SET, _, _) => false,
            Type::Apply(..) => internal_error!("cannot chase an Apply!"),
            Type::Alias { .. } => internal_error!("should be dealiased"),
            // Non-composite types are trivially narrow
            _ => true,
        }
    }
}

fn symbols_help(tipe: &Type, accum: &mut ImSet<Symbol>) {
    use Type::*;

    match tipe {
        Function(args, closure, ret) => {
            symbols_help(ret, accum);
            symbols_help(closure, accum);
            args.iter().for_each(|arg| symbols_help(arg, accum));
        }
        FunctionOrTagUnion(_, _, ext) => {
            symbols_help(ext, accum);
        }
        RecursiveTagUnion(_, tags, ext) | TagUnion(tags, ext) => {
            symbols_help(ext, accum);
            tags.iter()
                .map(|v| v.1.iter())
                .flatten()
                .for_each(|arg| symbols_help(arg, accum));
        }

        Record(fields, ext) => {
            symbols_help(ext, accum);
            fields.values().for_each(|field| {
                use RecordField::*;

                match field {
                    Optional(arg) => symbols_help(arg, accum),
                    Required(arg) => symbols_help(arg, accum),
                    Demanded(arg) => symbols_help(arg, accum),
                }
            });
        }
        Alias {
            symbol: alias_symbol,
            actual: actual_type,
            ..
        } => {
            accum.insert(*alias_symbol);
            symbols_help(actual_type, accum);
        }
        HostExposedAlias { name, actual, .. } => {
            accum.insert(*name);
            symbols_help(actual, accum);
        }
        Apply(symbol, args, _) => {
            accum.insert(*symbol);
            args.iter().for_each(|arg| symbols_help(arg, accum));
        }
        Erroneous(Problem::CyclicAlias(alias, _, _)) => {
            accum.insert(*alias);
        }
        RangedNumber(typ, _) => {
            symbols_help(typ, accum);
        }
        EmptyRec | EmptyTagUnion | ClosureTag { .. } | Erroneous(_) | Variable(_) => {}
    }
}

fn variables_help(tipe: &Type, accum: &mut ImSet<Variable>) {
    use Type::*;

    match tipe {
        EmptyRec | EmptyTagUnion | Erroneous(_) => (),

        ClosureTag { ext: v, .. } | Variable(v) => {
            accum.insert(*v);
        }

        Function(args, closure, ret) => {
            for arg in args {
                variables_help(arg, accum);
            }
            variables_help(closure, accum);
            variables_help(ret, accum);
        }
        Record(fields, ext) => {
            use RecordField::*;

            for (_, field) in fields {
                match field {
                    Optional(x) => variables_help(x, accum),
                    Required(x) => variables_help(x, accum),
                    Demanded(x) => variables_help(x, accum),
                };
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
        FunctionOrTagUnion(_, _, ext) => {
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
        Alias {
            type_arguments,
            actual,
            ..
        } => {
            for (_, arg) in type_arguments {
                variables_help(arg, accum);
            }
            variables_help(actual, accum);
        }
        HostExposedAlias {
            type_arguments: arguments,
            actual,
            ..
        } => {
            for (_, arg) in arguments {
                variables_help(arg, accum);
            }
            variables_help(actual, accum);
        }
        RangedNumber(typ, vars) => {
            variables_help(typ, accum);
            accum.extend(vars.iter().copied());
        }
        Apply(_, args, _) => {
            for x in args {
                variables_help(x, accum);
            }
        }
    }
}

#[derive(Default)]
pub struct VariableDetail {
    pub type_variables: MutSet<Variable>,
    pub lambda_set_variables: Vec<Variable>,
    pub recursion_variables: MutSet<Variable>,
}

impl VariableDetail {
    pub fn is_empty(&self) -> bool {
        self.type_variables.is_empty()
            && self.lambda_set_variables.is_empty()
            && self.recursion_variables.is_empty()
    }
}

fn variables_help_detailed(tipe: &Type, accum: &mut VariableDetail) {
    use Type::*;

    match tipe {
        EmptyRec | EmptyTagUnion | Erroneous(_) => (),

        ClosureTag { ext: v, .. } | Variable(v) => {
            accum.type_variables.insert(*v);
        }

        Function(args, closure, ret) => {
            for arg in args {
                variables_help_detailed(arg, accum);
            }
            if let Type::Variable(v) = **closure {
                accum.lambda_set_variables.push(v);
            } else {
                variables_help_detailed(closure, accum);
            }

            variables_help_detailed(ret, accum);
        }
        Record(fields, ext) => {
            use RecordField::*;

            for (_, field) in fields {
                match field {
                    Optional(x) => variables_help_detailed(x, accum),
                    Required(x) => variables_help_detailed(x, accum),
                    Demanded(x) => variables_help_detailed(x, accum),
                };
            }
            variables_help_detailed(ext, accum);
        }
        TagUnion(tags, ext) => {
            for (_, args) in tags {
                for x in args {
                    variables_help_detailed(x, accum);
                }
            }
            variables_help_detailed(ext, accum);
        }
        FunctionOrTagUnion(_, _, ext) => {
            variables_help_detailed(ext, accum);
        }
        RecursiveTagUnion(rec, tags, ext) => {
            for (_, args) in tags {
                for x in args {
                    variables_help_detailed(x, accum);
                }
            }
            variables_help_detailed(ext, accum);

            // just check that this is actually a recursive type
            debug_assert!(accum.type_variables.contains(rec));

            // this rec var doesn't need to be in flex_vars or rigid_vars
            accum.type_variables.remove(rec);

            accum.recursion_variables.insert(*rec);
        }
        Alias {
            type_arguments,
            actual,
            ..
        } => {
            for (_, arg) in type_arguments {
                variables_help_detailed(arg, accum);
            }
            variables_help_detailed(actual, accum);
        }
        HostExposedAlias {
            type_arguments: arguments,
            actual,
            ..
        } => {
            for (_, arg) in arguments {
                variables_help_detailed(arg, accum);
            }
            variables_help_detailed(actual, accum);
        }
        RangedNumber(typ, vars) => {
            variables_help_detailed(typ, accum);
            accum.type_variables.extend(vars);
        }
        Apply(_, args, _) => {
            for x in args {
                variables_help_detailed(x, accum);
            }
        }
    }
}

#[derive(Debug)]
pub struct RecordStructure {
    /// Invariant: these should be sorted!
    pub fields: Vec<(Lowercase, RecordField<Variable>)>,
    pub ext: Variable,
}

#[derive(Debug)]
pub struct TagUnionStructure<'a> {
    /// Invariant: these should be sorted!
    pub fields: Vec<(TagName, &'a [Variable])>,
    pub ext: Variable,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PReason {
    TypedArg {
        opt_name: Option<Symbol>,
        index: Index,
    },
    WhenMatch {
        index: Index,
    },
    TagArg {
        tag_name: TagName,
        index: Index,
    },
    PatternGuard,
    OptionalField,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnnotationSource {
    TypedIfBranch {
        index: Index,
        num_branches: usize,
        region: Region,
    },
    TypedWhenBranch {
        index: Index,
        region: Region,
    },
    TypedBody {
        region: Region,
    },
}

impl AnnotationSource {
    pub fn region(&self) -> Region {
        match self {
            &Self::TypedIfBranch { region, .. }
            | &Self::TypedWhenBranch { region, .. }
            | &Self::TypedBody { region, .. } => region,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Reason {
    FnArg {
        name: Option<Symbol>,
        arg_index: Index,
    },
    FnCall {
        name: Option<Symbol>,
        arity: u8,
    },
    LowLevelOpArg {
        op: LowLevel,
        arg_index: Index,
    },
    ForeignCallArg {
        foreign_symbol: ForeignSymbol,
        arg_index: Index,
    },
    FloatLiteral,
    IntLiteral,
    NumLiteral,
    StrInterpolation,
    WhenBranch {
        index: Index,
    },
    WhenGuard,
    ExpectCondition,
    IfCondition,
    IfBranch {
        index: Index,
        total_branches: usize,
    },
    ElemInList {
        index: Index,
    },
    RecordUpdateValue(Lowercase),
    RecordUpdateKeys(Symbol, SendMap<Lowercase, Region>),
    RecordDefaultField(Lowercase),
    NumericLiteralSuffix,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Category {
    Lookup(Symbol),
    CallResult(Option<Symbol>, CalledVia),
    LowLevelOpResult(LowLevel),
    ForeignCall,
    TagApply {
        tag_name: TagName,
        args_count: usize,
    },
    Lambda,
    Uniqueness,
    ClosureSize,
    StrInterpolation,

    // storing variables in the ast
    Storage(&'static str, u32),

    // control flow
    If,
    When,

    // types
    Float,
    Int,
    Num,
    List,
    Str,

    // records
    Record,
    Accessor(Lowercase),
    Access(Lowercase),
    DefaultValue(Lowercase), // for setting optional fields
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternCategory {
    Record,
    EmptyRecord,
    PatternGuard,
    PatternDefault,
    Set,
    Map,
    Ctor(TagName),
    Str,
    Num,
    Int,
    Float,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AliasKind {
    /// A structural alias is something like
    ///   List a : [ Nil, Cons a (List a) ]
    /// It is typed structurally, so that a `List U8` is always equal to a `[ Nil ]_`, for example.
    Structural,
    /// An opaque alias corresponds to an opaque type from the language syntax, like
    ///   Age := U32
    /// It is type nominally, so that `Age` is never equal to `U8` - the only way to unwrap the
    /// structural type inside `Age` is to unwrap the opaque, so `Age` = `@Age U8`.
    Opaque,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Alias {
    pub region: Region,
    pub type_variables: Vec<Loc<(Lowercase, Variable)>>,

    /// lambda set variables, e.g. the one annotating the arrow in
    /// a |c|-> b
    pub lambda_set_variables: Vec<LambdaSet>,

    pub recursion_variables: MutSet<Variable>,

    pub typ: Type,

    pub kind: AliasKind,
}

impl Alias {
    pub fn header_region(&self) -> Region {
        Region::across_all(
            [self.region]
                .iter()
                .chain(self.type_variables.iter().map(|tv| &tv.region)),
        )
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum Problem {
    CanonicalizationProblem,
    CircularType(Symbol, Box<ErrorType>, Region),
    CyclicAlias(Symbol, Region, Vec<Symbol>),
    UnrecognizedIdent(Ident),
    Shadowed(Region, Loc<Ident>),
    BadTypeArguments {
        symbol: Symbol,
        region: Region,
        type_got: u8,
        alias_needs: u8,
    },
    InvalidModule,
    SolvedTypeError,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Mismatch {
    TypeMismatch,
    IfConditionNotBool,
    InconsistentIfElse,
    InconsistentWhenBranches,
    CanonicalizationProblem,
    TypeNotInRange,
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum ErrorType {
    Infinite,
    Type(Symbol, Vec<ErrorType>),
    FlexVar(Lowercase),
    RigidVar(Lowercase),
    Record(SendMap<Lowercase, RecordField<ErrorType>>, TypeExt),
    TagUnion(SendMap<TagName, Vec<ErrorType>>, TypeExt),
    RecursiveTagUnion(Box<ErrorType>, SendMap<TagName, Vec<ErrorType>>, TypeExt),
    Function(Vec<ErrorType>, Box<ErrorType>, Box<ErrorType>),
    Alias(Symbol, Vec<ErrorType>, Box<ErrorType>),
    Range(Box<ErrorType>, Vec<ErrorType>),
    Error,
}

impl std::fmt::Debug for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO remove clone
        write!(f, "{:?}", write_debug_error_type(self.clone()))
    }
}

impl ErrorType {
    pub fn unwrap_alias(self) -> ErrorType {
        match self {
            ErrorType::Alias(_, _, real) => real.unwrap_alias(),
            real => real,
        }
    }

    /// Adds all named type variables used in the type to a set.
    pub fn add_names(&self, taken: &mut MutSet<Lowercase>) {
        use ErrorType::*;
        match self {
            Infinite => {}
            Type(_, ts) => ts.iter().for_each(|t| t.add_names(taken)),
            FlexVar(v) => {
                taken.insert(v.clone());
            }
            RigidVar(v) => {
                taken.insert(v.clone());
            }
            Record(fields, ext) => {
                fields
                    .iter()
                    .for_each(|(_, t)| t.as_inner().add_names(taken));
                ext.add_names(taken);
            }
            TagUnion(tags, ext) => {
                tags.iter()
                    .for_each(|(_, ts)| ts.iter().for_each(|t| t.add_names(taken)));
                ext.add_names(taken);
            }
            RecursiveTagUnion(t, tags, ext) => {
                t.add_names(taken);
                tags.iter()
                    .for_each(|(_, ts)| ts.iter().for_each(|t| t.add_names(taken)));
                ext.add_names(taken);
            }
            Function(args, capt, ret) => {
                args.iter().for_each(|t| t.add_names(taken));
                capt.add_names(taken);
                ret.add_names(taken);
            }
            Alias(_, ts, t) => {
                ts.iter().for_each(|t| {
                    t.add_names(taken);
                });
                t.add_names(taken);
            }
            Range(typ, ts) => {
                typ.add_names(taken);
                ts.iter().for_each(|t| {
                    t.add_names(taken);
                });
            }
            Error => {}
        }
    }
}

pub fn write_error_type(home: ModuleId, interns: &Interns, error_type: ErrorType) -> String {
    let mut buf = String::new();
    write_error_type_help(home, interns, error_type, &mut buf, Parens::Unnecessary);

    buf
}

fn write_error_type_help(
    home: ModuleId,
    interns: &Interns,
    error_type: ErrorType,
    buf: &mut String,
    parens: Parens,
) {
    use ErrorType::*;

    match error_type {
        Infinite => buf.push('∞'),
        Error => buf.push('?'),
        FlexVar(name) => buf.push_str(name.as_str()),
        RigidVar(name) => buf.push_str(name.as_str()),
        Type(symbol, arguments) => {
            let write_parens = parens == Parens::InTypeParam && !arguments.is_empty();

            if write_parens {
                buf.push('(');
            }
            buf.push_str(symbol.ident_str(interns).as_str());

            for arg in arguments {
                buf.push(' ');

                write_error_type_help(home, interns, arg, buf, Parens::InTypeParam);
            }

            if write_parens {
                buf.push(')');
            }
        }
        Alias(Symbol::NUM_NUM, mut arguments, _actual) => {
            debug_assert!(arguments.len() == 1);

            let argument = arguments.remove(0);

            match argument {
                Type(Symbol::NUM_INTEGER, _) => {
                    buf.push_str("Int");
                }
                Type(Symbol::NUM_FLOATINGPOINT, _) => {
                    buf.push_str("F64");
                }
                other => {
                    let write_parens = parens == Parens::InTypeParam;

                    if write_parens {
                        buf.push('(');
                    }
                    buf.push_str("Num ");
                    write_error_type_help(home, interns, other, buf, Parens::InTypeParam);

                    if write_parens {
                        buf.push(')');
                    }
                }
            }
        }
        Function(arguments, _closure, result) => {
            let write_parens = parens != Parens::Unnecessary;

            if write_parens {
                buf.push(')');
            }

            let mut it = arguments.into_iter().peekable();

            while let Some(arg) = it.next() {
                write_error_type_help(home, interns, arg, buf, Parens::InFn);
                if it.peek().is_some() {
                    buf.push_str(", ");
                }
            }

            buf.push_str(" -> ");

            write_error_type_help(home, interns, *result, buf, Parens::InFn);

            if write_parens {
                buf.push(')');
            }
        }
        Record(fields, ext) => {
            buf.push('{');

            for (label, field) in fields {
                use RecordField::*;

                buf.push_str(label.as_str());

                let content = match field {
                    Optional(content) => {
                        buf.push_str(" ? ");
                        content
                    }
                    Required(content) => {
                        buf.push_str(" : ");
                        content
                    }
                    Demanded(content) => {
                        buf.push_str(" : ");
                        content
                    }
                };

                write_error_type_help(home, interns, content, buf, Parens::Unnecessary);
            }

            buf.push('}');
            write_type_ext(ext, buf);
        }

        other => todo!("cannot format {:?} yet", other),
    }
}

pub fn write_debug_error_type(error_type: ErrorType) -> String {
    let mut buf = String::new();
    write_debug_error_type_help(error_type, &mut buf, Parens::Unnecessary);

    buf
}

fn write_debug_error_type_help(error_type: ErrorType, buf: &mut String, parens: Parens) {
    use ErrorType::*;

    match error_type {
        Infinite => buf.push('∞'),
        Error => buf.push('?'),
        FlexVar(name) => buf.push_str(name.as_str()),
        RigidVar(name) => buf.push_str(name.as_str()),
        Type(symbol, arguments) => {
            let write_parens = parens == Parens::InTypeParam && !arguments.is_empty();

            if write_parens {
                buf.push('(');
            }
            buf.push_str(&format!("{:?}", symbol));

            for arg in arguments {
                buf.push(' ');

                write_debug_error_type_help(arg, buf, Parens::InTypeParam);
            }

            if write_parens {
                buf.push(')');
            }
        }
        Alias(Symbol::NUM_NUM, mut arguments, _actual) => {
            debug_assert!(arguments.len() == 1);

            let argument = arguments.remove(0);

            match argument {
                Type(Symbol::NUM_INTEGER, _) => {
                    buf.push_str("Int");
                }
                Type(Symbol::NUM_FLOATINGPOINT, _) => {
                    buf.push_str("F64");
                }
                other => {
                    let write_parens = parens == Parens::InTypeParam;

                    if write_parens {
                        buf.push('(');
                    }
                    buf.push_str("Num ");
                    write_debug_error_type_help(other, buf, Parens::InTypeParam);

                    if write_parens {
                        buf.push(')');
                    }
                }
            }
        }
        Alias(symbol, arguments, _actual) => {
            let write_parens = parens == Parens::InTypeParam && !arguments.is_empty();

            if write_parens {
                buf.push('(');
            }
            buf.push_str(&format!("{:?}", symbol));

            for arg in arguments {
                buf.push(' ');

                write_debug_error_type_help(arg, buf, Parens::InTypeParam);
            }

            // useful for debugging
            let write_out_alias = true;
            if write_out_alias {
                buf.push_str("[[ but really ");
                write_debug_error_type_help(*_actual, buf, Parens::Unnecessary);
                buf.push_str("]]");
            }

            if write_parens {
                buf.push(')');
            }
        }
        Function(arguments, _closure, result) => {
            let write_parens = parens != Parens::Unnecessary;

            if write_parens {
                buf.push('(');
            }

            let mut it = arguments.into_iter().peekable();

            while let Some(arg) = it.next() {
                write_debug_error_type_help(arg, buf, Parens::InFn);
                if it.peek().is_some() {
                    buf.push_str(", ");
                }
            }

            buf.push_str(" -> ");

            write_debug_error_type_help(*result, buf, Parens::InFn);

            if write_parens {
                buf.push(')');
            }
        }
        Record(fields, ext) => {
            buf.push('{');

            for (label, field) in fields {
                use RecordField::*;

                buf.push_str(label.as_str());

                let content = match field {
                    Optional(content) => {
                        buf.push_str(" ? ");
                        content
                    }
                    Required(content) => {
                        buf.push_str(" : ");
                        content
                    }
                    Demanded(content) => {
                        buf.push_str(" : ");
                        content
                    }
                };

                write_debug_error_type_help(content, buf, Parens::Unnecessary);
            }

            buf.push('}');
            write_type_ext(ext, buf);
        }
        TagUnion(tags, ext) => {
            buf.push('[');

            let mut it = tags.into_iter().peekable();

            while let Some((tag, args)) = it.next() {
                buf.push_str(&format!("{:?}", tag));
                for arg in args {
                    buf.push(' ');
                    write_debug_error_type_help(arg, buf, Parens::InTypeParam);
                }

                if it.peek().is_some() {
                    buf.push_str(", ");
                }
            }

            buf.push(']');
            write_type_ext(ext, buf);
        }
        RecursiveTagUnion(rec, tags, ext) => {
            buf.push('[');

            let mut it = tags.into_iter().peekable();
            while let Some((tag, args)) = it.next() {
                buf.push_str(&format!("{:?}", tag));
                for arg in args {
                    buf.push(' ');
                    write_debug_error_type_help(arg, buf, Parens::Unnecessary);
                }

                if it.peek().is_some() {
                    buf.push_str(", ");
                }
            }

            buf.push(']');
            write_type_ext(ext, buf);

            buf.push_str(" as ");

            write_debug_error_type_help(*rec, buf, Parens::Unnecessary);
        }
        Range(typ, types) => {
            write_debug_error_type_help(*typ, buf, parens);
            buf.push('<');

            let mut it = types.into_iter().peekable();
            while let Some(typ) = it.next() {
                write_debug_error_type_help(typ, buf, Parens::Unnecessary);

                if it.peek().is_some() {
                    buf.push_str(", ");
                }
            }

            buf.push('>');
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum TypeExt {
    Closed,
    FlexOpen(Lowercase),
    RigidOpen(Lowercase),
}

impl TypeExt {
    pub fn add_names(&self, taken: &mut MutSet<Lowercase>) {
        use TypeExt::*;
        match self {
            Closed => {}
            FlexOpen(n) | RigidOpen(n) => {
                taken.insert(n.clone());
            }
        }
    }
}

fn write_type_ext(ext: TypeExt, buf: &mut String) {
    use TypeExt::*;
    match ext {
        Closed => {}
        FlexOpen(lowercase) | RigidOpen(lowercase) => {
            buf.push_str(lowercase.as_str());
        }
    }
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

#[derive(Debug, Copy, Clone)]
pub struct RecordFieldsError;

pub fn gather_fields_unsorted_iter(
    subs: &Subs,
    other_fields: RecordFields,
    mut var: Variable,
) -> Result<
    (
        impl Iterator<Item = (&Lowercase, RecordField<Variable>)> + '_,
        Variable,
    ),
    RecordFieldsError,
> {
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

    let mut stack = vec![other_fields];

    loop {
        match subs.get_content_without_compacting(var) {
            Structure(Record(sub_fields, sub_ext)) => {
                stack.push(*sub_fields);

                var = *sub_ext;
            }

            Alias(_, _, actual_var) => {
                // TODO according to elm/compiler: "TODO may be dropping useful alias info here"
                var = *actual_var;
            }

            Structure(EmptyRecord) => break,
            FlexVar(_) => break,

            // TODO investigate apparently this one pops up in the reporting tests!
            RigidVar(_) => break,

            _ => return Err(RecordFieldsError),
        }
    }

    let it = stack
        .into_iter()
        .map(|fields| fields.iter_all())
        .flatten()
        .map(move |(i1, i2, i3)| {
            let field_name: &Lowercase = &subs[i1];
            let variable = subs[i2];
            let record_field: RecordField<Variable> = subs[i3].map(|_| variable);

            (field_name, record_field)
        });

    Ok((it, var))
}

pub fn gather_fields(
    subs: &Subs,
    other_fields: RecordFields,
    var: Variable,
) -> Result<RecordStructure, RecordFieldsError> {
    let (it, ext) = gather_fields_unsorted_iter(subs, other_fields, var)?;

    let mut result: Vec<_> = it
        .map(|(ref_label, field)| (ref_label.clone(), field))
        .collect();

    result.sort_by(|(a, _), (b, _)| a.cmp(b));

    Ok(RecordStructure {
        fields: result,
        ext,
    })
}

pub fn gather_tags_unsorted_iter(
    subs: &Subs,
    other_fields: UnionTags,
    mut var: Variable,
) -> (
    impl Iterator<Item = (&TagName, VariableSubsSlice)> + '_,
    Variable,
) {
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

    let mut stack = vec![other_fields];

    loop {
        match subs.get_content_without_compacting(var) {
            Structure(TagUnion(sub_fields, sub_ext)) => {
                stack.push(*sub_fields);

                var = *sub_ext;
            }

            Structure(FunctionOrTagUnion(_tag_name_index, _, _sub_ext)) => {
                todo!("this variant does not use SOA yet, and therefore this case is unreachable right now")
                //                let sub_fields: UnionTags = (*tag_name_index).into();
                //                stack.push(sub_fields);
                //
                //                var = *sub_ext;
            }

            Structure(RecursiveTagUnion(_, _sub_fields, _sub_ext)) => {
                todo!("this variant does not use SOA yet, and therefore this case is unreachable right now")
                //                stack.push(*sub_fields);
                //
                //                var = *sub_ext;
            }

            Alias(_, _, actual_var) => {
                // TODO according to elm/compiler: "TODO may be dropping useful alias info here"
                var = *actual_var;
            }

            Structure(EmptyTagUnion) => break,
            FlexVar(_) => break,

            // TODO investigate this likely can happen when there is a type error
            RigidVar(_) => break,

            other => unreachable!("something weird ended up in a tag union type: {:?}", other),
        }
    }

    let it = stack
        .into_iter()
        .map(|union_tags| union_tags.iter_all())
        .flatten()
        .map(move |(i1, i2)| {
            let tag_name: &TagName = &subs[i1];
            let subs_slice = subs[i2];

            (tag_name, subs_slice)
        });

    (it, var)
}

pub fn gather_tags_slices(
    subs: &Subs,
    other_fields: UnionTags,
    var: Variable,
) -> (Vec<(TagName, VariableSubsSlice)>, Variable) {
    let (it, ext) = gather_tags_unsorted_iter(subs, other_fields, var);

    let mut result: Vec<_> = it
        .map(|(ref_label, field): (_, VariableSubsSlice)| (ref_label.clone(), field))
        .collect();

    result.sort_by(|(a, _), (b, _)| a.cmp(b));

    (result, ext)
}

pub fn gather_tags(subs: &Subs, other_fields: UnionTags, var: Variable) -> TagUnionStructure {
    let (it, ext) = gather_tags_unsorted_iter(subs, other_fields, var);

    let mut result: Vec<_> = it
        .map(|(ref_label, field): (_, VariableSubsSlice)| {
            (ref_label.clone(), subs.get_subs_slice(field))
        })
        .collect();

    result.sort_by(|(a, _), (b, _)| a.cmp(b));

    TagUnionStructure {
        fields: result,
        ext,
    }
}
