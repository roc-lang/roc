use crate::boolean_algebra;
use crate::pretty_print::Parens;
use crate::subs::{Subs, VarStore, Variable};
use inlinable_string::InlinableString;
use roc_collections::all::{union, ImMap, ImSet, Index, MutMap, MutSet, SendMap};
use roc_module::ident::{Ident, Lowercase, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_region::all::{Located, Region};
use std::fmt;

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
    pub fn is_recursive(&self) -> bool {
        match self {
            Type::RecursiveTagUnion(_, _, _) => true,
            Type::Alias(Symbol::ATTR_ATTR, _, actual) => actual.is_recursive(),
            _ => false,
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
            Boolean(b) => {
                let mut mapper = |var| match substitutions.get(&var) {
                    Some(Type::Variable(new_var)) => *new_var,
                    Some(_) => panic!("cannot substitute boolean var for Type"),
                    None => var,
                };

                *b = b.map_variables(&mut mapper)
            }

            EmptyRec | EmptyTagUnion | Erroneous(_) => {}
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

    pub fn contains_variable(&self, rep_variable: Variable) -> bool {
        use Type::*;

        match self {
            Variable(v) => *v == rep_variable,
            Function(args, ret) => {
                ret.contains_variable(rep_variable)
                    || args.iter().any(|arg| arg.contains_variable(rep_variable))
            }
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
            Alias(_, _, actual_type) => actual_type.contains_variable(rep_variable),
            Apply(_, args) => args.iter().any(|arg| arg.contains_variable(rep_variable)),
            EmptyRec | EmptyTagUnion | Erroneous(_) | Boolean(_) => false,
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

    pub fn instantiate_aliases(
        &mut self,
        region: Region,
        aliases: &ImMap<Symbol, Alias>,
        var_store: &mut VarStore,
        introduced: &mut ImSet<Variable>,
    ) {
        use Type::*;

        match self {
            Function(args, ret) => {
                for arg in args {
                    arg.instantiate_aliases(region, aliases, var_store, introduced);
                }
                ret.instantiate_aliases(region, aliases, var_store, introduced);
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
                for x in fields.iter_mut() {
                    x.instantiate_aliases(region, aliases, var_store, introduced);
                }
                ext.instantiate_aliases(region, aliases, var_store, introduced);
            }
            Alias(_, type_args, actual_type) => {
                for arg in type_args {
                    arg.1
                        .instantiate_aliases(region, aliases, var_store, introduced);
                }

                actual_type.instantiate_aliases(region, aliases, var_store, introduced);
            }
            Apply(Symbol::ATTR_ATTR, attr_args) => {
                use boolean_algebra::Bool;

                let mut substitution = ImMap::default();

                if let Apply(symbol, _) = attr_args[1] {
                    if let Some(alias) = aliases.get(&symbol) {
                        if let Some(Bool::Container(unbound_cvar, mvars1)) =
                            alias.uniqueness.clone()
                        {
                            debug_assert!(mvars1.is_empty());

                            if let Type::Boolean(Bool::Container(bound_cvar, mvars2)) =
                                &attr_args[0]
                            {
                                debug_assert!(mvars2.is_empty());
                                substitution.insert(unbound_cvar, Type::Variable(*bound_cvar));
                            }
                        }
                    }
                }

                for x in attr_args {
                    x.instantiate_aliases(region, aliases, var_store, introduced);
                    if !substitution.is_empty() {
                        x.substitute(&substitution);
                    }
                }
            }
            Apply(symbol, args) => {
                if let Some(alias) = aliases.get(symbol) {
                    if args.len() != alias.vars.len() {
                        *self = Type::Erroneous(Problem::BadTypeArguments {
                            symbol: *symbol,
                            region,
                            type_got: args.len() as u8,
                            alias_needs: alias.vars.len() as u8,
                        });
                        return;
                    }

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
                        filler.instantiate_aliases(region, aliases, var_store, introduced);
                        named_args.push((lowercase.clone(), filler.clone()));
                        substitution.insert(*placeholder, filler);
                    }

                    use boolean_algebra::Bool;

                    // Instantiate "hidden" uniqueness variables
                    //
                    // Aliases can hide uniqueness variables: e.g. in
                    //
                    // Model : { x : Int, y : Bool }
                    //
                    // Its lifted variant is
                    //
                    // Attr a Model
                    //
                    // where the `a` doesn't really mention the attributes on the fields.
                    for variable in actual.variables() {
                        if !substitution.contains_key(&variable) {
                            // Leave attributes on recursion variables untouched!
                            //
                            // In a recursive type like
                            //
                            // > [ Z, S Peano ] as Peano
                            //
                            // By default the lifted version is
                            //
                            // > Attr a ([ Z, S (Attr b Peano) ] as Peano)
                            //
                            // But, it must be true that a = b because Peano is self-recursive.
                            // Therefore we earlier have substituted
                            //
                            // > Attr a ([ Z, S (Attr a Peano) ] as Peano)
                            //
                            // And now we must make sure the `a`s stay the same variable, i.e.
                            // don't re-instantiate it here.
                            if let Some(Bool::Container(unbound_cvar, _)) = alias.uniqueness {
                                if variable == unbound_cvar {
                                    introduced.insert(variable);
                                    continue;
                                }
                            }
                            let var = var_store.fresh();
                            substitution.insert(variable, Type::Variable(var));

                            introduced.insert(var);
                        }
                    }

                    actual.substitute(&substitution);
                    actual.instantiate_aliases(region, aliases, var_store, introduced);

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
                    // one of the special-cased Apply types.
                    for x in args {
                        x.instantiate_aliases(region, aliases, var_store, introduced);
                    }
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
            for (_, arg) in args {
                variables_help(arg, accum);
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

pub struct RecordStructure {
    pub fields: MutMap<Lowercase, Variable>,
    pub ext: Variable,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PReason {
    WhenMatch { index: Index },
    TagArg { tag_name: TagName, index: Index },
    PatternGuard,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnnotationSource {
    TypedIfBranch { index: Index, num_branches: usize },
    TypedWhenBranch { index: Index },
    TypedBody { region: Region },
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
    FloatLiteral,
    IntLiteral,
    NumLiteral,
    InterpolatedStringVar,
    WhenBranch {
        index: Index,
    },
    WhenGuard,
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
}

#[derive(PartialEq, Debug, Clone)]
pub enum Category {
    Lookup(Symbol),
    CallResult(Option<Symbol>),
    LowLevelOpResult(LowLevel),
    TagApply(TagName),
    Lambda,
    Uniqueness,

    // storing variables in the ast
    Storage,

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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternCategory {
    Record,
    EmptyRecord,
    PatternGuard,
    Set,
    Map,
    Ctor(TagName),
    Str,
    Num,
    Int,
    Float,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Alias {
    pub region: Region,
    pub vars: Vec<Located<(Lowercase, Variable)>>,
    pub uniqueness: Option<boolean_algebra::Bool>,
    pub typ: Type,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Problem {
    CanonicalizationProblem,
    CircularType(Symbol, ErrorType, Region),
    CyclicAlias(Symbol, Region, Vec<Symbol>),
    UnrecognizedIdent(InlinableString),
    Shadowed(Region, Located<Ident>),
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
}

#[derive(PartialEq, Eq, Clone)]
pub enum ErrorType {
    Infinite,
    Type(Symbol, Vec<ErrorType>),
    FlexVar(Lowercase),
    RigidVar(Lowercase),
    Record(SendMap<Lowercase, ErrorType>, TypeExt),
    TagUnion(SendMap<TagName, Vec<ErrorType>>, TypeExt),
    RecursiveTagUnion(Box<ErrorType>, SendMap<TagName, Vec<ErrorType>>, TypeExt),
    Function(Vec<ErrorType>, Box<ErrorType>),
    Alias(Symbol, Vec<(Lowercase, ErrorType)>, Box<ErrorType>),
    Boolean(boolean_algebra::Bool),
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
        Infinite => buf.push_str("∞"),
        Error => buf.push_str("?"),
        FlexVar(name) => buf.push_str(name.as_str()),
        RigidVar(name) => buf.push_str(name.as_str()),
        Type(symbol, arguments) => {
            let write_parens = parens == Parens::InTypeParam && !arguments.is_empty();

            if write_parens {
                buf.push('(');
            }
            buf.push_str(symbol.ident_string(interns));

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

            let argument = arguments.remove(0).1;

            match argument {
                Type(Symbol::NUM_INTEGER, _) => {
                    buf.push_str("Int");
                }
                Type(Symbol::NUM_FLOATINGPOINT, _) => {
                    buf.push_str("Float");
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
        Function(arguments, result) => {
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

            for (label, content) in fields {
                buf.push_str(label.as_str());
                buf.push_str(": ");
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
        Infinite => buf.push_str("∞"),
        Error => buf.push_str("?"),
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

            let argument = arguments.remove(0).1;

            match argument {
                Type(Symbol::NUM_INTEGER, _) => {
                    buf.push_str("Int");
                }
                Type(Symbol::NUM_FLOATINGPOINT, _) => {
                    buf.push_str("Float");
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

                write_debug_error_type_help(arg.1, buf, Parens::InTypeParam);
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
        Function(arguments, result) => {
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

            for (label, content) in fields {
                buf.push_str(label.as_str());
                buf.push_str(": ");
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
                    buf.push_str(" ");
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
                    buf.push_str(" ");
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

        Boolean(boolean_algebra::Bool::Shared) => buf.push_str("Shared"),
        Boolean(boolean_algebra::Bool::Container(mvar, cvars)) => {
            buf.push_str(&format!("Container({:?}, {:?})", mvar, cvars))
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TypeExt {
    Closed,
    FlexOpen(Lowercase),
    RigidOpen(Lowercase),
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

pub fn gather_fields(
    subs: &Subs,
    fields: MutMap<Lowercase, Variable>,
    var: Variable,
) -> RecordStructure {
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

    match subs.get_without_compacting(var).content {
        Structure(Record(sub_fields, sub_ext)) => {
            gather_fields(subs, union(fields, &sub_fields), sub_ext)
        }

        Alias(_, _, var) => {
            // TODO according to elm/compiler: "TODO may be dropping useful alias info here"
            gather_fields(subs, fields, var)
        }

        _ => RecordStructure { fields, ext: var },
    }
}
