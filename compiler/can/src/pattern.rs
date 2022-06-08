use crate::annotation::freshen_opaque_def;
use crate::env::Env;
use crate::expr::{canonicalize_expr, unescape_char, Expr, IntValue, Output};
use crate::num::{
    finish_parsing_base, finish_parsing_float, finish_parsing_num, FloatBound, IntBound, NumBound,
    ParsedNumResult,
};
use crate::scope::Scope;
use roc_module::ident::{Ident, Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_parse::ast::{self, StrLiteral, StrSegment};
use roc_parse::pattern::PatternType;
use roc_problem::can::{MalformedPatternProblem, Problem, RuntimeError, ShadowKind};
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{LambdaSet, OptAbleVar, PatternCategory, Type};

/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
#[derive(Clone, Debug)]
pub enum Pattern {
    Identifier(Symbol),
    AppliedTag {
        whole_var: Variable,
        ext_var: Variable,
        tag_name: TagName,
        arguments: Vec<(Variable, Loc<Pattern>)>,
    },
    UnwrappedOpaque {
        whole_var: Variable,
        opaque: Symbol,
        argument: Box<(Variable, Loc<Pattern>)>,

        // The following help us link this opaque reference to the type specified by its
        // definition, which we then use during constraint generation. For example
        // suppose we have
        //
        //   Id n := [Id U64 n]
        //   strToBool : Str -> Bool
        //
        //   f = \@Id who -> strToBool who
        //
        // Then `opaque` is "Id", `argument` is "who", but this is not enough for us to
        // infer the type of the expression as "Id Str" - we need to link the specialized type of
        // the variable "n".
        // That's what `specialized_def_type` and `type_arguments` are for; they are specialized
        // for the expression from the opaque definition. `type_arguments` is something like
        // [(n, fresh1)], and `specialized_def_type` becomes "[Id U64 fresh1]".
        specialized_def_type: Box<Type>,
        type_arguments: Vec<OptAbleVar>,
        lambda_set_variables: Vec<LambdaSet>,
    },
    RecordDestructure {
        whole_var: Variable,
        ext_var: Variable,
        destructs: Vec<Loc<RecordDestruct>>,
    },
    NumLiteral(Variable, Box<str>, IntValue, NumBound),
    IntLiteral(Variable, Variable, Box<str>, IntValue, IntBound),
    FloatLiteral(Variable, Variable, Box<str>, f64, FloatBound),
    StrLiteral(Box<str>),
    SingleQuote(char),
    Underscore,

    /// An identifier that marks a specialization of an ability member.
    /// For example, given an ability member definition `hash : a -> U64 | a has Hash`,
    /// there may be the specialization `hash : Bool -> U64`. In this case we generate a
    /// new symbol for the specialized "hash" identifier.
    AbilityMemberSpecialization {
        /// The symbol for this specialization.
        ident: Symbol,
        /// The ability name being specialized.
        specializes: Symbol,
    },

    // Runtime Exceptions
    Shadowed(Region, Loc<Ident>, Symbol),
    OpaqueNotInScope(Loc<Ident>),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Region),
    // parse error patterns
    MalformedPattern(MalformedPatternProblem, Region),
}

impl Pattern {
    pub fn opt_var(&self) -> Option<Variable> {
        use Pattern::*;
        match self {
            Identifier(_) => None,

            AppliedTag { whole_var, .. } => Some(*whole_var),
            UnwrappedOpaque { whole_var, .. } => Some(*whole_var),
            RecordDestructure { whole_var, .. } => Some(*whole_var),
            NumLiteral(var, ..) => Some(*var),
            IntLiteral(var, ..) => Some(*var),
            FloatLiteral(var, ..) => Some(*var),
            StrLiteral(_) => None,
            SingleQuote(_) => None,
            Underscore => None,

            AbilityMemberSpecialization { .. } => None,

            Shadowed(..) | OpaqueNotInScope(..) | UnsupportedPattern(..) | MalformedPattern(..) => {
                None
            }
        }
    }

    /// Is this pattern sure to cover all instances of a type T, assuming it typechecks against T?
    pub fn surely_exhaustive(&self) -> bool {
        use Pattern::*;
        match self {
            Identifier(..)
            | Underscore
            | Shadowed(..)
            | OpaqueNotInScope(..)
            | UnsupportedPattern(..)
            | MalformedPattern(..)
            | AbilityMemberSpecialization { .. } => true,
            RecordDestructure { destructs, .. } => destructs.is_empty(),
            AppliedTag { .. }
            | NumLiteral(..)
            | IntLiteral(..)
            | FloatLiteral(..)
            | StrLiteral(..)
            | SingleQuote(..) => false,
            UnwrappedOpaque { argument, .. } => {
                // Opaques can only match against one constructor (the opaque symbol), so this is
                // surely exhaustive against T if the inner pattern is surely exhaustive against
                // its type U.
                argument.1.value.surely_exhaustive()
            }
        }
    }

    pub fn category(&self) -> PatternCategory {
        use Pattern::*;
        use PatternCategory as C;

        match self {
            Identifier(_) => C::PatternDefault,

            AppliedTag { tag_name, .. } => C::Ctor(tag_name.clone()),
            UnwrappedOpaque { opaque, .. } => C::Opaque(*opaque),
            RecordDestructure { destructs, .. } if destructs.is_empty() => C::EmptyRecord,
            RecordDestructure { .. } => C::Record,
            NumLiteral(..) => C::Num,
            IntLiteral(..) => C::Int,
            FloatLiteral(..) => C::Float,
            StrLiteral(_) => C::Str,
            SingleQuote(_) => C::Character,
            Underscore => C::PatternDefault,

            AbilityMemberSpecialization { .. } => C::PatternDefault,

            Shadowed(..) | OpaqueNotInScope(..) | UnsupportedPattern(..) | MalformedPattern(..) => {
                C::PatternDefault
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct RecordDestruct {
    pub var: Variable,
    pub label: Lowercase,
    pub symbol: Symbol,
    pub typ: DestructType,
}

#[derive(Clone, Debug)]
pub enum DestructType {
    Required,
    Optional(Variable, Loc<Expr>),
    Guard(Variable, Loc<Pattern>),
}

pub fn canonicalize_def_header_pattern<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    output: &mut Output,
    pattern_type: PatternType,
    pattern: &ast::Pattern<'a>,
    region: Region,
) -> Loc<Pattern> {
    use roc_parse::ast::Pattern::*;

    match pattern {
        // Identifiers that shadow ability members may appear (and may only appear) at the header of a def.
        Identifier(name) => {
            match scope.introduce_or_shadow_ability_member((*name).into(), region) {
                Ok((symbol, shadowing_ability_member)) => {
                    let can_pattern = match shadowing_ability_member {
                        // A fresh identifier.
                        None => {
                            output.references.insert_bound(symbol);
                            Pattern::Identifier(symbol)
                        }
                        // Likely a specialization of an ability.
                        Some(ability_member_name) => {
                            output.references.insert_value_lookup(ability_member_name);
                            Pattern::AbilityMemberSpecialization {
                                ident: symbol,
                                specializes: ability_member_name,
                            }
                        }
                    };
                    Loc::at(region, can_pattern)
                }
                Err((original_region, shadow, new_symbol)) => {
                    env.problem(Problem::RuntimeError(RuntimeError::Shadowing {
                        original_region,
                        shadow: shadow.clone(),
                        kind: ShadowKind::Variable,
                    }));
                    output.references.insert_bound(new_symbol);

                    let can_pattern = Pattern::Shadowed(original_region, shadow, new_symbol);
                    Loc::at(region, can_pattern)
                }
            }
        }
        _ => canonicalize_pattern(env, var_store, scope, output, pattern_type, pattern, region),
    }
}

pub fn canonicalize_pattern<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    output: &mut Output,
    pattern_type: PatternType,
    pattern: &ast::Pattern<'a>,
    region: Region,
) -> Loc<Pattern> {
    use roc_parse::ast::Pattern::*;
    use PatternType::*;

    let can_pattern = match pattern {
        Identifier(name) => match scope.introduce_str(name, region) {
            Ok(symbol) => {
                output.references.insert_bound(symbol);

                Pattern::Identifier(symbol)
            }
            Err((original_region, shadow, new_symbol)) => {
                env.problem(Problem::RuntimeError(RuntimeError::Shadowing {
                    original_region,
                    shadow: shadow.clone(),
                    kind: ShadowKind::Variable,
                }));
                output.references.insert_bound(new_symbol);

                Pattern::Shadowed(original_region, shadow, new_symbol)
            }
        },
        Tag(name) => {
            // Canonicalize the tag's name.
            Pattern::AppliedTag {
                whole_var: var_store.fresh(),
                ext_var: var_store.fresh(),
                tag_name: TagName((*name).into()),
                arguments: vec![],
            }
        }
        OpaqueRef(name) => {
            // If this opaque ref had an argument, we would be in the "Apply" branch.
            let loc_name = Loc::at(region, (*name).into());
            env.problem(Problem::RuntimeError(RuntimeError::OpaqueNotApplied(
                loc_name,
            )));
            Pattern::UnsupportedPattern(region)
        }
        Apply(tag, patterns) => {
            let mut can_patterns = Vec::with_capacity(patterns.len());
            for loc_pattern in *patterns {
                let can_pattern = canonicalize_pattern(
                    env,
                    var_store,
                    scope,
                    output,
                    pattern_type,
                    &loc_pattern.value,
                    loc_pattern.region,
                );

                can_patterns.push((var_store.fresh(), can_pattern));
            }

            match tag.value {
                Tag(name) => {
                    let tag_name = TagName(name.into());
                    Pattern::AppliedTag {
                        whole_var: var_store.fresh(),
                        ext_var: var_store.fresh(),
                        tag_name,
                        arguments: can_patterns,
                    }
                }

                OpaqueRef(name) => match scope.lookup_opaque_ref(name, tag.region) {
                    Ok((opaque, opaque_def)) => {
                        debug_assert!(!can_patterns.is_empty());

                        if can_patterns.len() > 1 {
                            env.problem(Problem::RuntimeError(
                                RuntimeError::OpaqueAppliedToMultipleArgs(region),
                            ));

                            Pattern::UnsupportedPattern(region)
                        } else {
                            let argument = Box::new(can_patterns.pop().unwrap());

                            let (type_arguments, lambda_set_variables, specialized_def_type) =
                                freshen_opaque_def(var_store, opaque_def);

                            output.references.insert_type_lookup(opaque);

                            Pattern::UnwrappedOpaque {
                                whole_var: var_store.fresh(),
                                opaque,
                                argument,
                                specialized_def_type: Box::new(specialized_def_type),
                                type_arguments,
                                lambda_set_variables,
                            }
                        }
                    }
                    Err(runtime_error) => {
                        env.problem(Problem::RuntimeError(runtime_error));

                        Pattern::OpaqueNotInScope(Loc::at(tag.region, name.into()))
                    }
                },
                _ => unreachable!("Other patterns cannot be applied"),
            }
        }

        &FloatLiteral(str) => match pattern_type {
            WhenBranch => match finish_parsing_float(str) {
                Err(_error) => {
                    let problem = MalformedPatternProblem::MalformedFloat;
                    malformed_pattern(env, problem, region)
                }
                Ok((str_without_suffix, float, bound)) => Pattern::FloatLiteral(
                    var_store.fresh(),
                    var_store.fresh(),
                    str_without_suffix.into(),
                    float,
                    bound,
                ),
            },
            ptype => unsupported_pattern(env, ptype, region),
        },

        Underscore(_) => match pattern_type {
            WhenBranch | FunctionArg => Pattern::Underscore,
            TopLevelDef | DefExpr => bad_underscore(env, region),
        },

        &NumLiteral(str) => match pattern_type {
            WhenBranch => match finish_parsing_num(str) {
                Err(_error) => {
                    let problem = MalformedPatternProblem::MalformedInt;
                    malformed_pattern(env, problem, region)
                }
                Ok((parsed, ParsedNumResult::UnknownNum(int, bound))) => {
                    Pattern::NumLiteral(var_store.fresh(), (parsed).into(), int, bound)
                }
                Ok((parsed, ParsedNumResult::Int(int, bound))) => Pattern::IntLiteral(
                    var_store.fresh(),
                    var_store.fresh(),
                    (parsed).into(),
                    int,
                    bound,
                ),
                Ok((parsed, ParsedNumResult::Float(float, bound))) => Pattern::FloatLiteral(
                    var_store.fresh(),
                    var_store.fresh(),
                    (parsed).into(),
                    float,
                    bound,
                ),
            },
            ptype => unsupported_pattern(env, ptype, region),
        },

        &NonBase10Literal {
            string,
            base,
            is_negative,
        } => match pattern_type {
            WhenBranch => match finish_parsing_base(string, base, is_negative) {
                Err(_error) => {
                    let problem = MalformedPatternProblem::MalformedBase(base);
                    malformed_pattern(env, problem, region)
                }
                Ok((IntValue::U128(_), _)) if is_negative => {
                    // Can't negate a u128; that doesn't fit in any integer literal type we support.
                    let problem = MalformedPatternProblem::MalformedInt;
                    malformed_pattern(env, problem, region)
                }
                Ok((int, bound)) => {
                    use std::ops::Neg;

                    let sign_str = if is_negative { "-" } else { "" };
                    let int_str = format!("{}{}", sign_str, int).into_boxed_str();
                    let i = match int {
                        // Safety: this is fine because I128::MAX = |I128::MIN| - 1
                        IntValue::I128(n) if is_negative => {
                            IntValue::I128(i128::from_ne_bytes(n).neg().to_ne_bytes())
                        }
                        IntValue::I128(n) => IntValue::I128(n),
                        IntValue::U128(_) => unreachable!(),
                    };
                    Pattern::IntLiteral(var_store.fresh(), var_store.fresh(), int_str, i, bound)
                }
            },
            ptype => unsupported_pattern(env, ptype, region),
        },

        StrLiteral(literal) => match pattern_type {
            WhenBranch => flatten_str_literal(literal),
            ptype => unsupported_pattern(env, ptype, region),
        },

        SingleQuote(string) => {
            let mut it = string.chars().peekable();
            if let Some(char) = it.next() {
                if it.peek().is_none() {
                    Pattern::SingleQuote(char)
                } else {
                    // multiple chars is found
                    let problem = MalformedPatternProblem::MultipleCharsInSingleQuote;
                    malformed_pattern(env, problem, region)
                }
            } else {
                // no characters found
                let problem = MalformedPatternProblem::EmptySingleQuote;
                malformed_pattern(env, problem, region)
            }
        }

        SpaceBefore(sub_pattern, _) | SpaceAfter(sub_pattern, _) => {
            return canonicalize_pattern(
                env,
                var_store,
                scope,
                output,
                pattern_type,
                sub_pattern,
                region,
            )
        }
        RecordDestructure(patterns) => {
            let ext_var = var_store.fresh();
            let whole_var = var_store.fresh();
            let mut destructs = Vec::with_capacity(patterns.len());
            let mut opt_erroneous = None;

            for loc_pattern in patterns.iter() {
                match loc_pattern.value {
                    Identifier(label) => {
                        match scope.introduce(label.into(), region) {
                            Ok(symbol) => {
                                output.references.insert_bound(symbol);

                                destructs.push(Loc {
                                    region: loc_pattern.region,
                                    value: RecordDestruct {
                                        var: var_store.fresh(),
                                        label: Lowercase::from(label),
                                        symbol,
                                        typ: DestructType::Required,
                                    },
                                });
                            }
                            Err((original_region, shadow, new_symbol)) => {
                                env.problem(Problem::RuntimeError(RuntimeError::Shadowing {
                                    original_region,
                                    shadow: shadow.clone(),
                                    kind: ShadowKind::Variable,
                                }));

                                // No matter what the other patterns
                                // are, we're definitely shadowed and will
                                // get a runtime exception as soon as we
                                // encounter the first bad pattern.
                                opt_erroneous =
                                    Some(Pattern::Shadowed(original_region, shadow, new_symbol));
                            }
                        };
                    }

                    RequiredField(label, loc_guard) => {
                        // a guard does not introduce the label into scope!
                        let symbol =
                            scope.scopeless_symbol(&Ident::from(label), loc_pattern.region);
                        let can_guard = canonicalize_pattern(
                            env,
                            var_store,
                            scope,
                            output,
                            pattern_type,
                            &loc_guard.value,
                            loc_guard.region,
                        );

                        destructs.push(Loc {
                            region: loc_pattern.region,
                            value: RecordDestruct {
                                var: var_store.fresh(),
                                label: Lowercase::from(label),
                                symbol,
                                typ: DestructType::Guard(var_store.fresh(), can_guard),
                            },
                        });
                    }
                    OptionalField(label, loc_default) => {
                        // an optional DOES introduce the label into scope!
                        match scope.introduce(label.into(), region) {
                            Ok(symbol) => {
                                let (can_default, expr_output) = canonicalize_expr(
                                    env,
                                    var_store,
                                    scope,
                                    loc_default.region,
                                    &loc_default.value,
                                );

                                // an optional field binds the symbol!
                                output.references.insert_bound(symbol);

                                output.union(expr_output);

                                destructs.push(Loc {
                                    region: loc_pattern.region,
                                    value: RecordDestruct {
                                        var: var_store.fresh(),
                                        label: Lowercase::from(label),
                                        symbol,
                                        typ: DestructType::Optional(var_store.fresh(), can_default),
                                    },
                                });
                            }
                            Err((original_region, shadow, new_symbol)) => {
                                env.problem(Problem::RuntimeError(RuntimeError::Shadowing {
                                    original_region,
                                    shadow: shadow.clone(),
                                    kind: ShadowKind::Variable,
                                }));

                                // No matter what the other patterns
                                // are, we're definitely shadowed and will
                                // get a runtime exception as soon as we
                                // encounter the first bad pattern.
                                opt_erroneous =
                                    Some(Pattern::Shadowed(original_region, shadow, new_symbol));
                            }
                        };
                    }
                    _ => unreachable!("Any other pattern should have given a parse error"),
                }
            }

            // If we encountered an erroneous pattern (e.g. one with shadowing),
            // use the resulting RuntimeError. Otherwise, return a successful record destructure.
            opt_erroneous.unwrap_or(Pattern::RecordDestructure {
                whole_var,
                ext_var,
                destructs,
            })
        }

        RequiredField(_name, _loc_pattern) => {
            unreachable!("should have been handled in RecordDestructure");
        }
        OptionalField(_name, _loc_pattern) => {
            unreachable!("should have been handled in RecordDestructure");
        }

        Malformed(_str) => {
            let problem = MalformedPatternProblem::Unknown;
            malformed_pattern(env, problem, region)
        }

        MalformedIdent(_str, problem) => {
            let problem = MalformedPatternProblem::BadIdent(*problem);
            malformed_pattern(env, problem, region)
        }

        QualifiedIdentifier { .. } => {
            let problem = MalformedPatternProblem::QualifiedIdentifier;
            malformed_pattern(env, problem, region)
        }
    };

    Loc {
        region,
        value: can_pattern,
    }
}

/// When we detect an unsupported pattern type (e.g. 5 = 1 + 2 is unsupported because you can't
/// assign to Int patterns), report it to Env and return an UnsupportedPattern runtime error pattern.
fn unsupported_pattern(env: &mut Env, pattern_type: PatternType, region: Region) -> Pattern {
    use roc_problem::can::BadPattern;
    env.problem(Problem::UnsupportedPattern(
        BadPattern::Unsupported(pattern_type),
        region,
    ));

    Pattern::UnsupportedPattern(region)
}

fn bad_underscore(env: &mut Env, region: Region) -> Pattern {
    use roc_problem::can::BadPattern;
    env.problem(Problem::UnsupportedPattern(
        BadPattern::UnderscoreInDef,
        region,
    ));

    Pattern::UnsupportedPattern(region)
}

/// When we detect a malformed pattern like `3.X` or `0b5`,
/// report it to Env and return an UnsupportedPattern runtime error pattern.
fn malformed_pattern(env: &mut Env, problem: MalformedPatternProblem, region: Region) -> Pattern {
    env.problem(Problem::RuntimeError(RuntimeError::MalformedPattern(
        problem, region,
    )));

    Pattern::MalformedPattern(problem, region)
}

/// An iterator over the bindings made by a pattern.
///
/// We attempt to make no allocations when we can.
pub enum BindingsFromPattern<'a> {
    Empty,
    One(&'a Loc<Pattern>),
    Many(Vec<BindingsFromPatternWork<'a>>),
}

pub enum BindingsFromPatternWork<'a> {
    Pattern(&'a Loc<Pattern>),
    Destruct(&'a Loc<RecordDestruct>),
}

impl<'a> BindingsFromPattern<'a> {
    pub fn new(initial: &'a Loc<Pattern>) -> Self {
        Self::One(initial)
    }

    pub fn new_many<I>(mut it: I) -> Self
    where
        I: Iterator<Item = &'a Loc<Pattern>>,
    {
        if let (1, Some(1)) = it.size_hint() {
            Self::new(it.next().unwrap())
        } else {
            Self::Many(it.map(BindingsFromPatternWork::Pattern).collect())
        }
    }

    fn next_many(stack: &mut Vec<BindingsFromPatternWork<'a>>) -> Option<(Symbol, Region)> {
        use Pattern::*;

        while let Some(work) = stack.pop() {
            match work {
                BindingsFromPatternWork::Pattern(loc_pattern) => {
                    use BindingsFromPatternWork::*;

                    match &loc_pattern.value {
                        Identifier(symbol)
                        | AbilityMemberSpecialization {
                            ident: symbol,
                            specializes: _,
                        } => {
                            return Some((*symbol, loc_pattern.region));
                        }
                        AppliedTag {
                            arguments: loc_args,
                            ..
                        } => {
                            let it = loc_args.iter().rev().map(|(_, p)| Pattern(p));
                            stack.extend(it);
                        }
                        UnwrappedOpaque { argument, .. } => {
                            let (_, loc_arg) = &**argument;
                            stack.push(Pattern(loc_arg));
                        }
                        RecordDestructure { destructs, .. } => {
                            let it = destructs.iter().rev().map(Destruct);
                            stack.extend(it);
                        }
                        NumLiteral(..)
                        | IntLiteral(..)
                        | FloatLiteral(..)
                        | StrLiteral(_)
                        | SingleQuote(_)
                        | Underscore
                        | Shadowed(_, _, _)
                        | MalformedPattern(_, _)
                        | UnsupportedPattern(_)
                        | OpaqueNotInScope(..) => (),
                    }
                }
                BindingsFromPatternWork::Destruct(loc_destruct) => {
                    match &loc_destruct.value.typ {
                        DestructType::Required | DestructType::Optional(_, _) => {
                            return Some((loc_destruct.value.symbol, loc_destruct.region));
                        }
                        DestructType::Guard(_, inner) => {
                            // a guard does not introduce the symbol
                            stack.push(BindingsFromPatternWork::Pattern(inner))
                        }
                    }
                }
            }
        }

        None
    }
}

impl<'a> Iterator for BindingsFromPattern<'a> {
    type Item = (Symbol, Region);

    fn next(&mut self) -> Option<Self::Item> {
        use Pattern::*;

        match self {
            BindingsFromPattern::Empty => None,
            BindingsFromPattern::One(loc_pattern) => match &loc_pattern.value {
                Identifier(symbol)
                | AbilityMemberSpecialization {
                    ident: symbol,
                    specializes: _,
                } => {
                    let region = loc_pattern.region;
                    *self = Self::Empty;
                    Some((*symbol, region))
                }
                _ => {
                    *self = Self::Many(vec![BindingsFromPatternWork::Pattern(loc_pattern)]);
                    self.next()
                }
            },
            BindingsFromPattern::Many(stack) => Self::next_many(stack),
        }
    }
}

fn flatten_str_literal(literal: &StrLiteral<'_>) -> Pattern {
    use ast::StrLiteral::*;

    match literal {
        PlainLine(str_slice) => Pattern::StrLiteral((*str_slice).into()),
        Line(segments) => flatten_str_lines(&[segments]),
        Block(lines) => flatten_str_lines(lines),
    }
}

fn flatten_str_lines(lines: &[&[StrSegment<'_>]]) -> Pattern {
    use StrSegment::*;

    let mut buf = String::new();

    for line in lines {
        for segment in line.iter() {
            match segment {
                Plaintext(string) => {
                    buf.push_str(string);
                }
                Unicode(loc_digits) => {
                    todo!("parse unicode digits {:?}", loc_digits);
                }
                Interpolated(loc_expr) => {
                    return Pattern::UnsupportedPattern(loc_expr.region);
                }
                EscapedChar(escaped) => buf.push(unescape_char(escaped)),
            }
        }
    }

    Pattern::StrLiteral(buf.into())
}
