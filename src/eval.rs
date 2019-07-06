use expr::{Expr, Pattern, Ident};
use expr::Pattern::*;
use operator::Operator::*;
use operator::Operator;
use std::rc::Rc;
use std::fmt;
use im_rc::hashmap::HashMap;
use self::Evaluated::*;
use self::Problem::*;
use fraction::Fraction;

pub fn eval(expr: Expr) -> Evaluated {
    scoped_eval(expr, &HashMap::new())
}

#[derive(Clone, Debug, PartialEq)]
pub enum Evaluated {
    // Literals
    Int(i64),
    Frac(Fraction),
    EmptyStr,
    Str(String),
    InterpolatedStr(Vec<(String, Ident)>, String),
    Char(char),
    Closure(Vec<Pattern>, Box<Expr>, Scope),

    // Sum Types
    ApplyVariant(String, Option<Vec<Evaluated>>),

    // Product Types
    EmptyRecord,

    // Errors
    EvalError(Problem)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Problem {
    UnrecognizedVarName(String),
    TypeMismatch(String),
    ReassignedVarName(String),
    WrongArity(u32 /* Expected */, u32 /* Provided */),
    NotEqual, // Used when (for example) a string literal pattern match fails
    NoBranchesMatched,
}

type Scope = HashMap<String, Rc<Evaluated>>;

pub fn scoped_eval(expr: Expr, vars: &Scope) -> Evaluated {
    match expr {
        Expr::Int(num) => Int(num),
        Expr::EmptyStr => EmptyStr,
        Expr::Str(string) => Str(string),
        Expr::Frac(numerator, denominator) => Frac(fraction_from_i64s(numerator, denominator)),
        Expr::Char(ch) => Char(ch),
        Expr::Closure(args, body) => Closure(args.into_iter().map(|e| e.value).collect(), Box::new(body.value), vars.clone()),
        Expr::EmptyRecord => EmptyRecord,

        // Resolve variable names
        Expr::Var(name) => match vars.get(&name) {
            Some(resolved) => (**resolved).clone(),
            None => EvalError(UnrecognizedVarName(name))
        },

        Expr::InterpolatedStr(pairs, trailing_str) => {
            let mut output = String::new();

            for (string, var_name) in pairs.into_iter() {
                match vars.get(&var_name.value) {
                    Some(resolved) => {
                        match **resolved {
                            Str(ref var_string) => {
                                output.push_str(string.as_str());
                                output.push_str(var_string.as_str());
                            },
                            _ => {
                                return EvalError(TypeMismatch(var_name.value));
                            }
                        }
                    },
                    None => { return EvalError(UnrecognizedVarName(var_name.value)); }
                }
            }

            output.push_str(trailing_str.as_str());

            Str(output)
        },

        Expr::Assign(Identifier(name), definition, in_expr) => {
            if vars.contains_key(&name) {
                EvalError(ReassignedVarName(name))
            } else {
                // Create a new scope containing the new declaration.
                let mut new_vars = vars.clone();
                let evaluated_defn = scoped_eval(*definition, vars);

                new_vars.insert(name, Rc::new(evaluated_defn));

                // Evaluate in_expr with that new scope's variables.
                scoped_eval(*in_expr, &new_vars)
            }
        },

        Expr::Assign(Integer(_), _, _) => {
            panic!("You cannot assign integers to other values!");
        },

        Expr::Assign(Fraction(_, _), _, _) => {
            panic!("You cannot assign fractions to other values!");
        },

        Expr::Assign(Variant(_name, _patterns), _definition, _in_expr) => {
            panic!("Pattern matching on variants is not yet supported!");
        },

        Expr::Assign(Underscore, _definition, _in_expr) => {
            panic!("Cannot assign to the _ pattern!");
        },

        Expr::Assign(Pattern::EmptyRecordLiteral, _definition, _in_expr) => {
            panic!("Cannot assign to the {} pattern!");
        },

        Expr::CallByName(name, args) => {
            let func_expr = match vars.get(&name) {
                Some(resolved) => (**resolved).clone(),
                None => EvalError(UnrecognizedVarName(name))
            };

            eval_apply(func_expr, args, vars)
        },

        Expr::ApplyVariant(name, None) => ApplyVariant(name, None),

        Expr::ApplyVariant(name, Some(exprs)) => {
            ApplyVariant(
                name,
                Some(exprs.into_iter().map(|arg| scoped_eval(arg, vars)).collect())
            )
        }

        Expr::Apply(func_expr, args) => {
            eval_apply(scoped_eval(*func_expr, vars), args, vars)
        },

        Expr::Case(condition, branches) => {
            eval_case(scoped_eval(*condition, vars), branches, vars)
        },

        Expr::Operator(left_arg, op, right_arg) => {
            eval_operator(
                &scoped_eval(*left_arg, vars),
                op,
                &scoped_eval(*right_arg, vars)
            )
        },

        Expr::If(condition, if_true, if_false) => {
            match scoped_eval(*condition, vars) {
                ApplyVariant(variant_name, None) => {
                    match variant_name.as_str() {
                        "True" => scoped_eval(*if_true, vars),
                        "False" => scoped_eval(*if_false, vars),
                        _ => EvalError(TypeMismatch("non-Bool used in `if` condition".to_string()))
                    }
                },
                _ => EvalError(TypeMismatch("non-Bool used in `if` condition".to_string()))
            }
        }
    }
}

#[inline(always)]
pub fn call(evaluated: Evaluated, args: Vec<Expr>) -> Evaluated {
    eval_apply(evaluated, args, &HashMap::new())
}

#[inline(always)]
fn eval_apply(evaluated: Evaluated, args: Vec<Expr>, vars: &Scope) -> Evaluated {
    match evaluated {
        Closure(arg_patterns, body, closure_vars) => {
            let combined_vars = vars.clone().union(closure_vars);
            let evaluated_args =
                args.into_iter()
                    .map(|arg| scoped_eval(arg, &combined_vars))
                    .collect();

            match eval_closure(evaluated_args, arg_patterns, &combined_vars) {
                Ok(new_vars) => scoped_eval(*body, &new_vars),
                Err(prob) => EvalError(prob)
            }
        },
        expr => {
            EvalError(TypeMismatch(format!("Tried to call a non-function: {}", expr)))
        }
    }
}

#[inline(always)]
fn eval_closure(args: Vec<Evaluated>, arg_patterns: Vec<Pattern>, vars: &Scope)
    -> Result<Scope, Problem>
{
    if arg_patterns.len() == args.len() {
        // Create a new scope for the function to use.
        let mut new_vars = vars.clone();

        for ( arg, pattern ) in args.into_iter().zip(arg_patterns) {
            pattern_match(&arg, &pattern, &mut new_vars)?;
        }

        Ok(new_vars)
    } else {
        Err(WrongArity(arg_patterns.len() as u32, args.len() as u32))
    }
}

fn bool_variant(is_true: bool) -> Evaluated {
    if is_true {
        ApplyVariant("True".to_string(), None)
    } else {
        ApplyVariant("False".to_string(), None)
    }
}

fn eq(evaluated1: &Evaluated, evaluated2: &Evaluated) -> Evaluated {
    match (evaluated1, evaluated2) {
        // All functions are defined as equal
        (Closure(_, _, _), Closure(_, _, _)) => bool_variant(true),

        (ApplyVariant(left, None), ApplyVariant(right, None)) => {
            bool_variant(left == right)
        },

        (ApplyVariant(left, Some(left_args)), ApplyVariant(right, Some(right_args))) => {
            bool_variant(left == right && left_args.len() == right_args.len())
        },

        (ApplyVariant(_, None), ApplyVariant(_, Some(_))) => {
            bool_variant(false)
        },

        (ApplyVariant(_, Some(_)), ApplyVariant(_, None)) => {
            bool_variant(false)
        },

        (Int(left), Int(right)) => bool_variant(left == right),
        (Str(left), Str(right)) => bool_variant(left == right),
        (Char(left), Char(right)) => bool_variant(left == right),
        (Frac(left), Frac(right)) => bool_variant(left == right),

        (_, _) => EvalError(TypeMismatch("tried to use == on two values with incompatible types".to_string())),
    }
}

#[inline(always)]
fn eval_operator(left_expr: &Evaluated, op: Operator, right_expr: &Evaluated) -> Evaluated {
    // TODO in the future, replace these with named function calls to stdlib
    match (left_expr, op, right_expr) {
        // Equals
        (_, Equals, _) => eq(left_expr, right_expr),

        // LessThan
        (Int(left_num), LessThan, Int(right_num)) => bool_variant(left_num < right_num),
        (Frac(left_num), LessThan, Frac(right_num)) => bool_variant(left_num < right_num),
        (Int(_), LessThan, Frac(_)) => EvalError(TypeMismatch("tried check Frac < Int. Explicitly convert them to the same type first!".to_string())),
        (Frac(_), LessThan, Int(_)) => EvalError(TypeMismatch("tried check Int < Frac. Explicitly convert them to the same type first!".to_string())),
        (_, LessThan, _) => EvalError(TypeMismatch("tried to check if one non-number < another non-number".to_string())),

        // LessThanOrEq
        (Int(left_num), LessThanOrEq, Int(right_num)) => bool_variant(left_num <= right_num),
        (Frac(left_num), LessThanOrEq, Frac(right_num)) => bool_variant(left_num <= right_num),
        (Int(_), LessThanOrEq, Frac(_)) => EvalError(TypeMismatch("tried check Frac <= Int. Explicitly convert them to the same type first!".to_string())),
        (Frac(_), LessThanOrEq, Int(_)) => EvalError(TypeMismatch("tried check Int <= Frac. Explicitly convert them to the same type first!".to_string())),
        (_, LessThanOrEq, _) => EvalError(TypeMismatch("tried to check if one non-number <= another non-number".to_string())),

        // GreaterThan
        (Int(left_num), GreaterThan, Int(right_num)) => bool_variant(left_num > right_num),
        (Frac(left_num), GreaterThan, Frac(right_num)) => bool_variant(left_num > right_num),
        (Int(_), GreaterThan, Frac(_)) => EvalError(TypeMismatch("tried check Frac > Int. Explicitly convert them to the same type first!".to_string())),
        (Frac(_), GreaterThan, Int(_)) => EvalError(TypeMismatch("tried check Int > Frac. Explicitly convert them to the same type first!".to_string())),
        (_, GreaterThan, _) => EvalError(TypeMismatch("tried to check if one non-number > another non-number".to_string())),

        // GreaterThanOrEq
        (Int(left_num), GreaterThanOrEq, Int(right_num)) => bool_variant(left_num >= right_num),
        (Frac(left_num), GreaterThanOrEq, Frac(right_num)) => bool_variant(left_num >= right_num),
        (Int(_), GreaterThanOrEq, Frac(_)) => EvalError(TypeMismatch("tried check Frac >= Int. Explicitly convert them to the same type first!".to_string())),
        (Frac(_), GreaterThanOrEq, Int(_)) => EvalError(TypeMismatch("tried check Int >= Frac. Explicitly convert them to the same type first!".to_string())),
        (_, GreaterThanOrEq, _) => EvalError(TypeMismatch("tried to check if one non-number >= another non-number".to_string())),

        // Plus
        (Int(left_num), Plus, Int(right_num)) => Int(left_num + right_num),
        (Frac(left_num), Plus, Frac(right_num)) => Frac(left_num + right_num),

        (Int(_), Plus, Frac(_)) => EvalError(TypeMismatch("tried to add Frac to Int. Explicitly convert them to the same type first!".to_string())),

        (Frac(_), Plus, Int(_)) => EvalError(TypeMismatch("tried to add Int to Frac. Explicitly convert them to the same type first!".to_string())),

        (_, Plus, _) => EvalError(TypeMismatch("tried to add non-numbers".to_string())),

        // Star
        (Int(left_num), Star, Int(right_num)) => Int(left_num * right_num),
        (Frac(left_num), Star, Frac(right_num)) => Frac(left_num * right_num),

        (Int(_), Star, Frac(_)) => EvalError(TypeMismatch("tried to multiply Int by Frac. Explicitly convert them to the same type first!".to_string())),

        (Frac(_), Star, Int(_)) => EvalError(TypeMismatch("tried to multiply Frac by Int. Explicitly convert them to the same type first!".to_string())),

        (_, Star, _) => EvalError(TypeMismatch("tried to multiply non-numbers".to_string())),

        // Minus
        (Int(left_num), Minus, Int(right_num)) => Int(left_num - right_num),
        (Frac(left_num), Minus, Frac(right_num)) => Frac(left_num - right_num),

        (Int(_), Minus, Frac(_)) => EvalError(TypeMismatch("tried to subtract Frac from Int. Explicitly convert them to the same type first!".to_string())),

        (Frac(_), Minus, Int(_)) => EvalError(TypeMismatch("tried to subtract Int from Frac. Explicitly convert them to the same type first!".to_string())),

        (_, Minus, _) => EvalError(TypeMismatch("tried to subtract non-numbers".to_string())),

        // Slash
        (Int(left_num), Slash, Int(right_num)) => Int(left_num / right_num),
        (Frac(left_num), Slash, Frac(right_num)) => {
            let answer = left_num / right_num;

            if answer.is_finite() {
                ok_variant(Frac(answer))
            } else {
                err_variant(ApplyVariant("DivisionByZero".to_string(), None))
            }
        },

        (Int(_), Slash, Frac(_)) => EvalError(TypeMismatch("tried to divide Int by Frac. Explicitly convert them to the same type first!".to_string())),

        (Frac(_), Slash, Int(_)) => EvalError(TypeMismatch("tried to divide Frac by Int. Explicitly convert them to the same type first!".to_string())),

        (_, Slash, _) => EvalError(TypeMismatch("tried to divide non-numbers".to_string())),

        // DoubleSlash
        (Int(left_num), DoubleSlash, Int(right_num)) => Int(left_num / right_num),
        (Frac(_), DoubleSlash, Frac(_)) => EvalError(TypeMismatch("tried to do integer division on two Frac values".to_string())),

        (Int(_), DoubleSlash, Frac(_)) => EvalError(TypeMismatch("tried to integer-divide Int by Frac".to_string())),

        (Frac(_), DoubleSlash, Int(_)) => EvalError(TypeMismatch("tried to integer-divide Frac by Int".to_string())),

        (_, DoubleSlash, _) => EvalError(TypeMismatch("tried to do integer division on two non-numbers".to_string())),
    }
}

#[inline(always)]
fn eval_case (evaluated: Evaluated, branches: Vec<(Pattern, Box<Expr>)>, vars: &Scope) -> Evaluated {
    for (pattern, definition) in branches {
        let mut branch_vars = vars.clone();

        if pattern_match(&evaluated, &pattern, &mut branch_vars).is_ok() {
            return scoped_eval(*definition, &branch_vars);
        }
    }

    EvalError(NoBranchesMatched)
}

fn pattern_match(evaluated: &Evaluated, pattern: &Pattern, vars: &mut Scope) -> Result<(), Problem> {
    match pattern {
        Identifier(name) => {
            vars.insert(name.clone(), Rc::new(evaluated.clone()));

            Ok(())
        },
        Underscore => {
            // Underscore matches anything, and records no new vars.
            Ok(())
        },
        EmptyRecordLiteral => {
            match evaluated {
                EmptyRecord => Ok(()),
                expr => Err(TypeMismatch(
                    format!("Wanted a `{}`, but was given `{}`.", "{}", expr)
                ))
            }
        },

        Integer(pattern_num) => {
            match evaluated {
                Int(evaluated_num) => {
                    if *pattern_num == *evaluated_num {
                        Ok(())
                    } else {
                        Err(Problem::NotEqual)
                    }
                },

                expr => Err(TypeMismatch(
                    format!("Wanted a `{}`, but was given `{}`.", "{}", expr)
                ))
            }
        },

        Fraction(numerator, denominator) => {
            match evaluated {
                Frac(actual_frac) => {
                    let expected_frac = fraction_from_i64s(*numerator, *denominator);

                    if expected_frac == *actual_frac {
                        Ok(())
                    } else {
                        Err(Problem::NotEqual)
                    }
                },

                expr => Err(TypeMismatch(
                    format!("Wanted a `{}`, but was given `{}`.", "{}", expr)
                ))
            }
        }

        Variant(pattern_variant_name, opt_pattern_contents) => {
            match evaluated {
                ApplyVariant(applied_variant_name, opt_applied_contents) => {
                    if *pattern_variant_name != *applied_variant_name {
                        return Err(TypeMismatch(
                                format!("Wanted a `{}` variant, but was given a `{}` variant.",
                                        pattern_variant_name,
                                        applied_variant_name
                                )
                            )
                        );
                    }

                    match (opt_pattern_contents, opt_applied_contents) {
                        ( Some(ref pattern_contents), Some(applied_contents) ) => {
                            if pattern_contents.len() == applied_contents.len() {
                                // Recursively pattern match
                                for ( pattern_val, applied_val ) in pattern_contents.into_iter().zip(applied_contents) {
                                    pattern_match(applied_val, pattern_val, vars)?;
                                }

                                Ok(())
                            } else {
                                Err(WrongArity(
                                        pattern_contents.len() as u32,
                                        applied_contents.len() as u32
                                    )
                                )
                            }
                        },
                        ( None, None ) => {
                            // It's the variant we expected, but it has no values in it,
                            // so we don't insert anything into vars.
                            Ok(())
                        },
                        ( None, Some(contents) ) => {
                            // It's the variant we expected, but the arity is wrong.
                            Err(WrongArity(contents.len() as u32, 0))
                        },
                        ( Some(patterns), None ) => {
                            // It's the variant we expected, but the arity is wrong.
                            Err(WrongArity(0, patterns.len() as u32))
                        },
                    }
                },
                _ => {
                    Err(TypeMismatch(format!("Wanted to destructure a `{}` variant, but was given a non-variant.", pattern_variant_name)))
                }
            }
        }
    }
}

impl fmt::Display for Evaluated {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // PRIMITIVES
            Int(num) => write!(f, "{}", *num),
            Frac(fraction) => {
                let numerator = *fraction.numer().unwrap();
                let denominator = *fraction.denom().unwrap();

                if denominator == 10 {
                    write!(f, "{}", (numerator as f64 / 10.0))
                } else {
                    write!(f, "{}/{}", numerator, denominator)
                }
            },
            Str(string) => {
                let escaped_str =
                    (*string)
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\"")
                        .replace("\t", "\\t")
                        .replace("\n", "\\n")
                        .replace("\r", "\\r");

                write!(f, "\"{}\"", escaped_str)
            },
            Char(ch) => write!(f, "'{}'", *ch),
            Closure(args, _, _) => write!(f, "<{}-argument function>", args.len()),
            ApplyVariant(name, opt_exprs) => {
                match opt_exprs {
                    None => write!(f, "{}", name),
                    Some(exprs) => {
                        let contents =
                            exprs.into_iter()
                                .map(|expr| format!(" {}", expr))
                                .collect::<Vec<_>>()
                                .join(",");

                        write!(f, "{}{}", name, contents)
                    }
                }
            },

            // ERRORS
            EvalError(problem) => write!(f, "ERROR: {}", format!("{}", problem)),

            // UNFORMATTED
            _ => write!(f, "<partially evaluated expression>")
        }
    }
}

impl fmt::Display for Problem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Problem::UnrecognizedVarName(name) => write!(f, "Unrecognized var name `{}`", name),
            Problem::NoBranchesMatched => write!(f, "No branches matched in this case-expression"),
            Problem::TypeMismatch(info) => write!(f, "Type Mismatch - {}", info),
            Problem::ReassignedVarName(name) => write!(f, "Reassigned constant - {}", name),
            Problem::NotEqual => write!(f, "Pattern match on literal value failed; the branch wasn't equal."),
            Problem::WrongArity(expected_arity, provided_arity) => {
                if provided_arity > expected_arity {
                  write!(f, "Too many arguments! Needed {} arguments, but got {}", expected_arity, provided_arity)
                } else {
                  write!(f, "Missing arguments! Needed {} arguments, but got {}", expected_arity, provided_arity)
                }
            }
        }
    }
}

fn ok_variant(contents: Evaluated) -> Evaluated{
    ApplyVariant("Ok".to_string(), Some(vec![contents]))
}

fn err_variant(contents: Evaluated) -> Evaluated {
    ApplyVariant("Err".to_string(), Some(vec![contents]))
}

fn fraction_from_i64s(numerator: i64, denominator: i64) -> Fraction {
    if numerator.is_negative() {
        Fraction::new_neg(numerator as u64, denominator as u64)
    } else {
        Fraction::new(numerator as u64, denominator as u64)
    }
}
