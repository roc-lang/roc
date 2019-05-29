use expr;
use expr::{Expr, Operator, Pattern};
use expr::Expr::*;
use expr::Problem::*;
use expr::Pattern::*;
use expr::Operator::*;
use std::rc::Rc;
use im_rc::hashmap::HashMap;

pub fn eval(expr: &Expr) -> Expr {
    scoped_eval(expr, &HashMap::new())
}

pub fn scoped_eval(expr: &Expr, vars: &HashMap<String, Rc<Expr>>) -> Expr {
    match expr {
        // Primitives need no further evaluation
        Error(_) | Int(_) | Str(_) | Frac(_, _) | Char(_) | Bool(_) | Closure(_, _) => *expr,

        // Resolve variable names
        Var(name) => match vars.get(name) {
            Some(resolved) => (*Rc::clone(resolved)).clone(),
            None => Error(UnrecognizedVarName(*name))
        }

        Let(Identifier(name), definition, in_expr) => {
            if vars.contains_key(name) {
                Error(ReassignedVarName(*name))
            } else {
                // Create a new scope containing the new declaration.
                let mut new_vars = (*vars).clone();
                new_vars.insert(*name, Rc::new(scoped_eval(definition, vars)));

                // Evaluate in_expr with that new scope's variables.
                scoped_eval(in_expr, &new_vars)
            }
        },

        Let(Variant(name, patterns), definition, in_expr) => {
            panic!("Pattern matching on variants is not yet supported!");
        },

        Let(Underscore, definition, in_expr) => {
            // Faithfully eval this, but discard its result.
            scoped_eval(definition, vars);

            // Actually use this part.
            scoped_eval(in_expr, vars)
        },

        Func(name, args) => {
            let func_expr = match vars.get(name) {
                Some(resolved) => (*Rc::clone(resolved)).clone(),
                None => Error(UnrecognizedVarName(*name))
            };

            eval_apply(func_expr, *args, vars)
        },

        ApplyVariant(_, None) => *expr, // This is all we do - for now...

        ApplyVariant(name, Some(args)) => {
            let mut evaluated_args = Vec::with_capacity(args.len());

            for arg in args {
                evaluated_args.push(scoped_eval(arg, vars))
            }

            ApplyVariant(*name, Some(evaluated_args))
        }

        Apply(func_expr, args) => {
            eval_apply(**func_expr, *args, vars)
        },

        Operator(left_arg, op, right_arg) => {
            eval_operator(
                &scoped_eval(left_arg, vars),
                &op,
                &scoped_eval(right_arg, vars)
            )
        },

        Match(condition, branches) => {
            match scoped_eval(condition, vars) {
                _ => { panic!("TODO implement eval for match-expressions"); }
            }
        },

        If(condition, if_true, if_false) => {
            match scoped_eval(condition, vars) {
                Bool(true) => scoped_eval(if_true, vars),
                Bool(false) => scoped_eval(if_false, vars),
                _ => Error(TypeMismatch("non-Bool used in `if` condition".to_string()))
            }
        }
    }
}

#[inline(always)]
fn eval_apply(expr: Expr, args: Vec<Expr>, vars: &HashMap<String, Rc<Expr>>) -> Expr {
    match expr {
        Closure(arg_patterns, body) => {
            match eval_closure(args, arg_patterns, vars) {
                Ok(new_vars) => scoped_eval(&*body, &new_vars),
                Err(problem) => Error(problem)
            }
        },
        _ => Error(TypeMismatch("Tried to call a non-function.".to_string()))
    }
}

#[inline(always)]
fn eval_closure(args: Vec<Expr>, arg_patterns: Vec<Pattern>, vars: &HashMap<String, Rc<Expr>>)
    -> Result<HashMap<String, Rc<Expr>>, expr::Problem>
{
    if arg_patterns.len() == args.len() {
        // Create a new scope for the function to use.
        let mut new_vars = (*vars).clone();

        for ( arg, pattern ) in args.into_iter().zip(arg_patterns) {
            pattern_match(&arg, pattern, &mut new_vars)?;
        }

        Ok(new_vars)
    } else {
        Err(WrongArity(arg_patterns.len() as u32, args.len() as u32))
    }
}

#[inline(always)]
fn eval_operator(left_expr: &Expr, op: &Operator, right_expr: &Expr) -> Expr {
    match (left_expr, op, right_expr) {
        // Error
        (Error(_), _, _) => left_expr.clone(),
        (_, _, Error(_)) => right_expr.clone(),

        // Equals
        
        // All functions are defined as equal
        (Closure(_, _), Equals, Closure(_, _)) => Bool(true),

        (Bool(left), Equals, Bool(right)) => Bool(left == right),
        (Int(left), Equals, Int(right)) => Bool(left == right),
        (Str(left), Equals, Str(right)) => Bool(left == right),
        (Char(left), Equals, Char(right)) => Bool(left == right),
        (Frac(_, _), Equals, Frac(_, _)) => panic!("Don't know how to == on Fracs yet"),

        (_, Equals, _) => Error(TypeMismatch("tried to use == on two values with incompatible types".to_string())),

        // Plus
        (Int(left_num), Plus, Int(right_num)) => Int(left_num + right_num),
        (Frac(_, _), Plus, Frac(_, _)) => panic!("Don't know how to add fracs yet"),

        (Int(_), Plus, Frac(_, _)) => panic!("Tried to add Int and Frac"),

        (Frac(_, _), Plus, Int(_)) => panic!("Tried to add Frac and Int"),

        (_, Plus, _) => panic!("Tried to add non-numbers"),

        // Star
        (Int(left_num), Star, Int(right_num)) => Int(left_num * right_num),
        (Frac(_, _), Star, Frac(_, _)) => panic!("Don't know how to multiply fracs yet"),

        (Int(_), Star, Frac(_, _)) => panic!("Tried to multiply Int and Frac"),

        (Frac(_, _), Star, Int(_)) => panic!("Tried to multiply Frac and Int"),

        (_, Star, _) => panic!("Tried to multiply non-numbers"),

        // Minus
        (Int(left_num), Minus, Int(right_num)) => Int(left_num - right_num),
        (Frac(_, _), Minus, Frac(_, _)) => panic!("Don't know how to subtract fracs yet"),

        (Int(_), Minus, Frac(_, _)) => panic!("Tried to subtract Frac from Int"),

        (Frac(_, _), Minus, Int(_)) => panic!("Tried to subtract Int from Frac"),

        (_, Minus, _) => panic!("Tried to subtract non-numbers"),

        // Slash
        (Int(left_num), Slash, Int(right_num)) => Int(left_num / right_num),
        (Frac(_, _), Slash, Frac(_, _)) => panic!("Don't know how to divide fracs yet"),

        (Int(_), Slash, Frac(_, _)) => panic!("Tried to divide Int by Frac"),

        (Frac(_, _), Slash, Int(_)) => panic!("Tried to divide Frac by Int"),

        (_, Slash, _) => panic!("Tried to divide non-numbers"),

        // DoubleSlash
        (Int(left_num), DoubleSlash, Int(right_num)) => Int(left_num / right_num),
        (Frac(_, _), DoubleSlash, Frac(_, _)) => panic!("Tried to do integer division on fracs"),

        (Int(_), DoubleSlash, Frac(_, _)) => panic!("Tried to integer-divide Int by Frac"),

        (Frac(_, _), DoubleSlash, Int(_)) => panic!("Tried to integer-divide Frac by Int"),

        (_, DoubleSlash, _) => panic!("Tried to integer-divide non-numbers"),
    }
}

#[inline(always)]
fn eval_match(condition: &Expr, branches: Vec<(Pattern, Box<Expr>)>, vars: HashMap<String, Rc<Expr>>) -> Expr {
    for (pattern, expr) in branches {
        let mut branch_vars = vars.clone();

        if pattern_match(&condition, pattern, &mut branch_vars).is_ok() {
            return scoped_eval(&*expr, &branch_vars);
        }
    }

    Error(NoBranchesMatched)
}

fn pattern_match(expr: &Expr, pattern: Pattern, vars: &mut HashMap<String, Rc<Expr>>) -> Result<(), expr::Problem> {
    match pattern {
        Identifier(name) => {
            let new_val = scoped_eval(expr, vars);

            vars.insert(name, Rc::new(new_val));

            Ok(())
        },
        Underscore => {
            // Underscore matches anything, and records no new vars.
            Ok(())
        },
        Variant(expected_variant_name, opt_contents) => {
            match *expr {
                ApplyVariant(variant_name, opt_expected_patterns) => {
                    if expected_variant_name != variant_name {
                        return Err(TypeMismatch(format!("Wanted a `{}` variant, but was given a `{}` variant.", expected_variant_name, variant_name)));
                    }

                    match (opt_expected_patterns, opt_contents) {
                        ( Some(contents), Some(patterns) ) => {
                            if contents.len() == patterns.len() {
                                // Recursively pattern match
                                for ( arg, pattern ) in contents.into_iter().zip(patterns) {
                                    pattern_match(&arg, pattern, vars)?;
                                }

                                Ok(())
                            } else {
                                Err(WrongArity(patterns.len() as u32, contents.len() as u32))
                            }
                        },
                        ( None, None ) => {
                            // It's the variant we expected, but it has no values in it,
                            // so we don't insert anything into vars.
                            Ok(())
                        },
                        ( None, Some(patterns) ) => {
                            // It's the variant we expected, but the arity is wrong.
                            Err(WrongArity(0, patterns.len() as u32))
                        },
                        ( Some(contents), None ) => {
                            // It's the variant we expected, but the arity is wrong.
                            Err(WrongArity(contents.len() as u32, 0))
                        },
                    }
                },
                _ => {
                    Err(TypeMismatch(format!("Wanted to destructure a `{}` variant, but was given a non-variant.", expected_variant_name)))
                }
            }
        }
    }
}

