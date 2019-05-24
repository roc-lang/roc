use expr::{Expr, Operator};
use expr::Expr::*;
use expr::Problem::*;
use expr::Pattern::*;
use expr::Operator::*;
use im_rc::hashmap::HashMap;

pub fn eval(expr: Expr) -> Expr {
    scoped_eval(expr, &HashMap::new())
}

pub fn scoped_eval(expr: Expr, vars: &HashMap<String, Expr>) -> Expr {
    match expr {
        // Primitives need no further evaluation
        Error(_) | Int(_) | Str(_) | Frac(_, _) | Char(_) | Bool(_) | Closure(_, _) => expr,

        Var(name) => match vars.get(&name) {
            Some(resolved) => resolved.clone(),
            None => Error(UnrecognizedVarName(name))
        }

        Let(Identifier(name), definition, in_expr) => {
            let mut new_vars = (*vars).clone();
                
            new_vars.insert(name, *definition);

            scoped_eval(*in_expr, &new_vars)
        },

        Let(Underscore, definition, in_expr) => {
            // Faithfully eval this, but discard its result.
            scoped_eval(*definition, vars);

            // Actually use this.
            scoped_eval(*in_expr, vars)
        },

        Func(name, arg) => {
            let func_expr = match vars.get(&name) {
                Some(resolved) => resolved.clone(),
                None => Error(UnrecognizedVarName(name))
            };

            scoped_eval(Apply(Box::new((func_expr, *arg))), vars)
        },

        Apply(boxed_tuple) => {
            match *boxed_tuple {
                // (Closure(args, definition), arg) => {
                //     panic!("TODO apply arg to closure");
                // },
                _ => { panic!("Type mismatch: trying to call a non-function!"); }
            }
        },

        Operator(left_arg, op, right_arg) => 
            eval_operator(
                &scoped_eval(*left_arg, vars),
                &op,
                &scoped_eval(*right_arg, vars)
            )
        ,

        If(condition, if_true, if_false) => {
            match eval(*condition) {
                Bool(true) => eval(*if_true),
                Bool(false) => eval(*if_false),
                _ => { panic!("Type mismatch on if condition."); }
            }
        }
    }
}

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

        (_, Equals, _) => panic!("Tried to == two expressions with different types"),
        
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

