use can::expr::Expr;
use can::expr::Expr::*;
use can::procedure::{Procedure};
use can::symbol::Symbol;
use collections::MutMap;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum ReferenceCount {
    Unique,
    Shared,
}

impl ReferenceCount {
    pub fn add(_a: &ReferenceCount, _b: &ReferenceCount) -> Self {
        Self::Shared
    }

    pub fn or(a: &ReferenceCount, b: &ReferenceCount) -> Self {
        match (a, b) {
            (Self::Unique, Self::Unique) => Self::Unique,
            _ => Self::Shared,
        }
    }
}

fn register(symbol: &Symbol, usage: &mut HashMap<Symbol, ReferenceCount>) -> () {
    use self::ReferenceCount::*;
    let value = match usage.get(symbol) {
        None => Unique,
        Some(current) => ReferenceCount::add(current, &Unique),
    };

    usage.insert(symbol.clone(), value);
}

pub fn sharing_analysis(
    expr: &Expr,
    procedures: &MutMap<Symbol, Procedure>,
) -> HashMap<Symbol, ReferenceCount> {
    let mut usage = HashMap::new();

    sharing_analysis_help(expr, &mut usage);

    for (_, proc) in procedures {
        sharing_analysis_help(&proc.body.value, &mut usage);
    }

    usage
}

// NOTE (20 nov 2019) this could potentially be optimized:
//
// actually, I think it would also work to do it as HashMap<Symbol, Variable>
//
// you get the same "if there's already an entry in the map, then this must be shared"
// but also, at the same time, you can now retroactively mark that other Variable as Shared because you know what it is - you got it right there out of the map
// and if there is no entry for that Symbol in the map, then cool - you insert your current Variable and move on assuming uniqueness until someone else later decides (or not) that you were actually Shared

pub fn sharing_analysis_help(expr: &Expr, usage: &mut HashMap<Symbol, ReferenceCount>) -> () {
    match expr {
        Var(_, symbol) | FunctionPointer(_, symbol) => {
            register(symbol, usage);
        }

        List(_, elements) => {
            for element in elements {
                sharing_analysis_help(&element.value, usage);
            }
        }

        Case(_, boxed_loc_expr, branches) => {
            sharing_analysis_help(&boxed_loc_expr.value, usage);

            // sharing state before this case
            let before = usage.clone();

            for (_pattern, branch) in branches {
                let mut local = before.clone();
                sharing_analysis_help(&branch.value, &mut local);

                // merge/join sharing into the `usage` map
                for (key, value) in local {
                    match usage.get(&key) {
                        None => {
                            usage.insert(key, value);
                        }
                        Some(current) => {
                            let result = ReferenceCount::or(current, &value);
                            usage.insert(key, result);
                        }
                    }
                }
            }
        }

        Defs(_, assignments, body) => {
            for (_pattern, value) in assignments {
                sharing_analysis_help(&value.value, usage);
            }

            sharing_analysis_help(&body.value, usage);
        }

        CallByName(symbol, arguments, _) => {
            register(symbol, usage);

            for argument in arguments {
                sharing_analysis_help(&argument.value, usage);
            }
        }

        CallPointer(function, arguments, _) => {
            sharing_analysis_help(function, usage);

            for argument in arguments {
                sharing_analysis_help(&argument.value, usage);
            }
        }

        Record(_, fields) => {
            for field in fields {
                sharing_analysis_help(&field.value.1.value, usage);
            }
        }

        Field(record, _) => {
            sharing_analysis_help(&record.value, usage);
        }
        Int(_) | Float(_) | Str(_) | BlockStr(_) | EmptyRecord | RuntimeError(_) => {}
    }
}
