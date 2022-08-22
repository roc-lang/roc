use super::expr2::{Expr2, ExprId};
use crate::{
    lang::core::{expr::record_field::RecordField, val_def::value_def_to_string},
    mem_pool::pool::Pool,
};
use roc_types::subs::Variable;
use std::fmt::Write as _; // import without risk of name clashing

pub fn expr2_to_string(node_id: ExprId, pool: &Pool) -> String {
    let mut full_string = String::new();
    let expr2 = pool.get(node_id);

    expr2_to_string_helper(expr2, 0, pool, &mut full_string);

    full_string
}

fn get_spacing(indent_level: usize) -> String {
    std::iter::repeat("    ")
        .take(indent_level)
        .collect::<Vec<&str>>()
        .join("")
}

fn expr2_to_string_helper(
    expr2: &Expr2,
    indent_level: usize,
    pool: &Pool,
    out_string: &mut String,
) {
    out_string.push_str(&get_spacing(indent_level));

    match expr2 {
        Expr2::SmallStr(arr_string) => {
            let _ = write!(out_string, "SmallStr(\"{}\")", arr_string.as_str());
        }
        Expr2::Str(pool_str) => {
            let _ = write!(out_string, "Str(\"{}\")", pool_str.as_str(pool));
        }
        Expr2::Blank => out_string.push_str("Blank"),
        Expr2::EmptyRecord => out_string.push_str("EmptyRecord"),
        Expr2::Record { record_var, fields } => {
            out_string.push_str("Record:\n");
            out_string.push_str(&var_to_string(record_var, indent_level + 1));

            let _ = writeln!(out_string, "{}fields: [", get_spacing(indent_level + 1));

            let mut first_child = true;

            for field in fields.iter(pool) {
                if !first_child {
                    out_string.push_str(", ")
                } else {
                    first_child = false;
                }

                match field {
                    RecordField::InvalidLabelOnly(pool_str, var) => {
                        let _ = write!(
                            out_string,
                            "{}({}, Var({:?})",
                            get_spacing(indent_level + 2),
                            pool_str.as_str(pool),
                            var,
                        );
                    }
                    RecordField::LabelOnly(pool_str, var, symbol) => {
                        let _ = write!(
                            out_string,
                            "{}({}, Var({:?}), Symbol({:?})",
                            get_spacing(indent_level + 2),
                            pool_str.as_str(pool),
                            var,
                            symbol
                        );
                    }
                    RecordField::LabeledValue(pool_str, var, val_node_id) => {
                        let _ = writeln!(
                            out_string,
                            "{}({}, Var({:?}), Expr2(",
                            get_spacing(indent_level + 2),
                            pool_str.as_str(pool),
                            var,
                        );

                        let val_expr2 = pool.get(*val_node_id);
                        expr2_to_string_helper(val_expr2, indent_level + 3, pool, out_string);
                        let _ = writeln!(out_string, "{})", get_spacing(indent_level + 2));
                    }
                }
            }

            let _ = writeln!(out_string, "{}]", get_spacing(indent_level + 1));
        }
        Expr2::List { elem_var, elems } => {
            out_string.push_str("List:\n");
            out_string.push_str(&var_to_string(elem_var, indent_level + 1));
            let _ = writeln!(out_string, "{}elems: [\n", get_spacing(indent_level + 1));

            let mut first_elt = true;

            for elem_expr2_id in elems.iter(pool) {
                if !first_elt {
                    out_string.push_str(", ")
                } else {
                    first_elt = false;
                }

                let elem_expr2 = pool.get(*elem_expr2_id);

                expr2_to_string_helper(elem_expr2, indent_level + 2, pool, out_string)
            }

            let _ = writeln!(out_string, "{}]", get_spacing(indent_level + 1));
        }
        Expr2::InvalidLookup(pool_str) => {
            let _ = write!(out_string, "InvalidLookup({})", pool_str.as_str(pool));
        }
        Expr2::SmallInt { text, .. } => {
            let _ = write!(out_string, "SmallInt({})", text.as_str(pool));
        }
        Expr2::LetValue {
            def_id, body_id, ..
        } => {
            let _ = write!(
                out_string,
                "LetValue(def_id: >>{:?}), body_id: >>{:?})",
                value_def_to_string(pool.get(*def_id), pool),
                pool.get(*body_id)
            );
        }
        Expr2::Call { .. } => {
            let _ = write!(out_string, "Call({:?})", expr2);
        }
        Expr2::Closure { args, .. } => {
            out_string.push_str("Closure:\n");
            let _ = writeln!(out_string, "{}args: [", get_spacing(indent_level + 1));

            for (_, pattern_id) in args.iter(pool) {
                let arg_pattern2 = pool.get(*pattern_id);

                let _ = writeln!(
                    out_string,
                    "{}{:?}",
                    get_spacing(indent_level + 2),
                    arg_pattern2
                );
            }
        }
        &Expr2::Var { .. } => {
            let _ = write!(out_string, "{:?}", expr2);
        }
        Expr2::RuntimeError { .. } => {
            out_string.push_str("RuntimeError\n");
        }
        other => todo!("Implement for {:?}", other),
    }

    out_string.push('\n');
}

fn var_to_string(some_var: &Variable, indent_level: usize) -> String {
    format!("{}Var({:?})\n", get_spacing(indent_level + 1), some_var)
}
