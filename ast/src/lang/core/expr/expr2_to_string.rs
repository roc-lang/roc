use crate::{
    lang::core::{expr::record_field::RecordField, val_def::value_def_to_string},
    mem_pool::pool::Pool,
};

use super::expr2::{Expr2, ExprId};
use roc_types::subs::Variable;

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
        Expr2::SmallStr(arr_string) => out_string.push_str(&format!(
            "{}{}{}",
            "SmallStr(\"",
            arr_string.as_str(),
            "\")",
        )),
        Expr2::Str(pool_str) => {
            out_string.push_str(&format!("{}{}{}", "Str(\"", pool_str.as_str(pool), "\")",))
        }
        Expr2::Blank => out_string.push_str("Blank"),
        Expr2::EmptyRecord => out_string.push_str("EmptyRecord"),
        Expr2::Record { record_var, fields } => {
            out_string.push_str("Record:\n");
            out_string.push_str(&var_to_string(record_var, indent_level + 1));

            out_string.push_str(&format!("{}fields: [\n", get_spacing(indent_level + 1)));

            let mut first_child = true;

            for field in fields.iter(pool) {
                if !first_child {
                    out_string.push_str(", ")
                } else {
                    first_child = false;
                }

                match field {
                    RecordField::InvalidLabelOnly(pool_str, var) => {
                        out_string.push_str(&format!(
                            "{}({}, Var({:?})",
                            get_spacing(indent_level + 2),
                            pool_str.as_str(pool),
                            var,
                        ));
                    }
                    RecordField::LabelOnly(pool_str, var, symbol) => {
                        out_string.push_str(&format!(
                            "{}({}, Var({:?}), Symbol({:?})",
                            get_spacing(indent_level + 2),
                            pool_str.as_str(pool),
                            var,
                            symbol
                        ));
                    }
                    RecordField::LabeledValue(pool_str, var, val_node_id) => {
                        out_string.push_str(&format!(
                            "{}({}, Var({:?}), Expr2(\n",
                            get_spacing(indent_level + 2),
                            pool_str.as_str(pool),
                            var,
                        ));

                        let val_expr2 = pool.get(*val_node_id);
                        expr2_to_string_helper(val_expr2, indent_level + 3, pool, out_string);
                        out_string.push_str(&format!("{})\n", get_spacing(indent_level + 2)));
                    }
                }
            }

            out_string.push_str(&format!("{}]\n", get_spacing(indent_level + 1)));
        }
        Expr2::List { elem_var, elems } => {
            out_string.push_str("List:\n");
            out_string.push_str(&var_to_string(elem_var, indent_level + 1));
            out_string.push_str(&format!("{}elems: [\n", get_spacing(indent_level + 1)));

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

            out_string.push_str(&format!("{}]\n", get_spacing(indent_level + 1)));
        }
        Expr2::InvalidLookup(pool_str) => {
            out_string.push_str(&format!("InvalidLookup({})", pool_str.as_str(pool)));
        }
        Expr2::SmallInt { text, .. } => {
            out_string.push_str(&format!("SmallInt({})", text.as_str(pool)));
        }
        Expr2::LetValue {
            def_id, body_id, ..
        } => {
            out_string.push_str(&format!(
                "LetValue(def_id: >>{:?}), body_id: >>{:?})",
                value_def_to_string(pool.get(*def_id), pool),
                pool.get(*body_id)
            ));
        }
        Expr2::Call { .. } => {
            out_string.push_str(&format!("Call({:?})", expr2,));
        }
        Expr2::Closure { args, .. } => {
            out_string.push_str("Closure:\n");
            out_string.push_str(&format!("{}args: [\n", get_spacing(indent_level + 1)));

            for (_, pattern_id) in args.iter(pool) {
                let arg_pattern2 = pool.get(*pattern_id);

                out_string.push_str(&format!(
                    "{}{:?}\n",
                    get_spacing(indent_level + 2),
                    arg_pattern2
                ));
            }
        }
        &Expr2::Var { .. } => {
            out_string.push_str(&format!("{:?}", expr2,));
        }
        other => todo!("Implement for {:?}", other),
    }

    out_string.push('\n');
}

fn var_to_string(some_var: &Variable, indent_level: usize) -> String {
    format!("{}Var({:?})\n", get_spacing(indent_level + 1), some_var)
}
