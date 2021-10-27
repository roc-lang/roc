use crate::{
    markup::{
        attribute::Attributes,
        common_nodes::{
            new_arg_name_mn, new_arrow_mn, new_blank_mn, new_colon_mn, new_comma_mn, new_equals_mn,
            new_left_accolade_mn, new_left_square_mn, new_operator_mn, new_right_accolade_mn,
            new_right_square_mn,
        },
        nodes::{
            get_string, join_mark_nodes_commas, join_mark_nodes_spaces, new_markup_node, MarkupNode,
        },
    },
    slow_pool::{MarkNodeId, SlowPool},
    syntax_highlight::HighlightStyle,
};

use itertools::Itertools;
use roc_ast::{
    ast_error::ASTResult,
    lang::{
        core::{
            ast::ASTNodeId,
            expr::{
                expr2::{Expr2, ExprId},
                record_field::RecordField,
            },
            pattern::{get_identifier_string, Pattern2},
            val_def::ValueDef,
        },
        env::Env,
    },
};
use roc_module::{module_err::ModuleResult, symbol::Interns};

// make Markup Nodes: generate String representation, assign Highlighting Style
pub fn expr2_to_markup<'a>(
    env: &Env<'a>,
    expr2: &Expr2,
    expr2_node_id: ExprId,
    mark_node_pool: &mut SlowPool,
    interns: &Interns,
    indent_level: usize,
) -> ASTResult<MarkNodeId> {
    let ast_node_id = ASTNodeId::AExprId(expr2_node_id);

    // for debugging
    //println!("EXPR2 {:?}", expr2);

    let mark_node_id = match expr2 {
        Expr2::SmallInt { text, .. }
        | Expr2::I128 { text, .. }
        | Expr2::U128 { text, .. }
        | Expr2::Float { text, .. } => {
            let num_str = get_string(env, text);

            new_markup_node(
                with_indent(indent_level, &num_str),
                ast_node_id,
                HighlightStyle::Number,
                mark_node_pool,
                indent_level,
            )
        }
        Expr2::Str(text) => {
            let content = format!("\"{}\"", text.as_str(env.pool));

            new_markup_node(
                with_indent(indent_level, &content),
                ast_node_id,
                HighlightStyle::String,
                mark_node_pool,
                indent_level,
            )
        }
        Expr2::GlobalTag { name, .. } => new_markup_node(
            with_indent(indent_level, &get_string(env, name)),
            ast_node_id,
            HighlightStyle::Type,
            mark_node_pool,
            indent_level,
        ),
        Expr2::Call { args, expr_id, .. } => {
            let expr = env.pool.get(*expr_id);
            let fun_call_mark_id =
                expr2_to_markup(env, expr, *expr_id, mark_node_pool, interns, indent_level)?;

            let arg_expr_ids: Vec<ExprId> =
                args.iter(env.pool).map(|(_, arg_id)| *arg_id).collect();

            let arg_call_mark_ids: Vec<MarkNodeId> = arg_expr_ids
                .iter()
                .map(|arg_id| {
                    let arg_expr = env.pool.get(*arg_id);

                    expr2_to_markup(env, arg_expr, *arg_id, mark_node_pool, interns, 0)
                })
                .collect::<ASTResult<Vec<MarkNodeId>>>()?;

            let mut args_with_sapces =
                join_mark_nodes_spaces(arg_call_mark_ids, true, ast_node_id, mark_node_pool);

            let mut children_ids = vec![fun_call_mark_id];
            children_ids.append(&mut args_with_sapces);

            let call_node = MarkupNode::Nested {
                ast_node_id,
                children_ids,
                parent_id_opt: None,
                newlines_at_end: 0,
            };

            mark_node_pool.add(call_node)
        }
        Expr2::Var(symbol) => {
            let text = symbol.fully_qualified(interns, env.home);

            new_markup_node(
                text.to_string(),
                ast_node_id,
                HighlightStyle::Value,
                mark_node_pool,
                indent_level,
            )
        }
        Expr2::List { elems, .. } => {
            let mut children_ids =
                vec![mark_node_pool.add(new_left_square_mn(expr2_node_id, None))];

            let indexed_node_ids: Vec<(usize, ExprId)> =
                elems.iter(env.pool).copied().enumerate().collect();

            for (idx, node_id) in indexed_node_ids.iter() {
                let sub_expr2 = env.pool.get(*node_id);

                children_ids.push(expr2_to_markup(
                    env,
                    sub_expr2,
                    *node_id,
                    mark_node_pool,
                    interns,
                    indent_level,
                )?);

                if idx + 1 < elems.len() {
                    children_ids.push(mark_node_pool.add(new_comma_mn(expr2_node_id, None)));
                }
            }
            children_ids.push(mark_node_pool.add(new_right_square_mn(expr2_node_id, None)));

            let list_node = MarkupNode::Nested {
                ast_node_id,
                children_ids,
                parent_id_opt: None,
                newlines_at_end: 0,
            };

            mark_node_pool.add(list_node)
        }
        Expr2::EmptyRecord => {
            let children_ids = vec![
                mark_node_pool.add(new_left_accolade_mn(expr2_node_id, None)),
                mark_node_pool.add(new_right_accolade_mn(expr2_node_id, None)),
            ];

            let record_node = MarkupNode::Nested {
                ast_node_id,
                children_ids,
                parent_id_opt: None,
                newlines_at_end: 0,
            };

            mark_node_pool.add(record_node)
        }
        Expr2::Record { fields, .. } => {
            let mut children_ids =
                vec![mark_node_pool.add(new_left_accolade_mn(expr2_node_id, None))];

            for (idx, field_node_id) in fields.iter_node_ids().enumerate() {
                let record_field = env.pool.get(field_node_id);

                let field_name = record_field.get_record_field_pool_str();

                children_ids.push(new_markup_node(
                    field_name.as_str(env.pool).to_owned(),
                    ast_node_id,
                    HighlightStyle::RecordField,
                    mark_node_pool,
                    indent_level,
                ));

                match record_field {
                    RecordField::InvalidLabelOnly(_, _) => (),
                    RecordField::LabelOnly(_, _, _) => (),
                    RecordField::LabeledValue(_, _, sub_expr2_node_id) => {
                        children_ids.push(mark_node_pool.add(new_colon_mn(expr2_node_id, None)));

                        let sub_expr2 = env.pool.get(*sub_expr2_node_id);
                        children_ids.push(expr2_to_markup(
                            env,
                            sub_expr2,
                            *sub_expr2_node_id,
                            mark_node_pool,
                            interns,
                            indent_level,
                        )?);
                    }
                }

                if idx + 1 < fields.len() {
                    children_ids.push(mark_node_pool.add(new_comma_mn(expr2_node_id, None)));
                }
            }

            children_ids.push(mark_node_pool.add(new_right_accolade_mn(expr2_node_id, None)));

            let record_node = MarkupNode::Nested {
                ast_node_id,
                children_ids,
                parent_id_opt: None,
                newlines_at_end: 0,
            };

            mark_node_pool.add(record_node)
        }
        Expr2::Blank => mark_node_pool.add(new_blank_mn(ast_node_id, None)),
        Expr2::LetValue {
            def_id,
            body_id: _,
            body_var: _,
        } => {
            let pattern_id = env.pool.get(*def_id).get_pattern_id();

            let pattern2 = env.pool.get(pattern_id);

            let val_name = get_identifier_string(pattern2, interns)?;

            let val_name_mn = MarkupNode::Text {
                content: val_name,
                ast_node_id,
                syn_high_style: HighlightStyle::Value,
                attributes: Attributes::default(),
                parent_id_opt: None,
                newlines_at_end: 0,
            };

            let val_name_mn_id = mark_node_pool.add(val_name_mn);

            let equals_mn_id = mark_node_pool.add(new_equals_mn(ast_node_id, None));

            let value_def = env.pool.get(*def_id);

            match value_def {
                ValueDef::NoAnnotation {
                    pattern_id: _,
                    expr_id,
                    expr_var: _,
                } => {
                    let body_mn_id = expr2_to_markup(
                        env,
                        env.pool.get(*expr_id),
                        *expr_id,
                        mark_node_pool,
                        interns,
                        indent_level,
                    )?;

                    let body_mn = mark_node_pool.get_mut(body_mn_id);
                    body_mn.add_newline_at_end();

                    let full_let_node = MarkupNode::Nested {
                        ast_node_id,
                        children_ids: vec![val_name_mn_id, equals_mn_id, body_mn_id],
                        parent_id_opt: None,
                        newlines_at_end: 1,
                    };

                    mark_node_pool.add(full_let_node)
                }
                other => {
                    unimplemented!(
                        "I don't know how to convert {:?} into a MarkupNode yet.",
                        other
                    )
                }
            }
        }
        Expr2::Closure {
            function_type: _,
            uniq_symbol: _,
            recursive: _,
            args,
            body_id,
            extra: _,
        } => {
            let backslash_mn = new_operator_mn("\\".to_string(), expr2_node_id, None);
            let backslash_mn_id = mark_node_pool.add(backslash_mn);

            let arg_names: Vec<&str> = args
                .iter(env.pool)
                .map(|(_, arg_node_id)| {
                    let arg_pattern2 = env.pool.get(*arg_node_id);

                    match arg_pattern2 {
                        Pattern2::Identifier(id_symbol) => {
                            let ident_id = id_symbol.ident_id();

                            env.ident_ids.get_name_str_res(ident_id)
                        }
                        Pattern2::Shadowed { shadowed_ident } => {
                            Ok(shadowed_ident.as_str(env.pool))
                        }
                        other => {
                            todo!(
                                "TODO: support the following pattern2 as function arg: {:?}",
                                other
                            );
                        }
                    }
                })
                .collect::<ModuleResult<Vec<&str>>>()?;

            let arg_mark_nodes = arg_names
                .iter()
                .map(|arg_name| new_arg_name_mn(arg_name.to_string(), expr2_node_id))
                .collect_vec();

            let args_with_commas: Vec<MarkupNode> =
                join_mark_nodes_commas(arg_mark_nodes, ASTNodeId::AExprId(expr2_node_id));

            let mut args_with_commas_ids: Vec<MarkNodeId> = args_with_commas
                .into_iter()
                .map(|mark_node| mark_node_pool.add(mark_node))
                .collect();

            let arrow_mn = new_arrow_mn(ASTNodeId::AExprId(expr2_node_id), 1);
            let arrow_mn_id = mark_node_pool.add(arrow_mn);

            let mut children_ids = vec![backslash_mn_id];
            children_ids.append(&mut args_with_commas_ids);
            children_ids.push(arrow_mn_id);

            let args_mn = MarkupNode::Nested {
                ast_node_id: ASTNodeId::AExprId(expr2_node_id),
                children_ids,
                parent_id_opt: None,
                newlines_at_end: 0,
            };
            let args_mn_id = mark_node_pool.add(args_mn);

            let body_expr = env.pool.get(*body_id);
            let body_mn_id = expr2_to_markup(
                env,
                body_expr,
                *body_id,
                mark_node_pool,
                interns,
                indent_level + 1,
            )?;

            let function_node = MarkupNode::Nested {
                ast_node_id,
                children_ids: vec![args_mn_id, body_mn_id],
                parent_id_opt: None,
                newlines_at_end: 0,
            };

            mark_node_pool.add(function_node)
        }
        Expr2::RuntimeError() => new_markup_node(
            "RunTimeError".to_string(),
            ast_node_id,
            HighlightStyle::Blank,
            mark_node_pool,
            indent_level,
        ),
        rest => todo!("implement expr2_to_markup for {:?}", rest),
    };

    Ok(mark_node_id)
}

fn with_indent(indent_level: usize, some_str: &str) -> String {
    let full_indent = std::iter::repeat(" ").take(indent_level * 4);
    let mut full_string: String = full_indent.collect();

    full_string.push_str(some_str);

    full_string
}
