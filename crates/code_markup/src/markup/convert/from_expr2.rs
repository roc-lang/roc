use crate::{
    markup::{
        attribute::Attributes,
        common_nodes::{
            new_arg_name_mn, new_arrow_mn, new_blank_mn, new_colon_mn, new_comma_mn, new_equals_mn,
            new_left_accolade_mn, new_left_square_mn, new_operator_mn, new_right_accolade_mn,
            new_right_square_mn,
        },
        mark_id_ast_id_map::MarkIdAstIdMap,
        nodes::{
            get_string, join_mark_nodes_commas, join_mark_nodes_spaces, new_markup_node, MarkupNode,
        },
    },
    slow_pool::{MarkNodeId, SlowPool},
    syntax_highlight::HighlightStyle,
};

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

use super::from_def2::add_node;

// make Markup Nodes: generate String representation, assign Highlighting Style
pub fn expr2_to_markup<'a>(
    env: &Env<'a>,
    expr2: &Expr2,
    expr2_node_id: ExprId,
    mark_node_pool: &mut SlowPool,
    mark_id_ast_id_map: &mut MarkIdAstIdMap,
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
                mark_id_ast_id_map,
                indent_level,
            )
        }
        Expr2::Str(text) => {
            let content = format!("\"{}\"", text.as_str(env.pool));

            string_mark_node(
                &content,
                indent_level,
                ast_node_id,
                mark_node_pool,
                mark_id_ast_id_map,
            )
        }
        Expr2::SmallStr(array_str) => {
            let content = format!("\"{}\"", array_str.as_str());

            string_mark_node(
                &content,
                indent_level,
                ast_node_id,
                mark_node_pool,
                mark_id_ast_id_map,
            )
        }
        Expr2::Tag { name, .. } => new_markup_node(
            with_indent(indent_level, &get_string(env, name)),
            ast_node_id,
            HighlightStyle::Type,
            mark_node_pool,
            mark_id_ast_id_map,
            indent_level,
        ),
        Expr2::Call { args, expr_id, .. } => {
            let expr = env.pool.get(*expr_id);
            let fun_call_mark_id = expr2_to_markup(
                env,
                expr,
                *expr_id,
                mark_node_pool,
                mark_id_ast_id_map,
                interns,
                indent_level,
            )?;

            let arg_expr_ids: Vec<ExprId> =
                args.iter(env.pool).map(|(_, arg_id)| *arg_id).collect();

            let arg_call_mark_ids: Vec<MarkNodeId> = arg_expr_ids
                .iter()
                .map(|arg_id| {
                    let arg_expr = env.pool.get(*arg_id);

                    expr2_to_markup(
                        env,
                        arg_expr,
                        *arg_id,
                        mark_node_pool,
                        mark_id_ast_id_map,
                        interns,
                        0,
                    )
                })
                .collect::<ASTResult<Vec<MarkNodeId>>>()?;

            let mut args_with_sapces =
                join_mark_nodes_spaces(arg_call_mark_ids, true, mark_node_pool);

            let mut children_ids = vec![fun_call_mark_id];
            children_ids.append(&mut args_with_sapces);

            let call_node = MarkupNode::Nested {
                children_ids,
                parent_id_opt: None,
                newlines_at_end: 0,
            };

            add_node(call_node, ast_node_id, mark_node_pool, mark_id_ast_id_map)
        }
        Expr2::Var(symbol) => {
            let text = symbol.fully_qualified(interns, env.home);

            new_markup_node(
                text.to_string(),
                ast_node_id,
                HighlightStyle::Value,
                mark_node_pool,
                mark_id_ast_id_map,
                indent_level,
            )
        }
        Expr2::List { elems, .. } => {
            let mut children_ids = vec![add_node(
                new_left_square_mn(),
                ast_node_id,
                mark_node_pool,
                mark_id_ast_id_map,
            )];

            let indexed_node_ids: Vec<(usize, ExprId)> =
                elems.iter(env.pool).copied().enumerate().collect();

            for (idx, node_id) in indexed_node_ids.iter() {
                let sub_expr2 = env.pool.get(*node_id);

                children_ids.push(expr2_to_markup(
                    env,
                    sub_expr2,
                    *node_id,
                    mark_node_pool,
                    mark_id_ast_id_map,
                    interns,
                    indent_level,
                )?);

                if idx + 1 < elems.len() {
                    children_ids.push(add_node(
                        new_comma_mn(),
                        ast_node_id,
                        mark_node_pool,
                        mark_id_ast_id_map,
                    ));
                }
            }
            children_ids.push(add_node(
                new_right_square_mn(),
                ast_node_id,
                mark_node_pool,
                mark_id_ast_id_map,
            ));

            let list_mn = MarkupNode::Nested {
                children_ids,
                parent_id_opt: None,
                newlines_at_end: 0,
            };

            add_node(list_mn, ast_node_id, mark_node_pool, mark_id_ast_id_map)
        }
        Expr2::EmptyRecord => {
            let children_ids = vec![
                add_node(
                    new_left_accolade_mn(),
                    ast_node_id,
                    mark_node_pool,
                    mark_id_ast_id_map,
                ),
                add_node(
                    new_right_accolade_mn(),
                    ast_node_id,
                    mark_node_pool,
                    mark_id_ast_id_map,
                ),
            ];

            let record_mn = MarkupNode::Nested {
                children_ids,
                parent_id_opt: None,
                newlines_at_end: 0,
            };

            add_node(record_mn, ast_node_id, mark_node_pool, mark_id_ast_id_map)
        }
        Expr2::Record { fields, .. } => {
            let mut children_ids = vec![add_node(
                new_left_accolade_mn(),
                ast_node_id,
                mark_node_pool,
                mark_id_ast_id_map,
            )];

            for (idx, field_node_id) in fields.iter_node_ids().enumerate() {
                let record_field = env.pool.get(field_node_id);

                let field_name = record_field.get_record_field_pool_str();

                children_ids.push(new_markup_node(
                    field_name.as_str(env.pool).to_owned(),
                    ast_node_id,
                    HighlightStyle::RecordField,
                    mark_node_pool,
                    mark_id_ast_id_map,
                    indent_level,
                ));

                match record_field {
                    RecordField::InvalidLabelOnly(_, _) => (),
                    RecordField::LabelOnly(_, _, _) => (),
                    RecordField::LabeledValue(_, _, sub_expr2_node_id) => {
                        children_ids.push(add_node(
                            new_colon_mn(),
                            ast_node_id,
                            mark_node_pool,
                            mark_id_ast_id_map,
                        ));

                        let sub_expr2 = env.pool.get(*sub_expr2_node_id);
                        children_ids.push(expr2_to_markup(
                            env,
                            sub_expr2,
                            *sub_expr2_node_id,
                            mark_node_pool,
                            mark_id_ast_id_map,
                            interns,
                            indent_level,
                        )?);
                    }
                }

                if idx + 1 < fields.len() {
                    children_ids.push(add_node(
                        new_comma_mn(),
                        ast_node_id,
                        mark_node_pool,
                        mark_id_ast_id_map,
                    ));
                }
            }

            children_ids.push(add_node(
                new_right_accolade_mn(),
                ast_node_id,
                mark_node_pool,
                mark_id_ast_id_map,
            ));

            let record_mn = MarkupNode::Nested {
                children_ids,
                parent_id_opt: None,
                newlines_at_end: 0,
            };

            add_node(record_mn, ast_node_id, mark_node_pool, mark_id_ast_id_map)
        }
        Expr2::Blank => add_node(
            new_blank_mn(),
            ast_node_id,
            mark_node_pool,
            mark_id_ast_id_map,
        ),
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
                syn_high_style: HighlightStyle::Value,
                attributes: Attributes::default(),
                parent_id_opt: None,
                newlines_at_end: 0,
            };

            let val_name_mn_id =
                add_node(val_name_mn, ast_node_id, mark_node_pool, mark_id_ast_id_map);

            let equals_mn_id = add_node(
                new_equals_mn(),
                ast_node_id,
                mark_node_pool,
                mark_id_ast_id_map,
            );

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
                        mark_id_ast_id_map,
                        interns,
                        indent_level,
                    )?;

                    let body_mn = mark_node_pool.get_mut(body_mn_id);
                    body_mn.add_newline_at_end();

                    let full_let_mn = MarkupNode::Nested {
                        children_ids: vec![val_name_mn_id, equals_mn_id, body_mn_id],
                        parent_id_opt: None,
                        newlines_at_end: 1,
                    };

                    add_node(full_let_mn, ast_node_id, mark_node_pool, mark_id_ast_id_map)
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
            let backslash_mn = new_operator_mn("\\".to_string());
            let backslash_mn_id = add_node(
                backslash_mn,
                ast_node_id,
                mark_node_pool,
                mark_id_ast_id_map,
            );

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

            let arg_mark_nodes: Vec<_> = arg_names
                .iter()
                .map(|arg_name| new_arg_name_mn(arg_name.to_string()))
                .collect();

            let args_with_commas: Vec<MarkupNode> = join_mark_nodes_commas(arg_mark_nodes);

            let mut args_with_commas_ids: Vec<MarkNodeId> = args_with_commas
                .into_iter()
                .map(|mark_node| {
                    add_node(mark_node, ast_node_id, mark_node_pool, mark_id_ast_id_map)
                })
                .collect();

            let arrow_mn = new_arrow_mn(1);
            let arrow_mn_id = add_node(arrow_mn, ast_node_id, mark_node_pool, mark_id_ast_id_map);

            let mut children_ids = vec![backslash_mn_id];
            children_ids.append(&mut args_with_commas_ids);
            children_ids.push(arrow_mn_id);

            let args_mn = MarkupNode::Nested {
                children_ids,
                parent_id_opt: None,
                newlines_at_end: 0,
            };
            let args_mn_id = add_node(args_mn, ast_node_id, mark_node_pool, mark_id_ast_id_map);

            let body_expr = env.pool.get(*body_id);
            let body_mn_id = expr2_to_markup(
                env,
                body_expr,
                *body_id,
                mark_node_pool,
                mark_id_ast_id_map,
                interns,
                indent_level + 1,
            )?;

            let function_mn = MarkupNode::Nested {
                children_ids: vec![args_mn_id, body_mn_id],
                parent_id_opt: None,
                newlines_at_end: 0,
            };

            add_node(function_mn, ast_node_id, mark_node_pool, mark_id_ast_id_map)
        }
        Expr2::RuntimeError() => new_markup_node(
            "RunTimeError".to_string(),
            ast_node_id,
            HighlightStyle::Blank,
            mark_node_pool,
            mark_id_ast_id_map,
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

fn string_mark_node(
    content: &str,
    indent_level: usize,
    ast_node_id: ASTNodeId,
    mark_node_pool: &mut SlowPool,
    mark_id_ast_id_map: &mut MarkIdAstIdMap,
) -> MarkNodeId {
    new_markup_node(
        with_indent(indent_level, content),
        ast_node_id,
        HighlightStyle::String,
        mark_node_pool,
        mark_id_ast_id_map,
        indent_level,
    )
}
