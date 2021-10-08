
use crate::{markup::{attribute::Attributes, common_nodes::{new_arg_name_mn, new_blank_mn, new_colon_mn, new_comma_mn, new_equals_mn, new_func_name_mn, new_left_accolade_mn, new_left_square_mn, new_operator_mn, new_right_accolade_mn, new_right_square_mn}, nodes::{MarkupNode, get_string, new_markup_node}}, slow_pool::{MarkNodeId, SlowPool}, syntax_highlight::HighlightStyle};

use bumpalo::Bump;
use itertools::Itertools;
use roc_ast::{ast_error::ASTResult, lang::{core::{ast::ASTNodeId, expr::{
                expr2::{Expr2, ExprId},
                record_field::RecordField,
            }, pattern::{Pattern2, get_identifier_string}, val_def::ValueDef}, env::Env}};
use roc_module::symbol::Interns;

// make Markup Nodes: generate String representation, assign Highlighting Style
pub fn expr2_to_markup<'a, 'b>(
    arena: &'a Bump,
    env: &mut Env<'b>,
    expr2: &Expr2,
    expr2_node_id: ExprId,
    mark_node_pool: &mut SlowPool,
    interns: &Interns,
) -> ASTResult<MarkNodeId> {
    dbg!(expr2);
    let ast_node_id = ASTNodeId::AExprId(expr2_node_id);

    let mark_node_id = match expr2 {
        Expr2::SmallInt { text, .. }
        | Expr2::I128 { text, .. }
        | Expr2::U128 { text, .. }
        | Expr2::Float { text, .. } => {
            let num_str = get_string(env, text);

            new_markup_node(num_str, ast_node_id, HighlightStyle::Number, mark_node_pool)
        }
        Expr2::Str(text) => new_markup_node(
            "\"".to_owned() + text.as_str(env.pool) + "\"",
            ast_node_id,
            HighlightStyle::String,
            mark_node_pool,
        ),
        Expr2::GlobalTag { name, .. } => new_markup_node(
            get_string(env, name),
            ast_node_id,
            HighlightStyle::Type,
            mark_node_pool,
        ),
        Expr2::Call { expr: expr_id, .. } => {
            let expr = env.pool.get(*expr_id);
            expr2_to_markup(arena, env, expr, *expr_id, mark_node_pool, interns)?
        }
        Expr2::Var(symbol) => {
            //TODO make bump_format with arena
            let text = format!("{:?}", symbol);
            new_markup_node(text, ast_node_id, HighlightStyle::Variable, mark_node_pool)
        }
        Expr2::List { elems, .. } => {
            let mut children_ids =
                vec![mark_node_pool.add(new_left_square_mn(expr2_node_id, None))];

            let indexed_node_ids: Vec<(usize, ExprId)> =
                elems.iter(env.pool).copied().enumerate().collect();

            for (idx, node_id) in indexed_node_ids.iter() {
                let sub_expr2 = env.pool.get(*node_id);

                children_ids.push(expr2_to_markup(
                    arena,
                    env,
                    sub_expr2,
                    *node_id,
                    mark_node_pool,
                    interns,
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
                ));

                match record_field {
                    RecordField::InvalidLabelOnly(_, _) => (),
                    RecordField::LabelOnly(_, _, _) => (),
                    RecordField::LabeledValue(_, _, sub_expr2_node_id) => {
                        children_ids.push(mark_node_pool.add(new_colon_mn(expr2_node_id, None)));

                        let sub_expr2 = env.pool.get(*sub_expr2_node_id);
                        children_ids.push(expr2_to_markup(
                            arena,
                            env,
                            sub_expr2,
                            *sub_expr2_node_id,
                            mark_node_pool,
                            interns,
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
                syn_high_style: HighlightStyle::Variable,
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
                        arena,
                        env,
                        env.pool.get(*expr_id),
                        *expr_id,
                        mark_node_pool,
                        interns,
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
        Expr2::Closure{
            function_type:_,
            name,
            recursive:_,
            args,
            body_id,
            extra:_
        } => {
            let func_name = name.ident_str(interns).as_str();

            let func_name_mn =
                new_func_name_mn(func_name.to_string(), expr2_node_id);

            let func_name_mn_id = mark_node_pool.add(func_name_mn);
        
            let backslash_mn = new_operator_mn("\\".to_string(), expr2_node_id, None);
            let backslash_mn_id = mark_node_pool.add(backslash_mn);

            let arg_names: Vec<&str> = 
            args.iter(env.pool).map(
                | (_, arg_node_id) | {
                    let arg_pattern2 = env.pool.get(*arg_node_id);

                    match arg_pattern2 {
                        Pattern2::Identifier(id_symbol) => {
                            id_symbol.ident_str(interns).as_str()
                        },
                        other => {
                            todo!("TODO: support the following pattern2 as function arg: {:?}", other);
                        }
                    }
                }
            )
            .collect();

            let arg_mark_nodes = arg_names.iter().map(
                |arg_name|
                new_arg_name_mn(arg_name.to_string(), expr2_node_id)
            );
        
            let commas = 
                (0..(arg_mark_nodes.len() - 1)).map(
                    |_|
                    new_comma_mn(expr2_node_id, None)
                );
        
            let args_with_commas: Vec<MarkupNode> = arg_mark_nodes.interleave(commas).collect_vec();

            let mut args_with_commas_ids: Vec<MarkNodeId> = 
                args_with_commas.into_iter().map(
                    |mark_node|
                    mark_node_pool.add(mark_node)
                ).collect();

            let mut children_ids = vec![backslash_mn_id];
            children_ids.append(&mut args_with_commas_ids);
        
            let args_mn = MarkupNode::Nested {
                ast_node_id: ASTNodeId::AExprId(expr2_node_id),
                children_ids,
                parent_id_opt: None,
                newlines_at_end: 0,
            };
            let args_mn_id = mark_node_pool.add(args_mn);

            let body_expr = env.pool.get(*body_id);
            let body_mn_id = expr2_to_markup(
                arena,
                env,
                body_expr,
                *body_id,
                mark_node_pool,
                interns,
            )?;

            let function_node = MarkupNode::Nested {
                ast_node_id,
                children_ids: vec![func_name_mn_id, args_mn_id, body_mn_id],
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
        ),
        rest => todo!("implement expr2_to_markup for {:?}", rest),
    };

    Ok(mark_node_id)
}