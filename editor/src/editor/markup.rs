#![allow(dead_code)]

use super::syntax_highlight::HighlightStyle;
use crate::lang::{
    ast::Expr2,
    expr::Env,
    pool::{NodeId, PoolStr},
};
use bumpalo::Bump;

#[derive(Debug)]
pub enum MarkupNode {
    // TODO add parent field, necessary for moving caret to next node
    Nested {
        ast_node_id: NodeId<Expr2>,
        children: Vec<MarkupNode>,
    },
    Text {
        content: String,
        ast_node_id: NodeId<Expr2>,
        syn_high_style: HighlightStyle,
        attributes: Vec<Attribute>,
    },
    Hole {
        ast_node_id: NodeId<Expr2>,
        attributes: Vec<Attribute>,
        syn_high_style: HighlightStyle,
    },
}

#[derive(Debug)]
pub enum Attribute {
    Caret { offset_col: usize },
    SelectionStart { offset_col: usize },
    SelectionEnd { offset_col: usize },
    // Highlight is used for example when searching for a specific string to highlight all search results in the module
    HighlightStart { offset_col: usize },
    HighlightEnd { offset_col: usize },
    // Underline is used for warnings and errors
    UnderlineStart { offset_col: usize },
    UnderlineEnd { offset_col: usize },
}

fn get_string<'a>(env: &Env<'a>, pool_str: &PoolStr) -> String {
    pool_str.as_str(env.pool).to_owned()
}

fn new_markup(text: String, node_id: NodeId<Expr2>, highlight_style: HighlightStyle) -> MarkupNode {
    MarkupNode::Text {
        content: text,
        ast_node_id: node_id,
        syn_high_style: highlight_style,
        attributes: Vec::new(),
    }
}

// make Markup Nodes: generate String representation, assign Highlighting Style
pub fn expr2_to_markup<'a, 'b>(arena: &'a Bump, env: &mut Env<'b>, expr2: &Expr2) -> MarkupNode {
    // TODO find way to add current expr2 to pool
    let node_id = env.pool.add(Expr2::Hole);

    match expr2 {
        Expr2::SmallInt { text, .. }
        | Expr2::I128 { text, .. }
        | Expr2::U128 { text, .. }
        | Expr2::Float { text, .. } => {
            new_markup(get_string(env, &text), node_id, HighlightStyle::Number)
        }
        Expr2::Str(text) => new_markup(
            "\"".to_owned() + text.as_str(env.pool) + "\"",
            node_id,
            HighlightStyle::String,
        ),
        Expr2::GlobalTag { name, .. } => {
            new_markup(get_string(env, &name), node_id, HighlightStyle::Type)
        }
        Expr2::Call { expr: expr_id, .. } => {
            let expr = env.pool.get(*expr_id);
            expr2_to_markup(arena, env, expr)
        }
        Expr2::Var(symbol) => {
            //TODO make bump_format with arena
            let text = format!("{:?}", symbol);
            new_markup(text, node_id, HighlightStyle::Variable)
        }
        Expr2::List { elems, .. } => {
            let mut children: Vec<MarkupNode> = Vec::new();
            children.push(new_markup(
                "[ ".to_string(),
                node_id,
                HighlightStyle::Bracket,
            ));

            for (idx, node_id) in elems.iter_node_ids().enumerate() {
                let sub_expr2 = env.pool.get(node_id);

                children.push(expr2_to_markup(arena, env, sub_expr2));

                if idx + 1 < elems.len() {
                    children.push(new_markup(
                        ", ".to_string(),
                        node_id,
                        HighlightStyle::Operator,
                    ));
                }
            }
            children.push(new_markup(
                "] ".to_string(),
                node_id,
                HighlightStyle::Bracket,
            ));

            MarkupNode::Nested {
                ast_node_id: node_id,
                children,
            }
        }
        Expr2::Record { fields, .. } => {
            let mut children: Vec<MarkupNode> = Vec::new();
            children.push(new_markup(
                "{ ".to_string(),
                node_id,
                HighlightStyle::Bracket,
            ));

            for (idx, field_node_id) in fields.iter_node_ids().enumerate() {
                let (pool_field_name, _, sub_expr2_node_id) = env.pool.get(field_node_id);

                let field_name = pool_field_name.as_str(env.pool);

                let sub_expr2 = env.pool.get(*sub_expr2_node_id);

                children.push(new_markup(
                    field_name.to_string(),
                    node_id,
                    HighlightStyle::RecordField,
                ));
                children.push(new_markup(
                    ": ".to_string(),
                    node_id,
                    HighlightStyle::Operator,
                ));

                children.push(expr2_to_markup(arena, env, sub_expr2));

                if idx + 1 < fields.len() {
                    children.push(new_markup(
                        ", ".to_string(),
                        node_id,
                        HighlightStyle::Operator,
                    ));
                }
            }

            children.push(new_markup(
                " }".to_string(),
                node_id,
                HighlightStyle::Bracket,
            ));

            MarkupNode::Nested {
                ast_node_id: node_id,
                children,
            }
        }
        Expr2::Hole => MarkupNode::Hole {
            ast_node_id: node_id,
            attributes: Vec::new(),
            syn_high_style: HighlightStyle::Hole,
        },
        rest => todo!("implement expr2_to_markup for {:?}", rest),
    }
}

pub fn set_caret_at_start(markup_node: &mut MarkupNode) {
    match markup_node {
        MarkupNode::Nested {
            ast_node_id: _,
            children,
        } => {
            if let Some(child) = children.first_mut() { set_caret_at_start(child) }
        }
        MarkupNode::Text {
            content: _,
            ast_node_id: _,
            syn_high_style: _,
            attributes,
        } => attributes.push(Attribute::Caret { offset_col: 0 }),
        MarkupNode::Hole {
            ast_node_id: _,
            attributes,
            syn_high_style: _,
        } => attributes.push(Attribute::Caret { offset_col: 0 }),
    };
}
