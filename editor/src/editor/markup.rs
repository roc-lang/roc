#![allow(dead_code)]

use super::syntax_highlight::HighlightStyle;
use crate::editor::slow_pool::{SlowNodeId, SlowPool};
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
        children_ids: Vec<SlowNodeId>,
        parent_id_opt: Option<SlowNodeId>,
    },
    Text {
        content: String,
        ast_node_id: NodeId<Expr2>,
        syn_high_style: HighlightStyle,
        attributes: Vec<Attribute>,
        parent_id_opt: Option<SlowNodeId>,
    },
    Blank {
        ast_node_id: NodeId<Expr2>,
        attributes: Vec<Attribute>,
        syn_high_style: HighlightStyle,
        parent_id_opt: Option<SlowNodeId>,
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

fn new_markup_node(
    text: String,
    node_id: NodeId<Expr2>,
    highlight_style: HighlightStyle,
    markup_node_pool: &mut SlowPool,
) -> SlowNodeId {
    let node = MarkupNode::Text {
        content: text,
        ast_node_id: node_id,
        syn_high_style: highlight_style,
        attributes: Vec::new(),
        parent_id_opt: None,
    };

    markup_node_pool.add(node)
}

// make Markup Nodes: generate String representation, assign Highlighting Style
pub fn expr2_to_markup<'a, 'b>(
    arena: &'a Bump,
    env: &mut Env<'b>,
    expr2: &Expr2,
    markup_node_pool: &mut SlowPool,
) -> SlowNodeId {
    // TODO find way to add current expr2 to pool
    let node_id = env.pool.add(Expr2::Blank);

    match expr2 {
        Expr2::SmallInt { text, .. }
        | Expr2::I128 { text, .. }
        | Expr2::U128 { text, .. }
        | Expr2::Float { text, .. } => new_markup_node(
            get_string(env, &text),
            node_id,
            HighlightStyle::Number,
            markup_node_pool,
        ),
        Expr2::Str(text) => new_markup_node(
            "\"".to_owned() + text.as_str(env.pool) + "\"",
            node_id,
            HighlightStyle::String,
            markup_node_pool,
        ),
        Expr2::GlobalTag { name, .. } => new_markup_node(
            get_string(env, &name),
            node_id,
            HighlightStyle::Type,
            markup_node_pool,
        ),
        Expr2::Call { expr: expr_id, .. } => {
            let expr = env.pool.get(*expr_id);
            expr2_to_markup(arena, env, expr, markup_node_pool)
        }
        Expr2::Var(symbol) => {
            //TODO make bump_format with arena
            let text = format!("{:?}", symbol);
            new_markup_node(text, node_id, HighlightStyle::Variable, markup_node_pool)
        }
        Expr2::List { elems, .. } => {
            let mut children_ids = Vec::new();

            children_ids.push(new_markup_node(
                "[ ".to_string(),
                node_id,
                HighlightStyle::Bracket,
                markup_node_pool,
            ));

            for (idx, node_id) in elems.iter_node_ids().enumerate() {
                let sub_expr2 = env.pool.get(node_id);

                children_ids.push(expr2_to_markup(arena, env, sub_expr2, markup_node_pool));

                if idx + 1 < elems.len() {
                    children_ids.push(new_markup_node(
                        ", ".to_string(),
                        node_id,
                        HighlightStyle::Operator,
                        markup_node_pool,
                    ));
                }
            }
            children_ids.push(new_markup_node(
                "] ".to_string(),
                node_id,
                HighlightStyle::Bracket,
                markup_node_pool,
            ));

            let list_node = MarkupNode::Nested {
                ast_node_id: node_id,
                children_ids,
                parent_id_opt: None,
            };

            markup_node_pool.add(list_node)
        }
        Expr2::Record { fields, .. } => {
            let mut children_ids = Vec::new();

            children_ids.push(new_markup_node(
                "{ ".to_string(),
                node_id,
                HighlightStyle::Bracket,
                markup_node_pool,
            ));

            for (idx, field_node_id) in fields.iter_node_ids().enumerate() {
                let (pool_field_name, _, sub_expr2_node_id) = env.pool.get(field_node_id);

                let field_name = pool_field_name.as_str(env.pool);

                let sub_expr2 = env.pool.get(*sub_expr2_node_id);

                children_ids.push(new_markup_node(
                    field_name.to_string(),
                    node_id,
                    HighlightStyle::RecordField,
                    markup_node_pool,
                ));

                children_ids.push(new_markup_node(
                    ": ".to_string(),
                    node_id,
                    HighlightStyle::Operator,
                    markup_node_pool,
                ));

                children_ids.push(expr2_to_markup(arena, env, sub_expr2, markup_node_pool));

                if idx + 1 < fields.len() {
                    children_ids.push(new_markup_node(
                        ", ".to_string(),
                        node_id,
                        HighlightStyle::Operator,
                        markup_node_pool,
                    ));
                }
            }

            children_ids.push(new_markup_node(
                " }".to_string(),
                node_id,
                HighlightStyle::Bracket,
                markup_node_pool,
            ));

            let record_node = MarkupNode::Nested {
                ast_node_id: node_id,
                children_ids,
                parent_id_opt: None,
            };

            markup_node_pool.add(record_node)
        }
        Expr2::Blank => markup_node_pool.add(MarkupNode::Blank {
            ast_node_id: node_id,
            attributes: Vec::new(),
            syn_high_style: HighlightStyle::Blank,
            parent_id_opt: None,
        }),
        rest => todo!("implement expr2_to_markup for {:?}", rest),
    }
}

pub fn set_parent_for_all(markup_node_id: SlowNodeId, markup_node_pool: &mut SlowPool) {
    let node = markup_node_pool.get(markup_node_id);

    if let MarkupNode::Nested {
        ast_node_id: _,
        children_ids,
        parent_id_opt: _,
    } = node
    {
        if let Some(child_id) = children_ids.first() {
            set_parent_for_all_helper(*child_id, markup_node_id, markup_node_pool);
        }
    }
}

pub fn set_parent_for_all_helper(
    markup_node_id: SlowNodeId,
    parent_node_id: SlowNodeId,
    markup_node_pool: &mut SlowPool,
) {
    let node = markup_node_pool.get_mut(markup_node_id);

    match node {
        MarkupNode::Nested {
            ast_node_id: _,
            children_ids,
            parent_id_opt,
        } => {
            *parent_id_opt = Some(parent_node_id);

            if let Some(child_id) = children_ids.first() {
                set_parent_for_all_helper(*child_id, markup_node_id, markup_node_pool);
            }
        }
        MarkupNode::Text {
            content: _,
            ast_node_id: _,
            syn_high_style: _,
            attributes: _,
            parent_id_opt,
        } => *parent_id_opt = Some(parent_node_id),
        MarkupNode::Blank {
            ast_node_id: _,
            attributes: _,
            syn_high_style: _,
            parent_id_opt,
        } => *parent_id_opt = Some(parent_node_id),
    }
}

pub fn set_caret_at_start(markup_node_id: SlowNodeId, markup_node_pool: &mut SlowPool) {
    let markup_node = markup_node_pool.get_mut(markup_node_id);

    match markup_node {
        MarkupNode::Nested {
            ast_node_id: _,
            children_ids,
            parent_id_opt: _,
        } => {
            if let Some(child_id) = children_ids.first() {
                set_caret_at_start(*child_id, markup_node_pool);
            }
        }
        MarkupNode::Text {
            content: _,
            ast_node_id: _,
            syn_high_style: _,
            attributes,
            parent_id_opt: _,
        } => attributes.push(Attribute::Caret { offset_col: 0 }),
        MarkupNode::Blank {
            ast_node_id: _,
            attributes,
            syn_high_style: _,
            parent_id_opt: _,
        } => attributes.push(Attribute::Caret { offset_col: 0 }),
    };
}
