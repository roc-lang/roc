#![allow(dead_code)]

use super::syntax_highlight::HighlightStyle;
use crate::editor::ed_error::{CaretNotFound, EdResult};
use crate::editor::slow_pool::{SlowNodeId, SlowPool};
use crate::lang::{
    ast::Expr2,
    expr::Env,
    pool::{NodeId, PoolStr},
};
use bumpalo::Bump;
use snafu::ensure;

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
        attributes: Attributes,
        parent_id_opt: Option<SlowNodeId>,
    },
    Blank {
        ast_node_id: NodeId<Expr2>,
        attributes: Attributes,
        syn_high_style: HighlightStyle,
        parent_id_opt: Option<SlowNodeId>,
    },
}

#[derive(Debug)]
pub struct Caret {
    pub offset_col: usize,
}

impl Caret {
    pub fn new_attr(offset_col: usize) -> Attribute {
        Attribute::Caret {
            caret: Caret { offset_col },
        }
    }
}
#[derive(Debug)]
struct SelectionStart {
    offset_col: usize,
}
#[derive(Debug)]
struct SelectionEnd {
    offset_col: usize,
}

// Highlight is used for example when searching for a specific string to highlight all search results in the module
#[derive(Debug)]
struct HighlightStart {
    offset_col: usize,
}
#[derive(Debug)]
struct HighlightEnd {
    offset_col: usize,
}

// Underline is used for warnings and errors
#[derive(Debug)]
struct UnderlineStart {
    offset_col: usize,
}
#[derive(Debug)]
struct UnderlineEnd {
    offset_col: usize,
}

#[derive(Debug)]
pub enum Attribute {
    // Rust does not yet support types for enum variants so we have to do it like this
    Caret { caret: Caret },

    SelectionStart { selection_start: SelectionStart },
    SelectionEnd { selection_end: SelectionEnd },

    HighlightStart { highlight_start: HighlightStart },
    HighlightEnd { highlight_end: HighlightEnd },

    UnderlineStart { underline_start: UnderlineStart },
    UnderlineEnd { underline_end: UnderlineEnd },
}

#[derive(Debug)]
pub struct Attributes {
    pub all: Vec<Attribute>,
}

impl Attributes {
    pub fn new() -> Attributes {
        Attributes { all: Vec::new() }
    }

    pub fn add(&mut self, attr: Attribute) {
        self.all.push(attr);
    }

    pub fn get_carets(&self) -> Vec<&Caret> {
        let mut carets = Vec::new();

        for attr in self.all.iter() {
            if let Attribute::Caret { caret } = attr {
                carets.push(caret)
            }
        }

        carets
    }

    pub fn move_caret(&mut self, old_offset_col: usize, new_offset_col: usize) -> EdResult<()> {
        let mut caret_changed = false;

        for attr in self.all.iter_mut() {
            if let Attribute::Caret { ref mut caret } = attr {
                if caret.offset_col == old_offset_col {
                    caret.offset_col = new_offset_col;
                    caret_changed = true;
                    break;
                }
            }
        }

        ensure!(
            caret_changed,
            CaretNotFound {
                offset_col: old_offset_col,
                str_attrs: format!("{:?}", self)
            }
        );

        Ok(())
    }

    // TODO add move_carets function
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
        attributes: Attributes::new(),
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
            attributes: Attributes::new(),
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

// Returns id of node that has Caret attribute
pub fn set_caret_at_start(
    markup_node_id: SlowNodeId,
    markup_node_pool: &mut SlowPool,
) -> SlowNodeId {
    let markup_node = markup_node_pool.get_mut(markup_node_id);

    match markup_node {
        MarkupNode::Nested {
            ast_node_id: _,
            children_ids,
            parent_id_opt: _,
        } => {
            if let Some(child_id) = children_ids.first() {
                set_caret_at_start(*child_id, markup_node_pool)
            } else {
                //TODO use result instead
                unreachable!()
            }
        }
        MarkupNode::Text {
            content: _,
            ast_node_id: _,
            syn_high_style: _,
            attributes,
            parent_id_opt: _,
        } => {
            attributes.add(Caret::new_attr(0));
            markup_node_id
        }
        MarkupNode::Blank {
            ast_node_id: _,
            attributes,
            syn_high_style: _,
            parent_id_opt: _,
        } => {
            attributes.add(Caret::new_attr(0));
            markup_node_id
        }
    }
}

// returns node containing the caret after the move
pub fn move_carets_right(
    node_with_caret_id: SlowNodeId,
    markup_node_pool: &mut SlowPool,
) -> Vec<SlowNodeId> {
    let current_caret_node = markup_node_pool.get_mut(node_with_caret_id);

    match current_caret_node {
        MarkupNode::Nested { .. } => unreachable!(), // TODO use result instead
        MarkupNode::Text {
            content,
            ast_node_id: _,
            syn_high_style: _,
            attributes,
            parent_id_opt: _,
        } => {
            let carets = attributes.get_carets();
            for caret in carets {
                if caret.offset_col + 1 < content.len() {
                    // TODO return Result
                    attributes.move_caret(caret.offset_col, caret.offset_col + 1);
                } else {
                    // TODO move caret to next node
                }
            }
        }
        MarkupNode::Blank {
            ast_node_id: _,
            attributes,
            syn_high_style: _,
            parent_id_opt: _,
        } => {
            //TODO DRY
            let carets = attributes.get_carets();
            for caret in carets {
                if caret.offset_col < 1 {
                    // TODO return Result
                    attributes.move_caret(caret.offset_col, caret.offset_col + 1);
                } else {
                    // TODO move caret to next node
                }
            }
        }
    };

    //TODO return correct node
    vec![node_with_caret_id]
}
