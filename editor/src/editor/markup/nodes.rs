use super::attribute::Attributes;
use crate::editor::ed_error::GetContentOnNestedNode;
use crate::editor::ed_error::NodeWithoutAttributes;
use crate::editor::ed_error::{NestedNodeMissingChild, NestedNodeWithoutChildren};
use crate::editor::{
    ed_error::EdResult,
    slow_pool::{SlowNodeId, SlowPool},
    syntax_highlight::HighlightStyle,
};
use crate::lang::{
    ast::Expr2,
    expr::Env,
    pool::{NodeId, PoolStr},
};
use bumpalo::Bump;
use snafu::OptionExt;

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

pub const BLANK_PLACEHOLDER: &str = " ";

impl MarkupNode {
    pub fn get_parent_id(&self) -> Option<SlowNodeId> {
        match self {
            MarkupNode::Nested {
                ast_node_id: _,
                children_ids: _,
                parent_id_opt,
            } => *parent_id_opt,
            MarkupNode::Text {
                content: _,
                ast_node_id: _,
                syn_high_style: _,
                attributes: _,
                parent_id_opt,
            } => *parent_id_opt,
            MarkupNode::Blank {
                ast_node_id: _,
                attributes: _,
                syn_high_style: _,
                parent_id_opt,
            } => *parent_id_opt,
        }
    }

    pub fn get_children_ids(&self) -> Vec<SlowNodeId> {
        match self {
            MarkupNode::Nested {
                ast_node_id: _,
                children_ids,
                parent_id_opt: _,
            } => children_ids.to_vec(),
            MarkupNode::Text {
                content: _,
                ast_node_id: _,
                syn_high_style: _,
                attributes: _,
                parent_id_opt: _,
            } => unreachable!(), //TODO use result
            MarkupNode::Blank {
                ast_node_id: _,
                attributes: _,
                syn_high_style: _,
                parent_id_opt: _,
            } => unreachable!(),
        }
    }

    // can't be &str, this creates borrowing issues
    pub fn get_content(&self) -> EdResult<String> {
        match self {
            MarkupNode::Nested {
                ast_node_id: _,
                children_ids: _,
                parent_id_opt: _,
            } => GetContentOnNestedNode {}.fail(),
            MarkupNode::Text {
                content,
                ast_node_id: _,
                syn_high_style: _,
                attributes: _,
                parent_id_opt: _,
            } => Ok(content.clone()),
            MarkupNode::Blank {
                ast_node_id: _,
                attributes: _,
                syn_high_style: _,
                parent_id_opt: _,
            } => Ok(BLANK_PLACEHOLDER.to_owned()),
        }
    }

    // Goes up to the parent and if it has a child after the current one, that child will be returned.
    // If the child is a nested node we return the left most child of a possible chain of nested nodes.
    pub fn get_next_leaf(
        &self,
        curr_child_id: SlowNodeId,
        markup_node_pool: &SlowPool,
    ) -> EdResult<Option<SlowNodeId>> {
        let parent_id_opt = self.get_parent_id();

        if let Some(parent_id) = parent_id_opt {
            let parent = markup_node_pool.get(parent_id);
            let children_ids = parent.get_children_ids();
            let nr_of_children = children_ids.len();

            for (indx, child_id) in children_ids.iter().enumerate() {
                if *child_id == curr_child_id {
                    if indx + 1 < nr_of_children {
                        if let Some(next_child_id) = children_ids.get(indx + 1) {
                            return Ok(Some(MarkupNode::descend_to_left_leaf(
                                *next_child_id,
                                markup_node_pool,
                            )?));
                        } else {
                            return Ok(None);
                        }
                    } else {
                        return Ok(None);
                    }
                }
            }

            NestedNodeMissingChild {
                node_id: parent_id,
                children_ids,
            }
            .fail()
        } else {
            Ok(None)
        }
    }

    pub fn descend_to_left_leaf(
        node_id: SlowNodeId,
        markup_node_pool: &SlowPool,
    ) -> EdResult<SlowNodeId> {
        let node = markup_node_pool.get(node_id);

        match node {
            MarkupNode::Nested {
                ast_node_id: _,
                children_ids,
                parent_id_opt: _,
            } => {
                let first_child_id = children_ids
                    .first()
                    .with_context(|| NestedNodeWithoutChildren { node_id })?;

                MarkupNode::descend_to_left_leaf(*first_child_id, markup_node_pool)
            }
            MarkupNode::Text { .. } => Ok(node_id),
            MarkupNode::Blank { .. } => Ok(node_id),
        }
    }

    pub fn get_mut_attributes(&mut self) -> EdResult<&mut Attributes> {
        let attrs_ref = match self {
            MarkupNode::Nested { .. } => None,
            MarkupNode::Text {
                content: _,
                ast_node_id: _,
                syn_high_style: _,
                attributes,
                parent_id_opt: _,
            } => Some(attributes),
            MarkupNode::Blank {
                ast_node_id: _,
                attributes,
                syn_high_style: _,
                parent_id_opt: _,
            } => Some(attributes),
        };

        attrs_ref.with_context(|| NodeWithoutAttributes {})
    }

    pub fn get_attributes(&self) -> EdResult<&Attributes> {
        let attrs_ref = match self {
            MarkupNode::Nested { .. } => None,
            MarkupNode::Text {
                content: _,
                ast_node_id: _,
                syn_high_style: _,
                attributes,
                parent_id_opt: _,
            } => Some(attributes),
            MarkupNode::Blank {
                ast_node_id: _,
                attributes,
                syn_high_style: _,
                parent_id_opt: _,
            } => Some(attributes),
        };

        attrs_ref.with_context(|| NodeWithoutAttributes {})
    }
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
