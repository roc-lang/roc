use super::attribute::Attributes;
use crate::editor::ed_error::EdResult;
use crate::editor::ed_error::ExpectedTextNode;
use crate::editor::ed_error::{NestedNodeMissingChild, NestedNodeRequired};
use crate::editor::markup::common_nodes::new_blank_mn;
use crate::editor::markup::common_nodes::new_colon_mn;
use crate::editor::markup::common_nodes::new_comma_mn;
use crate::editor::markup::common_nodes::new_equals_mn;
use crate::editor::markup::common_nodes::new_left_accolade_mn;
use crate::editor::markup::common_nodes::new_left_square_mn;
use crate::editor::markup::common_nodes::new_right_accolade_mn;
use crate::editor::markup::common_nodes::new_right_square_mn;
use crate::editor::slow_pool::MarkNodeId;
use crate::editor::slow_pool::SlowPool;
use crate::editor::syntax_highlight::HighlightStyle;
use crate::editor::util::index_of;
use crate::lang::ast::ExprId;
use crate::lang::ast::RecordField;
use crate::lang::ast::ValueDef;
use crate::lang::parse::AppHeader;
use crate::lang::pattern::get_identifier_string;
use crate::lang::{
    ast::Expr2,
    expr::Env,
    pool::{NodeId, PoolStr},
};
use crate::ui::util::slice_get;
use bumpalo::Bump;
use roc_module::symbol::Interns;
use std::fmt;

#[derive(Debug)]
pub enum MarkupNode {
    Nested {
        ast_node_id: NodeId<Expr2>,
        children_ids: Vec<MarkNodeId>,
        parent_id_opt: Option<MarkNodeId>,
    },
    Text {
        content: String,
        ast_node_id: NodeId<Expr2>,
        syn_high_style: HighlightStyle,
        attributes: Attributes,
        parent_id_opt: Option<MarkNodeId>,
    },
    Blank {
        ast_node_id: NodeId<Expr2>,
        attributes: Attributes,
        syn_high_style: HighlightStyle, // TODO remove HighlightStyle, this is always HighlightStyle::Blank
        parent_id_opt: Option<MarkNodeId>,
    },
}

impl MarkupNode {
    pub fn get_ast_node_id(&self) -> NodeId<Expr2> {
        match self {
            MarkupNode::Nested { ast_node_id, .. } => *ast_node_id,
            MarkupNode::Text { ast_node_id, .. } => *ast_node_id,
            MarkupNode::Blank { ast_node_id, .. } => *ast_node_id,
        }
    }

    pub fn get_parent_id_opt(&self) -> Option<MarkNodeId> {
        match self {
            MarkupNode::Nested { parent_id_opt, .. } => *parent_id_opt,
            MarkupNode::Text { parent_id_opt, .. } => *parent_id_opt,
            MarkupNode::Blank { parent_id_opt, .. } => *parent_id_opt,
        }
    }

    pub fn get_children_ids(&self) -> Vec<MarkNodeId> {
        match self {
            MarkupNode::Nested { children_ids, .. } => children_ids.to_vec(),
            MarkupNode::Text { .. } => vec![],
            MarkupNode::Blank { .. } => vec![],
        }
    }

    pub fn get_sibling_ids(&self, markup_node_pool: &SlowPool) -> Vec<MarkNodeId> {
        if let Some(parent_id) = self.get_parent_id_opt() {
            let parent_node = markup_node_pool.get(parent_id);

            parent_node.get_children_ids()
        } else {
            vec![]
        }
    }

    // return (index of child in list of children, closest ast index of child corresponding to ast node)
    pub fn get_child_indices(
        &self,
        child_id: MarkNodeId,
        markup_node_pool: &SlowPool,
    ) -> EdResult<(usize, usize)> {
        match self {
            MarkupNode::Nested { children_ids, .. } => {
                let mark_position_opt = children_ids.iter().position(|&c_id| c_id == child_id);

                if let Some(child_index) = mark_position_opt {
                    let self_ast_id = self.get_ast_node_id();

                    let child_ids_with_ast = children_ids
                        .iter()
                        .filter(|c_id| {
                            let child_mark_node = markup_node_pool.get(**c_id);
                            // a node that points to the same ast_node as the parent is a ',', '[', ']'
                            // those are not "real" ast children
                            child_mark_node.get_ast_node_id() != self_ast_id
                        })
                        .copied()
                        .collect::<Vec<MarkNodeId>>();

                    if child_index == (children_ids.len() - 1) {
                        let ast_child_index = child_ids_with_ast.len();

                        Ok((child_index, ast_child_index))
                    } else {
                        // we want to find the index of the closest ast mark node to child_index
                        let indices_in_mark_res: EdResult<Vec<usize>> = child_ids_with_ast
                            .iter()
                            .map(|c_id| index_of(*c_id, children_ids))
                            .collect();

                        let indices_in_mark = indices_in_mark_res?;

                        let mut last_diff = usize::MAX;
                        let mut best_index = 0;

                        for index in indices_in_mark.iter() {
                            let curr_diff =
                                isize::abs((*index as isize) - (child_index as isize)) as usize;

                            if curr_diff >= last_diff {
                                break;
                            } else {
                                last_diff = curr_diff;
                                best_index = *index;
                            }
                        }

                        let closest_ast_child = slice_get(best_index, &children_ids)?;

                        let closest_ast_child_index =
                            index_of(*closest_ast_child, &child_ids_with_ast)?;

                        // +1 because we want to insert after ast_child
                        Ok((child_index, closest_ast_child_index + 1))
                    }
                } else {
                    NestedNodeMissingChild {
                        node_id: child_id,
                        children_ids: children_ids.clone(),
                    }
                    .fail()
                }
            }
            _ => NestedNodeRequired {
                node_type: self.node_type_as_string(),
            }
            .fail(),
        }
    }

    pub fn get_content(&self) -> String {
        match self {
            MarkupNode::Nested { .. } => "".to_owned(),
            MarkupNode::Text { content, .. } => content.clone(),
            MarkupNode::Blank { .. } => BLANK_PLACEHOLDER.to_owned(),
        }
    }

    pub fn get_content_mut(&mut self) -> EdResult<&mut String> {
        match self {
            MarkupNode::Nested { .. } => ExpectedTextNode {
                function_name: "set_content".to_owned(),
                node_type: self.node_type_as_string(),
            }
            .fail(),
            MarkupNode::Text { content, .. } => Ok(content),
            MarkupNode::Blank { .. } => ExpectedTextNode {
                function_name: "set_content".to_owned(),
                node_type: self.node_type_as_string(),
            }
            .fail(),
        }
    }

    pub fn is_all_alphanumeric(&self) -> bool {
        self
            .get_content()
            .chars()
            .all(|chr| chr.is_ascii_alphanumeric())
    }

    pub fn add_child_at_index(&mut self, index: usize, child_id: MarkNodeId) -> EdResult<()> {
        if let MarkupNode::Nested { children_ids, .. } = self {
            children_ids.splice(index..index, vec![child_id]);
        } else {
            NestedNodeRequired {
                node_type: self.node_type_as_string(),
            }
            .fail()?;
        }

        Ok(())
    }

    pub fn node_type_as_string(&self) -> String {
        let type_str = match self {
            MarkupNode::Nested { .. } => "Nested",
            MarkupNode::Text { .. } => "Text",
            MarkupNode::Blank { .. } => "Blank",
        };

        type_str.to_owned()
    }

    pub fn is_blank(&self) -> bool {
        matches!(self, MarkupNode::Blank { .. })
    }

    pub fn is_nested(&self) -> bool {
        matches!(self, MarkupNode::Nested { .. })
    }
}

fn get_string<'a>(env: &Env<'a>, pool_str: &PoolStr) -> String {
    pool_str.as_str(env.pool).to_owned()
}

pub const BLANK_PLACEHOLDER: &str = " ";
pub const LEFT_ACCOLADE: &str = "{ ";
pub const RIGHT_ACCOLADE: &str = " }";
pub const LEFT_SQUARE_BR: &str = "[ ";
pub const RIGHT_SQUARE_BR: &str = " ]";
pub const COLON: &str = ": ";
pub const COMMA: &str = ", ";
pub const STRING_QUOTES: &str = "\"\"";
pub const EQUALS: &str = " = ";

fn new_markup_node(
    text: String,
    node_id: NodeId<Expr2>,
    highlight_style: HighlightStyle,
    markup_node_pool: &mut SlowPool,
) -> MarkNodeId {
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
    expr2_node_id: NodeId<Expr2>,
    markup_node_pool: &mut SlowPool,
    interns: &Interns,
) -> EdResult<MarkNodeId> {

    let mark_node_id = match expr2 {
        Expr2::SmallInt { text, .. }
        | Expr2::I128 { text, .. }
        | Expr2::U128 { text, .. }
        | Expr2::Float { text, .. } => {
            let num_str = get_string(env, &text);

            new_markup_node(
                num_str,
                expr2_node_id,
                HighlightStyle::Number,
                markup_node_pool,
            )
        }
        Expr2::Str(text) => new_markup_node(
            "\"".to_owned() + text.as_str(env.pool) + "\"",
            expr2_node_id,
            HighlightStyle::String,
            markup_node_pool,
        ),
        Expr2::GlobalTag { name, .. } => new_markup_node(
            get_string(env, &name),
            expr2_node_id,
            HighlightStyle::Type,
            markup_node_pool,
        ),
        Expr2::Call { expr: expr_id, .. } => {
            let expr = env.pool.get(*expr_id);
            expr2_to_markup(arena, env, expr, *expr_id, markup_node_pool, interns)?
        }
        Expr2::Var(symbol) => {
            //TODO make bump_format with arena
            let text = format!("{:?}", symbol);
            new_markup_node(
                text,
                expr2_node_id,
                HighlightStyle::Variable,
                markup_node_pool,
            )
        }
        Expr2::List { elems, .. } => {
            let mut children_ids = vec![
                    markup_node_pool.add(
                        new_left_square_mn(
                            expr2_node_id,
                            None
                        )
                    )
                ];

            let indexed_node_ids: Vec<(usize, ExprId)> =
                elems.iter(env.pool).copied().enumerate().collect();

            for (idx, node_id) in indexed_node_ids.iter() {
                let sub_expr2 = env.pool.get(*node_id);

                children_ids.push(expr2_to_markup(
                    arena,
                    env,
                    sub_expr2,
                    *node_id,
                    markup_node_pool,
                    interns
                )?);

                if idx + 1 < elems.len() {
                    children_ids.push(
                        markup_node_pool.add(
                        new_comma_mn(
                                expr2_node_id,
                                None
                            )
                        )
                    );
                }
            }
            children_ids.push(
                markup_node_pool.add(
                    new_right_square_mn(
                        expr2_node_id,
                        None
                    )
                )
            );

            let list_node = MarkupNode::Nested {
                ast_node_id: expr2_node_id,
                children_ids,
                parent_id_opt: None,
            };

            markup_node_pool.add(list_node)
        }
        Expr2::EmptyRecord => {
            let children_ids = vec![
                markup_node_pool.add(
                    new_left_accolade_mn(
                        expr2_node_id,
                        None
                    )
                ),
                markup_node_pool.add(
                    new_right_accolade_mn(
                        expr2_node_id,
                        None
                    )
                ),
            ];

            let record_node = MarkupNode::Nested {
                ast_node_id: expr2_node_id,
                children_ids,
                parent_id_opt: None,
            };

            markup_node_pool.add(record_node)
        }
        Expr2::Record { fields, .. } => {
            let mut children_ids = vec![markup_node_pool.add(
                new_left_accolade_mn(
                    expr2_node_id,
                    None
                )
            )];

            for (idx, field_node_id) in fields.iter_node_ids().enumerate() {
                let record_field = env.pool.get(field_node_id);

                let field_name = record_field.get_record_field_pool_str();

                children_ids.push(new_markup_node(
                    field_name.as_str(env.pool).to_owned(),
                    expr2_node_id,
                    HighlightStyle::RecordField,
                    markup_node_pool,
                ));

                match record_field {
                    RecordField::InvalidLabelOnly(_, _) => (),
                    RecordField::LabelOnly(_, _, _) => (),
                    RecordField::LabeledValue(_, _, sub_expr2_node_id) => {

                        children_ids.push(
                            markup_node_pool.add(
                                new_colon_mn(
                                    expr2_node_id,
                                    None
                                )
                            )
                        );

                        let sub_expr2 = env.pool.get(*sub_expr2_node_id);
                        children_ids.push(expr2_to_markup(
                            arena,
                            env,
                            sub_expr2,
                            *sub_expr2_node_id,
                            markup_node_pool,
                            interns
                        )?);
                    }
                }

                if idx + 1 < fields.len() {
                    children_ids.push(
                        markup_node_pool.add(
                            new_comma_mn(
                                expr2_node_id,
                                None
                            )
                        )
                    );
                }
            }

            children_ids.push(
                markup_node_pool.add(
                    new_right_accolade_mn(
                        expr2_node_id,
                        None
                    )
                )
            );

            let record_node = MarkupNode::Nested {
                ast_node_id: expr2_node_id,
                children_ids,
                parent_id_opt: None,
            };

            markup_node_pool.add(record_node)
        }
        Expr2::Blank => markup_node_pool.add(
        new_blank_mn(
                expr2_node_id,
                None
            )
        ),
        Expr2::LetValue { def_id, body_id:_, body_var:_} => {

            let pattern_id = 
                env.pool.get(*def_id).get_pattern_id();

            let pattern2 =
                env.pool.get(pattern_id);

            //TODO remove me
            /*dbg!(env.pool.get(expr_id));
            dbg!(env.pool.get(env.pool.get(*def_id).get_pattern_id()));
            dbg!(env.pool.get(*body_id));*/

            let val_name =
                get_identifier_string(pattern2, interns)?;

            let val_name_mn = MarkupNode::Text {
                content: val_name,
                ast_node_id: expr2_node_id,
                syn_high_style: HighlightStyle::Variable,
                attributes: Attributes::new(),
                parent_id_opt: None,
            };

            let val_name_mn_id = markup_node_pool.add(val_name_mn);

            let equals_mn_id = markup_node_pool.add(new_equals_mn(expr2_node_id, None));

            let value_def = env.pool.get(*def_id);

            match value_def {
                ValueDef::NoAnnotation {
                    pattern_id:_,
                    expr_id,
                    expr_var:_,
                } => {
                    let body_mn_id = expr2_to_markup(
                        arena,
                        env,
                        env.pool.get(*expr_id),
                        *expr_id,
                        markup_node_pool,
                        interns
                    )?;
        
                    let full_let_node = MarkupNode::Nested {
                        ast_node_id: expr2_node_id,
                        children_ids: vec![val_name_mn_id, equals_mn_id, body_mn_id],
                        parent_id_opt: None,
                    };
        
                    markup_node_pool.add(full_let_node)
                },
                other => {
                    unimplemented!("I don't know how to convert {:?} into a MarkupNode yet.", other)
                }
            }
        },
        Expr2::RuntimeError() => new_markup_node(
            "RunTimeError".to_string(),
            expr2_node_id,
            HighlightStyle::Blank,
            markup_node_pool,
        ),
        rest => todo!("implement expr2_to_markup for {:?}", rest),
    };

    Ok(mark_node_id)
}

pub fn set_parent_for_all(markup_node_id: MarkNodeId, markup_node_pool: &mut SlowPool) {
    let node = markup_node_pool.get(markup_node_id);

    if let MarkupNode::Nested {
        ast_node_id: _,
        children_ids,
        parent_id_opt: _,
    } = node
    {
        // need to clone because of borrowing issues
        let children_ids_clone = children_ids.clone();

        for child_id in children_ids_clone {
            set_parent_for_all_helper(child_id, markup_node_id, markup_node_pool);
        }
    }
}

pub fn set_parent_for_all_helper(
    markup_node_id: MarkNodeId,
    parent_node_id: MarkNodeId,
    markup_node_pool: &mut SlowPool,
) {
    let node = markup_node_pool.get_mut(markup_node_id);

    match node {
        MarkupNode::Nested {
            children_ids,
            parent_id_opt,
            ..
        } => {
            *parent_id_opt = Some(parent_node_id);

            // need to clone because of borrowing issues
            let children_ids_clone = children_ids.clone();

            for child_id in children_ids_clone {
                set_parent_for_all_helper(child_id, markup_node_id, markup_node_pool);
            }
        }
        MarkupNode::Text { parent_id_opt, .. } => *parent_id_opt = Some(parent_node_id),
        MarkupNode::Blank { parent_id_opt, .. } => *parent_id_opt = Some(parent_node_id),
    }
}

fn header_mn(content: String, ast_node_id: ExprId, mark_node_pool: &mut SlowPool) -> MarkNodeId {
    let mark_node = MarkupNode::Text {
        content: content,
        ast_node_id,
        syn_high_style: HighlightStyle::PackageRelated,
        attributes: Attributes::new(),
        parent_id_opt: None,
    };

    mark_node_pool.add(mark_node)
}

fn header_val_mn(content: String, ast_node_id: ExprId, highlight_style: HighlightStyle, mark_node_pool: &mut SlowPool) -> MarkNodeId {
    let mark_node = MarkupNode::Text {
        content: content,
        ast_node_id,
        syn_high_style: highlight_style,
        attributes: Attributes::new(),
        parent_id_opt: None,
    };

    mark_node_pool.add(mark_node)
}

pub fn header_to_markup(app_header: &AppHeader, mark_node_pool: &mut SlowPool) -> MarkNodeId {
    let ast_node_id = app_header.ast_node_id;

    let app_node_id =
        header_mn("app ".to_owned(), ast_node_id, mark_node_pool);

    let app_name_node_id =
        header_val_mn( 
            app_header.app_name,
            ast_node_id,
            HighlightStyle::String,
            mark_node_pool
        );

    let full_app_node = MarkupNode::Nested {
        ast_node_id,
        children_ids: vec![app_node_id, app_name_node_id],
        parent_id_opt: None,
    };

    let packages_node_id =
        header_mn("    packages ".to_owned(), ast_node_id, mark_node_pool);

    let pack_left_acc_node_id =  mark_node_pool.add(new_left_accolade_mn(ast_node_id, None));

    let pack_base_node_id =
        header_val_mn( 
            "base: ".to_owned(),
            ast_node_id,
            HighlightStyle::RecordField,
            mark_node_pool
        );

    let pack_val_node_id =
        header_val_mn( 
            app_header.packages_base,
            ast_node_id,
            HighlightStyle::String,
            mark_node_pool
        );

    let pack_right_acc_node_id = mark_node_pool.add(new_right_accolade_mn(ast_node_id, None));

    let full_packages_node = MarkupNode::Nested {
        ast_node_id,
        children_ids: vec![packages_node_id, pack_left_acc_node_id, pack_base_node_id, pack_val_node_id, pack_right_acc_node_id],
        parent_id_opt: None,
    };

    let imports_node_id = 
        header_mn("    imports ".to_owned(), ast_node_id, mark_node_pool);

    let imports_left_square_node_id =
        mark_node_pool.add(new_left_square_mn(ast_node_id, None));

    let nr_of_imports = app_header.imports.len();

    let import_child_ids =
        app_header.imports.iter().enumerate().map(|(indx, import)| {

            let import_val_mn_id = 
                header_val_mn( 
                    import.to_owned(),
                    ast_node_id,
                    HighlightStyle::Import,
                    mark_node_pool
                );

            if indx != nr_of_imports - 1 {
                vec![
                    import_val_mn_id,
                    mark_node_pool.add(new_comma_mn(ast_node_id, None))
                ]
            } else {
                vec![
                    import_val_mn_id
                ]
            }
        });

    let imports_right_square_node_id =
        mark_node_pool.add(new_right_square_mn(ast_node_id, None));
    
    let full_import_node = 
        MarkupNode::Nested {
            ast_node_id,
            children_ids: vec![imports_node_id, imports_left_square_node_id],
            parent_id_opt: None,
        };

    let provides_node_id =
        header_mn( 
            "    provides ".to_owned(),
            ast_node_id,
            mark_node_pool
        );

    let provides_left_square_node_id =
        mark_node_pool.add(new_left_square_mn(ast_node_id, None));

    let provides_val_node_id =
        header_val_mn( 
            // TODO iter over provides like with imports
            app_header.provides.first().unwrap().to_owned(),
            ast_node_id,
            HighlightStyle::Provides,
            mark_node_pool
        );

    let provides_right_square_node_id =
        mark_node_pool.add(new_right_square_mn(ast_node_id, None));

    let provides_end_node_id =
        header_mn( 
            " to base".to_owned(),
            ast_node_id,
            mark_node_pool
        );

    let full_provides_node =
        MarkupNode::Nested {
            ast_node_id,
            children_ids: vec![
                provides_node_id,
                provides_left_square_node_id,
                provides_val_node_id,
                provides_right_square_node_id,
                provides_end_node_id
            ],
            parent_id_opt: None,
        };

    let full_provides_node_id = mark_node_pool.add(full_provides_node);
    let full_import_node_id = mark_node_pool.add(full_import_node);
    let full_packages_node = mark_node_pool.add(full_packages_node);
    let full_app_node_id = mark_node_pool.add(full_app_node);
    
    let header_mark_node =
        MarkupNode::Nested {
            ast_node_id,
            children_ids: vec![
                full_app_node_id, full_packages_node, full_import_node_id, full_provides_node_id
            ],
            parent_id_opt: None,
        };

    let header_mn_id = mark_node_pool.add(header_mark_node);

    set_parent_for_all(header_mn_id, mark_node_pool);

    header_mn_id
}

pub fn AST_to_mark_nodes() -> MarkNodeId {
    //TODO call for header and Expr2's
}

impl fmt::Display for MarkupNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} ({})",
            self.node_type_as_string(),
            self.get_content()
        )
    }
}

pub fn tree_as_string(root_node_id: MarkNodeId, mark_node_pool: &SlowPool) -> String {
    let mut full_string = "\n\n(mark_node_tree)\n".to_owned();

    let node = mark_node_pool.get(root_node_id);

    full_string.push_str(&format!("{} mn_id {}\n", node, root_node_id));

    tree_as_string_helper(node, 1, &mut full_string, mark_node_pool);

    full_string
}

fn tree_as_string_helper(
    node: &MarkupNode,
    level: usize,
    tree_string: &mut String,
    mark_node_pool: &SlowPool,
) {
    for child_id in node.get_children_ids() {
        let mut full_str = std::iter::repeat("|--- ")
            .take(level)
            .collect::<Vec<&str>>()
            .join("")
            .to_owned();

        let child = mark_node_pool.get(child_id);

        full_str.push_str(&format!("{} mn_id {}\n", child, child_id));

        tree_string.push_str(&full_str);

        tree_as_string_helper(child, level + 1, tree_string, mark_node_pool);
    }
}
