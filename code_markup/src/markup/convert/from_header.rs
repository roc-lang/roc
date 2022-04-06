use roc_ast::lang::core::{ast::ASTNodeId, header::AppHeader};

use crate::{
    markup::{
        attribute::Attributes,
        common_nodes::{
            new_comma_mn, new_left_accolade_mn, new_left_square_mn, new_right_accolade_mn,
            new_right_square_mn,
        },
        mark_id_ast_id_map::MarkIdAstIdMap,
        nodes::{set_parent_for_all, MarkupNode},
    },
    slow_pool::{MarkNodeId, SlowPool},
    syntax_highlight::HighlightStyle,
};

use super::from_def2::add_node;

pub fn header_to_markup(
    app_header: &AppHeader,
    mark_node_pool: &mut SlowPool,
    mark_id_ast_id_map: &mut MarkIdAstIdMap,
) -> MarkNodeId {
    let expr_id = app_header.ast_node_id;
    let ast_node_id = ASTNodeId::AExprId(expr_id);

    let app_node_id = header_mn(
        "app ".to_owned(),
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let app_name_node_id = header_val_mn(
        app_header.app_name.clone(),
        ast_node_id,
        HighlightStyle::String,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let full_app_node = MarkupNode::Nested {
        children_ids: vec![app_node_id, app_name_node_id],
        parent_id_opt: None,
        newlines_at_end: 1,
    };

    let packages_node_id = header_mn(
        "    packages ".to_owned(),
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let pack_left_acc_node_id = add_node(
        new_left_accolade_mn(),
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let pack_base_node_id = header_val_mn(
        "base: ".to_owned(),
        ast_node_id,
        HighlightStyle::RecordField,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let pack_val_node_id = header_val_mn(
        app_header.packages_base.clone(),
        ast_node_id,
        HighlightStyle::String,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let pack_right_acc_node_id = add_node(
        new_right_accolade_mn(),
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let full_packages_node = MarkupNode::Nested {
        children_ids: vec![
            packages_node_id,
            pack_left_acc_node_id,
            pack_base_node_id,
            pack_val_node_id,
            pack_right_acc_node_id,
        ],
        parent_id_opt: None,
        newlines_at_end: 1,
    };

    let imports_node_id = header_mn(
        "    imports ".to_owned(),
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let imports_left_square_node_id = add_node(
        new_left_square_mn(),
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let mut import_child_ids: Vec<MarkNodeId> = add_header_mn_list(
        &app_header.imports,
        ast_node_id,
        HighlightStyle::Import,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let imports_right_square_node_id = add_node(
        new_right_square_mn(),
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let mut full_import_children = vec![imports_node_id, imports_left_square_node_id];

    full_import_children.append(&mut import_child_ids);
    full_import_children.push(imports_right_square_node_id);

    let full_import_node = MarkupNode::Nested {
        children_ids: full_import_children,
        parent_id_opt: None,
        newlines_at_end: 1,
    };

    let provides_node_id = header_mn(
        "    provides ".to_owned(),
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let provides_left_square_node_id = add_node(
        new_left_square_mn(),
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let mut provides_val_node_ids: Vec<MarkNodeId> = add_header_mn_list(
        &app_header.provides,
        ast_node_id,
        HighlightStyle::Provides,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let provides_right_square_node_id = add_node(
        new_right_square_mn(),
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let provides_end_node_id = header_mn(
        " to base".to_owned(),
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let mut full_provides_children = vec![provides_node_id, provides_left_square_node_id];

    full_provides_children.append(&mut provides_val_node_ids);
    full_provides_children.push(provides_right_square_node_id);
    full_provides_children.push(provides_end_node_id);

    let full_provides_node = MarkupNode::Nested {
        children_ids: full_provides_children,
        parent_id_opt: None,
        newlines_at_end: 1,
    };

    let full_app_node_id = add_node(
        full_app_node,
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );
    let full_packages_node = add_node(
        full_packages_node,
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );
    let full_import_node_id = add_node(
        full_import_node,
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );
    let full_provides_node_id = add_node(
        full_provides_node,
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    let header_mark_node = MarkupNode::Nested {
        children_ids: vec![
            full_app_node_id,
            full_packages_node,
            full_import_node_id,
            full_provides_node_id,
        ],
        parent_id_opt: None,
        newlines_at_end: 1,
    };

    let header_mn_id = add_node(
        header_mark_node,
        ast_node_id,
        mark_node_pool,
        mark_id_ast_id_map,
    );

    set_parent_for_all(header_mn_id, mark_node_pool);

    header_mn_id
}

// Used for provides and imports
fn add_header_mn_list(
    str_vec: &[String],
    ast_node_id: ASTNodeId,
    highlight_style: HighlightStyle,
    mark_node_pool: &mut SlowPool,
    mark_id_ast_id_map: &mut MarkIdAstIdMap,
) -> Vec<MarkNodeId> {
    let nr_of_elts = str_vec.len();

    str_vec
        .iter()
        .enumerate()
        .flat_map(|(indx, provide_str)| {
            let provide_str = header_val_mn(
                provide_str.to_owned(),
                ast_node_id,
                highlight_style,
                mark_node_pool,
                mark_id_ast_id_map,
            );

            if indx != nr_of_elts - 1 {
                vec![
                    provide_str,
                    add_node(
                        new_comma_mn(),
                        ast_node_id,
                        mark_node_pool,
                        mark_id_ast_id_map,
                    ),
                ]
            } else {
                vec![provide_str]
            }
        })
        .collect()
}

fn header_mn(
    content: String,
    ast_node_id: ASTNodeId,
    mark_node_pool: &mut SlowPool,
    mark_id_ast_id_map: &mut MarkIdAstIdMap,
) -> MarkNodeId {
    let mark_node = MarkupNode::Text {
        content,
        syn_high_style: HighlightStyle::PackageRelated,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    };

    add_node(mark_node, ast_node_id, mark_node_pool, mark_id_ast_id_map)
}

fn header_val_mn(
    content: String,
    ast_node_id: ASTNodeId,
    highlight_style: HighlightStyle,
    mark_node_pool: &mut SlowPool,
    mark_id_ast_id_map: &mut MarkIdAstIdMap,
) -> MarkNodeId {
    let mark_node = MarkupNode::Text {
        content,
        syn_high_style: highlight_style,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    };

    add_node(mark_node, ast_node_id, mark_node_pool, mark_id_ast_id_map)
}
