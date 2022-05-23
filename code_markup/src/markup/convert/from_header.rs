use roc_ast::lang::core::{ast::ASTNodeId, header::AppHeader};
use roc_load_internal::file::ModuleHeader;
use roc_parse::header::{HeaderFor, ModuleNameEnum, PackageName};

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

use super::from_def::add_node;

pub fn header_to_markup<'a>(
    header: &ModuleHeader<'a>,
    module_names: MutMap<ModuleId, PQModuleName<'a>>,
    mark_node_pool: &mut SlowPool,
    mark_id_ast_id_map: &mut MarkIdAstIdMap,
) -> MarkNodeId {

    let header_mark_node = match header.module_name {
        ModuleNameEnum::App(str_literal) => {
            // ex: app "quicksort"
            let app_node_id = header_mn(
                "app ".to_owned(),
                mark_node_pool,
            );
        
            let app_name_node_id = header_val_mn(
                format!("{}", str_literal),
                HighlightStyle::String,
                mark_node_pool,
            );

            let full_app_node = MarkupNode::Nested {
                children_ids: vec![app_node_id, app_name_node_id],
                parent_id_opt: None,
                newlines_at_end: 1,
            };
        
            // ex: packages { pf: "./platform" }
            let mut package_node_ids = vec![];

            let packages_node_id = header_mn(
                "    packages ".to_owned(),
                mark_node_pool,
            );

            package_node_ids.push(packages_node_id);
        
            let pack_left_acc_node_id = add_node(
                new_left_accolade_mn(),
                mark_node_pool,
            );

            package_node_ids.push(pack_left_acc_node_id);

            for (i, (key, &PackageName(full_pkg_str))) in header.packages.iter().enumerate() {
                let pack_base_node_id = header_val_mn(
                    format!("{}: ", key),
                    HighlightStyle::RecordField,
                    mark_node_pool,
                );

                let pack_val_node_id = header_val_mn(
                    full_pkg_str.to_owned(),
                    HighlightStyle::String,
                    mark_node_pool,
                );

                package_node_ids.push(pack_base_node_id);
                package_node_ids.push(pack_val_node_id);

                if i < header.packages.len() - 1 {
                    package_node_ids.push(
                        add_node(new_comma_mn(), mark_node_pool)
                    );
                }
            }
        
            let pack_right_acc_node_id = add_node(
                new_right_accolade_mn(),
                mark_node_pool,
            );

            package_node_ids.push(pack_right_acc_node_id);
        
            let full_packages_node = MarkupNode::Nested {
                children_ids: package_node_ids,
                parent_id_opt: None,
                newlines_at_end: 1,
            };
        
            let imports_node_id = header_mn(
                "    imports ".to_owned(),
                mark_node_pool,
            );
        
            let imports_left_square_node_id = add_node(
                new_left_square_mn(),
                mark_node_pool,
            );

            let import_list: Vec<String> = 
                header.imported_modules.iter().map(|(mod_id, _)| {
                    format!("{:?}", module_names.get(key).unwrap().as_inner())
                }).collect();
        
            let mut import_child_ids: Vec<MarkNodeId> = add_header_mn_list(
                &import_list,
                HighlightStyle::Import,
                mark_node_pool,
            );
        
            let imports_right_square_node_id = add_node(
                new_right_square_mn(),
                mark_node_pool,
            );
        
            let mut full_import_children = vec![imports_node_id, imports_left_square_node_id];
        
            full_import_children.append(&mut import_child_ids);
            full_import_children.push(imports_right_square_node_id);
        
            let full_import_node = MarkupNode::Nested {
                children_ids: full_import_children,
                parent_id_opt: None,
                newlines_at_end: 1,
            };
        
            // ex: provides [ mainForHost ]
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
        
            MarkupNode::Nested {
                children_ids: vec![
                    full_app_node_id,
                    full_packages_node,
                    full_import_node_id,
                    full_provides_node_id,
                ],
                parent_id_opt: None,
                newlines_at_end: 1,
            };
        }
        _ => {}
    }

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
    highlight_style: HighlightStyle,
    mark_node_pool: &mut SlowPool,
) -> Vec<MarkNodeId> {
    let nr_of_elts = str_vec.len();

    str_vec
        .iter()
        .enumerate()
        .flat_map(|(indx, provide_str)| {
            let provide_str = header_val_mn(
                provide_str.to_owned(),
                highlight_style,
                mark_node_pool,
            );

            if indx != nr_of_elts - 1 {
                vec![
                    provide_str,
                    add_node(
                        new_comma_mn(),
                        mark_node_pool,
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
    mark_node_pool: &mut SlowPool,
) -> MarkNodeId {
    let mark_node = MarkupNode::Text {
        content,
        syn_high_style: HighlightStyle::PackageRelated,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    };

    mark_node_pool.add(mark_node)
}

fn header_val_mn(
    content: String,
    highlight_style: HighlightStyle,
    mark_node_pool: &mut SlowPool,
) -> MarkNodeId {
    let mark_node = MarkupNode::Text {
        content,
        syn_high_style: highlight_style,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    };

    mark_node_pool.add(mark_node)
}
