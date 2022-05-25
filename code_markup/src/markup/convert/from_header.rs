use roc_ast::lang::core::{ast::ASTNodeId, header::AppHeader};
use roc_load_internal::file::ModuleHeader;
use roc_parse::{header::{HeaderFor, ModuleNameEnum, PackageName}, ident::UppercaseIdent};

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
        
            // ex: provides [ mainForHost ]
            let provides_node_id = header_mn(
                "    provides ".to_owned(),
                mark_node_pool,
            );
        
            let provides_strs =
                header.exposed_ident_ids.ident_strs.map(|(_, ident_str)| ident_str).collect::<Vec<_>>();

            let mut provides_list_ids: Vec<MarkNodeId> = add_header_mn_list(
                provides_strs,
                HighlightStyle::Provides,
                mark_node_pool,
            );

            let provides_to = 
                match header.header_for {
                    HeaderFor::App { to_platform } => {format!("{}", to_platform)},
                    other => { panic!("ModuleHeader.module_name indicates an App but the corresponding header_for is not an App, instead it is {:?}", other)}
                };
        
            let provides_end_node_id = header_mn(
                format!(" to {}", provides_to),
                mark_node_pool,
            );
        
            let mut full_provides_children = vec![provides_node_id];
        
            full_provides_children.append(&mut provides_list_ids);
            full_provides_children.push(provides_end_node_id);
        
            let full_provides_node = MarkupNode::Nested {
                children_ids: full_provides_children,
                parent_id_opt: None,
                newlines_at_end: 1,
            };
        
            let full_app_node_id = add_node(
                full_app_node,
                mark_node_pool,
            );
            let full_packages_node_id = gen_packages_node(header.packages);
            
            let import_list: Vec<String> = 
                header.imported_modules.iter().map(|(mod_id, _)| {
                    format!("{:?}", module_names.get(key).unwrap().as_inner())
                }).collect();
            let full_import_node_id = gen_list_node(
                "imports",
                import_list,
                mark_node_pool,
            );

            let full_provides_node_id = add_node(
                full_provides_node,
                mark_node_pool,
            );
        
            MarkupNode::Nested {
                children_ids: vec![
                    full_app_node_id,
                    full_packages_node_id,
                    full_import_node_id,
                    full_provides_node_id,
                ],
                parent_id_opt: None,
                newlines_at_end: 1,
            }
        },
        ModuleNameEnum::Interface(ModuleName(name_str)) => {
            // ex: interface WithBuiltins
            let iface_node_id = header_mn(
                "interface ".to_owned(),
                mark_node_pool,
            );
        
            let iface_name_node_id = header_val_mn(
                name_str,
                HighlightStyle::String,
                mark_node_pool,
            );

            let full_iface_node = MarkupNode::Nested {
                children_ids: vec![iface_node_id, iface_name_node_id],
                parent_id_opt: None,
                newlines_at_end: 1,
            };

            let full_iface_node_id = add_node(
                full_iface_node,
                mark_node_pool,
            );

            let exposes_list =
                header.exposed_ident_ids.ident_strs.map(|(_, ident_str)| ident_str).collect::<Vec<_>>();
            let full_exposes_node_id = gen_list_node(
                "exposes",
                exposes_list,
                mark_node_pool,
            );

            let import_list: Vec<String> = 
                header.imported_modules.iter().map(|(mod_id, _)| {
                    format!("{:?}", module_names.get(key).unwrap().as_inner())
                }).collect();
            let full_import_node_id = gen_list_node(
                "imports",
                import_list,
                mark_node_pool,
            );

            MarkupNode::Nested {
                children_ids: vec![
                    full_iface_node_id,
                    full_exposes_node_id,
                    full_import_node_id,
                ],
                parent_id_opt: None,
                newlines_at_end: 1,
            }
        }
        Hosted(ModuleName(name_str)) => {
            // ex: hosted Effect
            let hosted_node_id = header_mn(
                "hosted ".to_owned(),
                mark_node_pool,
            );
        
            let hosted_name_node_id = header_val_mn(
                name_str,
                HighlightStyle::String,
                mark_node_pool,
            );

            let full_hosted_node = MarkupNode::Nested {
                children_ids: vec![hosted_node_id, hosted_name_node_id],
                parent_id_opt: None,
                newlines_at_end: 1,
            };

            // ex: generates Effect with [after, map]
            let generates_node_id = header_mn(
                "    generates ".to_owned(),
                mark_node_pool,
            );

            let (generates_str, generates_with_strs) = 
                match header.header_for {
                    HeaderFor::Hosted { generates: UppercaseIdent(gen_str), generates_with } => {
                        (
                            gen_str,
                            generates_with.map(|loc_exposed_name| loc_exposed_name.value.as_str).collect()
                        )
                    },
                    other => { panic!("ModuleHeader.module_name indicates a Hosted module but the corresponding header_for is not a Hosted variant, instead it is {:?}", other)}
                };

            let generates_name_node_id = header_val_mn(
                generates_str,
                HighlightStyle::String,
                mark_node_pool,
            );

            let with_node_id = header_mn(
                " with ".to_owned(),
                mark_node_pool,
            );

            let mut generates_with_list_ids: Vec<MarkNodeId> = add_header_mn_list(
                &generates_with_strs,
                HighlightStyle::Import,
                mark_node_pool,
            );

            let mut full_generates_children = vec![generates_node_id, generates_name_node_id, with_node_id];
        
            full_import_children.append(&mut generates_with_list_ids);
        
            let full_generates_node = MarkupNode::Nested {
                children_ids: full_import_children,
                parent_id_opt: None,
                newlines_at_end: 1,
            };

            let full_hosted_node_id = add_node(
                full_hosted_node,
                mark_node_pool,
            );

            let exposes_list =
                header.exposed_ident_ids.ident_strs.map(|(_, ident_str)| ident_str).collect::<Vec<_>>();
            let full_exposes_node_id = gen_list_node(
                "exposes",
                exposes_list,
                mark_node_pool,
            );

            let import_list: Vec<String> = 
                header.imported_modules.iter().map(|(mod_id, _)| {
                    format!("{:?}", module_names.get(key).unwrap().as_inner())
                }).collect();
            let full_import_node_id = gen_list_node(
                "imports",
                import_list,
                mark_node_pool,
            );

            let full_generates_node_id = add_node(
                full_generates_node,
                mark_node_pool,
            );

            MarkupNode::Nested {
                children_ids: vec![
                    full_hosted_node_id,
                    full_exposes_node_id,
                    full_import_node_id,
                    full_generates_node_id,
                ],
                parent_id_opt: None,
                newlines_at_end: 1,
            }
        }
        PkgConfig => {
            // ex: platform quicksort
            let platform_node_id = header_mn(
                "platform ".to_owned(),
                mark_node_pool,
            );
        
            let platform_name_node_id = header_val_mn(
                "TODO",
                // TODO this platform string is parsed but discarded soon after, making it hard to pass on to the editor
                // this should be easier once it is used to specify the name of the roc package.
                HighlightStyle::String,
                mark_node_pool,
            );

            let full_platform_node = MarkupNode::Nested {
                children_ids: vec![platform_node_id, platform_name_node_id],
                parent_id_opt: None,
                newlines_at_end: 1,
            };

            let full_platform_node_id = add_node(
                full_platform_node,
                mark_node_pool,
            );

            // ex: requires {Model} { main : {} }
            let requires_node_id = header_mn(
                "    requires {TODO} {TODO}".to_owned(), // see TODO below (convert platfo...)
                mark_node_pool,
            );

            match header.header_for {
                HeaderFor::PkgConfig{
                    config_shorthand:_,
                    platform_main_type,
                    main_for_host:_,
                } => {
                    // TODO convert platform_main_type to string
                    // this string can be put in the second {} of requires
                    // for the first {} in requires you'll need
                    // PlatformHeaderInfo.requires_types to be passed on to the editor starting from file.rs
                },
                other => { panic!("ModuleHeader.module_name indicates a PkgConfig but the corresponding header_for is not a PkgConfig variant, instead it is {:?}", other)}
            };

            // exposes is parsed but currently unused in the compiler
            /*let exposes_list =
                header.exposed_ident_ids.ident_strs.map(|(_, ident_str)| ident_str).collect::<Vec<_>>();
            let full_exposes_node_id = gen_list_node(
                "exposes",
                exposes_list,
                mark_node_pool,
            );*/
            let exposes_node_id = header_mn(
                "    exposes []".to_owned(),
                mark_node_pool,
            );

            let full_packages_node_id = gen_packages_node(header.packages);

            let import_list: Vec<String> = 
                header.imported_modules.iter().map(|(mod_id, _)| {
                    format!("{:?}", module_names.get(key).unwrap().as_inner())
                }).collect();
            let full_import_node_id = gen_list_node(
                "imports",
                import_list,
                mark_node_pool,
            );

            let provides_list: Vec<String> = 
                header.exposed_ident_ids.ident_strs.map(|(_, ident_str)| ident_str).collect::<Vec<_>>();
            // the exposed_ident_ids here is not an error, values for provides are put here

            let full_provides_node_id = gen_list_node(
                "provides",
                provides_list,
                mark_node_pool,
            );

            MarkupNode::Nested {
                children_ids: vec![
                    full_platform_node_id,
                    full_requires_node_id,
                    full_exposes_node_id,
                    full_packages_node_id,
                    full_import_node_id,
                    full_provides_node_id,
                ],
                parent_id_opt: None,
                newlines_at_end: 1,
            }
        }
    };

    let header_mn_id = add_node(
        header_mark_node,
        mark_node_pool,
    );

    set_parent_for_all(header_mn_id, mark_node_pool);

    header_mn_id
}

// Used for provides, imports...
// assumes square brackets around the list
fn add_header_mn_list(
    str_vec: &[String],
    highlight_style: HighlightStyle,
    mark_node_pool: &mut SlowPool,
) -> Vec<MarkNodeId> {
    let all_node_ids = vec![
        add_node(
            new_left_square_mn(),
            mark_node_pool,
        )
    ];

    let nr_of_elts = str_vec.len();

    let elt_node_ids = str_vec
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
        .collect();

    all_node_ids.append(elt_node_ids);

    all_node_ids.push(
        add_node(
            new_right_square_mn(),
            mark_node_pool,
        )
    );

    all_node_ids
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

fn gen_list_node(
    list_name: &str, //imports, exposes, provides...
    list_items: &[&str],
    mark_node_pool: &mut SlowPool,
) -> MarkNodeId {
    let list_node_id = header_mn(
        format!("    {} "),
        mark_node_pool,
    );

    let mut list_ids: Vec<MarkNodeId> = add_header_mn_list(
        &list_items,
        HighlightStyle::Import,
        mark_node_pool,
    );

    let mut full_node_children = vec![imports_node_id];

    full_node_children.append(&mut list_ids);

    let full_node = MarkupNode::Nested {
        children_ids: full_import_children,
        parent_id_opt: None,
        newlines_at_end: 1,
    };

    mark_node_pool.add(full_node)
}

/// ex: packages { pf: "./platform" }
fn gen_packages_node(
    packages: MutMap<&'a str, PackageName<'a>>
) -> MarkNodeId {

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

  for (i, (key, &PackageName(full_pkg_str))) in packages.iter().enumerate() {
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

  mark_node_pool.add(full_packages_node)
}
