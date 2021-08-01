#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
mod insert_doc_links {
    use roc_can::env::Env;
    use roc_can::scope::Scope;
    use roc_collections::all::MutMap;
    use roc_docs::insert_doc_links;
    use roc_module::symbol::{IdentIds, Interns, ModuleIds};
    use roc_types::subs::VarStore;

    #[test]
    fn no_doc_links() {
        let home = ModuleIds::default().get_or_insert(&"Test".into());

        let module_ids = ModuleIds::default();

        let dep_idents = IdentIds::exposed_builtins(0);

        let env = Env::new(home, &dep_idents, &module_ids, IdentIds::default());

        let all_ident_ids = MutMap::default();

        let interns = Interns {
            module_ids: env.module_ids.clone(),
            all_ident_ids,
        };

        let var_store = &mut VarStore::default();
        let scope = &mut Scope::new(home, var_store);

        let markdown = r#"
            # Hello
            Hello thanks for using my package
        "#;

        assert_eq!(
            markdown,
            insert_doc_links(scope, &interns, markdown.to_string()),
        );
    }
}
