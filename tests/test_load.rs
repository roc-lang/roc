#[macro_use]
extern crate pretty_assertions;
// #[macro_use]
// extern crate indoc;

extern crate bumpalo;
extern crate inkwell;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_load {
    use bumpalo::Bump;
    use helpers::{fixtures_dir, im_map_from_pairs, mut_map_from_pairs};
    use roc::can::symbol::Symbol;
    use roc::ident::UnqualifiedIdent;
    use roc::load::LoadedHeader::*;
    use roc::load::{load, LoadedHeader};
    use roc::module::ModuleName;
    use roc::region::Region;

    #[test]
    fn interface_with_deps() {
        let src_dir = fixtures_dir().join("interface_with_deps");
        let filename = src_dir.join("Primary.roc");
        let arena = Bump::new();
        let loaded = load(&arena, &src_dir, &filename);

        assert!(loaded.problems.is_empty());

        let dep1_scope = im_map_from_pairs(vec![(
            UnqualifiedIdent::new("foo"),
            (Symbol::new("Dep3.Blah.", "foo"), Region::new(2, 2, 26, 29)),
        )]);
        let dep2_scope = im_map_from_pairs(vec![
            (
                UnqualifiedIdent::new("bar"),
                (Symbol::new("Dep3.Blah.", "bar"), Region::new(2, 2, 31, 34)),
            ),
            (
                UnqualifiedIdent::new("foo"),
                (Symbol::new("Dep3.Blah.", "foo"), Region::new(2, 2, 26, 29)),
            ),
        ]);
        let dep3_scope = im_map_from_pairs(vec![]);

        assert_eq!(
            loaded.dependent_headers,
            mut_map_from_pairs(vec![
                (ModuleName::new("Dep1"), Valid { scope: dep1_scope }),
                (ModuleName::new("Dep3.Blah"), Valid { scope: dep3_scope }),
                (ModuleName::new("Dep2"), Valid { scope: dep2_scope }),
            ])
        );

        assert_eq!(loaded.defs.len(), 4);

        let defs = loaded
            .defs
            .get(&ModuleName::new("Primary"))
            .expect("No defs found for `Primary` module")
            .clone()
            .expect("Defs failed to parse for `Primary` module");

        assert_eq!(
            dbg!(/* problem: module_defs() only parses 1 module - TODO add parsing unit test for it!*/ defs)
                .len(),
            6
        );

        match loaded.requested_header {
            LoadedHeader::Valid { scope } => assert_eq!(
                scope,
                im_map_from_pairs(vec![
                    (
                        UnqualifiedIdent::new("bar"),
                        (Symbol::new("Dep3.Blah.", "bar"), Region::new(2, 2, 51, 54)),
                    ),
                    (
                        UnqualifiedIdent::new("foo"),
                        (Symbol::new("Dep2.", "foo"), Region::new(2, 2, 32, 35)),
                    ),
                    (
                        UnqualifiedIdent::new("two"),
                        (Symbol::new("Dep2.", "two"), Region::new(2, 2, 27, 30)),
                    ),
                ])
            ),

            other => panic!(
                "app_header should have been Valid, but instead was: {:?}",
                other
            ),
        };
    }
}
