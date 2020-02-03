#[macro_use]
extern crate maplit;
#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_usage_analysis {
    use crate::helpers::{can_expr, test_home, CanExprOut};
    use roc::can::ident::Lowercase;
    use roc::collections::{ImMap, ImSet};
    use roc::module::symbol::Interns;
    use roc::uniqueness::sharing::FieldAccess;
    use roc::uniqueness::sharing::ReferenceCount::{self, *};
    use roc::uniqueness::sharing::VarUsage;

    fn field_access_seq(
        accesses: Vec<Vec<&str>>,
        expected: std::collections::HashMap<&str, ReferenceCount>,
    ) {
        let mut state = FieldAccess::default();

        for access in accesses {
            let temp: Vec<Lowercase> = access.into_iter().map(|v| v.into()).collect();
            state.sequential(temp);
        }

        let mut im_expected: std::collections::HashMap<String, ReferenceCount> =
            std::collections::HashMap::default();

        for (k, v) in expected {
            im_expected.insert(k.into(), v);
        }

        let actual: std::collections::HashMap<String, ReferenceCount> = state.into();

        assert_eq!(actual, im_expected);
    }

    fn field_access_par(
        accesses: Vec<Vec<&str>>,
        expected: std::collections::HashMap<&str, ReferenceCount>,
    ) {
        let mut state = FieldAccess::default();

        for access in accesses {
            let temp: Vec<Lowercase> = access.into_iter().map(|v| v.into()).collect();
            state.parallel(temp);
        }

        let mut im_expected: std::collections::HashMap<String, ReferenceCount> =
            std::collections::HashMap::default();

        for (k, v) in expected {
            im_expected.insert(k.into(), v);
        }

        let actual: std::collections::HashMap<String, ReferenceCount> = state.into();

        assert_eq!(actual, im_expected);
    }

    #[test]
    fn usage_access_two_fields() {
        field_access_seq(
            vec![vec!["foo"], vec!["bar"]],
            hashmap![
                "foo" => Unique,
                "bar" => Unique,
            ],
        );

        field_access_par(
            vec![vec!["foo"], vec!["bar"]],
            hashmap![
                "foo" => Unique,
                "bar" => Unique,
            ],
        );
    }

    #[test]
    fn usage_access_repeated_field_seq() {
        field_access_seq(
            vec![vec!["foo"], vec!["foo"]],
            hashmap![
                "foo" => Shared,
            ],
        );
    }

    #[test]
    fn usage_access_repeated_field_par() {
        field_access_par(
            vec![vec!["foo"], vec!["foo"]],
            hashmap![
                "foo" => Unique,
            ],
        );
    }

    #[test]
    fn usage_access_nested_field_seq() {
        field_access_seq(
            vec![vec!["foo", "bar"], vec!["foo"]],
            hashmap![
                "foo" => Unique,
                "foo.bar" => Shared,
            ],
        );
        field_access_seq(
            vec![vec!["foo"], vec!["foo", "bar"]],
            hashmap![
                "foo" => Unique,
                "foo.bar" => Shared,
            ],
        );
    }
    #[test]
    fn usage_access_nested_field_par() {
        field_access_par(
            vec![vec!["foo", "bar"], vec!["foo"]],
            hashmap![
                "foo" => Unique,
                "foo.bar" => Unique,
            ],
        );
        field_access_par(
            vec![vec!["foo"], vec!["foo", "bar"]],
            hashmap![
                "foo" => Unique,
                "foo.bar" => Unique,
            ],
        );
    }

    #[test]
    fn usage_access_deeply_nested_field_seq() {
        field_access_seq(
            vec![vec!["foo", "bar", "baz"], vec!["foo", "bar"]],
            hashmap![
                "foo" => Seen,
                "foo.bar" => Unique,
                "foo.bar.baz" => Shared,
            ],
        );
        field_access_seq(
            vec![vec!["foo", "bar"], vec!["foo", "bar", "baz"]],
            hashmap![
                "foo" => Seen,
                "foo.bar" => Unique,
                "foo.bar.baz" => Shared,
            ],
        );
    }
    fn usage_eq<F>(src: &str, get_expected: F)
    where
        F: FnOnce(Interns) -> VarUsage,
    {
        let CanExprOut {
            loc_expr, interns, ..
        } = can_expr(src);

        use roc::uniqueness::sharing::annotate_usage;
        let mut usage = VarUsage::default();
        annotate_usage(&loc_expr.value, &mut usage);

        dbg!(&usage);

        assert_eq!(usage, get_expected(interns))
    }

    #[test]
    fn usage_factorial() {
        usage_eq(
            indoc!(
                r#"
                    factorial = \n ->
                        when n is
                            0 -> 1
                            1 -> 1
                            m -> factorial m

                    factorial
                "#
            ),
            |interns| {
                let home = test_home();
                let mut usage = VarUsage::default();

                usage.register_with(interns.symbol(home, "m".into()), &Unique);
                usage.register_with(interns.symbol(home, "n".into()), &Unique);
                usage.register_with(interns.symbol(home, "factorial".into()), &Shared);

                usage
            },
        );
    }

    #[test]
    fn usage_record_access() {
        usage_eq(
            indoc!(
                r#"
                    rec = { foo : 42, bar : "baz" }
                    rec.foo
                "#
            ),
            |interns| {
                let home = test_home();
                let mut usage = VarUsage::default();
                let fa = FieldAccess::from_chain(vec!["foo".into()]);

                usage.register_with(
                    interns.symbol(home, "rec".into()),
                    &ReferenceCount::Access(fa),
                );

                usage
            },
        );
    }

    #[test]
    fn usage_record_update() {
        usage_eq(
            indoc!(
                r#"
                    rec = { foo : 42, bar : "baz" }
                    { rec & foo: rec.foo }
                "#
            ),
            |interns| {
                let home = test_home();
                let mut usage = VarUsage::default();
                let fa = FieldAccess::from_chain(vec!["foo".into()]);

                let overwritten = hashset!["foo".into()].into();
                usage.register_with(
                    interns.symbol(home, "rec".into()),
                    &ReferenceCount::Update(overwritten, fa),
                );

                usage
            },
        );
    }

    #[test]
    fn update_then_unique() {
        usage_eq(
            indoc!(
                r#"
                    rec = { foo : 42, bar : "baz" }
                    v = { rec & foo: 53 }

                    rec
                "#
            ),
            |interns| {
                let home = test_home();
                let mut usage = VarUsage::default();
                usage.register_with(interns.symbol(home, "rec".into()), &ReferenceCount::Shared);

                usage
            },
        );
    }

    #[test]
    fn access_then_unique() {
        usage_eq(
            indoc!(
                r#"
                    rec = { foo : 42, bar : "baz" }
                    v = rec.foo

                    rec
                "#
            ),
            |interns| {
                let home = test_home();
                let mut usage = VarUsage::default();
                let mut fields = ImMap::default();
                fields.insert(
                    "foo".into(),
                    (ReferenceCount::Shared, FieldAccess::default()),
                );
                let fa = FieldAccess { fields: fields };
                usage.register_with(
                    interns.symbol(home, "rec".into()),
                    &ReferenceCount::Update(ImSet::default(), fa),
                );

                usage
            },
        );
    }

    #[test]
    fn access_then_alias() {
        usage_eq(
            indoc!(
                r#"
                    \r ->
                        v = r.x
                        w = r.y

                        p = r

                        p
                "#
            ),
            |interns| {
                let home = test_home();
                let mut usage = VarUsage::default();

                let mut fields = ImMap::default();
                fields.insert("x".into(), (ReferenceCount::Shared, FieldAccess::default()));
                fields.insert("y".into(), (ReferenceCount::Shared, FieldAccess::default()));

                usage.register_with(interns.symbol(home, "p".into()), &ReferenceCount::Unique);
                usage.register_with(
                    interns.symbol(home, "r".into()),
                    &ReferenceCount::Update(ImSet::default(), FieldAccess { fields }),
                );

                usage
            },
        );
    }

    #[test]
    fn access_nested_then_unique() {
        usage_eq(
            indoc!(
                r#"
                    \r ->
                        v = r.foo.bar
                        w = r.foo.baz

                        r
                "#
            ),
            |interns| {
                let home = test_home();
                let mut usage = VarUsage::default();

                let mut nested_fields = ImMap::default();
                nested_fields.insert(
                    "bar".into(),
                    (ReferenceCount::Shared, FieldAccess::default()),
                );
                nested_fields.insert(
                    "baz".into(),
                    (ReferenceCount::Shared, FieldAccess::default()),
                );
                let nested_fa = FieldAccess {
                    fields: nested_fields,
                };

                let mut fields = ImMap::default();
                fields.insert("foo".into(), (ReferenceCount::Seen, nested_fa));

                let fa = FieldAccess { fields: fields };
                usage.register_with(
                    interns.symbol(home, "r".into()),
                    &ReferenceCount::Update(ImSet::default(), fa),
                );

                usage
            },
        );
    }

    #[test]
    fn usage_record_update_unique_not_overwritten() {
        usage_eq(
            indoc!(
                r#"
                    r = { x : 42, y : 2020 }
                    s = { r & y: r.x }

                    p = s.x
                    q = s.y

                    42
                "#
            ),
            |interns| {
                let home = test_home();
                let mut usage = VarUsage::default();

                let mut fields = ImMap::default();
                fields.insert("x".into(), (ReferenceCount::Shared, FieldAccess::default()));
                let fa = FieldAccess { fields: fields };
                let overwritten = hashset!["y".into()].into();
                usage.register_with(
                    interns.symbol(home, "r".into()),
                    &ReferenceCount::Update(overwritten, fa),
                );

                let mut fields = ImMap::default();
                fields.insert("x".into(), (ReferenceCount::Unique, FieldAccess::default()));
                fields.insert("y".into(), (ReferenceCount::Unique, FieldAccess::default()));
                let fa = FieldAccess { fields: fields };
                usage.register_with(
                    interns.symbol(home, "s".into()),
                    &ReferenceCount::Access(fa),
                );

                usage
            },
        );
    }

    #[test]
    fn usage_record_update_unique_overwritten() {
        usage_eq(
            indoc!(
                r#"
                    r = { x : 42, y : 2020 }
                    s = { r & x: 0, y: r.x }

                    p = s.x
                    q = s.y

                    42
                "#
            ),
            |interns| {
                let home = test_home();
                // pub fields: ImMap<String, (ReferenceCount, FieldAccess)>,
                let mut usage = VarUsage::default();

                let fa = FieldAccess::from_chain(vec!["x".into()]);
                let overwritten = hashset!["x".into(), "y".into()].into();
                usage.register_with(
                    interns.symbol(home, "r".into()),
                    &ReferenceCount::Update(overwritten, fa),
                );

                let mut fields = ImMap::default();
                fields.insert("x".into(), (ReferenceCount::Unique, FieldAccess::default()));
                fields.insert("y".into(), (ReferenceCount::Unique, FieldAccess::default()));
                let fa = FieldAccess { fields: fields };
                usage.register_with(
                    interns.symbol(home, "s".into()),
                    &ReferenceCount::Access(fa),
                );

                usage
            },
        );
    }

    #[test]
    fn usage_if_access() {
        usage_eq(
            indoc!(
                r#"
                    r = { x : 42, y : 2020 }

                    if True then r.x else r.y
                "#
            ),
            |interns| {
                let home = test_home();
                // pub fields: ImMap<String, (ReferenceCount, FieldAccess)>,
                let mut usage = VarUsage::default();

                let mut fields = ImMap::default();
                fields.insert("x".into(), (ReferenceCount::Unique, FieldAccess::default()));
                fields.insert("y".into(), (ReferenceCount::Unique, FieldAccess::default()));
                let fa = FieldAccess { fields: fields };
                usage.register_with(
                    interns.symbol(home, "r".into()),
                    &ReferenceCount::Access(fa),
                );

                usage
            },
        );
    }

    #[test]
    fn usage_if_update() {
        usage_eq(
            indoc!(
                r#"
                    r = { x : 42, y : 2020 }

                    if True then { r & y: r.x } else r
                "#
            ),
            |interns| {
                let home = test_home();
                // pub fields: ImMap<String, (ReferenceCount, FieldAccess)>,
                let mut usage = VarUsage::default();

                let mut fields = ImMap::default();
                fields.insert("x".into(), (ReferenceCount::Shared, FieldAccess::default()));
                let fa = FieldAccess { fields: fields };
                usage.register_with(
                    interns.symbol(home, "r".into()),
                    &ReferenceCount::Update(hashset!["y".into()].into(), fa),
                );

                usage
            },
        );
    }

    #[test]
    fn usage_nested_if_access() {
        usage_eq(
            indoc!(
                r#"
                    r = { x : 42, y : 2020 }

                    if True then r.x else if False then r.x else r.y
                "#
            ),
            |interns| {
                let home = test_home();
                // pub fields: ImMap<String, (ReferenceCount, FieldAccess)>,
                let mut usage = VarUsage::default();

                let mut fields = ImMap::default();
                fields.insert("x".into(), (ReferenceCount::Unique, FieldAccess::default()));
                fields.insert("y".into(), (ReferenceCount::Unique, FieldAccess::default()));
                let fa = FieldAccess { fields: fields };
                usage.register_with(
                    interns.symbol(home, "r".into()),
                    &ReferenceCount::Access(fa),
                );

                usage
            },
        );
    }

    #[test]
    fn usage_closures_with_same_bound_name() {
        usage_eq(
            indoc!(
                r#"
                   (\val -> val) (\val -> val)
               "#
            ),
            |_interns| {
                let home = test_home();
                let mut usage = VarUsage::default();

                usage.register_with(Interns::from_index(home, 1), &ReferenceCount::Unique);
                usage.register_with(Interns::from_index(home, 3), &ReferenceCount::Unique);

                usage
            },
        );
    }
}
