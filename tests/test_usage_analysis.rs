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
    use roc::uniqueness::sharing::Composable;
    use roc::uniqueness::sharing::FieldAccess;
    use roc::uniqueness::sharing::VarUsage;
    use roc::uniqueness::sharing::{Container, Mark, Usage};

    use Container::*;
    use Mark::*;
    use Usage::*;

    fn field_access_seq(
        accesses: Vec<Vec<&str>>,
        expected_ref: std::collections::HashMap<&str, Usage>,
    ) {
        use Mark::*;
        use Usage::*;

        let mut usage = Simple(Seen);

        for access in accesses {
            let temp: Vec<Lowercase> = access.into_iter().map(|v| v.into()).collect();
            dbg!(&usage);
            usage.sequential_chain(temp);
        }
        dbg!(&usage);

        match usage {
            Usage::Access(_, _, fields) => {
                let mut actual: std::collections::HashMap<Lowercase, Usage> =
                    std::collections::HashMap::default();
                for (k, v) in fields.into_iter() {
                    actual.insert(k, v);
                }

                let mut expected = std::collections::HashMap::default();
                for (k, v) in expected_ref {
                    expected.insert(k.into(), v);
                }

                assert_eq!(actual, expected);
            }
            _ => panic!("Not an access, but {:?}", usage),
        }
    }

    fn field_access_par(
        accesses: Vec<Vec<&str>>,
        expected_ref: std::collections::HashMap<&str, Usage>,
    ) {
        use Mark::*;
        use Usage::*;

        let mut usage = Simple(Seen);

        for access in accesses {
            let temp: Vec<Lowercase> = access.into_iter().map(|v| v.into()).collect();
            usage.parallel_chain(temp);
        }

        match usage {
            Usage::Access(_, _, fields) => {
                let mut actual: std::collections::HashMap<Lowercase, Usage> =
                    std::collections::HashMap::default();
                for (k, v) in fields.into_iter() {
                    actual.insert(k, v);
                }

                let mut expected = std::collections::HashMap::default();
                for (k, v) in expected_ref {
                    expected.insert(k.into(), v);
                }

                assert_eq!(actual, expected);
            }
            _ => panic!("Not an access, but {:?}", usage),
        }
    }

    fn field_access(fields: std::collections::HashMap<&str, Usage>) -> FieldAccess {
        let mut new_fields = ImMap::default();

        for (k, v) in fields {
            new_fields.insert(k.into(), v);
        }

        FieldAccess::new(new_fields)
    }

    #[test]
    fn usage_access_two_fields() {
        field_access_seq(
            vec![vec!["foo"], vec!["bar"]],
            hashmap![
                "foo" => Simple(Unique),
                "bar" => Simple(Unique),
            ],
        );

        field_access_par(
            vec![vec!["foo"], vec!["bar"]],
            hashmap![
                "foo" => Simple(Unique),
                "bar" => Simple(Unique),
            ],
        );
    }

    #[test]
    fn usage_access_repeated_field_seq() {
        field_access_seq(
            vec![vec!["foo"], vec!["foo"]],
            hashmap![ "foo" => Simple(Shared) ],
        );
    }

    #[test]
    fn usage_access_repeated_field_par() {
        field_access_par(
            vec![vec!["foo"], vec!["foo"]],
            hashmap![
                "foo" => Simple(Unique),
            ],
        );
    }

    #[test]
    fn usage_access_nested_field_seq() {
        field_access_seq(
            vec![vec!["foo", "bar"], vec!["foo"]],
            hashmap![
                "foo" => Access(Record, Unique, field_access(hashmap![ "bar" => Simple(Shared) ]))
            ],
        );

        field_access_seq(
            vec![vec!["foo"], vec!["foo", "bar"]],
            hashmap![
                "foo" => Access(Record, Unique, field_access(hashmap![ "bar" => Simple(Shared) ]))
            ],
        );
    }
    #[test]
    fn usage_access_nested_field_par() {
        field_access_par(
            vec![vec!["foo", "bar"], vec!["foo"]],
            hashmap![
                "foo" => Access(Record, Unique, field_access(hashmap![ "bar" => Simple(Unique) ]))
            ],
        );
        field_access_par(
            vec![vec!["foo"], vec!["foo", "bar"]],
            hashmap![
                "foo" => Access(Record, Unique, field_access(hashmap![ "bar" => Simple(Unique) ]))
            ],
        );
    }

    #[test]
    fn usage_access_deeply_nested_field_seq() {
        field_access_seq(
            vec![vec!["foo", "bar", "baz"], vec!["foo", "bar"]],
            hashmap![
                "foo" => Access(Record, Seen, field_access(hashmap![ "bar" => Access(Record, Unique, field_access(hashmap![ "baz" => Simple(Shared) ]))]))
            ],
        );
        field_access_seq(
            vec![vec!["foo", "bar"], vec!["foo", "bar", "baz"]],
            hashmap![
                "foo" => Access(Record, Seen, field_access(hashmap![ "bar" => Access(Record, Unique, field_access(hashmap![ "baz" => Simple(Shared) ]))]))
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

                usage.register_unique(interns.symbol(home, "m".into()));
                usage.register_unique(interns.symbol(home, "n".into()));
                usage.register_shared(interns.symbol(home, "factorial".into()));

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

                usage.register_with(
                    interns.symbol(home, "rec".into()),
                    &Access(
                        Record,
                        Seen,
                        field_access(hashmap![ "foo" => Simple(Unique) ]),
                    ),
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

                let overwritten = hashset!["foo".into()].into();
                usage.register_with(
                    interns.symbol(home, "rec".into()),
                    &Update(
                        Record,
                        overwritten,
                        field_access(hashmap![ "foo" => Simple(Unique) ]),
                    ),
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
                usage.register_with(interns.symbol(home, "rec".into()), &Simple(Shared));

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

                usage.register_with(
                    interns.symbol(home, "rec".into()),
                    &Access(
                        Record,
                        Unique,
                        field_access(hashmap![ "foo" => Simple(Shared) ]),
                    ),
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

                        # nested let-block. Force the assignement after the access
                        (p = r

                        p)
                "#
            ),
            |interns| {
                let home = test_home();
                println!("---- test -----");
                let mut usage = VarUsage::default();

                let fa = field_access(hashmap![
                    "x" => Simple(Shared),
                    "y" => Simple(Shared),
                ]);

                usage.register_unique(interns.symbol(home, "p".into()));
                usage.register_with(
                    interns.symbol(home, "r".into()),
                    &Access(Record, Unique, fa),
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

                let fa = field_access(hashmap![
                    "foo" =>
                        Access(Record, Seen, field_access(hashmap![
                            "bar" => Simple(Shared),
                            "baz" => Simple(Shared),
                        ]))
                ]);

                usage.register_with(
                    interns.symbol(home, "r".into()),
                    &Access(Record, Unique, fa),
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

                let r = interns.symbol(home, "r".into());
                let s = interns.symbol(home, "s".into());

                let overwritten = hashset!["y".into()].into();
                let fa = field_access(hashmap![
                    "x" => Simple(Shared),
                ]);

                usage.register_with(r, &Update(Record, overwritten, fa));

                let fa = field_access(hashmap![
                    "x" => Simple(Unique),
                    "y" => Simple(Unique),
                ]);

                usage.register_with(s, &Access(Record, Seen, fa));
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
                let mut usage = VarUsage::default();

                let r = interns.symbol(home, "r".into());
                let s = interns.symbol(home, "s".into());

                let overwritten = hashset!["x".into(), "y".into()].into();
                let fa = field_access(hashmap![
                    "x" => Simple(Unique),
                ]);

                usage.register_with(r, &Update(Record, overwritten, fa));

                let fa = field_access(hashmap![
                    "x" => Simple(Unique),
                    "y" => Simple(Unique),
                ]);

                usage.register_with(s, &Access(Record, Seen, fa));

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

                let r = interns.symbol(home, "r".into());

                let fa = field_access(hashmap![
                    "x" => Simple(Unique),
                    "y" => Simple(Unique),
                ]);

                usage.register_with(r, &Access(Record, Seen, fa));

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
                let mut usage = VarUsage::default();

                let r = interns.symbol(home, "r".into());

                let fa = field_access(hashmap![
                    "x" => Simple(Shared),
                ]);

                let overwritten = hashset!["y".into()].into();

                usage.register_with(r, &Update(Record, overwritten, fa));

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
                let mut usage = VarUsage::default();

                let access = Access(
                    Record,
                    Seen,
                    field_access(hashmap![
                        "x" => Simple(Unique),
                        "y" => Simple(Unique)
                    ]),
                );

                usage.register_with(interns.symbol(home, "r".into()), &access);
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

                usage.register_unique(Interns::from_index(home, 1));
                usage.register_unique(Interns::from_index(home, 3));

                usage
            },
        );
    }

    #[test]
    fn shared_branch_unique_branch() {
        usage_eq(
            indoc!(
                r#"
                r = { x: 20, y: 20 }
                s = { x: 20, y: 20 }

                if True then
                    { x: s.x, y: r.y }
                else
                    { x: s.x, y: s.x }
                "#
            ),
            |interns| {
                let mut usage = VarUsage::default();
                let home = test_home();

                let access_r = Access(
                    Record,
                    Seen,
                    field_access(hashmap![ "y" => Simple(Unique) ]),
                );
                let access_s = Access(
                    Record,
                    Seen,
                    field_access(hashmap![ "x" => Simple(Shared) ]),
                );

                let r = interns.symbol(home, "r".into());
                let s = interns.symbol(home, "s".into());

                usage.register_with(r, &access_r);
                usage.register_with(s, &access_s);

                usage
            },
        );
    }

    #[test]
    fn shared_branch_unique_branch_access() {
        usage_eq(
            indoc!(
                r#"
                r = { x: 20 }
                s = { x: 20 }

                if True then
                    { y: r.x }
                else
                    v = s.x
                    { y: s.x }
                "#
            ),
            |interns| {
                let mut usage = VarUsage::default();
                let home = test_home();

                let access_r = Access(
                    Record,
                    Seen,
                    field_access(hashmap![ "x" => Simple(Unique) ]),
                );
                let access_s = Access(
                    Record,
                    Seen,
                    field_access(hashmap![ "x" => Simple(Shared) ]),
                );

                let r = interns.symbol(home, "r".into());
                let s = interns.symbol(home, "s".into());

                usage.register_with(r, &access_r);
                usage.register_with(s, &access_s);

                usage
            },
        );
    }
    #[test]
    fn record_update_is_safe() {
        usage_eq(
            indoc!(
                r#"
                    \r ->

                        s = { r & y: r.x }

                        p = s.x
                        q = s.y

                        s
                "#
            ),
            |interns| {
                let mut usage = VarUsage::default();
                let home = test_home();

                let overwritten = hashset!["y".into()].into();
                let access_r = Update(
                    Record,
                    overwritten,
                    field_access(hashmap![ "x" => Simple(Shared) ]),
                );

                let access_s = Access(
                    Record,
                    Unique,
                    field_access(hashmap![
                        "x" => Simple(Shared),
                        "y" => Simple(Shared)
                    ]),
                );

                let r = interns.symbol(home, "r".into());
                let s = interns.symbol(home, "s".into());

                usage.register_with(r, &access_r);
                usage.register_with(s, &access_s);

                usage
            },
        );
    }
}
