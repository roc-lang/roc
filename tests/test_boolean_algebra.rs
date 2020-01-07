#[macro_use]
extern crate maplit;

#[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_boolean_algebra {
    use roc::collections::ImSet;
    use roc::subs;
    use roc::subs::VarStore;
    use roc::uniqueness::boolean_algebra;
    use roc::uniqueness::boolean_algebra::Bool::{self, *};

    // HELPERS
    fn to_var(v: usize) -> subs::Variable {
        subs::Variable::unsafe_debug_variable(v)
    }

    fn simplify_eq(a: Bool, b: Bool) {
        assert_eq!(boolean_algebra::simplify(a), boolean_algebra::simplify(b));
    }

    fn unify_eq(a: Bool, b: Bool, expected: std::collections::HashMap<roc::subs::Variable, Bool>) {
        let result = boolean_algebra::try_unify(a, b);

        assert_eq!(result, Some(expected.into()));
    }

    #[test]
    fn true_in_or() {
        let var_store = VarStore::default();
        let var = var_store.fresh();

        simplify_eq(Bool::or(One, Variable(var)), One);
    }

    #[test]
    fn false_in_or() {
        let var_store = VarStore::default();
        let var = var_store.fresh();

        simplify_eq(Bool::or(Zero, Variable(var)), Variable(var));
    }

    #[test]
    fn false_in_and() {
        let var_store = VarStore::default();
        let var = var_store.fresh();

        simplify_eq(Bool::and(Zero, Variable(var)), Zero);
    }

    #[test]
    fn unify_example_const() {
        unify_eq(
            Variable(to_var(1)),
            Variable(to_var(4)),
            hashmap![to_var(1) => Variable(to_var(4))].into(),
        );
        unify_eq(
            Variable(to_var(5)),
            Variable(to_var(2)),
            hashmap![to_var(2) => Variable(to_var(5))].into(),
        );
        unify_eq(
            Variable(to_var(6)),
            Variable(to_var(0)),
            hashmap![to_var(0) => Variable(to_var(6))].into(),
        );
    }

    #[test]
    fn unify_example_apply() {
        unify_eq(
            Variable(to_var(1)),
            Variable(to_var(6)),
            hashmap![to_var(1) => Variable(to_var(6))].into(),
        );
        unify_eq(
            Variable(to_var(5)),
            Variable(to_var(6)),
            hashmap![to_var(5) => Variable(to_var(6))].into(),
        );
        unify_eq(
            Variable(to_var(3)),
            Variable(to_var(7)),
            hashmap![to_var(3) => Variable(to_var(7))].into(),
        );
        unify_eq(
            Variable(to_var(8)),
            Variable(to_var(4)),
            hashmap![to_var(4) => Variable(to_var(8))].into(),
        );
        unify_eq(
            Variable(to_var(9)),
            Variable(to_var(2)),
            hashmap![to_var(2) => Variable(to_var(9))].into(),
        );
        unify_eq(
            Variable(to_var(10)),
            Variable(to_var(0)),
            hashmap![to_var(0) => Variable(to_var(10))].into(),
        );
    }

    #[test]
    fn unify_example_fst() {
        unify_eq(
            Variable(to_var(1)),
            Variable(to_var(5)),
            hashmap![to_var(1) => Variable(to_var(5))].into(),
        );
        unify_eq(
            Variable(to_var(3)),
            Variable(to_var(2)),
            hashmap![to_var(2) => Variable(to_var(3))].into(),
        );
        unify_eq(
            Variable(to_var(5)),
            Bool::or(Bool::or(Variable(to_var(3)), Zero), Zero),
            hashmap![to_var(3) => Variable(to_var(5))].into(),
        );
        unify_eq(
            Variable(to_var(6)),
            Variable(to_var(0)),
            hashmap![to_var(0) => Variable(to_var(6))].into(),
        );
    }

    #[test]
    fn unify_example_idid() {
        unify_eq(
            Variable(to_var(3)),
            Variable(to_var(4)),
            hashmap![to_var(3) => Variable(to_var(4))].into(),
        );
        unify_eq(
            Variable(to_var(5)),
            Variable(to_var(2)),
            hashmap![to_var(2) => Variable(to_var(5))].into(),
        );
        unify_eq(
            Variable(to_var(5)),
            Variable(to_var(1)),
            hashmap![to_var(1) => Variable(to_var(5))].into(),
        );
        unify_eq(
            Zero,
            Variable(to_var(5)),
            hashmap![to_var(5) => Zero].into(),
        );
        unify_eq(
            Variable(to_var(7)),
            Variable(to_var(4)),
            hashmap![to_var(4) => Variable(to_var(7))].into(),
        );
        unify_eq(
            Variable(to_var(8)),
            Variable(to_var(9)),
            hashmap![to_var(8) => Variable(to_var(9))].into(),
        );
        unify_eq(
            Variable(to_var(7)),
            Zero,
            hashmap![to_var(7) => Zero].into(),
        );
        unify_eq(
            Zero,
            Variable(to_var(0)),
            hashmap![to_var(0) => Zero].into(),
        );
    }

    #[test]
    fn unify_or() {
        let var_store = VarStore::default();
        let a = var_store.fresh();
        let b = var_store.fresh();

        let cond = Bool::or(
            Bool::or(
                Bool::and(Zero, Bool::not(One)),
                Bool::and(Bool::not(Zero), One),
            ),
            Bool::and(
                Variable(a),
                Bool::not(Bool::or(
                    Bool::and(One, Bool::not(One)),
                    Bool::and(Bool::not(One), One),
                )),
            ),
        );

        let ours = Bool::or(
            Bool::or(
                Bool::and(Zero, Bool::not(One)),
                Bool::and(Bool::not(Zero), One),
            ),
            Bool::and(
                Variable(a),
                Bool::not(Bool::or(
                    Bool::and(One, Bool::not(One)),
                    Bool::and(Bool::not(One), One),
                )),
            ),
        );

        assert_eq!(cond.clone(), ours);

        assert_eq!(
            Zero,
            boolean_algebra::normalize_term(Bool::and(
                Bool::or(
                    Bool::and(Zero, Bool::not(One)),
                    Bool::and(Bool::not(Zero), One)
                ),
                Bool::or(
                    Bool::and(One, Bool::not(One)),
                    Bool::and(Bool::not(One), One)
                )
            ))
        );

        assert_eq!(
            boolean_algebra::term_to_sop(One),
            hashset![hashset![One].into()].into()
        );

        assert_eq!(
            boolean_algebra::normalize_sop(hashset![hashset![One].into()].into()),
            hashset![hashset![].into()].into()
        );

        assert_eq!(
            boolean_algebra::sop_to_term(hashset![hashset![].into()].into()),
            One
        );

        unify_eq(Variable(a), One, hashmap![ a => One ]);
        unify_eq(Variable(a), Zero, hashmap![ a => Zero ]);
        simplify_eq(ours, One);
        unify_eq(
            Bool::or(Variable(a), Variable(b)),
            One,
            hashmap![ a => Bool::or(Bool::not(Variable(b)), Variable(a)) ],
        );
    }

    #[test]
    fn unify_current() {
        assert_eq!(
            boolean_algebra::simplify(Bool::and(
                Bool::and(
                    Bool::or(
                        Bool::and(Zero, Bool::not(Zero)),
                        Bool::and(Bool::not(Zero), Zero)
                    ),
                    Bool::or(
                        Bool::and(One, Bool::not(Zero)),
                        Bool::and(Bool::not(One), Zero)
                    )
                ),
                Bool::and(
                    Bool::or(
                        Bool::and(Zero, Bool::not(One)),
                        Bool::and(Bool::not(Zero), One)
                    ),
                    Bool::or(
                        Bool::and(One, Bool::not(One)),
                        Bool::and(Bool::not(One), One)
                    )
                )
            )),
            Zero
        );

        assert_eq!(
            boolean_algebra::simplify(Bool::or(
                Bool::and(
                    Bool::or(
                        Bool::and(Zero, Bool::not(Zero)),
                        Bool::and(Bool::not(Zero), Zero)
                    ),
                    Bool::or(
                        Bool::and(One, Bool::not(Zero)),
                        Bool::and(Bool::not(One), Zero)
                    )
                ),
                Bool::and(
                    Variable(to_var(6)),
                    Bool::not(Bool::and(
                        Bool::or(
                            Bool::and(Zero, Bool::not(One)),
                            Bool::and(Bool::not(Zero), One)
                        ),
                        Bool::or(
                            Bool::and(One, Bool::not(One)),
                            Bool::and(Bool::not(One), One)
                        )
                    ))
                )
            )),
            Variable(to_var(6))
        );

        assert_eq!(
            boolean_algebra::normalize_term(Bool::or(
                Bool::or(
                    Bool::and(Zero, Bool::not(Variable(to_var(6)))),
                    Bool::and(Bool::not(Zero), Variable(to_var(6)))
                ),
                Bool::and(
                    Variable(to_var(1)),
                    Bool::not(Bool::or(
                        Bool::and(One, Bool::not(Variable(to_var(6)))),
                        Bool::and(Bool::not(One), Variable(to_var(6)))
                    ))
                )
            )),
            Bool::or(
                Variable(to_var(6)),
                Bool::and(
                    Variable(to_var(1)),
                    Bool::not(Bool::not(Variable(to_var(6))))
                )
            )
        );

        assert_eq!(
            boolean_algebra::simplify(Bool::or(
                Bool::or(
                    Bool::and(Zero, Bool::not(Variable(to_var(6)))),
                    Bool::and(Bool::not(Zero), Variable(to_var(6)))
                ),
                Bool::and(
                    Variable(to_var(1)),
                    Bool::not(Bool::or(
                        Bool::and(One, Bool::not(Variable(to_var(6)))),
                        Bool::and(Bool::not(One), Variable(to_var(6)))
                    ))
                )
            )),
            Variable(to_var(6))
        );

        assert_eq!(
            {
                let mut expected = ImSet::default();

                let mut a = ImSet::default();
                a.insert(Variable(to_var(6)));

                expected.insert(a.clone());
                a.insert(Variable(to_var(1)));
                expected.insert(a.clone());
                boolean_algebra::simplify_sop(expected)
            },
            {
                let mut expected = ImSet::default();

                let mut a = ImSet::default();
                a.insert(Variable(to_var(6)));
                expected.insert(a);
                expected
            }
        );
        let a = to_var(1);
        let b = to_var(6);

        assert_eq!(
            boolean_algebra::contradiction(Bool::and(
                Bool::and(Variable(a), Variable(b)),
                Bool::not(Variable(b))
            )),
            true
        );

        assert_eq!(
            boolean_algebra::included(
                Bool::and(Variable(to_var(1)), Variable(to_var(6))),
                Variable(to_var(6))
            ),
            true
        );
        assert_eq!(boolean_algebra::included(Variable(to_var(6)), Zero), false);

        assert_eq!(
            boolean_algebra::term_to_sop(Bool::or(
                Variable(to_var(6)),
                Bool::and(
                    Variable(to_var(1)),
                    Bool::not(Bool::not(Variable(to_var(6))))
                )
            )),
            {
                let mut expected = ImSet::default();

                let mut a = ImSet::default();
                a.insert(Variable(to_var(6)));

                expected.insert(a.clone());
                a.insert(Variable(to_var(1)));
                expected.insert(a.clone());
                expected
            }
        );
        assert_eq!(
            boolean_algebra::dnf(Bool::or(
                Variable(to_var(6)),
                Bool::and(
                    Variable(to_var(1)),
                    Bool::not(Bool::not(Variable(to_var(6))))
                )
            )),
            Bool::or(
                Variable(to_var(6)),
                Bool::and(Variable(to_var(1)), Variable(to_var(6)))
            )
        );
        unify_eq(
            Variable(to_var(1)),
            Variable(to_var(6)),
            hashmap![to_var(1) => Variable(to_var(6))].into(),
        );
    }
}
