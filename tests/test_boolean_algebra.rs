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

        let mut expected_simpl = std::collections::HashMap::default();

        for (k, v) in expected.into_iter() {
            expected_simpl.insert(k, boolean_algebra::simplify(v));
        }

        assert_eq!(result, Some(expected_simpl.into()));
    }

    fn absorbs_eq(a: ImSet<Bool>, b: ImSet<Bool>, expected: bool) {
        assert_eq!(boolean_algebra::absorbs(&a, &b), expected);
    }

    fn vecs_to_sets(input: Vec<Vec<Bool>>) -> ImSet<ImSet<Bool>> {
        let mut result = ImSet::default();

        for v in input.into_iter() {
            result.insert(v.into());
        }

        result
    }

    fn absorptive_eq(actual: Vec<Vec<Bool>>, expected: Vec<Vec<Bool>>) {
        let actual_vec: ImSet<ImSet<Bool>> = vecs_to_sets(actual);
        let expected_vec: ImSet<ImSet<Bool>> = vecs_to_sets(expected);
        assert_eq!(actual_vec, expected_vec);
    }

    fn bcf_eq(actual: Vec<Vec<Bool>>, expected: Vec<Vec<Bool>>) {
        let actual_vec: ImSet<ImSet<Bool>> = vecs_to_sets(actual);
        let expected_vec: ImSet<ImSet<Bool>> = vecs_to_sets(expected);
        assert_eq!(actual_vec, expected_vec);
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
    fn test_absorptive() {
        let a = to_var(1);
        let b = to_var(2);
        let c = to_var(3);
        let d = to_var(4);

        absorbs_eq(
            vec![Variable(a), Variable(d)].into(),
            vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
            false,
        );
        absorbs_eq(
            vec![Variable(a), Variable(c)].into(),
            vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
            false,
        );
        absorbs_eq(
            vec![Variable(a), Variable(d)].into(),
            vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
            false,
        );
        absorbs_eq(
            vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
            vec![Bool::not(Variable(b)), Variable(d)].into(),
            false,
        );
        absorbs_eq(
            vec![Variable(a), Variable(c)].into(),
            vec![Bool::not(Variable(b)), Variable(d)].into(),
            false,
        );
        absorbs_eq(
            vec![Variable(a), Variable(d)].into(),
            vec![Bool::not(Variable(b)), Variable(d)].into(),
            false,
        );
        absorbs_eq(
            vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
            vec![Bool::not(Variable(b)), Variable(c)].into(),
            false,
        );
        absorbs_eq(
            vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
            vec![Bool::not(Variable(b)), Variable(c)].into(),
            false,
        );
        absorbs_eq(
            vec![Variable(a), Variable(c)].into(),
            vec![Bool::not(Variable(b)), Variable(c)].into(),
            false,
        );
        absorbs_eq(
            vec![Variable(a), Variable(d)].into(),
            vec![Bool::not(Variable(b)), Variable(c)].into(),
            false,
        );
        absorbs_eq(
            vec![Bool::not(Variable(b)), Variable(c)].into(),
            vec![Bool::not(Variable(b)), Variable(d)].into(),
            false,
        );
        absorbs_eq(
            vec![Bool::not(Variable(b)), Variable(d)].into(),
            vec![Bool::not(Variable(b)), Variable(d)].into(),
            true,
        );
        absorbs_eq(
            vec![Bool::not(Variable(b)), Variable(c)].into(),
            vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
            true,
        );
        absorbs_eq(
            vec![Bool::not(Variable(b)), Variable(c)].into(),
            vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
            false,
        );
        absorbs_eq(
            vec![Bool::not(Variable(b)), Variable(d)].into(),
            vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
            true,
        );
        absorbs_eq(
            vec![Bool::not(Variable(b)), Variable(c)].into(),
            vec![Variable(a), Variable(c)].into(),
            false,
        );
        absorbs_eq(
            vec![Bool::not(Variable(b)), Variable(d)].into(),
            vec![Variable(a), Variable(c)].into(),
            false,
        );
        absorbs_eq(
            vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
            vec![Variable(a), Variable(c)].into(),
            false,
        );
        absorbs_eq(
            vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
            vec![Variable(a), Variable(c)].into(),
            false,
        );
        absorbs_eq(
            vec![Variable(a), Variable(c)].into(),
            vec![Variable(a), Variable(c)].into(),
            true,
        );
        absorbs_eq(
            vec![Bool::not(Variable(b)), Variable(c)].into(),
            vec![Variable(a), Variable(d)].into(),
            false,
        );
        absorbs_eq(
            vec![Bool::not(Variable(b)), Variable(d)].into(),
            vec![Variable(a), Variable(d)].into(),
            false,
        );
        absorbs_eq(
            vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
            vec![Variable(a), Variable(d)].into(),
            false,
        );
        absorbs_eq(
            vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
            vec![Variable(a), Variable(d)].into(),
            false,
        );
        absorbs_eq(
            vec![Variable(a), Variable(c)].into(),
            vec![Variable(a), Variable(d)].into(),
            false,
        );
        absorbs_eq(
            vec![Variable(a), Variable(d)].into(),
            vec![Variable(a), Variable(d)].into(),
            true,
        );
    }

    #[test]
    fn variables_and_or() {
        use Bool::*;
        let a = to_var(1);
        let b = to_var(2);
        let c = to_var(3);
        let d = to_var(4);
        unify_eq(
            Variable(a),
            Bool::or(Variable(b), Variable(c)),
            hashmap![ a => Bool::or(Variable(b), Variable(c))].into(),
        );

        simplify_eq(
            Bool::or(
                Bool::and(
                    Bool::and(
                        Bool::and(
                            Bool::or(
                                Bool::and(Bool::or(Zero, Zero), Bool::not(Bool::or(Zero, Zero))),
                                Bool::and(Bool::not(Bool::or(Zero, Zero)), Bool::or(Zero, Zero)),
                            ),
                            Bool::or(
                                Bool::and(Bool::or(One, Zero), Bool::not(Bool::or(Zero, Zero))),
                                Bool::and(Bool::not(Bool::or(One, Zero)), Bool::or(Zero, Zero)),
                            ),
                        ),
                        Bool::and(
                            Bool::or(
                                Bool::and(Bool::or(Zero, One), Bool::not(Bool::or(Zero, Zero))),
                                Bool::and(Bool::not(Bool::or(Zero, One)), Bool::or(Zero, Zero)),
                            ),
                            Bool::or(
                                Bool::and(Bool::or(One, One), Bool::not(Bool::or(Zero, Zero))),
                                Bool::and(Bool::not(Bool::or(One, One)), Bool::or(Zero, Zero)),
                            ),
                        ),
                    ),
                    Bool::and(
                        Bool::and(
                            Bool::or(
                                Bool::and(Bool::or(Zero, Zero), Bool::not(Bool::or(One, Zero))),
                                Bool::and(Bool::not(Bool::or(Zero, Zero)), Bool::or(One, Zero)),
                            ),
                            Bool::or(
                                Bool::and(Bool::or(One, Zero), Bool::not(Bool::or(One, Zero))),
                                Bool::and(Bool::not(Bool::or(One, Zero)), Bool::or(One, Zero)),
                            ),
                        ),
                        Bool::and(
                            Bool::or(
                                Bool::and(Bool::or(Zero, One), Bool::not(Bool::or(One, Zero))),
                                Bool::and(Bool::not(Bool::or(Zero, One)), Bool::or(One, Zero)),
                            ),
                            Bool::or(
                                Bool::and(Bool::or(One, One), Bool::not(Bool::or(One, Zero))),
                                Bool::and(Bool::not(Bool::or(One, One)), Bool::or(One, Zero)),
                            ),
                        ),
                    ),
                ),
                Bool::and(
                    Variable(d),
                    Bool::not(Bool::and(
                        Bool::and(
                            Bool::and(
                                Bool::or(
                                    Bool::and(Bool::or(Zero, Zero), Bool::not(Bool::or(Zero, One))),
                                    Bool::and(Bool::not(Bool::or(Zero, Zero)), Bool::or(Zero, One)),
                                ),
                                Bool::or(
                                    Bool::and(Bool::or(One, Zero), Bool::not(Bool::or(Zero, One))),
                                    Bool::and(Bool::not(Bool::or(One, Zero)), Bool::or(Zero, One)),
                                ),
                            ),
                            Bool::and(
                                Bool::or(
                                    Bool::and(Bool::or(Zero, One), Bool::not(Bool::or(Zero, One))),
                                    Bool::and(Bool::not(Bool::or(Zero, One)), Bool::or(Zero, One)),
                                ),
                                Bool::or(
                                    Bool::and(Bool::or(One, One), Bool::not(Bool::or(Zero, One))),
                                    Bool::and(Bool::not(Bool::or(One, One)), Bool::or(Zero, One)),
                                ),
                            ),
                        ),
                        Bool::and(
                            Bool::and(
                                Bool::or(
                                    Bool::and(Bool::or(Zero, Zero), Bool::not(Bool::or(One, One))),
                                    Bool::and(Bool::not(Bool::or(Zero, Zero)), Bool::or(One, One)),
                                ),
                                Bool::or(
                                    Bool::and(Bool::or(One, Zero), Bool::not(Bool::or(One, One))),
                                    Bool::and(Bool::not(Bool::or(One, Zero)), Bool::or(One, One)),
                                ),
                            ),
                            Bool::and(
                                Bool::or(
                                    Bool::and(Bool::or(Zero, One), Bool::not(Bool::or(One, One))),
                                    Bool::and(Bool::not(Bool::or(Zero, One)), Bool::or(One, One)),
                                ),
                                Bool::or(
                                    Bool::and(Bool::or(One, One), Bool::not(Bool::or(One, One))),
                                    Bool::and(Bool::not(Bool::or(One, One)), Bool::or(One, One)),
                                ),
                            ),
                        ),
                    )),
                ),
            ),
            Variable(d),
        );
        simplify_eq(
            Bool::or(
                Bool::and(
                    Bool::and(
                        Bool::or(
                            Bool::and(Bool::or(Zero, Zero), Bool::not(Bool::or(Zero, Variable(d)))),
                            Bool::and(Bool::not(Bool::or(Zero, Zero)), Bool::or(Zero, Variable(d))),
                        ),
                        Bool::or(
                            Bool::and(Bool::or(One, Zero), Bool::not(Bool::or(Zero, Variable(d)))),
                            Bool::and(Bool::not(Bool::or(One, Zero)), Bool::or(Zero, Variable(d))),
                        ),
                    ),
                    Bool::and(
                        Bool::or(
                            Bool::and(Bool::or(Zero, One), Bool::not(Bool::or(Zero, Variable(d)))),
                            Bool::and(Bool::not(Bool::or(Zero, One)), Bool::or(Zero, Variable(d))),
                        ),
                        Bool::or(
                            Bool::and(Bool::or(One, One), Bool::not(Bool::or(Zero, Variable(d)))),
                            Bool::and(Bool::not(Bool::or(One, One)), Bool::or(Zero, Variable(d))),
                        ),
                    ),
                ),
                Bool::and(
                    Variable(c),
                    Bool::not(Bool::and(
                        Bool::and(
                            Bool::or(
                                Bool::and(
                                    Bool::or(Zero, Zero),
                                    Bool::not(Bool::or(One, Variable(d))),
                                ),
                                Bool::and(
                                    Bool::not(Bool::or(Zero, Zero)),
                                    Bool::or(One, Variable(d)),
                                ),
                            ),
                            Bool::or(
                                Bool::and(
                                    Bool::or(One, Zero),
                                    Bool::not(Bool::or(One, Variable(d))),
                                ),
                                Bool::and(
                                    Bool::not(Bool::or(One, Zero)),
                                    Bool::or(One, Variable(d)),
                                ),
                            ),
                        ),
                        Bool::and(
                            Bool::or(
                                Bool::and(
                                    Bool::or(Zero, One),
                                    Bool::not(Bool::or(One, Variable(d))),
                                ),
                                Bool::and(
                                    Bool::not(Bool::or(Zero, One)),
                                    Bool::or(One, Variable(d)),
                                ),
                            ),
                            Bool::or(
                                Bool::and(
                                    Bool::or(One, One),
                                    Bool::not(Bool::or(One, Variable(d))),
                                ),
                                Bool::and(
                                    Bool::not(Bool::or(One, One)),
                                    Bool::or(One, Variable(d)),
                                ),
                            ),
                        ),
                    )),
                ),
            ),
            Variable(c),
        );
        simplify_eq(
            Bool::or(
                Bool::and(
                    Bool::or(
                        Bool::and(
                            Bool::or(Zero, Zero),
                            Bool::not(Bool::or(Variable(c), Variable(d))),
                        ),
                        Bool::and(
                            Bool::not(Bool::or(Zero, Zero)),
                            Bool::or(Variable(c), Variable(d)),
                        ),
                    ),
                    Bool::or(
                        Bool::and(
                            Bool::or(One, Zero),
                            Bool::not(Bool::or(Variable(c), Variable(d))),
                        ),
                        Bool::and(
                            Bool::not(Bool::or(One, Zero)),
                            Bool::or(Variable(c), Variable(d)),
                        ),
                    ),
                ),
                Bool::and(
                    Variable(b),
                    Bool::not(Bool::and(
                        Bool::or(
                            Bool::and(
                                Bool::or(Zero, One),
                                Bool::not(Bool::or(Variable(c), Variable(d))),
                            ),
                            Bool::and(
                                Bool::not(Bool::or(Zero, One)),
                                Bool::or(Variable(c), Variable(d)),
                            ),
                        ),
                        Bool::or(
                            Bool::and(
                                Bool::or(One, One),
                                Bool::not(Bool::or(Variable(c), Variable(d))),
                            ),
                            Bool::and(
                                Bool::not(Bool::or(One, One)),
                                Bool::or(Variable(c), Variable(d)),
                            ),
                        ),
                    )),
                ),
            ),
            Bool::or(
                Bool::and(Variable(b), Variable(c)),
                Bool::and(Variable(b), Variable(d)),
            ),
        );

        assert_eq!(
            boolean_algebra::normalize_term(Bool::or(
                Bool::or(
                    Bool::and(
                        Bool::or(
                            Zero,
                            Bool::or(
                                Bool::and(Variable(b), Variable(c)),
                                Bool::and(Variable(b), Variable(d))
                            )
                        ),
                        Bool::not(Bool::or(Variable(c), Variable(d)))
                    ),
                    Bool::and(
                        Bool::not(Bool::or(
                            Zero,
                            Bool::or(
                                Bool::and(Variable(b), Variable(c)),
                                Bool::and(Variable(b), Variable(d))
                            )
                        )),
                        Bool::or(Variable(c), Variable(d))
                    )
                ),
                Bool::and(
                    Variable(a),
                    Bool::not(Bool::or(
                        Bool::and(
                            Bool::or(
                                One,
                                Bool::or(
                                    Bool::and(Variable(b), Variable(c)),
                                    Bool::and(Variable(b), Variable(d))
                                )
                            ),
                            Bool::not(Bool::or(Variable(c), Variable(d)))
                        ),
                        Bool::and(
                            Bool::not(Bool::or(
                                One,
                                Bool::or(
                                    Bool::and(Variable(b), Variable(c)),
                                    Bool::and(Variable(b), Variable(d))
                                )
                            )),
                            Bool::or(Variable(c), Variable(d))
                        )
                    ))
                )
            )),
            Bool::or(
                Bool::or(
                    Bool::and(
                        Bool::or(
                            Bool::and(Variable(b), Variable(c)),
                            Bool::and(Variable(b), Variable(d))
                        ),
                        Bool::not(Bool::or(Variable(c), Variable(d)))
                    ),
                    Bool::and(
                        Bool::not(Bool::or(
                            Bool::and(Variable(b), Variable(c)),
                            Bool::and(Variable(b), Variable(d))
                        )),
                        Bool::or(Variable(c), Variable(d))
                    )
                ),
                Bool::and(
                    Variable(a),
                    Bool::not(Bool::not(Bool::or(Variable(c), Variable(d))))
                )
            )
        );

        assert_eq!(
            boolean_algebra::term_to_sop(Bool::or(
                Bool::or(
                    Bool::and(
                        Bool::or(
                            Bool::and(Variable(b), Variable(c)),
                            Bool::and(Variable(b), Variable(d))
                        ),
                        Bool::not(Bool::or(Variable(c), Variable(d)))
                    ),
                    Bool::and(
                        Bool::not(Bool::or(
                            Bool::and(Variable(b), Variable(c)),
                            Bool::and(Variable(b), Variable(d))
                        )),
                        Bool::or(Variable(c), Variable(d))
                    )
                ),
                Bool::and(
                    Variable(a),
                    Bool::not(Bool::not(Bool::or(Variable(c), Variable(d))))
                )
            )),
            vec![
                vec![
                    Variable(b),
                    Variable(c),
                    Bool::not(Variable(c)),
                    Bool::not(Variable(d))
                ]
                .into(),
                vec![
                    Variable(b),
                    Variable(d),
                    Bool::not(Variable(c)),
                    Bool::not(Variable(d))
                ]
                .into(),
                vec![Bool::not(Variable(b)), Bool::not(Variable(b)), Variable(c)].into(),
                vec![Bool::not(Variable(b)), Bool::not(Variable(b)), Variable(d)].into(),
                vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
                vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(d)].into(),
                vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(c)].into(),
                vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
                vec![Bool::not(Variable(c)), Bool::not(Variable(d)), Variable(c)].into(),
                vec![Bool::not(Variable(c)), Bool::not(Variable(d)), Variable(d)].into(),
                vec![Variable(a), Variable(c)].into(),
                vec![Variable(a), Variable(d)].into()
            ]
            .into()
        );

        assert_eq!(
            boolean_algebra::normalize_sop(
                vec![
                    vec![
                        Variable(b),
                        Variable(c),
                        Bool::not(Variable(c)),
                        Bool::not(Variable(d))
                    ]
                    .into(),
                    vec![
                        Variable(b),
                        Variable(d),
                        Bool::not(Variable(c)),
                        Bool::not(Variable(d))
                    ]
                    .into(),
                    vec![Bool::not(Variable(b)), Bool::not(Variable(b)), Variable(c)].into(),
                    vec![Bool::not(Variable(b)), Bool::not(Variable(b)), Variable(d)].into(),
                    vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
                    vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(d)].into(),
                    vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(c)].into(),
                    vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
                    vec![Bool::not(Variable(c)), Bool::not(Variable(d)), Variable(c)].into(),
                    vec![Bool::not(Variable(c)), Bool::not(Variable(d)), Variable(d)].into(),
                    vec![Variable(a), Variable(c)].into(),
                    vec![Variable(a), Variable(d)].into()
                ]
                .into()
            ),
            vec![
                vec![Bool::not(Variable(b)), Variable(c)].into(),
                vec![Bool::not(Variable(b)), Variable(d)].into(),
                vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
                vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
                vec![Variable(a), Variable(c)].into(),
                vec![Variable(a), Variable(d)].into()
            ]
            .into()
        );

        assert_eq!(
            boolean_algebra::simplify_sop(
                vec![
                    vec![Variable(a), Variable(d)].into(),
                    vec![Variable(a), Variable(c)].into(),
                    vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
                    vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
                    vec![Bool::not(Variable(b)), Variable(d)].into(),
                    vec![Bool::not(Variable(b)), Variable(c)].into()
                ]
                .into()
            ),
            vec![
                vec![Bool::not(Variable(b)), Variable(c)].into(),
                vec![Bool::not(Variable(b)), Variable(d)].into(),
                vec![Variable(a), Variable(c)].into(),
                vec![Variable(a), Variable(d)].into()
            ]
            .into()
        );

        assert_eq!(
            boolean_algebra::syllogistic(
                vec![
                    vec![Bool::not(Variable(b)), Variable(c)].into(),
                    vec![Bool::not(Variable(b)), Variable(d)].into(),
                    vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
                    vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
                    vec![Variable(a), Variable(c)].into(),
                    vec![Variable(a), Variable(d)].into()
                ]
                .into()
            ),
            vec![
                vec![Bool::not(Variable(b)), Variable(c)].into(),
                vec![Bool::not(Variable(b)), Variable(d)].into(),
                vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
                vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
                vec![Variable(a), Variable(c)].into(),
                vec![Variable(a), Variable(d)].into()
            ]
            .into()
        );

        absorptive_eq(
            boolean_algebra::absorptive(
                vec![vec![Bool::not(Variable(b)), Variable(c)].into()].into(),
            ),
            vec![vec![Bool::not(Variable(b)), Variable(c)]],
        );
        absorptive_eq(
            boolean_algebra::absorptive(
                vec![
                    vec![Bool::not(Variable(b)), Variable(c)].into(),
                    vec![Bool::not(Variable(b)), Variable(d)].into(),
                ]
                .into(),
            ),
            vec![
                vec![Bool::not(Variable(b)), Variable(d)],
                vec![Bool::not(Variable(b)), Variable(c)],
            ],
        );
        absorptive_eq(
            boolean_algebra::absorptive(
                vec![
                    vec![Bool::not(Variable(b)), Variable(c)].into(),
                    vec![Bool::not(Variable(b)), Variable(d)].into(),
                    vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
                ]
                .into(),
            ),
            vec![
                vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)],
                vec![Bool::not(Variable(b)), Variable(d)],
                vec![Bool::not(Variable(b)), Variable(c)],
            ],
        );
        absorptive_eq(
            boolean_algebra::absorptive(
                vec![
                    vec![Bool::not(Variable(b)), Variable(c)].into(),
                    vec![Bool::not(Variable(b)), Variable(d)].into(),
                    vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
                    vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
                ]
                .into(),
            ),
            vec![
                vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)],
                vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)],
                vec![Bool::not(Variable(b)), Variable(d)],
                vec![Bool::not(Variable(b)), Variable(c)],
            ],
        );
        absorptive_eq(
            boolean_algebra::absorptive(
                vec![
                    vec![Bool::not(Variable(b)), Variable(c)].into(),
                    vec![Bool::not(Variable(b)), Variable(d)].into(),
                    vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
                    vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
                    vec![Variable(a), Variable(c)].into(),
                ]
                .into(),
            ),
            vec![
                vec![Variable(a), Variable(c)].into(),
                vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)],
                vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)],
                vec![Bool::not(Variable(b)), Variable(d)],
                vec![Bool::not(Variable(b)), Variable(c)],
            ],
        );

        assert_eq!(
            boolean_algebra::sop_to_term_vector(vec![
                vec![Bool::not(Variable(b)), Variable(c)].into(),
                vec![Bool::not(Variable(b)), Variable(d)].into(),
                vec![Variable(a), Variable(c)].into(),
                vec![Variable(a), Variable(d)].into()
            ]),
            Bool::or(
                Bool::and(Bool::not(Variable(b)), Variable(c)),
                Bool::or(
                    Bool::and(Bool::not(Variable(b)), Variable(d)),
                    Bool::or(
                        Bool::and(Variable(a), Variable(c)),
                        Bool::and(Variable(a), Variable(d))
                    )
                )
            )
        );

        bcf_eq(
            boolean_algebra::bcf(
                vec![
                    vec![Bool::not(Variable(b)), Variable(c)].into(),
                    vec![Bool::not(Variable(b)), Variable(d)].into(),
                    vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)].into(),
                    vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)].into(),
                    vec![Variable(a), Variable(c)].into(),
                    vec![Variable(a), Variable(d)].into(),
                ]
                .into(),
            ),
            vec![
                vec![Variable(a), Variable(d)],
                vec![Variable(a), Variable(c)],
                vec![Bool::not(Variable(c)), Bool::not(Variable(b)), Variable(d)],
                vec![Bool::not(Variable(b)), Bool::not(Variable(d)), Variable(c)],
                vec![Bool::not(Variable(b)), Variable(d)],
                vec![Bool::not(Variable(b)), Variable(c)],
            ],
        );

        simplify_eq(
            Bool::or(
                Bool::or(
                    Bool::and(
                        Bool::or(
                            Zero,
                            Bool::or(
                                Bool::and(Variable(b), Variable(c)),
                                Bool::and(Variable(b), Variable(d)),
                            ),
                        ),
                        Bool::not(Bool::or(Variable(c), Variable(d))),
                    ),
                    Bool::and(
                        Bool::not(Bool::or(
                            Zero,
                            Bool::or(
                                Bool::and(Variable(b), Variable(c)),
                                Bool::and(Variable(b), Variable(d)),
                            ),
                        )),
                        Bool::or(Variable(c), Variable(d)),
                    ),
                ),
                Bool::and(
                    Variable(a),
                    Bool::not(Bool::or(
                        Bool::and(
                            Bool::or(
                                One,
                                Bool::or(
                                    Bool::and(Variable(b), Variable(c)),
                                    Bool::and(Variable(b), Variable(d)),
                                ),
                            ),
                            Bool::not(Bool::or(Variable(c), Variable(d))),
                        ),
                        Bool::and(
                            Bool::not(Bool::or(
                                One,
                                Bool::or(
                                    Bool::and(Variable(b), Variable(c)),
                                    Bool::and(Variable(b), Variable(d)),
                                ),
                            )),
                            Bool::or(Variable(c), Variable(d)),
                        ),
                    )),
                ),
            ),
            Bool::or(
                Bool::and(Bool::not(Variable(b)), Variable(c)),
                Bool::or(
                    Bool::and(Bool::not(Variable(b)), Variable(d)),
                    Bool::or(
                        Bool::and(Variable(a), Variable(c)),
                        Bool::and(Variable(a), Variable(d)),
                    ),
                ),
            ),
        );

        simplify_eq(
            (Bool::or(
                Bool::and(
                    Bool::or(
                        Bool::and(
                            Bool::or(Zero, Zero),
                            Bool::not(Bool::or(Variable(c), Variable(d))),
                        ),
                        Bool::and(
                            Bool::not(Bool::or(Zero, Zero)),
                            Bool::or(Variable(c), Variable(d)),
                        ),
                    ),
                    Bool::or(
                        Bool::and(
                            Bool::or(One, Zero),
                            Bool::not(Bool::or(Variable(c), Variable(d))),
                        ),
                        Bool::and(
                            Bool::not(Bool::or(One, Zero)),
                            Bool::or(Variable(c), Variable(d)),
                        ),
                    ),
                ),
                Bool::and(
                    Variable(b),
                    Bool::not(Bool::and(
                        Bool::or(
                            Bool::and(
                                Bool::or(Zero, One),
                                Bool::not(Bool::or(Variable(c), Variable(d))),
                            ),
                            Bool::and(
                                Bool::not(Bool::or(Zero, One)),
                                Bool::or(Variable(c), Variable(d)),
                            ),
                        ),
                        Bool::or(
                            Bool::and(
                                Bool::or(One, One),
                                Bool::not(Bool::or(Variable(c), Variable(d))),
                            ),
                            Bool::and(
                                Bool::not(Bool::or(One, One)),
                                Bool::or(Variable(c), Variable(d)),
                            ),
                        ),
                    )),
                ),
            )),
            Bool::or(
                Bool::and(Variable(b), Variable(c)),
                Bool::and(Variable(b), Variable(d)),
            ),
        );
        simplify_eq(
            (Bool::or(
                Bool::or(
                    Bool::and(
                        Bool::or(
                            Zero,
                            Bool::or(
                                Bool::and(Variable(b), Variable(c)),
                                Bool::and(Variable(b), Variable(d)),
                            ),
                        ),
                        Bool::not(Bool::or(Variable(c), Variable(d))),
                    ),
                    Bool::and(
                        Bool::not(Bool::or(
                            Zero,
                            Bool::or(
                                Bool::and(Variable(b), Variable(c)),
                                Bool::and(Variable(b), Variable(d)),
                            ),
                        )),
                        Bool::or(Variable(c), Variable(d)),
                    ),
                ),
                Bool::and(
                    Variable(a),
                    Bool::not(Bool::or(
                        Bool::and(
                            Bool::or(
                                One,
                                Bool::or(
                                    Bool::and(Variable(b), Variable(c)),
                                    Bool::and(Variable(b), Variable(d)),
                                ),
                            ),
                            Bool::not(Bool::or(Variable(c), Variable(d))),
                        ),
                        Bool::and(
                            Bool::not(Bool::or(
                                One,
                                Bool::or(
                                    Bool::and(Variable(b), Variable(c)),
                                    Bool::and(Variable(b), Variable(d)),
                                ),
                            )),
                            Bool::or(Variable(c), Variable(d)),
                        ),
                    )),
                ),
            )),
            Bool::or(
                Bool::and(Bool::not(Variable(b)), Variable(c)),
                Bool::or(
                    Bool::and(Bool::not(Variable(b)), Variable(d)),
                    Bool::or(
                        Bool::and(Variable(a), Variable(c)),
                        Bool::and(Variable(a), Variable(d)),
                    ),
                ),
            ),
        );
    }

    #[test]
    fn unify_two_disjunctions() {
        let a = to_var(1);
        let b = to_var(2);
        let c = to_var(3);
        let d = to_var(4);
        unify_eq(Bool::or(Variable(a), Variable(b)), Bool::or(Variable(c), Variable(d)), hashmap![a => Bool::or(Bool::and(Bool::not(Variable(b)), Variable(c)), Bool::or(Bool::and(Bool::not(Variable(b)), Variable(d)), Bool::or(Bool::and(Variable(a), Variable(c)), Bool::and(Variable(a), Variable(d))))), b => Bool::or(Bool::and(Variable(b), Variable(c)), Bool::and(Variable(b), Variable(d)))].into());
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
}
