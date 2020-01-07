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
    use roc::subs::VarStore;
    use roc::uniqueness::boolean_algebra;
    use roc::uniqueness::boolean_algebra::Bool::{self, *};

    // HELPERS
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
}
