#[macro_use]
extern crate maplit;

#[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_boolean_algebra {
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
}
