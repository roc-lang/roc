#[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_boolean_algebra {
    use roc::subs::VarStore;
    use roc::uniqueness::boolean_algebra;
    use roc::uniqueness::boolean_algebra::Bool::{self, *};

    // HELPERS
    fn simplify_eq(a: Bool, b: Bool) {
        assert_eq!(boolean_algebra::simplify(a), boolean_algebra::simplify(b));
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
    fn unify_single_var() {
        let var_store = VarStore::default();
        let var = var_store.fresh();

        let result = boolean_algebra::try_unify(Variable(var), One);

        if let Some(sub) = result {
            assert_eq!(Some(&Zero), sub.get(&var));
        } else {
            panic!("result is None");
        }
    }

    #[test]
    fn unify_or() {
        let var_store = VarStore::default();
        let a = var_store.fresh();
        let b = var_store.fresh();

        let result = boolean_algebra::try_unify(Bool::or(Variable(a), Variable(b)), One);

        if let Some(sub) = result {
            assert_eq!(Some(&Variable(b)), sub.get(&a));
            assert_eq!(Some(&Zero), sub.get(&b));
        } else {
            panic!("result is None");
        }
    }
}
