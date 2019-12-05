#[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_boolean_algebra {
    use roc::subs;
    use roc::uniqueness::boolean_algebra;
    use roc::uniqueness::boolean_algebra::BooleanAlgebra::{self, *};

    // HELPERS
    fn simplify_eq(mut a: BooleanAlgebra, mut b: BooleanAlgebra) {
        assert_eq!(a.simplify(), b.simplify());
    }

    #[test]
    fn true_in_or() {
        simplify_eq(
            Disjunction(
                Box::new(Ground(true)),
                Box::new(Variable(subs::Variable::new_for_testing_only(0))),
            ),
            Ground(true),
        );
    }

    #[test]
    fn false_in_or() {
        simplify_eq(
            Disjunction(
                Box::new(Ground(false)),
                Box::new(Variable(subs::Variable::new_for_testing_only(0))),
            ),
            Variable(subs::Variable::new_for_testing_only(0)),
        );
    }

    #[test]
    fn false_in_and() {
        simplify_eq(
            Conjunction(
                Box::new(Ground(false)),
                Box::new(Variable(subs::Variable::new_for_testing_only(0))),
            ),
            Ground(false),
        );
    }

    #[test]
    fn unify_single_var() {
        let var = subs::Variable::new_for_testing_only(0);

        let result = boolean_algebra::unify(&Variable(var), &Ground(true));

        if let Some(sub) = result {
            assert_eq!(Some(&Ground(false)), sub.get(var));
        } else {
            panic!("result is None");
        }
    }

    #[test]
    fn unify_or() {
        let a = subs::Variable::new_for_testing_only(0);
        let b = subs::Variable::new_for_testing_only(1);

        let result = boolean_algebra::unify(
            &Disjunction(Box::new(Variable(a)), Box::new(Variable(b))),
            &Ground(true),
        );

        if let Some(sub) = result {
            assert_eq!(Some(&Variable(b)), sub.get(a));
            assert_eq!(Some(&Ground(false)), sub.get(b));
        } else {
            panic!("result is None");
        }
    }
}
