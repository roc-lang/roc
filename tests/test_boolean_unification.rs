#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
#[macro_use]
extern crate roc;

#[cfg(test)]
mod test_format {
    use bumpalo::Bump;
    use roc::can::boolean_unification;
    use roc::can::boolean_unification::Term::*;
    use roc::can::boolean_unification::Var::*;

    #[test]
    fn ground_true() {
        let (sub, term) = boolean_unification::unify(&Ground(true), vec![].iter());
        assert_eq!(boolean_unification::solve(&term), Some(true));
    }

    #[test]
    fn ground_false() {
        let (sub, term) = boolean_unification::unify(&Ground(false), vec![].iter());
        assert_eq!(boolean_unification::solve(&term), Some(false));
    }

    #[test]
    fn disjunction() {
        let var = Var(0);
        let input = Disjunction(Box::new(Variable(var.clone())), Box::new(Ground(true)));
        let vars = vec![var];

        let (sub, term) = boolean_unification::unify(&input, vars.iter());
        assert_eq!(boolean_unification::solve(&term), Some(true));
    }
}
