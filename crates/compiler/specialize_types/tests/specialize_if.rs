#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
mod helpers;

#[cfg(test)]
mod specialize_if {
    use crate::helpers::expect_mono_expr_str;

    #[test]
    fn single_branch() {
        let cond = "Bool.true";
        let then = 42;
        let else_ = 0;

        expect_mono_expr_str(
            format!("if {cond} then {then} else {else_}"),
            format!("If(`{cond}` -> Number(I8({then}))), Number(I8({else_})))"),
        );
    }

    #[test]
    fn multiple_branches() {
        let cond1 = "Bool.false";
        let then1 = 256;
        let cond2 = "Bool.true";
        let then2 = 24;
        let then_else = 0;

        expect_mono_expr_str(
            format!("if {cond1} then {then1} else if {cond2} then {then2} else {then_else}"),
            format!("If(`{cond1}` -> Number(I16({then1}))), `{cond2}` -> Number(I16({then2}))), Number(I16({then_else})))"),
        );
    }
}
