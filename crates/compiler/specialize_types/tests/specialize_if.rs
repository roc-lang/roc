#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
mod helpers;

#[cfg(test)]
mod specialize_structs {
    use crate::helpers::expect_mono_expr_str;

    #[test]
    fn single_branch() {
        let cond = "Bool.true";
        let then = 42;
        let else_ = 0;

        expect_mono_expr_str(
            format!("if {cond} then {then} else {else_}"),
            format!("If(`Bool.true` -> Number(I8(42))), Number(I8(0)))"),
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
            format!("If(`Bool.false` -> Number(I16(256))), `Bool.true` -> Number(I16(24))), Number(I16(0)))"),
        );
    }
}
