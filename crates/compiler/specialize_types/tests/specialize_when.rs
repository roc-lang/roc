#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
mod helpers;

#[cfg(test)]
mod specialize_structs {
    use crate::helpers::expect_mono_expr_str;

    #[test]
    fn single_branch() {
        expect_mono_expr_str(
            r"
                when 123 is
                    num -> num
            ",
            "When(Number(I8(123)), Identifier(`#UserApp.IdentId(1)`) -> `#UserApp.IdentId(1)`)",
        );
    }
}
