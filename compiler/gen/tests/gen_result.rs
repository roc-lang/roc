#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate inkwell;
extern crate libc;
extern crate roc_gen;

#[macro_use]
mod helpers;

#[cfg(test)]
mod gen_result {

    #[test]
    fn with_default() {
        assert_evals_to!(
            indoc!(
                r#"
                result : Result I64 {}
                result = Ok 2

                Result.withDefault result 0
                "#
            ),
            2,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                result : Result I64 {}
                result = Err {} 

                Result.withDefault result 0
                "#
            ),
            0,
            i64
        );
    }

    #[test]
    fn result_map() {
        assert_evals_to!(
            indoc!(
                r#"
                result : Result I64 {}
                result = Ok 2

                result
                    |> Result.map (\x -> x + 1)
                    |> Result.withDefault 0
                "#
            ),
            3,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                result : Result I64 {}
                result = Err {} 

                result
                    |> Result.map (\x -> x + 1)
                    |> Result.withDefault 0
                "#
            ),
            0,
            i64
        );
    }

    #[test]
    fn result_map_err() {
        assert_evals_to!(
            indoc!(
                r#"
                result : Result {} I64
                result = Err 2

                when Result.mapErr result (\x -> x + 1) is
                    Err n -> n
                    Ok _ -> 0
                "#
            ),
            3,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                result : Result {} I64
                result = Ok {}

                when Result.mapErr result (\x -> x + 1) is
                    Err n -> n
                    Ok _ -> 0
                "#
            ),
            0,
            i64
        );
    }
}
