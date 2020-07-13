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
mod gen_list {
    use crate::helpers::{can_expr, infer_expr, uniq_expr, with_larger_debug_stack, CanExprOut};
    use bumpalo::Bump;
    use inkwell::context::Context;
    use inkwell::execution_engine::JitFunction;
    use inkwell::passes::PassManager;
    use inkwell::types::BasicType;
    use inkwell::OptimizationLevel;
    use roc_collections::all::ImMap;
    use roc_gen::llvm::build::{build_proc, build_proc_header};
    use roc_gen::llvm::convert::basic_type_from_layout;
    use roc_mono::expr::{Expr, Procs};
    use roc_mono::layout::Layout;
    use roc_types::subs::Subs;

    #[test]
    fn empty_list_literal() {
        assert_evals_to!("[]", &[], &'static [i64]);
    }

    #[test]
    fn int_list_literal() {
        assert_evals_to!("[ 12, 9, 6, 3 ]", &[12, 9, 6, 3], &'static [i64]);
    }

    #[test]
    fn list_push() {
        assert_evals_to!("List.push [1] 2", &[1, 2], &'static [i64]);
        assert_evals_to!("List.push [1, 1] 2", &[1, 1, 2], &'static [i64]);
        assert_evals_to!("List.push [] 3", &[3], &'static [i64]);
        assert_evals_to!(
            indoc!(
                r#"
                    initThrees : List Int
                    initThrees =
                        []

                    List.push (List.push initThrees 3) 3
                "#
            ),
            &[3, 3],
            &'static [i64]
        );
        assert_evals_to!(
            "List.push [ True, False ] True",
            &[true, false, true],
            &'static [bool]
        );
        assert_evals_to!(
            "List.push [ 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22 ] 23",
            &[11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23],
            &'static [i64]
        );
    }

    #[test]
    fn list_single() {
        assert_evals_to!("List.single 1", &[1], &'static [i64]);
        assert_evals_to!("List.single 5.6", &[5.6], &'static [f64]);
    }

    #[test]
    fn list_repeat() {
        assert_evals_to!("List.repeat 5 1", &[1, 1, 1, 1, 1], &'static [i64]);
        assert_evals_to!("List.repeat 4 2", &[2, 2, 2, 2], &'static [i64]);

        assert_evals_to!("List.repeat 2 []", &[&[], &[]], &'static [&'static [i64]]);
        assert_evals_to!(
            indoc!(
                r#"
                    noStrs : List Str
                    noStrs =
                        []

                    List.repeat 2 noStrs
                "#
            ),
            &[&[], &[]],
            &'static [&'static [i64]]
        );

        assert_evals_to!(
            "List.repeat 15 4",
            &[4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4],
            &'static [i64]
        );
    }

    #[test]
    fn list_reverse() {
        assert_evals_to!(
            "List.reverse [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ]",
            &[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1],
            &'static [i64]
        );
        assert_evals_to!("List.reverse [1, 2, 3]", &[3, 2, 1], &'static [i64]);
        assert_evals_to!("List.reverse [4]", &[4], &'static [i64]);
        assert_evals_to!(
            indoc!(
                r#"
                    emptyList : List Int
                    emptyList =
                        []

                    List.reverse emptyList
                "#
            ),
            &[],
            &'static [i64]
        );
        assert_evals_to!("List.reverse []", &[], &'static [i64]);
    }

    #[test]
    fn empty_list_len() {
        assert_evals_to!("List.len []", 0, usize);
    }

    #[test]
    fn basic_int_list_len() {
        assert_evals_to!("List.len [ 12, 9, 6, 3 ]", 4, usize);
    }

    #[test]
    fn loaded_int_list_len() {
        assert_evals_to!(
            indoc!(
                r#"
                    nums = [ 2, 4, 6 ]

                    List.len nums
                "#
            ),
            3,
            usize
        );
    }

    #[test]
    fn fn_int_list_len() {
        assert_evals_to!(
            indoc!(
                r#"
                    getLen = \list -> List.len list

                    nums = [ 2, 4, 6, 8 ]

                    getLen nums
                "#
            ),
            4,
            usize
        );
    }

    #[test]
    fn int_list_is_empty() {
        assert_evals_to!("List.isEmpty [ 12, 9, 6, 3 ]", false, bool);
    }

    #[test]
    fn empty_list_is_empty() {
        assert_evals_to!("List.isEmpty []", true, bool);
    }

    #[test]
    fn first_int_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    when List.first [ 12, 9, 6, 3 ] is
                        Ok val -> val
                        Err _ -> -1
                "#
            ),
            12,
            i64
        );
    }

    #[test]
    fn first_wildcard_empty_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    when List.first [] is
                        Ok _ -> 5
                        Err _ -> -1
                "#
            ),
            -1,
            i64
        );
    }

    // TODO getting this to work requires generating a runtime error for the Ok
    // branch here, which is not yet something we support as of when this
    // test was originally written.
    //
    // #[test]
    // fn first_empty_list() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 when List.first [] is
    //                     Ok val -> val
    //                     Err _ -> -1
    //             "#
    //         ),
    //         -1,
    //         i64
    //     );
    // }

    #[test]
    fn get_empty_list() {
        assert_evals_to!(
            indoc!(
                r#"
                   when List.get [] 0 is
                        Ok val -> val
                        Err _ -> -1
                "#
            ),
            -1,
            i64
        );
    }

    #[test]
    fn get_wildcard_empty_list() {
        assert_evals_to!(
            indoc!(
                r#"
                   when List.get [] 0 is
                        Ok _ -> 5
                        Err _ -> -1
                "#
            ),
            -1,
            i64
        );
    }

    #[test]
    fn get_int_list_ok() {
        assert_evals_to!(
            indoc!(
                r#"
                    when List.get [ 12, 9, 6 ] 1 is
                        Ok val -> val
                        Err _ -> -1
                "#
            ),
            9,
            i64
        );
    }

    #[test]
    fn get_int_list_oob() {
        assert_evals_to!(
            indoc!(
                r#"
                    when List.get [ 12, 9, 6 ] 1000 is
                        Ok val -> val
                        Err _ -> -1
                "#
            ),
            -1,
            i64
        );
    }

    #[test]
    fn get_set_unique_int_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    when List.get (List.set [ 12, 9, 7, 3 ] 1 42) 1 is
                        Ok val -> val
                        Err _ -> -1
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn set_unique_int_list() {
        assert_evals_to!(
            "List.set [ 12, 9, 7, 1, 5 ] 2 33",
            &[12, 9, 33, 1, 5],
            &'static [i64]
        );
    }

    #[test]
    fn set_unique_list_oob() {
        assert_evals_to!(
            "List.set [ 3, 17, 4.1 ] 1337 9.25",
            &[3.0, 17.0, 4.1],
            &'static [f64]
        );
    }

    #[test]
    fn set_shared_int_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    shared = [ 2.1, 4.3 ]

                    # This should not mutate the original
                    x =
                        when List.get (List.set shared 1 7.7) 1 is
                            Ok num -> num
                            Err _ -> 0

                    y =
                        when List.get shared 1 is
                            Ok num -> num
                            Err _ -> 0

                    { x, y }
                "#
            ),
            (7.7, 4.3),
            (f64, f64)
        );
    }

    #[test]
    fn set_shared_list_oob() {
        assert_evals_to!(
            indoc!(
                r#"
                    shared = [ 2, 4 ]

                    # This List.set is out of bounds, and should have no effect
                    x =
                        when List.get (List.set shared 422 0) 1 is
                            Ok num -> num
                            Err _ -> 0

                    y =
                        when List.get shared 1 is
                            Ok num -> num
                            Err _ -> 0

                    { x, y }
                "#
            ),
            (4, 4),
            (i64, i64)
        );
    }

    #[test]
    fn get_unique_int_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    unique = [ 2, 4 ]

                    when List.get unique 1 is
                        Ok num -> num
                        Err _ -> -1
                "#
            ),
            4,
            i64
        );
    }

    #[test]
    fn gen_wrap_len() {
        assert_evals_to!(
            indoc!(
                r#"
                    wrapLen = \list ->
                        [ List.len list ]

                    wrapLen [ 1, 7, 9 ]
                "#
            ),
            &[3],
            &'static [i64]
        );
    }

    #[test]
    fn gen_wrap_first() {
        assert_evals_to!(
            indoc!(
                r#"
                    wrapFirst = \list ->
                        List.first list

                    wrapFirst [ 1, 2 ]
                "#
            ),
            &[1],
            &'static [i64]
        );
    }

    #[test]
    fn gen_swap() {
        assert_evals_to!(
            indoc!(
                r#"
                    swap = \list ->
                        when List.first list is
                            Ok elem ->
                                List.set list 0 elem

                            _ ->
                                []

                    swap [ 1, 2 ]
                "#
            ),
            &[2, 1],
            &'static [i64]
        );
    }

    #[test]
    fn gen_quicksort() {
        with_larger_debug_stack(|| {
            assert_evals_to!(
                indoc!(
                    r#"
                    quicksort : List (Num a) -> List (Num a)
                    quicksort = \list ->
                        quicksortHelp list 0 (List.len list - 1)


                    quicksortHelp : List (Num a), Int, Int -> List (Num a)
                    quicksortHelp = \list, low, high ->
                        if low < high then
                            when partition low high list is
                                Pair partitionIndex partitioned ->
                                    partitioned
                                        |> quicksortHelp low (partitionIndex - 1)
                                        |> quicksortHelp (partitionIndex + 1) high
                        else
                            list


                    swap : Int, Int, List a -> List a
                    swap = \i, j, list ->
                        when Pair (List.get list i) (List.get list j) is
                            Pair (Ok atI) (Ok atJ) ->
                                list
                                    |> List.set i atJ
                                    |> List.set j atI

                            _ ->
                                []

                    partition : Int, Int, List (Num a) -> [ Pair Int (List (Num a)) ]
                    partition = \low, high, initialList ->
                        when List.get initialList high is
                            Ok pivot ->
                                when partitionHelp (low - 1) low initialList high pivot is
                                    Pair newI newList ->
                                        Pair (newI + 1) (swap (newI + 1) high newList)

                            Err _ ->
                                Pair (low - 1) initialList


                    partitionHelp : Int, Int, List (Num a), Int, Int -> [ Pair Int (List (Num a)) ]
                    partitionHelp = \i, j, list, high, pivot ->
                        if j < high then
                            when List.get list j is
                                Ok value ->
                                    if value <= pivot then
                                        partitionHelp (i + 1) (j + 1) (swap (i + 1) j list) high pivot
                                    else
                                        partitionHelp i (j + 1) list high pivot

                                Err _ ->
                                    Pair i list
                        else
                            Pair i list



                    quicksort [ 7, 4, 21, 19 ]
                "#
                ),
                &[4, 7, 19, 21],
                &'static [i64]
            );
        })
    }
}
