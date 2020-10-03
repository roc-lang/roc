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
    use crate::helpers::with_larger_debug_stack;
    //use roc_std::roclist;
    use roc_std::RocList;

    #[test]
    fn roc_list_construction() {
        let list = RocList::from_slice(&vec![1i64; 23]);
        assert_eq!(&list, &list);
    }

    #[test]
    fn empty_list_literal() {
        assert_evals_to!("[]", RocList::from_slice(&[]), RocList<i64>);
    }

    #[test]
    fn int_singleton_list_literal() {
        assert_evals_to!("[1, 2]", RocList::from_slice(&[1, 2]), RocList<i64>);
    }

    #[test]
    fn int_list_literal() {
        assert_evals_to!("[ 12, 9 ]", RocList::from_slice(&[12, 9]), RocList<i64>);
        assert_evals_to!(
            "[ 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ]",
            RocList::from_slice(&(vec![1i64; 23])),
            RocList<i64>
        );
    }

    #[test]
    fn bool_list_literal() {
        // NOTE: make sure to explicitly declare the elements to be of type bool, or
        // use both True and False; only using one of them causes the list to in practice be
        // of type `List [ True ]` or `List [ False ]`, those are tag unions with one constructor
        // and not fields, and don't have a runtime representation.
        assert_evals_to!(
            indoc!(
                r#"
                   false : Bool
                   false = False

                   [ false ]
                   "#
            ),
            RocList::from_slice(&(vec![false; 1])),
            RocList<bool>
        );

        assert_evals_to!(
            "[ True, False, True ]",
            RocList::from_slice(&[true, false, true]),
            RocList<bool>
        );

        assert_evals_to!(
            indoc!(
                r#"
                   false : Bool
                   false = False

                   [false ]
                   "#
            ),
            RocList::from_slice(&(vec![false; 1])),
            RocList<bool>
        );

        assert_evals_to!(
            indoc!(
                r#"
                   true : Bool
                   true = True

                   List.repeat 23 true
                   "#
            ),
            RocList::from_slice(&(vec![true; 23])),
            RocList<bool>
        );

        assert_evals_to!(
            indoc!(
                r#"
                   true : Bool
                   true = True

                   List.repeat 23 { x: true, y: true }
                   "#
            ),
            RocList::from_slice(&(vec![[true, true]; 23])),
            RocList<[bool; 2]>
        );

        assert_evals_to!(
            indoc!(
                r#"
                   true : Bool
                   true = True

                   List.repeat 23 { x: true, y: true, a: true, b: true, c: true, d : true, e: true, f: true }
                   "#
            ),
            RocList::from_slice(&(vec![[true, true, true, true, true, true, true, true]; 23])),
            RocList<[bool; 8]>
        );
    }

    #[test]
    fn variously_sized_list_literals() {
        assert_evals_to!("[]", RocList::from_slice(&[]), RocList<i64>);
        assert_evals_to!("[1]", RocList::from_slice(&[1]), RocList<i64>);
        assert_evals_to!("[1, 2]", RocList::from_slice(&[1, 2]), RocList<i64>);
        assert_evals_to!("[1, 2, 3]", RocList::from_slice(&[1, 2, 3]), RocList<i64>);
        assert_evals_to!(
            "[1, 2, 3, 4]",
            RocList::from_slice(&[1, 2, 3, 4]),
            RocList<i64>
        );
        assert_evals_to!(
            "[1, 2, 3, 4, 5]",
            RocList::from_slice(&[1, 2, 3, 4, 5]),
            RocList<i64>
        );
    }

    #[test]
    fn list_append() {
        assert_evals_to!(
            "List.append [1] 2",
            RocList::from_slice(&[1, 2]),
            RocList<i64>
        );
        assert_evals_to!(
            "List.append [1, 1] 2",
            RocList::from_slice(&[1, 1, 2]),
            RocList<i64>
        );
    }

    #[test]
    fn list_append_to_empty_list() {
        assert_evals_to!("List.append [] 3", RocList::from_slice(&[3]), RocList<i64>);
    }

    #[test]
    fn list_append_to_empty_list_of_int() {
        assert_evals_to!(
            indoc!(
                r#"
                    initThrees : List Int
                    initThrees =
                        []

                    List.append (List.append initThrees 3) 3
                "#
            ),
            RocList::from_slice(&[3, 3]),
            RocList<i64>
        );
    }

    #[test]
    fn list_append_bools() {
        assert_evals_to!(
            "List.append [ True, False ] True",
            RocList::from_slice(&[true, false, true]),
            RocList<bool>
        );
    }

    #[test]
    fn list_append_longer_list() {
        assert_evals_to!(
            "List.append [ 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22 ] 23",
            RocList::from_slice(&[11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]),
            RocList<i64>
        );
    }

    #[test]
    fn list_prepend() {
        assert_evals_to!("List.prepend [] 1", RocList::from_slice(&[1]), RocList<i64>);
        assert_evals_to!(
            "List.prepend [2] 1",
            RocList::from_slice(&[1, 2]),
            RocList<i64>
        );

        assert_evals_to!(
            indoc!(
                r#"
                    init : List Int
                    init =
                        []

                    List.prepend (List.prepend init 4) 6
                "#
            ),
            RocList::from_slice(&[6, 4]),
            RocList<i64>
        );
    }

    #[test]
    fn list_prepend_bools() {
        assert_evals_to!(
            "List.prepend [ True, False ] True",
            RocList::from_slice(&[true, true, false]),
            RocList<bool>
        );
    }

    #[test]
    fn list_prepend_big_list() {
        assert_evals_to!(
            "List.prepend [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 100, 100, 100, 100 ] 9",
            RocList::from_slice(&[
                9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 100, 100, 100, 100
            ]),
            RocList<i64>
        );
    }

    #[test]
    fn list_walk_right_empty_all_inline() {
        assert_evals_to!(
            indoc!(
                r#"
                List.walkRight [0x1] (\a, b -> a + b) 0
                "#
            ),
            1,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                empty : List Int
                empty =
                    []

                List.walkRight empty (\a, b -> a + b) 0
                "#
            ),
            0,
            i64
        );
    }

    #[test]
    fn list_keep_if_empty_list_of_int() {
        assert_evals_to!(
            indoc!(
                r#"
                empty : List Int
                empty =
                    []

                List.keepIf empty (\x -> True)
                "#
            ),
            RocList::from_slice(&[]),
            RocList<i64>
        );
    }

    #[test]
    fn list_keep_if_empty_list() {
        assert_evals_to!(
            indoc!(
                r#"
                alwaysTrue : Int -> Bool
                alwaysTrue = \_ -> 
                    True
                    

                List.keepIf [] alwaysTrue
                "#
            ),
            RocList::from_slice(&[]),
            RocList<i64>
        );
    }

    #[test]
    fn list_keep_if_always_true_for_non_empty_list() {
        assert_evals_to!(
            indoc!(
                r#"
                alwaysTrue : Int -> Bool
                alwaysTrue = \i ->
                    True
                    
                oneThroughEight : List Int
                oneThroughEight =
                    [1,2,3,4,5,6,7,8]
                    
                List.keepIf oneThroughEight alwaysTrue
                "#
            ),
            RocList::from_slice(&[1, 2, 3, 4, 5, 6, 7, 8]),
            RocList<i64>
        );
    }

    #[test]
    fn list_keep_if_always_false_for_non_empty_list() {
        assert_evals_to!(
            indoc!(
                r#"
                alwaysFalse : Int -> Bool
                alwaysFalse = \i ->
                    False
                    
                List.keepIf [1,2,3,4,5,6,7,8] alwaysFalse
                "#
            ),
            RocList::from_slice(&[]),
            RocList<i64>
        );
    }

    #[test]
    fn list_keep_if_one() {
        assert_evals_to!(
            indoc!(
                r#"
                intIsLessThanThree : Int -> Bool
                intIsLessThanThree = \i ->
                    i < 3
                    
                List.keepIf [1,2,3,4,5,6,7,8] intIsLessThanThree 
                "#
            ),
            RocList::from_slice(&[1, 2]),
            RocList<i64>
        );
    }

    //
    // "panicked at 'not yet implemented: Handle equals for builtin layouts Str == Str'"
    //
    // #[test]
    // fn list_keep_if_str_is_hello() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //             strIsHello : Str -> Bool
    //             strIsHello = \str ->
    //                 str == "Hello"
    //
    //             List.keepIf ["Hello", "Hello", "Goodbye"] strIsHello
    //             "#
    //         ),
    //         RocList::from_slice(&["Hello", "Hello"]),
    //         RocList<&'static str>
    //     );
    // }

    #[test]
    fn list_map_on_empty_list_with_int_layout() {
        assert_evals_to!(
            indoc!(
                r#"
                empty : List Int
                empty =
                    []

                List.map empty (\x -> x)
                "#
            ),
            RocList::from_slice(&[]),
            RocList<i64>
        );
    }

    #[test]
    fn list_map_on_non_empty_list() {
        assert_evals_to!(
            indoc!(
                r#"
                nonEmpty : List Int
                nonEmpty =
                    [ 1 ]

                List.map nonEmpty (\x -> x)
                "#
            ),
            RocList::from_slice(&[1]),
            RocList<i64>
        );
    }

    #[test]
    fn list_map_changes_input() {
        assert_evals_to!(
            indoc!(
                r#"
                nonEmpty : List Int
                nonEmpty =
                    [ 1 ]

                List.map nonEmpty (\x -> x + 1)
                "#
            ),
            RocList::from_slice(&[2]),
            RocList<i64>
        );
    }

    #[test]
    fn list_map_on_big_list() {
        assert_evals_to!(
            indoc!(
                r#"
                nonEmpty : List Int
                nonEmpty =
                    [ 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5 ]
    
                List.map nonEmpty (\x -> x * 2)
                "#
            ),
            RocList::from_slice(&[
                2, 4, 6, 8, 10, 2, 4, 6, 8, 10, 2, 4, 6, 8, 10, 2, 4, 6, 8, 10, 2, 4, 6, 8, 10
            ]),
            RocList<i64>
        );
    }

    #[test]
    fn list_map_with_type_change() {
        assert_evals_to!(
            indoc!(
                r#"
                nonEmpty : List Int
                nonEmpty =
                    [ 1, 1, -4, 1, 2 ]

    
                List.map nonEmpty (\x -> x > 0)
                "#
            ),
            RocList::from_slice(&[true, true, false, true, true]),
            RocList<bool>
        );
    }

    #[test]
    fn list_map_using_defined_function() {
        assert_evals_to!(
            indoc!(
                r#"
                 nonEmpty : List Int
                 nonEmpty =
                     [ 2, 2, -4, 2, 3 ]

                 greaterThanOne : Int -> Bool
                 greaterThanOne = \i ->
                     i > 1

                 List.map nonEmpty greaterThanOne
                 "#
            ),
            RocList::from_slice(&[true, true, false, true, true]),
            RocList<bool>
        );
    }

    #[test]
    fn list_map_all_inline() {
        assert_evals_to!(
            indoc!(
                r#"
                List.map [] (\x -> x > 0)
                "#
            ),
            RocList::from_slice(&[]),
            RocList<bool>
        );
    }

    #[test]
    fn list_join_empty_list() {
        assert_evals_to!("List.join []", RocList::from_slice(&[]), RocList<i64>);
    }

    #[test]
    fn list_join_one_list() {
        assert_evals_to!(
            "List.join [ [1, 2, 3 ] ]",
            RocList::from_slice(&[1, 2, 3]),
            RocList<i64>
        );
    }

    #[test]
    fn list_join_two_non_empty_lists() {
        assert_evals_to!(
            "List.join [ [1, 2, 3 ] , [4 ,5, 6] ]",
            RocList::from_slice(&[1, 2, 3, 4, 5, 6]),
            RocList<i64>
        );
    }

    #[test]
    fn list_join_two_non_empty_lists_of_float() {
        assert_evals_to!(
            "List.join [ [ 1.2, 1.1 ], [ 2.1, 2.2 ] ]",
            RocList::from_slice(&[1.2, 1.1, 2.1, 2.2]),
            RocList<f64>
        );
    }

    #[test]
    fn list_join_to_big_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    List.join
                        [
                            [ 1.2, 1.1 ],
                            [ 2.1, 2.2 ],
                            [ 3.0, 4.0, 5.0, 6.1, 9.0 ],
                            [ 3.0, 4.0, 5.0, 6.1, 9.0 ],
                            [ 3.0, 4.0, 5.0, 6.1, 9.0 ],
                            [ 3.0, 4.0, 5.0, 6.1, 9.0 ],
                            [ 3.0, 4.0, 5.0, 6.1, 9.0 ]
                        ]
                "#
            ),
            RocList::from_slice(&[
                1.2, 1.1, 2.1, 2.2, 3.0, 4.0, 5.0, 6.1, 9.0, 3.0, 4.0, 5.0, 6.1, 9.0, 3.0, 4.0,
                5.0, 6.1, 9.0, 3.0, 4.0, 5.0, 6.1, 9.0, 3.0, 4.0, 5.0, 6.1, 9.0
            ]),
            RocList<f64>
        );
    }

    #[test]
    fn list_join_defined_empty_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    empty : List Float
                    empty =
                        []

                    List.join [ [ 0.2, 11.11 ], empty ]
                "#
            ),
            RocList::from_slice(&[0.2, 11.11]),
            RocList<f64>
        );
    }

    #[test]
    fn list_join_all_empty_lists() {
        assert_evals_to!(
            "List.join [ [], [], [] ]",
            RocList::from_slice(&[]),
            RocList<f64>
        );
    }

    #[test]
    fn list_join_one_empty_list() {
        assert_evals_to!(
            "List.join [ [ 1.2, 1.1 ], [] ]",
            RocList::from_slice(&[1.2, 1.1]),
            RocList<f64>
        );
    }

    #[test]
    fn list_single() {
        assert_evals_to!("List.single 1", RocList::from_slice(&[1]), RocList<i64>);
        assert_evals_to!("List.single 5.6", RocList::from_slice(&[5.6]), RocList<f64>);
    }

    #[test]
    fn list_repeat() {
        assert_evals_to!(
            "List.repeat 5 1",
            RocList::from_slice(&[1, 1, 1, 1, 1]),
            RocList<i64>
        );
        assert_evals_to!(
            "List.repeat 4 2",
            RocList::from_slice(&[2, 2, 2, 2]),
            RocList<i64>
        );

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
            RocList::from_slice(&[4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]),
            RocList<i64>
        );
    }

    #[test]
    fn list_reverse() {
        assert_evals_to!(
            "List.reverse [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ]",
            RocList::from_slice(&[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]),
            RocList<i64>
        );
        assert_evals_to!(
            "List.reverse [1, 2, 3]",
            RocList::from_slice(&[3, 2, 1]),
            RocList<i64>
        );
        assert_evals_to!("List.reverse [4]", RocList::from_slice(&[4]), RocList<i64>);
    }

    #[test]
    fn list_reverse_empty_list_of_int() {
        assert_evals_to!(
            indoc!(
                r#"
                    emptyList : List Int
                    emptyList =
                        []

                    List.reverse emptyList
                "#
            ),
            RocList::from_slice(&[]),
            RocList<i64>
        );
    }

    #[test]
    fn list_reverse_empty_list() {
        assert_evals_to!("List.reverse []", RocList::from_slice(&[]), RocList<i64>);
    }

    #[test]
    fn foobarbaz() {
        assert_evals_to!(
            indoc!(
                r#"
                    firstList : List Int
                    firstList =
                        []

                    secondList : List Int
                    secondList =
                        []

                    List.concat firstList secondList
                "#
            ),
            RocList::from_slice(&[]),
            RocList<i64>
        );
    }

    #[test]
    fn list_concat_two_empty_lists() {
        assert_evals_to!("List.concat [] []", RocList::from_slice(&[]), RocList<i64>);
    }

    #[test]
    fn list_concat_two_empty_lists_of_int() {
        assert_evals_to!(
            indoc!(
                r#"
                    firstList : List Int
                    firstList =
                        []

                    secondList : List Int
                    secondList =
                        []

                    List.concat firstList secondList
                "#
            ),
            RocList::from_slice(&[]),
            RocList<i64>
        );
    }

    #[test]
    fn list_concat_second_list_is_empty() {
        assert_evals_to!(
            "List.concat [ 12, 13 ] []",
            RocList::from_slice(&[12, 13]),
            RocList<i64>
        );
        assert_evals_to!(
            "List.concat [ 34, 43 ] [ 64, 55, 66 ]",
            RocList::from_slice(&[34, 43, 64, 55, 66]),
            RocList<i64>
        );
    }

    #[test]
    fn list_concat_first_list_is_empty() {
        assert_evals_to!(
            "List.concat [] [ 23, 24 ]",
            RocList::from_slice(&[23, 24]),
            RocList<i64>
        );
    }

    #[test]
    fn list_concat_two_non_empty_lists() {
        assert_evals_to!(
            "List.concat [1, 2 ] [ 3, 4 ]",
            RocList::from_slice(&[1, 2, 3, 4]),
            RocList<i64>
        );
    }

    #[test]
    fn list_concat_two_bigger_non_empty_lists() {
        assert_evals_to!(
            "List.concat [ 1.1, 2.2 ] [ 3.3, 4.4, 5.5 ]",
            RocList::from_slice(&[1.1, 2.2, 3.3, 4.4, 5.5]),
            RocList<f64>
        );
    }

    fn assert_concat_worked(num_elems1: i64, num_elems2: i64) {
        let vec1: Vec<i64> = (0..num_elems1)
            .map(|i| 12345 % (i + num_elems1 + num_elems2 + 1))
            .collect();
        let vec2: Vec<i64> = (0..num_elems2)
            .map(|i| 54321 % (i + num_elems1 + num_elems2 + 1))
            .collect();
        let slice_str1 = format!("{:?}", vec1);
        let slice_str2 = format!("{:?}", vec2);
        let mut expected = vec1;

        expected.extend(vec2);

        assert_evals_to!(
            &format!("List.concat {} {}", slice_str1, slice_str2),
            RocList::from_slice(&expected),
            RocList<i64>
        );
    }

    #[test]
    fn list_concat_empty_list() {
        assert_concat_worked(0, 0);
        assert_concat_worked(1, 0);
        assert_concat_worked(2, 0);
        assert_concat_worked(3, 0);
        assert_concat_worked(4, 0);
        assert_concat_worked(7, 0);
        assert_concat_worked(8, 0);
        assert_concat_worked(9, 0);
        assert_concat_worked(25, 0);
        assert_concat_worked(150, 0);
        assert_concat_worked(0, 1);
        assert_concat_worked(0, 2);
        assert_concat_worked(0, 3);
        assert_concat_worked(0, 4);
        assert_concat_worked(0, 7);
        assert_concat_worked(0, 8);
        assert_concat_worked(0, 9);
        assert_concat_worked(0, 25);
        assert_concat_worked(0, 150);
    }

    #[test]
    fn list_concat_nonempty_lists() {
        assert_concat_worked(1, 1);
        assert_concat_worked(1, 2);
        assert_concat_worked(1, 3);
        assert_concat_worked(2, 3);
        assert_concat_worked(2, 1);
        assert_concat_worked(2, 2);
        assert_concat_worked(3, 1);
        assert_concat_worked(3, 2);
        assert_concat_worked(2, 3);
        assert_concat_worked(3, 3);
        assert_concat_worked(4, 4);
    }

    #[test]
    fn list_concat_large() {
        with_larger_debug_stack(|| {
            // these values produce mono ASTs so large that
            // it can cause a stack overflow. This has been solved
            // for current code, but may become a problem again in the future.
            assert_concat_worked(150, 150);
            assert_concat_worked(129, 350);
            assert_concat_worked(350, 129);
        })
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

    #[test]
    fn first_empty_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    when List.first [] is
                        Ok val -> val
                        Err _ -> -1
                "#
            ),
            -1,
            i64
        );
    }

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
            RocList::from_slice(&[12, 9, 33, 1, 5]),
            RocList<i64>
        );
    }

    #[test]
    fn set_unique_list_oob() {
        assert_evals_to!(
            "List.set [ 3, 17, 4.1 ] 1337 9.25",
            RocList::from_slice(&[3.0, 17.0, 4.1]),
            RocList<f64>
        );
    }

    #[test]
    fn set_shared_int_list() {
        assert_evals_to!(
            indoc!(
                r#"
                main = \shared -> 

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

                main [ 2.1, 4.3 ]
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
                main = \{} -> 
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

                main {}
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
            RocList::from_slice(&[3]),
            RocList<i64>
        );
    }

    #[test]
    fn gen_wrap_first() {
        assert_evals_to!(
            indoc!(
                r#"
                    wrapFirst = \list ->
                        [ List.first list ]

                    wrapFirst [ 1, 2 ]
                "#
            ),
            //            RocList::from_slice(&[1]),
            //            RocList<i64>
            &[1],
            &'static [i64]
        );
    }

    #[test]
    fn gen_duplicate() {
        assert_evals_to!(
            indoc!(
                r#"
                    # Duplicate the first element into the second index
                    dupe = \list ->
                        when List.first list is
                            Ok elem ->
                                List.set list 1 elem

                            _ ->
                                []

                    dupe [ 1, 2 ]
                "#
            ),
            RocList::from_slice(&[1, 1]),
            RocList<i64>
        );
    }

    #[test]
    fn gen_swap() {
        assert_evals_to!(
            indoc!(
                r#"
                    swap : Int, Int, List a -> List a
                    swap = \i, j, list ->
                        when Pair (List.get list i) (List.get list j) is
                            Pair (Ok atI) (Ok atJ) ->
                                list
                                    |> List.set i atJ
                                    |> List.set j atI

                            _ ->
                                []
                    swap 0 1 [ 1, 2 ]
                "#
            ),
            RocList::from_slice(&[2, 1]),
            RocList<i64>
        );
    }

    //    #[test]
    //    fn gen_partition() {
    //        assert_evals_to!(
    //            indoc!(
    //                r#"
    //                    swap : Int, Int, List a -> List a
    //                    swap = \i, j, list ->
    //                        when Pair (List.get list i) (List.get list j) is
    //                            Pair (Ok atI) (Ok atJ) ->
    //                                list
    //                                    |> List.set i atJ
    //                                    |> List.set j atI
    //
    //                            _ ->
    //                                []
    //                    partition : Int, Int, List (Num a) -> [ Pair Int (List (Num a)) ]
    //                    partition = \low, high, initialList ->
    //                        when List.get initialList high is
    //                            Ok pivot ->
    //                                when partitionHelp (low - 1) low initialList high pivot is
    //                                    Pair newI newList ->
    //                                        Pair (newI + 1) (swap (newI + 1) high newList)
    //
    //                            Err _ ->
    //                                Pair (low - 1) initialList
    //
    //
    //                    partitionHelp : Int, Int, List (Num a), Int, Int -> [ Pair Int (List (Num a)) ]
    //                    partitionHelp = \i, j, list, high, pivot ->
    //                        if j < high then
    //                            when List.get list j is
    //                                Ok value ->
    //                                    if value <= pivot then
    //                                        partitionHelp (i + 1) (j + 1) (swap (i + 1) j list) high pivot
    //                                    else
    //                                        partitionHelp i (j + 1) list high pivot
    //
    //                                Err _ ->
    //                                    Pair i list
    //                        else
    //                            Pair i list
    //
    //                    # when partition 0 0 [ 1,2,3,4,5 ] is
    //                    # Pair list _ -> list
    //                    [ 1,3 ]
    //                "#
    //            ),
    //            RocList::from_slice(&[2, 1]),
    //            RocList<i64>
    //        );
    //    }

    //    #[test]
    //    fn gen_partition() {
    //        assert_evals_to!(
    //            indoc!(
    //                r#"
    //                    swap : Int, Int, List a -> List a
    //                    swap = \i, j, list ->
    //                        when Pair (List.get list i) (List.get list j) is
    //                            Pair (Ok atI) (Ok atJ) ->
    //                                list
    //                                    |> List.set i atJ
    //                                    |> List.set j atI
    //
    //                            _ ->
    //                                []
    //                    partition : Int, Int, List (Num a) -> [ Pair Int (List (Num a)) ]
    //                    partition = \low, high, initialList ->
    //                        when List.get initialList high is
    //                            Ok pivot ->
    //                                when partitionHelp (low - 1) low initialList high pivot is
    //                                    Pair newI newList ->
    //                                        Pair (newI + 1) (swap (newI + 1) high newList)
    //
    //                            Err _ ->
    //                                Pair (low - 1) initialList
    //
    //
    //                    partitionHelp : Int, Int, List (Num a), Int, Int -> [ Pair Int (List (Num a)) ]
    //
    //                    # when partition 0 0 [ 1,2,3,4,5 ] is
    //                    # Pair list _ -> list
    //                    [ 1,3 ]
    //                "#
    //            ),
    //            RocList::from_slice(&[2, 1]),
    //            RocList<i64>
    //        );
    //    }

    #[test]
    fn gen_quicksort() {
        with_larger_debug_stack(|| {
            assert_evals_to!(
                indoc!(
                    r#"
                    quicksort : List (Num a) -> List (Num a)
                    quicksort = \list ->
                        n = List.len list
                        quicksortHelp list 0 (n - 1)


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


                    partitionHelp : Int, Int, List (Num a), Int, (Num a) -> [ Pair Int (List (Num a)) ]
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
                RocList::from_slice(&[4, 7, 19, 21]),
                RocList<i64>
            );
        })
    }

    #[test]
    fn foobar2() {
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
                           # if j < high then
                           if False then
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
                RocList::from_slice(&[19, 7, 4, 21]),
                RocList<i64>
            );
        })
    }

    #[test]
    fn foobar() {
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
    
    
    
                       when List.first (quicksort [0x1]) is
                           _ -> 4
                   "#
                ),
                4,
                i64
            );
        })
    }

    #[test]
    fn empty_list_increment_decrement() {
        assert_evals_to!(
            indoc!(
                r#"
                x : List Int
                x = []

                List.len x + List.len x
                "#
            ),
            0,
            i64
        );
    }

    #[test]
    fn list_literal_increment_decrement() {
        assert_evals_to!(
            indoc!(
                r#"
                x : List Int
                x = [1,2,3]

                List.len x + List.len x
                "#
            ),
            6,
            i64
        );
    }

    #[test]
    fn list_pass_to_function() {
        assert_evals_to!(
            indoc!(
                r#"
                x : List Int
                x = [1,2,3]

                id : List Int -> List Int
                id = \y -> y

                id x
                "#
            ),
            RocList::from_slice(&[1, 2, 3]),
            RocList<i64>
        );
    }

    #[test]
    fn list_pass_to_set() {
        assert_evals_to!(
            indoc!(
                r#"
                x : List Int
                x = [1,2,3]

                id : List Int -> List Int
                id = \y -> List.set y 0 0

                id x
                "#
            ),
            RocList::from_slice(&[0, 2, 3]),
            RocList<i64>
        );
    }

    #[test]
    fn list_wrap_in_tag() {
        assert_evals_to!(
            indoc!(
                r#"
                id : List Int -> [ Pair (List Int) Int ]
                id = \y -> Pair y 4

                when id [1,2,3] is
                    Pair v _ -> v
                "#
            ),
            RocList::from_slice(&[1, 2, 3]),
            RocList<i64>
        );
    }
}
