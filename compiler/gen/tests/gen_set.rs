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
mod gen_set {
    // use roc_std::RocStr;

    #[test]
    fn empty_len() {
        assert_evals_to!(
            indoc!(
                r#"
                Set.len Set.empty
                "#
            ),
            0,
            usize
        );
    }

    #[test]
    fn singleton_len() {
        assert_evals_to!(
            indoc!(
                r#"
                Set.len (Set.singleton 42)
                "#
            ),
            1,
            usize
        );
    }

    #[test]
    fn singleton_to_list() {
        assert_evals_to!(
            indoc!(
                r#"
                Set.toList (Set.singleton 42)
                "#
            ),
            &[42],
            &[i64]
        );

        assert_evals_to!(
            indoc!(
                r#"
                Set.toList (Set.singleton 1)
                "#
            ),
            &[1],
            &[i64]
        );

        assert_evals_to!(
            indoc!(
                r#"
                Set.toList (Set.singleton 1.0)
                "#
            ),
            &[1.0],
            &[f64]
        );
    }

    #[test]
    fn insert() {
        assert_evals_to!(
            indoc!(
                r#"
                Set.empty
                    |> Set.insert 0
                    |> Set.insert 1
                    |> Set.insert 2
                    |> Set.toList
                "#
            ),
            &[0, 1, 2],
            &[i64]
        );
    }

    #[test]
    fn remove() {
        assert_evals_to!(
            indoc!(
                r#"
                Set.empty
                    |> Set.insert 0
                    |> Set.insert 1
                    |> Set.remove 1
                    |> Set.remove 2
                    |> Set.toList
                "#
            ),
            &[0],
            &[i64]
        );
    }

    #[test]
    fn union() {
        assert_evals_to!(
            indoc!(
                r#"
                fromList : List a -> Set a
                fromList = \list -> List.walk list (\x, a -> Set.insert a x) Set.empty

                set1 : Set I64
                set1 = fromList [1,2]

                set2 : Set I64
                set2 = fromList [1,3,4] 

                Set.union set1 set2
                    |> Set.toList
                "#
            ),
            &[4, 2, 3, 1],
            &[i64]
        );
    }

    #[test]
    fn difference() {
        assert_evals_to!(
            indoc!(
                r#"
                fromList : List a -> Set a
                fromList = \list -> List.walk list (\x, a -> Set.insert a x) Set.empty

                set1 : Set I64
                set1 = fromList [1,2]

                set2 : Set I64
                set2 = fromList [1,3,4] 

                Set.difference set1 set2
                    |> Set.toList
                "#
            ),
            &[2],
            &[i64]
        );
    }

    #[test]
    fn intersection() {
        assert_evals_to!(
            indoc!(
                r#"
                fromList : List a -> Set a
                fromList = \list -> List.walk list (\x, a -> Set.insert a x) Set.empty

                set1 : Set I64
                set1 = fromList [1,2]

                set2 : Set I64
                set2 = fromList [1,3,4] 

                Set.intersection set1 set2
                    |> Set.toList
                "#
            ),
            &[1],
            &[i64]
        );
    }

    #[test]
    fn walk_sum() {
        assert_evals_to!(
            indoc!(
                r#"
                fromList : List a -> Set a
                fromList = \list -> List.walk list (\x, a -> Set.insert a x) Set.empty


                Set.walk (fromList [1,2,3]) (\x, y -> x + y) 0
                "#
            ),
            6,
            i64
        );
    }

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
