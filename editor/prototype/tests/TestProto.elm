module TestProto exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Prototype exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Editor Prototype"
        [ describe "Moving With Keyboard"
            [ test "Down" <|
                \_ ->
                    testMove Down End
                        |> Expect.equal (Next 0 End)
            , test "Down + Up = Same place" <|
                \_ ->
                    testMove Down End
                        |> testMove Up
                        |> Expect.equal End
            , test "Down + Down = Select whole record" <|
                \_ ->
                    testMove Down End
                        |> testMove Down
                        |> Expect.equal (Next 1 End)
            , test "3 x Down = Back to root" <|
                \_ ->
                    testMove Down End
                        |> testMove Down
                        |> testMove Down
                        |> Expect.equal End
            , test "Up = Select whole record" <|
                \_ ->
                    testMove Up End
                        |> Expect.equal (Next 1 End)
            , test "Up + Up = Select State record" <|
                \_ ->
                    testMove Up End
                        |> testMove Up
                        |> Expect.equal (Next 0 End)
            , test "3x Up = Select State record" <|
                \_ ->
                    testMove Up End
                        |> testMove Up
                        |> testMove Up
                        |> Expect.equal End
            , test "Right" <|
                \_ ->
                    testMove Right End
                        |> Expect.equal (Next 0 End)
            , test "Right + Left = Same Place" <|
                \_ ->
                    testMove Right End
                        |> testMove Left
                        |> Expect.equal End
            , test "Right + Right = Drill down" <|
                \_ ->
                    testMove Right End
                        |> testMove Right
                        |> Expect.equal (Next 0 (Next 0 End))
            , test "Right selects next sibiling in horizontal group" <|
                \_ ->
                    testMove Down End
                        |> testMove Down
                        |> testMove Right
                        |> testMove Right
                        |> testMove Right
                        |> Expect.equal (Next 1 (Next 1 (Next 2 End)))
            , test "Down selects first child in group if in horizontal group " <|
                \_ ->
                    testMove Down End
                        |> testMove Down
                        |> testMove Right
                        |> testMove Down
                        |> testMove Right
                        |> testMove Right
                        |> testMove Down
                        |> Expect.equal (Next 1 (Next 2 (Next 2 (Next 0 End))))
            ]
        , let
            t =
                token (FnName "view")

            a =
                syntax ":"

            b =
                token (TypeName "Model")

            c =
                token (TypeName "Result")
          in
          describe "Modifying tree given the commands"
            [ test "Add Child to a group" <|
                \_ ->
                    modify (AddChild Vert t) End (treeRoot [])
                        |> Expect.equal (treeRoot [ t ])
            , test "Add Child to a Edge" <|
                \_ ->
                    modify (AddChild Hori t) (Next 0 End) (treeRoot [ empty ])
                        |> Expect.equal (treeRoot [ group Hori [ t ] ])
            , test "Add Sibiling to a root Token " <|
                \_ ->
                    modify (AddSibiling t) End empty
                        |> Expect.equal (treeRoot [ empty, t ])
            , test "Add Sibiling to a root Group" <|
                \_ ->
                    modify (AddSibiling t) End (treeRoot [])
                        |> Expect.equal (treeRoot [ treeRoot [], t ])
            , test "Add Sibiling to a Child Atom " <|
                \_ ->
                    modify (AddSibiling t) (Next 1 End) (treeRoot [ empty, empty ])
                        |> Expect.equal (treeRoot [ empty, empty, t ])
            , test "Add Sibiling to a Child Group " <|
                \_ ->
                    modify (AddSibiling t) (Next 0 End) (treeRoot [ empty, treeRoot [] ])
                        |> Expect.equal (treeRoot [ empty, t, treeRoot [] ])
            , test "Add Sibiling to a deep group " <|
                \_ ->
                    modify (AddSibiling t)
                        (Next 1 (Next 1 End))
                        (treeRoot [ empty, treeRoot [ empty, a, b, c ] ])
                        |> Expect.equal
                            (treeRoot [ empty, treeRoot [ empty, a, t, b, c ] ])
            , test "Replace Token with a token" <|
                \_ ->
                    modify (Replace t) End (treeRoot [ empty ])
                        |> Expect.equal t
            , test "Replace Token deep in tree" <|
                \_ ->
                    modify (Replace t)
                        (Next 0 (Next 1 End))
                        (treeRoot [ group Vert [ a, b, c ] ])
                        |> Expect.equal (treeRoot [ group Vert [ a, t, c ] ])
            , test "Cut token at root empties it" <|
                \_ ->
                    modify Cut End (treeRoot [])
                        |> Expect.equal empty
            , test "Cut token at deeper path" <|
                \_ ->
                    modify Cut
                        (Next 0 (Next 1 End))
                        (treeRoot [ group Vert [ a, b, c ] ])
                        |> Expect.equal (treeRoot [ group Vert [ a, c ] ])
            ]
        , let
            t =
                token (FnName "view")

            a =
                syntax ":"

            b =
                token (TypeName "Model")

            c =
                token (TypeName "Result")

            p =
                token
                    (ParamName "model")

            tree =
                treeRoot
                    [ group Hori [ t, a, b, syntax "->", c ]
                    , group Hori [ t, syntax "=", syntax "\\", p ]
                    ]

            testMovePath txt cmd path exp =
                test txt <|
                    \_ ->
                        movePath cmd path tree
                            |> Expect.equal exp
          in
          describe "Moving to a new path if possible on a token"
            [ testMovePath "Move To Next Sibiling"
                ToNextSibiling
                (Next 0 End)
                (Just (Next 1 End))
            , testMovePath "Dont move to sibiling if against edge"
                ToNextSibiling
                (Next 1 End)
                Nothing
            , testMovePath "Dont move to prev sibiling if against edge"
                ToPrevSibiling
                (Next 0 End)
                Nothing
            , testMovePath "Move To Prev Sibiling"
                ToPrevSibiling
                (Next 1 End)
                (Just (Next 0 End))
            , testMovePath "Move To Parent"
                ToParent
                (Next 1 End)
                (Just End)
            , testMovePath "Move To First Child"
                ToFirstChild
                (Next 1 End)
                (Just (Next 1 (Next 0 End)))
            , testMovePath "Move To Last Child"
                ToLastChild
                (Next 1 End)
                (Just (Next 1 (Next 3 End)))
            ]
        , let
            c =
                Caret "" "" False ""

            path =
                End

            t =
                token Empty

            st =
                ( c, path, t )

            tst txt xs exp =
                test txt <|
                    \_ ->
                        List.foldl onKeyboardEvent st xs
                            |> Expect.equal exp
          in
          describe "On Keyboard Event"
            [ tst "Press Shift"
                [ Shift ]
                ( { c | shiftDown = True }, path, t )
            , tst "Press shift, lift up shift"
                [ Shift, LiftUp ]
                ( c, path, t )
            , tst "Press some uppercase"
                [ Shift
                , Char 's'
                , LiftUp
                ]
                ( { c | after = "S" }, path, t )
            , tst "write a bit"
                [ Shift
                , Char 's'
                , LiftUp
                , Char 't'
                , Char 'a'
                , Char 't'
                , Char 'e'
                ]
                ( { c | after = "State" }, path, t )
            , tst "Change the tree structure"
                [ Shift
                , Char 's'
                , LiftUp
                , Char 't'
                , Char 'a'
                , Char 't'
                , Char 'e'
                , Char ' '
                , Shift
                , Char ':'
                , LiftUp
                ]
                ( c, Next 1 End, treeRoot [ group Hori [ token (TypeName "State"), syntax ":" ], token Empty ] )
            ]
        ]


testMove : Direction -> TokenPath -> TokenPath
testMove d =
    move d testState


getTokenAtTestState : TokenPath -> Token
getTokenAtTestState p =
    getTokenAtPath p testState
