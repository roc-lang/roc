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
        ]


testMove : Direction -> TokenPath -> TokenPath
testMove d =
    move d testState


getTokenAtTestState : TokenPath -> Token
getTokenAtTestState p =
    getTokenAtPath p testState
