app "Tester"
    packages { base: "platform" }
    imports [base.Task,  Test]
    provides [ main ] to base

main : Task.Task {} []
main =
    t1 = { name : "1 + 1 = 2", test: 1 + 1 == 2 }
    t2 = { name : "2 + 2 = 5", test: 2 + 2 == 5 }
    # Loop.test1
    # Loop.test2
    # Loop.test3
    #
    # The first three lines below succeed, the last one fails: emitted runtime error ...
    # Test.evalTest t1
    # Test.evalTest t2
    # Test.strListToStr ["a", "b", "c"] ","
    # List.map [t1,t2] Test.evalTest |> Test.strListToStr ","
    #
    # ATTEMPTS TO DIAGNOSE THE PROBLEM
    # List.map [t1,t2] (\t -> t.name) |> Test.strListToStr "," # FAIL (THIS IS THE ONE TO SOLVE)
    List.map [1,2,3] (\x -> x + 1) |> List.map Str.fromInt |> Test.strListToStr ","
    # (\t -> t.name) t1                                        # SUCCEED
    # (\t -> t.name) t2                                        # SUCCEED
    # List.map [t1,t2] (\t -> t.name) |> (\_ -> "TEST")        # FAIL
    # "FOO" |> (\s -> s)                                         # SUCCEED
    # "BAR" |> (\_ -> "TEST")                                  # SUCCEED
       |> Task.putLine

