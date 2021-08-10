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
    List.map [t1,t2] evalTest |> Test.strListToStr ","
       |> Task.putLine

