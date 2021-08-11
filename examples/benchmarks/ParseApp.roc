app "parseapp"
     packages { base: "platform" }
     imports [base.Task,
        Parser.{succesful, runToString, showU8, any, satisfy, first, second, map, andThen },
        Test]
     provides [ main ] to base

main : Task.Task {} []
main =
    run = \input, parser -> runToString showU8 input parser 

    p1 = {name : "run \"abcd any => \"a\"", test: run "abcd" any == "a" }

    satisfyA = satisfy (\u -> u == 97)
    satisfyB = satisfy (\u -> u == 98)
    satisfyWhatCameBefore = \u2 -> Parser.satisfy (\u3 -> u3 == u2)

    p2 = {name : "run \"abcd\" satisfy (\\u -> u == 97)) => \"a\"", test : run "abcd" satisfyA == "a" }
    p3 = {name : "Use 'second' to recognize \"a\" then \"b\" returning \"b\"", test : run "abcd" (second  satisfyA satisfyB) == "b"}
    p4 = {name : "Use 'first' to recognize \"a\" then \"b\" returning \"a\"", test : run "abcd" (first  satisfyA satisfyB) == "a"}
    p5 = {name : "Use map to shift output of parser: run \"abcd\" (map any (\\u -> u + 25)) == \"z\"", test : run "abcd" (map any (\u -> u + 25)) == "z"  }
    p6 = {name: "Use andThen to recognize strings beginning with two repeated letters (succeed on input \"aaxyz\")", test: run "aaxyz" (andThen any satisfyWhatCameBefore) == "a"}
    p7 = {name: "is successful (positive)", test: List.len ( satisfyA [97, 98, 99, 100] ) == 1}
    p8 = {name: "is successful (negative)", test: List.len ( satisfyA [100, 98, 99, 100] ) != 1}
    # p7 = {name: "is successful (simple, positive)", test: successful []}
    
    [Test.eval p1, Test.eval p2, Test.eval p3, Test.eval p4, Test.eval p5, Test.eval p6, Test.eval p7, Test.eval p8] 
       |> Test.strListToStr "\n"
       |> Task.putLine

#    The below does not work:
#
#    List.map [p1, p2, p3, p4, p5, p6] Test.eval
#       |> Test.strListToStr "\n"
#       |> Task.putLine