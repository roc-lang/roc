app "parseapp"
     packages { base: "platform" }
     imports [base.Task, 
        Parser.{succesful, runToString, any, satisfy, first, second, map, andThen, oneOf, oneOfResult, isLowerCaseAlpha },
        Test, Utility]
     provides [ main ] to base

main : Task.Task {} []
main =
    run = \input, parser -> runToString Utility.showU8 input parser 

    p1 = {name : "run \"abcd any => \"a\"", test: run "abcd" any == "a" }

    satisfyA = satisfy (\u -> u == 97)
    satisfyB = satisfy (\u -> u == 98)
    satisfyWhatCameBefore = \u2 -> Parser.satisfy (\u3 -> u3 == u2)

    satisfyResult = satisfyA [97, 98, 99, 100]

 

    p2 = {name : "run \"abcd\" satisfy (\\u -> u == 97)) => \"a\"", test : run "abcd" satisfyA == "a" }
    p3 = {name : "Use 'second' to recognize \"a\" then \"b\" returning \"b\"", test : run "abcd" (second  satisfyA satisfyB) == "b"}
    p4 = {name : "Use 'first' to recognize \"a\" then \"b\" returning \"a\"", test : run "abcd" (first  satisfyA satisfyB) == "a"}
    p5 = {name : "Use map to shift output of parser: run \"abcd\" (map any (\\u -> u + 25)) == \"z\"", test : run "abcd" (map any (\u -> u + 25)) == "z"  }
    p6 = {name: "Use andThen to recognize strings beginning with two repeated letters (succeed on input \"aaxyz\")", test: run "aaxyz" (andThen any satisfyWhatCameBefore) == "a"}
    p7 = {name: "is successful (positive)", test: List.len satisfyResult == 1}
    p8 = {name: "is successful (negative)", test: List.len ( satisfyA [100, 98, 99, 100] ) != 1}
    p9 = {name: "test of oneOf combinator", test: List.len oneOfResult == 1 }
    # p10 = {name: "test (2) of oneOf combinator", test: List.len ((oneOf [satisfyA, satisfyB]) [97, 98, 99, 100]) == 1}
   
 
   
 
    Test.run [p1, p2, p3, p4, p5, p6, p7, p8, p9] 
      |> Task.putLine