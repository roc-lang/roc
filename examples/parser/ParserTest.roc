app "parseapp"
     packages { base: "platform" }
     imports [base.Task, 
        Parser.{succesful, runToString, any, satisfy, first, second, 
        map, andThen, oneOf, isLowerCaseAlpha },
        Test]
     provides [ main ] to base

main : Task.Task {} []
main =
 
Parser.testsP |> Test.run "Parser test"
   |> Task.putLine





