app "parseapp"
     packages { base: "platform" }
     imports [base.Task,   Parser, Test]
     provides [ main ] to base

main : Task.Task {} []
main =
 
Parser.tests
   |> Test.run "Parser test"
   |> Task.putLine





