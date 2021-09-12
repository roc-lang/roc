app "parseapp"
     packages { base: "platform" }
     imports [base.Task, Parser2, Test]
     provides [ main ] to base

main : Task.Task {} []
main =
 
Parser2.tests |> Test.run "Parser2 test"
# Parser2.mapT2
# "hello"
   |> Task.putLine





