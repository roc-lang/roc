app "parseapp"
     packages { base: "platform" }
     imports [base.Task, Parser2, Test]
     provides [ main ] to base

main : Task.Task {} []
main =
 
Parser2.testAndThen |> Test.run "Parser2 testAndThen"
# Parser2.mapT2
# "hello"
   |> Task.putLine





