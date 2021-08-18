app "app"
     packages { base: "platform" }
     imports [base.Task, Test] 
     provides [ main ] to base

main : Task.Task {} []
main =
  Test.run Test.testList "Test the Test library"
    |> Task.putLine 