app "testRunner"
     packages { base: "platform" }
     imports [base.Task, Test, Loop] 
     provides [ main ] to base

main : Task.Task {} []
main =
  Test.run Loop.tests
    |> Task.putLine 