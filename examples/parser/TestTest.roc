app "testRunner"
     packages { base: "platform" }
     imports [base.Task, Test] 
     provides [ main ] to base

main : Task.Task {} []
main =
    Test.run Test.validate2
    |> Task.putLine 