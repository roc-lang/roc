app "testRunner"
     packages { base: "platform" }
     imports [base.Task, Test, Utility] 
     provides [ main ] to base

main : Task.Task {} []
main =
   
    Test.run Utility.tests
    |> Task.putLine 