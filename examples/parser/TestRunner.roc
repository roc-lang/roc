app "testRunner"
     packages { base: "platform" }
     imports [base.Task, Test, Pair] 
     provides [ main ] to base

main : Task.Task {} []
main =
   
    Test.run Pair.tests
    |> 
      Task.putLine 