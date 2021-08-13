app "testPair"
     packages { base: "platform" }
     imports [base.Task, Test]
     provides [ main ] to base

main : Task.Task {} []
main =
   
    Task.putLine "Hello!"