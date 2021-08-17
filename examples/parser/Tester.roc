app "foo"
    packages { base: "platform" }
    imports [base.Task, Test,  StrExtra]
    provides [ main ] to base

 
main : Task.Task {} []
main =
  Test.run StrExtra.tests "StrExtra" |> Task.putLine 

  