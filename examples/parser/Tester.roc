app "foo"
    packages { base: "platform" }
    imports [base.Task,  Console, StrExtra]
    provides [ main ] to base

 
main : Task.Task {} []
main =
  StrExtra.concat [Console.green "The grass is green, but ", Console.red "roses are red."] |> Task.putLine 

  