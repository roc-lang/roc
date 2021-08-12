app "Tester"
    packages { base: "platform" }
    imports [base.Task,  Utility, Test]
    provides [ main ] to base

main : Task.Task {} []
main =
    t1 = {name : "isEven", test: Utility.isEven 4 }

    Test.run [t1] 
      |> Task.putLine

