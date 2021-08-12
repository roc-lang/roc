app "Tester"
    packages { base: "platform" }
    imports [base.Task,  Utility, Test]
    provides [ main ] to base

main : Task.Task {} []
main =
    t1 = {name : "isEven", test: Utility.isEven 4 }

    filteredList = Utility.filterList [1,2,3,4,5,6] isEven
    t2 = {name : "filterList", test: List.len filteredList == 3}

    Test.run [t1, t2] 
      |> Task.putLine

