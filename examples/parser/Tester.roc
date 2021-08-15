app "app"
    packages { base: "platform" }
    imports [base.Task,  Test]
    provides [ main ] to base

main : Task.Task {} []
main = 
    

    t1 = {name : "1 + 1 == 2", test: 1 + 1 == 2}

    t2 = {name: "Bozo", test: 1 + 1 == 0} 

    [t1,t2] |> Test.run "Test of test interface"  |> Task.putLine

    # [t1,t2] |> Test.runF "Test of test interface, failures only"  |> Task.putLine



  