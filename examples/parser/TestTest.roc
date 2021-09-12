app "app"
     packages { base: "platform" }
     imports [base.Task, Test]
     provides [ main ] to base


t1 = {name: "1 + 1 == 2", test: 1 + 1 == 2 }
t1b  = {name: "1 + 1 == 3", test: 1 + 1 == 3 }
t2 = {name: "List.len [1,2,3] == 3", test: List.len [1,2,3] == 3}
t3 = {name: "Extract field", test: (\t -> t.test) t1 == True}

a1 = {name: "1 + 1 == 2", test: 1 + 1 == 2 }
a2 = {name: "test: 2 * 3 == 6", test: 2 * 3 == 6 }
a2b = {name: "2 * 3 == 5", test: 2 * 3 == 5 }
a3 = {name: "7 - 5 == 2", test: 7 - 5 == 2 }
a4 = {name: "7 / 5 == Ok 1.4", test: 7 / 5 == Ok 1.4 }


suite1 = {name: "Simple tests for the test framework", tests: [t1,t2,t3]}
suite2 = {name: "Arithmetic", tests: [a1, a2, a3, a4]}

suite1b = {name: "Tests with failures", tests: [t1b, t2, t3]}
suite2b = {name: "Arithmetic", tests: [a1, a2b, a3, a4]}

main : Task.Task {} []
main =
  # Test.run [t1, t2, t3] "Bozo"
  # Test.runSuite  suite3
  # Test.runSuites [suite1, suite2]
  #
  # Test.runF [t1b, t2, t3] "Bozo"
  # Test.runSuiteF suite1b
  Test.runSuitesF [suite1b, suite2b]
    |> Task.putLine
