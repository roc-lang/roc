app "app"
     packages { base: "platform" }
     imports [base.Task, Pair, Test]
     provides [ main ] to base
    

## TESTS 

inc = \x -> x + 1
p1 = Pair 1 0

t1 = {name: "mapFirst, first", test: (Pair.mapFirst p1 inc |> Pair.first) == 2 }
t2 = {name: "mapSecond, second", test: (Pair.mapSecond p1 inc |> Pair.second) == 1 }
t3 = {name: "map2, first coordinate", test: (Pair.map2 p1 inc inc|> Pair.first) == 2 }
t4 = {name: "map2, second coordinate", test: (Pair.map2 p1 inc inc|> Pair.second) == 1 }


suite = {name: "Test Pair interface", tests: [t1,t2,t3, t4] }

main : Task.Task {} []
main =
  Test.runSuite suite
    |> Task.putLine