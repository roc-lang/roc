interface Pair exposes [  
      first, second, 
      mapFirst, mapSecond, map2,
      tests ] imports []


first : [Pair a b] -> a 
first = \(Pair a _) -> a 

second : [Pair a b] -> b  
second = \(Pair _ b) -> b
 

mapFirst : [Pair a x], (a -> b) -> [Pair b x]
mapFirst = 
  \(Pair a b), f -> (Pair (f a) b)

mapSecond : [Pair a x], (x -> y) -> [Pair a y]
mapSecond = 
  \(Pair a b), f -> (Pair a (f b))

map2 : [Pair a b], (a -> x), (b -> y) -> [Pair x y]
map2 = 
  \(Pair a b), f, g -> (Pair (f a) (g b))


## TESTS 

inc = \x -> x + 1
p1 = Pair 1 0

t1 = {name: "mapFirst, first", test: (Pair.mapFirst p1 inc |> Pair.first) == 2 }
t2 = {name: "mapSecond, second", test: (Pair.mapSecond p1 inc |> Pair.second) == 1 }
t3 = {name: "map2, first coordinate", test: (Pair.map2 p1 inc inc|> Pair.first) == 2 }
t4 = {name: "map2, second coordinate", test: (Pair.map2 p1 inc inc|> Pair.second) == 1 }

tests = [t1, t2, t3, t4]





