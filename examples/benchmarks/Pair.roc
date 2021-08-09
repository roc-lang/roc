interface Pair exposes [  
      first, second, 
      mapFirst, mapSecond, 
      showIntPair, testFirst,testSecond ] imports []


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


## TEST 

showIntPair = \(Pair a b) -> Str.concat (Str.concat (Str.fromInt a) ", ") (Str.fromInt b)


testFirst = 
  \a, b -> mapFirst (Pair a b) inc  |> showIntPair


testSecond = 
  \a, b -> mapSecond (Pair a b) inc  |> showIntPair  

inc = \x -> x + 1

