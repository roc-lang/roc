interface Parser exposes [  showPair, makePair, testPair, 
   Parser, map, andThen,
   succeed, testSucceed,
   item, testItem,
   fail,  testFail] imports [Pair]


## PAIRS

showPair : [Pair Str (List U8)] -> Str
showPair = \(Pair a b) -> 
    when Str.fromUtf8 b is 
        Ok bb -> Str.concat (Str.concat a "::") bb
        _ -> "Could not convert (List U8)"

showU8 : U8 -> Str
showU8 = 
   \u -> when Str.fromUtf8 [u] is 
      Ok str -> str
      _ -> "Oops, could not convert U8"

showPair2 : [Pair U8 (List U8)] -> Str
showPair2 = \(Pair a b) -> 
    when Str.fromUtf8 b is 
        Ok bb -> 
            Str.concat (Str.concat (showU8 a) "::") bb
        _ -> "Could not convert (List U8)"

makePair : Str, Str -> [Pair Str (List U8)]
makePair = 
   \a, b -> Pair a (Str.toUtf8 b)

testPair : Str, Str -> Str
testPair  = 
  \a, b -> makePair a b |> showPair



## PARSERS

Parser a : List U8 -> List ([Pair a (List U8)])

run : Str, Parser a -> Result a [ListWasEmpty]
run = 
  \str, parser -> parser (String.toUtf8) |> List.map Pair.first  |> List.first


## RESULT 

# succeed: succeed without consuming input
succeed : a -> Parser a
succeed = \v -> (\inp -> [Pair v inp])

testSucceed : Str, Str -> Str
testSucceed = 
   \a, b -> 
      when (succeed a) (Str.toUtf8 b) |> List.map showPair |> List.first is
        Ok str -> str
        _ -> "Oops, something happened"


## ZERO

# fail: always fail
#
fail = \_ -> [ ]


testFail = \s -> 
    when fail s == [] is 
        True -> "Ok"
        False -> "Error"

## ITEM 

# The item parser, which successfully consumes one character, and which otherwise fails. In Haskell:
# item = \inp  -> case inp of 
#  [ ] -> [ ]
#  (x:xs) -> [(x, xs)]

item: Parser U8
item = \inp -> 
   when List.first inp is 
     Ok u -> [Pair u (List.drop inp 1)]
     _ -> [ ]


testItem: Str -> Str 
testItem = 
  \input -> when item (Str.toUtf8 input) |> List.map showPair2 |> List.first is
        Ok str -> str
        _ -> "Oops, something went wrong"
    

## map

map : Parser a, (a -> b) -> Parser b  
map = 
  \p, f -> \input -> p input |> List.map (\pair -> Pair.mapFirst pair f) 


andThen : Parser a, (a -> Parser b) -> Parser b 
andThen = \p, f ->
            \input -> p input |> List.map (\(Pair a list) -> (f a) list) |> List.join

