interface Parser exposes [  showPair, makePair, testPair, 
   runToString, showU8,
   Parser, run, successful, map, andThen, first, second,
   succeed, any,  satisfy, fail] imports [Pair]


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


## PARSERS

Parser a : List U8 -> List ([Pair a (List U8)])


run : (List U8), Parser a -> List ([Pair a (List U8)])
run = 
  \input, parser -> parser input


runAux : Str, Parser a -> Result a [ListWasEmpty]
runAux = 
  \str, parser -> parser (Str.toUtf8 str) |> List.map Pair.first  |> List.first

runToString : (a -> Str), Str, Parser a -> Str
runToString = 
  \toString, str, parser -> 
    when runAux str parser is
      Ok a -> toString a 
      _ -> "Parse error (runToString)"


## SUCCESSFUL (Test for successful parse)

# successful : List ([Pair a (List U8)]) -> Bool
successful = \results -> List.len results == 1


## SUCCEED 

# succeed: succeed without consuming input
succeed : a -> Parser a
succeed = \v -> (\inp -> [Pair v inp])


## FAIL

# fail: always fail
#
fail = \_ -> [ ]


## ITEM 

# If succcessful, the any parser consumes one character

any: Parser U8
any = \inp -> 
   when List.first inp is 
     Ok u -> [Pair u (List.drop inp 1)]
     _ -> [ ]


## SATISFY  

satisfy : (U8 -> Bool) -> Parser U8  
satisfy = \predicate -> 
    \input -> when List.first (any input) is
        Ok (Pair u rest) ->  if predicate u then List.single (Pair u rest) else []
        _ -> [ ]

## MAP

map : Parser a, (a -> b) -> Parser b  
map = 
  \p, f -> \input -> p input |> List.map (\pair -> Pair.mapFirst pair f) 



## AND_THEN  

andThen : Parser a, (a -> Parser b) -> Parser b 
andThen = \p, q ->
            \input -> p input |> List.map (\(Pair a input2) -> (q a) input2) |> List.join


# Run p, then q, returning output of q and ignoring that of p
second : Parser a, Parser b -> Parser b
second = 
  \p, q ->  Parser.andThen p (\_ -> q)

# Run p, then q, returning output of p and ignoring that of q
first : Parser a, Parser b -> Parser a
first = 
  \p, q ->  Parser.andThen p (\out -> Parser.map q (\_ -> out))




## TESTS  
  

testPair : Str, Str -> Str
testPair  = 
  \a, b -> makePair a b |> showPair
