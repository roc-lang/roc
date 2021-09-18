interface Parser exposes [ 
    Parser, run,  runU8,
    succeed, fail, any, satisfy,
    andThen, first, second,
    map, oneOf, oneOfNew, many, manyAux
  ] imports [Pair, Utility]

## PARSER  

Parser a : List U8 -> List [Pair a (List U8)]


run : (List U8), Parser a -> List ([Pair a (List U8)])
run = 
  \input, parser -> parser input

runU8 : Str, Parser U8 -> Str
runU8 = \input, parser -> runToString Utility.showU8 input parser 

## SUCCEED 

# succeed: succeed without consuming input
succeed : a -> Parser a
succeed = \v -> (\inp -> [Pair v inp])

## FAIL

# fail: always fail
#
fail = \_ -> [ ]



## ANY 

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


## AND_THEN, FIRST, SECOND 

andThen : Parser a, (a -> Parser b) -> Parser b 
andThen = \p, q ->
            \input -> p input |> List.map (\(Pair a input2) -> (q a) input2) |> List.join


# Run p, then q, returning output of q and ignoring that of p
#
second : Parser a, Parser b -> Parser b
second = 
  \p, q ->  andThen p (\_ -> q)

# Run p, then q, returning output of p and ignoring that of q
#
first : Parser a, Parser b -> Parser a
first = 
  \p, q ->  andThen p (\out -> map q (\_ -> out))



## ONE OF

oneOfNew = \parserList ->
    \input ->
        List.walkUntil parserList
            (\p, accum ->
                output = p input
                if List.len output == 1 then
                    Stop output

                else
                    Continue accum
            )
            []

oneOf : List (Parser a) -> Parser a 
oneOf = \parserList -> 
          \input -> when List.first parserList is 
              Ok p -> 
                output = p input 
                if List.len output == 1 then output else (oneOf (List.drop parserList 1)) input 
              Err _ -> [ ]


## MANY

many : Parser a -> Parser (List a)
many = \p -> 
    \input -> (loop (\list -> manyAux p list) [])  input  

Step state a : [ Loop state, Done a ]

loop: ((List a) -> Parser (Step (List a) (List a))), (List a) -> Parser (List a) 
loop = \nextState, s ->
  \input -> 
      ps =  nextState s                 # Parser (Step (List a)(List a))
      out = ps input 
      if List.len out == 1 then
        when List.first out is 
          Ok (Pair (Done aa) input2) -> [(Pair aa input2)]
          Ok (Pair (Loop aa) input2) -> (loop nextState aa) input2
          Err _ -> [(Pair [] input)]
      else
         [(Pair [] input)]
 

manyAux : Parser a, List a -> Parser (Step (List a) (List a))
manyAux = \p, list ->
    \input -> (if input == [] 
              then 
                succeed (Done (List.reverse list))
              else 
                p1 = andThen p ( \a -> succeed (Loop (List.prepend list a))) # succeed (Done (List.reverse list)) #
                p2 = succeed (Done (List.reverse list))
                (oneOf [ p1, p2 ])) input


###############################################


## FOR STRING OUTPUT 

runAux : Str, Parser a -> Result a [ListWasEmpty]
runAux = 
  \str, parser -> parser (Str.toUtf8 str) |> List.map Pair.first  |> List.first

runToString : (a -> Str), Str, Parser a -> Str
runToString = 
  \toString, str, parser -> 
    when runAux str parser is
      Ok a -> toString a 
      _ -> "Parse error (runToString)"