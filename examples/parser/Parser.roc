interface Parser exposes [ 
    Parser, run,  successful, 
    succeed, any,  satisfy, fail,
    map, andThen, oneOf, oneOfResult,
    first, second,
    runToString
  ] imports [Pair, Loop]


## PARSERS

Parser a : List U8 -> List ([Pair a (List U8)])


run : (List U8), Parser a -> List ([Pair a (List U8)])
run = 
  \input, parser -> parser input


# for educational purposes
oneOfOLD : List (Parser a) -> Parser a 
oneOfOLD = \parserList -> 
          \input -> when List.first parserList is 
              Ok p -> 
                output = p input 
                if List.len output == 1 then output else (oneOfOLD (List.drop parserList 1)) input 
              Err _ -> [ ]
              
oneOf = \parserList ->
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



satisfyA = satisfy (\u -> u == 97)
satisfyB = satisfy (\u -> u == 98)
oneOfResult = (oneOf [satisfyA, satisfyB]) [97, 98, 99, 100]  


## MANY

# many : Parser a -> Parser (List a)
# many p =
#     Parser.loop [] (manyHelp p)


# manySeparatedBy : Parser () -> Parser a -> Parser (List a)
# manySeparatedBy sep p =
#     manyNonEmpty_ p (second sep p)


# manyHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
# manyHelp p vs =
#     Parser.oneOf
#         [ Parser.end EndOfInput |> Parser.map (\_ -> Parser.Done (List.reverse vs))
#         , Parser.succeed (\v -> Parser.Loop (v :: vs))
#             |= p
#         , Parser.succeed ()
#             |> Parser.map (\_ -> Parser.Done (List.reverse vs))
#         ]

manyAux : Parser a, List a -> Parser (Loop.Step (List a) (List a))


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




