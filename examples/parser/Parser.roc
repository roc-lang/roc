interface Parser exposes [ 
    Parser, run,  successful, 
    succeed, any,  satisfy, fail,
    map, andThen, oneOf, oneOfResult,
    first, second,
    lowerCase, manyAux, many, tests,
    runToString
  ] imports [Pair]


## PARSER

Parser a : List U8 -> List [Pair a (List U8)]


run : (List U8), Parser a -> List ([Pair a (List U8)])
run = 
  \input, parser -> parser input


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
#
second : Parser a, Parser b -> Parser b
second = 
  \p, q ->  Parser.andThen p (\_ -> q)

# Run p, then q, returning output of p and ignoring that of q
#
first : Parser a, Parser b -> Parser a
first = 
  \p, q ->  Parser.andThen p (\out -> Parser.map q (\_ -> out))




## ONE OF  

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



## SUCCESSFUL (Test for successful parse)

# successful : List ([Pair a (List U8)]) -> Bool
successful = \results -> List.len results == 1



## TESTS  

tests = [q1, q2, q3]

q1 = {name: "test of oneOf combinator", test: List.len oneOfResult == 1 }     
q2 = {name: "Parser.run [97,98,99] (manyAux lowerCase [ ]) => Pair ((Loop [97]) [98,99])", test: Parser.run [97,98,99] (manyAux lowerCase [ ]) == [Pair (Loop [97]) [98,99]]}
q3 = {name: "many lowerCase", test: Parser.run [97, 99, 100, 0] (many lowerCase) == [Pair [97, 98, 99] [0]] }


# Test for oneof

satisfyA = satisfy (\u -> u == 97)
satisfyB = satisfy (\u -> u == 98)
oneOfResult = (oneOf [satisfyA, satisfyB]) [97, 98, 99, 100]  

isLowerCaseAlpha : U8 -> Bool
isLowerCaseAlpha = \u -> u >= 97 && u <= 122
lowerCase = satisfy isLowerCaseAlpha

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
