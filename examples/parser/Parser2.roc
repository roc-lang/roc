interface Parser2 exposes [ 
    Parser, run, 
    # succeed, fail, any
    tests
  ] imports [Pair, Utility]

## PARSER  

Parser a : List U8 -> List [Pair a (List U8)]


run : (List U8), Parser a -> List ([Pair a (List U8)])
run = 
  \input, parser -> parser input

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



satisfyA = satisfy (\u -> u == 97)
satisfyB = satisfy (\u -> u == 98)


## AND_THEN, FIRST, SECOND 

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




###############################################


## TESTS

anyT = {name : "run \"abcd any => \"a\"", test: runU8 "abcd" any == "a" }

satisfyT = {name : "run \"abcd\" satisfy (\\u -> u == 97)) => \"a\"", test : runU8 "abcd" satisfyA == "a" }

satisfyA = satisfy (\u -> u == 97)
satisfyB = satisfy (\u -> u == 98)

satisfyWhatCameBefore = \u2 -> satisfy (\u3 -> u3 == u2)

testAndThen = { name: "andThen", test: runToString Utility.showU8 "aaxyz" (andThen any satisfyWhatCameBefore) == "a"}

## PANIC
secondT = {name : "Use 'second' to recognize \"a\" then \"b\" returning \"b\"", test : runU8 "abcd" (second  satisfyA satisfyB) == "b"}
firstT = {name : "Use 'first' to recognize \"a\" then \"b\" returning \"a\"", test : runU8 "abcd" (first  satisfyA satisfyB) == "a"}



tests = [ anyT, satisfyT, testAndThen]



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