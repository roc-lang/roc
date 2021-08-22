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
##


## TESTS

anyT = {name : "run \"abcd any => \"a\"", test: runU8 "abcd" any == "a" }

dummy = {name: "1 + 1 == 2", test: 1 + 1 == 2}

tests = [dummy, anyT]



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