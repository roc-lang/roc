# My latest travails with parser code (though the situation is now MUCH better.  Thanks all!)
# See the remarks just before the tests.
# This file is self-contained.

app "app"
     packages { base: "platform" }
     imports [base.Task]
     provides [ main ] to base



## PARSER

Parser a : List U8 -> List [Pair a (List U8)]


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


oneOf : List (Parser a) -> Parser a 
oneOf = \parserList -> 
          \input -> when List.first parserList is 
              Ok p -> 
                output = p input 
                if List.len output == 1 then output else (oneOf (List.drop parserList 1)) input 
              Err _ -> [ ]


satisfyA = satisfy (\u -> u == 97) # recognize 97
satisfyB = satisfy (\u -> u == 98) # recognize 98


# In all the below, the given parser supposedly recognize sequence beginning with 97 or 98
# The return value of a parser in this case has type 'List [Pair U8 (List U8)]'
# Thus parse results can be empty (failure), of length 1 (succes), or of length
# greater than 1 (multi-valued, failure).  The parse result is the first element of Pair
# 
# In theory, all of the below should succeed, but tests 2 and 3 fail.
# Maybe it is my code ?? Or maybe the compiler.  Dunno.  The code for the 
# oneOf combinator is pretty straightforward.

# For a fascinating panic and error message, replace (for example) 'satisfyB' by 'any'

test1 = if List.len ((oneOf [satisfyA, satisfyB]) [97, 98, 99, 100] ) == 1  then "PASS" else "FAIL"
test2 = if List.len ((oneOf [satisfyA, satisfyB]) [98, 99, 100, 97] ) == 1  then "PASS" else "FAIL"
test3 = if List.len ((oneOf [satisfyB , satisfyA]) [98, 99, 100, 97] ) == 1  then "PASS" else "FAIL"
test4 = if List.len ((oneOf [satisfyA, satisfyB]) [99, 100, 101] ) == 0  then "PASS" else "FAIL"


main : Task.Task {} []
main =
  [test1, test2, test3, test4] |> Str.joinWith ", "
   |> Task.putLine