interface MiniParsec exposes [ showPair, showPair2, makePair2, testPair2,
   Parser, result2,
   result, zero, testResult, testZero] imports []


## PAIRS

showPair : [Pair Str Str] -> Str
showPair = \(Pair a b) -> Str.concat (Str.concat a "::") b

showPair2 : [Pair Str (List U8)] -> Str
showPair2 = \(Pair a b) -> 
    when Str.fromUtf8 b is 
        Ok bb -> Str.concat (Str.concat a "::") bb
        _ -> "Could not convert (List U8)"

makePair2 : Str, Str -> [Pair Str (List U8)]
makePair2 = 
   \a, b -> Pair a (Str.toUtf8 b)

testPair2 : Str, Str -> Str
testPair2  = 
  \a, b -> makePair2 a b |> showPair2

## PARSERS

Parser a : List U8 -> List ([Pair a (List U8)])

result2 : a -> Parser a
result2 = \v -> (\inp -> [Pair v inp])

# result: succeed without consuming input
#
# » ((\v -> (\inp -> [Pair v inp])) "a") "xyz"
# [ Pair "a" "xyz" ] : List [ Pair Str Str ]*
#
result : a -> (b -> List [ Pair a b ]*)
result = \v -> (\inp -> [Pair v inp])

# zero: always fail
#
zero = \_ -> [ ]


testZero = \s -> 
    when zero s == [] is 
        True -> "Ok"
        False -> "Error"
   
testResult =  \a, input -> 
    when ((result a) input |> List.first) is
        Ok pair -> showPair pair
        _ -> "Error"

# The item parser, which successfully consumes one character, and which otherwise fails. In Haskell:
# item = \inp  -> case inp of 
#  [ ] -> [ ]
#  (x:xs) -> [(x, xs)]

