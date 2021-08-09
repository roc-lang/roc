interface MiniParsec exposes [ showPair, showPair2, makePair2, testPair2,
   Parser, result, testResult,
   zero,  testZero] imports []


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

result : a -> Parser a
result = \v -> (\inp -> [Pair v inp])

testResult : Str, Str -> Str
testResult = 
   \a, b -> 
      when (result a) (Str.toUtf8 b) |> List.map showPair2 |> List.first is
        Ok str -> str
        _ -> "Oops, something happened"

# result: succeed without consuming input
#
# » ((\v -> (\inp -> [Pair v inp])) "a") "xyz"
# [ Pair "a" "xyz" ] : List [ Pair Str Str ]*
#


# zero: always fail
#
zero = \_ -> [ ]


testZero = \s -> 
    when zero s == [] is 
        True -> "Ok"
        False -> "Error"
   
# The item parser, which successfully consumes one character, and which otherwise fails. In Haskell:
# item = \inp  -> case inp of 
#  [ ] -> [ ]
#  (x:xs) -> [(x, xs)]

