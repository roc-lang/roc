interface MiniParsec exposes [  showPair, makePair, testPair,
   Parser, result, testResult,
   zero,  testZero] imports []


## PAIRS

showPair : [Pair Str (List U8)] -> Str
showPair = \(Pair a b) -> 
    when Str.fromUtf8 b is 
        Ok bb -> Str.concat (Str.concat a "::") bb
        _ -> "Could not convert (List U8)"

makePair : Str, Str -> [Pair Str (List U8)]
makePair = 
   \a, b -> Pair a (Str.toUtf8 b)

testPair : Str, Str -> Str
testPair  = 
  \a, b -> makePair a b |> showPair

## PARSERS

Parser a : List U8 -> List ([Pair a (List U8)])

# result: succeed without consuming input
result : a -> Parser a
result = \v -> (\inp -> [Pair v inp])

testResult : Str, Str -> Str
testResult = 
   \a, b -> 
      when (result a) (Str.toUtf8 b) |> List.map showPair |> List.first is
        Ok str -> str
        _ -> "Oops, something happened"


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

