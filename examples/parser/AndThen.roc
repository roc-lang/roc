
app "app"
     packages { base: "platform" }
     imports [base.Task] 
     provides [ main ] to base


## PARSER STUFF

Parser a : List U8 -> List [Pair a (List U8)]

any: Parser U8
any = \inp -> 
   when List.first inp is 
     Ok u -> [Pair u (List.drop inp 1)]
     _ -> [ ]

satisfy : (U8 -> Bool) -> Parser U8  
satisfy = \predicate -> 
    \input -> when List.first (any input) is
        Ok (Pair u rest) ->  if predicate u then List.single (Pair u rest) else []
        _ -> [ ]

andThen : Parser a, (a -> Parser b) -> Parser b 
andThen = \p, q ->
            \input -> p input |> List.map (\(Pair a input2) -> (q a) input2) |> List.join


## FOR STRING OUTPUT 

pairFirst : [Pair a b] -> a 
pairFirst = \(Pair a _) -> a 

runAux : Str, Parser a -> Result a [ListWasEmpty]
runAux = 
  \str, parser -> parser (Str.toUtf8 str) |> List.map pairFirst  |> List.first

runToString : (a -> Str), Str, Parser a -> Str
runToString = 
  \toString, str, parser -> 
    when runAux str parser is
      Ok a -> toString a 
      _ -> "Parse error (runToString)"

showU8 : U8 -> Str
showU8 = 
   \u -> when Str.fromUtf8 [u] is 
      Ok str -> str
      _ -> "Oops, could not convert U8" 

showBool : Bool -> Str
showBool = \b -> if b then "Ok" else "Fail"

## TEST  

satisfyWhatCameBefore = \u2 -> satisfy (\u3 -> u3 == u2)

testAndThen = runToString showU8 "aaxyz" (andThen any satisfyWhatCameBefore) == "a" |> showBool


main : Task.Task {} []
main =
testAndThen
    |> Task.putLine  

