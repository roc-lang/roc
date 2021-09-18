# File: ManyTest.roc  
#
# The saga continues! Try `cargo run` on `ManyTest.roc` from place in which
# you have a suitable platform.  You will get the following panic message, where the line 
# beginning with "thread" extends far to the right#
#
# ➜  examples git:(jim-parser) ✗ cargo run parser/ManyTest.roc
#    Finished dev [unoptimized + debuginfo] target(s) in 1.01s
#     Running `/Users/jxxcarlson/dev/roc/roc/target/debug/roc parser/ManyTest.roc`
# thread 'main' panicked at 'internal error: entered unreachable code: symbol/layout `#UserApp.0` ProcLayout { arguments: [Builtin(List(Builtin(Int8))), LambdaSet(LambdaSet { set: [(`#UserApp.0`, [Union(NonRecursive([[Builtin(List(Builtin(Int8)))], [Builtin(List(Builtin(Int8)))]]))]), (`#UserApp.6`, [LambdaSet(LambdaSet { set: [(`#UserApp.3`, [LambdaSet(LambdaSet { set: [(`#UserApp.isLowerCaseAlpha`, [])], representation: Struct([]) })])], representation: Struct([LambdaSet(LambdaSet { set: [(`#UserApp.isLowerCaseAlpha`, [])], representation: Struct([]) })]) }), LambdaSet(LambdaSet { set: [(`#UserApp.10`, [Builtin(List(Builtin(Int8)))])], representation: Struct([Builtin(List(Builtin(Int8)))]) })])], representation: Union(NonRecursive([[Union(NonRecursive([[Builtin(List(Builtin(Int8)))], [Builtin(List(Builtin(Int8)))]]))], [LambdaSet(LambdaSet { set: [(`#UserApp.10`, [Builtin(List(Builtin(Int8)))])], representation: Struct([Builtin(List(Builtin(Int8)))]) }), LambdaSet(LambdaSet { set: [(`#UserApp.3`, [LambdaSet(LambdaSet { set: [(`#UserApp.isLowerCaseAlpha`, [])], representation: Struct([]) })])], representation: Struct([LambdaSet(LambdaSet { set: [(`#UserApp.isLowerCaseAlpha`, [])], representation: Struct([]) })]) })]])) })], result: Builtin(List(Struct([Union(NonRecursive([[Builtin(List(Builtin(Int8)))], [Builtin(List(Builtin(Int8)))]])), Builtin(List(Builtin(Int8)))]))) } combo must be in DeclarationToIndex', compiler/mono/src/borrow.rs:227:9
# : run with `RUST_BACKTRACE=1` environment variable to display a backtrace
#
# Sorry for the length of this file but there are quite a few prerequisites for the definition of the `manyAux` function
#


app "app"
     packages { base: "platform" }
     imports [base.Task]
     provides [ main ] to base



## PARSER

Parser a : List U8 -> List [Pair a (List U8)]


## SUCCEED 

# succeed: succeed without consuming input
succeed : a -> Parser a
succeed = \v -> (\inp -> [Pair v inp])


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


## ONE_OF

oneOf : List (Parser a) -> Parser a 
oneOf = \parserList -> 
          \input -> when List.first parserList is 
              Ok p -> 
                output = p input 
                if List.len output == 1 then output else (oneOf (List.drop parserList 1)) input 
              Err _ -> [ ]


## AND_THEN

andThen : Parser a, (a -> Parser b) -> Parser b 
andThen = \p, q ->
            \input -> p input |> List.map (\(Pair a input2) -> (q a) input2) |> List.join


## MANY


Step state a : [ Loop state, Done a ]


manyAux : Parser a, List a -> Parser (Step (List a) (List a))
manyAux = \p, list ->
    \input -> (if input == [] 
              then 
                succeed (Done (List.reverse list))
              else 
                p1 = andThen p ( \a -> succeed (Loop (List.prepend list a))) 
                p2 = succeed (Done (List.reverse list))
                (oneOf [ p1, p2 ])) input


## TEST

isLowerCaseAlpha : U8 -> Bool
isLowerCaseAlpha = \u -> u >= 97 && u <= 122
lowerCase = satisfy isLowerCaseAlpha

dummyTest = 1 + 1 == 2
manyAuxTest =  (manyAux lowerCase [ ]) [97,98,99] == [Pair (Loop [97]) [98,99]]

runTest = \t -> if t then "PASS" else "FAIL"

main : Task.Task {} []
main =
  runTest manyAuxTest
   |> Task.putLine