app "parseapp"
     packages { base: "platform" }
     imports [base.Task, 
        Parser.{succesful, runToString, any, satisfy, first, second, 
        map, andThen, oneOf, oneOfResult, isLowerCaseAlpha, manyAux },
        Test, Utility]
     provides [ main ] to base

main : Task.Task {} []
main =
    run = \input, parser -> runToString Utility.showU8 input parser 

    p1 = {name : "run \"abcd any => \"a\"", test: run "abcd" any == "a" }

    satisfyA = satisfy (\u -> u == 97)
    satisfyB = satisfy (\u -> u == 98)
    satisfyWhatCameBefore = \u2 -> Parser.satisfy (\u3 -> u3 == u2)

    satisfyResult = satisfyA [97, 98, 99, 100]

# loop : (state -> Parser (Step state a)), state -> Parser a 
# loop = \nextState, s ->
#   \input -> 
#       ss =  (nextState s)
#       when ss is 
#         Loop ss -> 
#           out = ss input
#           if List.len out == 1 then
#             Loop nextState ss
#           else Done (\input -> out)
#         Done aa -> (\input -> success aa)


    p2 = {name : "run \"abcd\" satisfy (\\u -> u == 97)) => \"a\"", test : run "abcd" satisfyA == "a" }
    p3 = {name : "Use 'second' to recognize \"a\" then \"b\" returning \"b\"", test : run "abcd" (second  satisfyA satisfyB) == "b"}
    p4 = {name : "Use 'first' to recognize \"a\" then \"b\" returning \"a\"", test : run "abcd" (first  satisfyA satisfyB) == "a"}
    p5 = {name : "Use map to shift output of parser: run \"abcd\" (map any (\\u -> u + 25)) == \"z\"", test : run "abcd" (map any (\u -> u + 25)) == "z"  }
    p6 = {name: "Use andThen to recognize strings beginning with two repeated letters (succeed on input \"aaxyz\")", test: run "aaxyz" (andThen any satisfyWhatCameBefore) == "a"}
    p7 = {name: "is successful (positive)", test: List.len satisfyResult == 1}
    p8 = {name: "is successful (negative)", test: List.len ( satisfyA [100, 98, 99, 100] ) != 1}
    p9 = {name: "test of lowerCase parser with u = 97", test: run "abcd" Parser.lowerCase == "a" } 

    q1 = {name: "test of oneOf combinator", test: List.len oneOfResult == 1 }
      
 
    [p1, p2, p3, p4, p5, p6, p7, p8, p9, q1]  |> Test.run "Parser test"
    ##  Parser.q2 <== Bug above if 'Parser.q2' is used.  Notes below
      |> Task.putLine


# NOTES.  Parser.q2 is test of 'Parser.manyAux.  If 'Parser.q2` is omitted from the list [p1, p2, ...], the code runs 
# without incident.  Hence the code for 'q2' type-checks.  However, if 'Parser.q2' in included in the test list,
# the app panics:

#   ➜  examples git:(dev) ✗ cargo run parser/ParserTest.roc
#    Finished dev [unoptimized + debuginfo] target(s) in 0.90s
#     Running `/Users/jxxcarlson/dev/roc/roc/target/debug/roc parser/ParserTest.roc`
# thread 'main' panicked at 'internal error: entered unreachable code: symbol/layout combo must be in DeclarationToIndex', compiler/mono/src/borrow.rs:223:9
# stack backtrace:


