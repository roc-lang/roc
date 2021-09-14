app "parseapp"
     packages { base: "platform" }
     imports [base.Task, 
        Parser2.{runU8, any, satisfy, andThen, second, first, map, oneOf },
        Test]
     provides [ main ] to base


# Definition of the tests

anyT = {name : "(a) run \"abcd any => \"a\"", test: runU8 "abcd" any == "a" }

satisfyT = {name : "(b) run \"abcd\" satisfy (\\u -> u == 97)) => \"a\"", test : runU8 "abcd" (satisfy (\u -> u == 97)) == "a" }

andThenT = { name: "(c) andThen: recognize strings with repeated first character", test: runU8 "aaxyz" (andThen any satisfyWhatCameBefore) == "a"}
satisfyWhatCameBefore = \u2 -> satisfy (\u3 -> u3 == u2)

satisfyA = satisfy (\u -> u == 97)
satisfyB = satisfy (\u -> u == 98)

secondT = {name : "(d) Use 'second' to recognize strings with second character 'b'", test : runU8 "abcd" (second  any satisfyB) == "b"}
firstT = {name : "(e) Use 'first' to recognize strings beginning with 'a' and with any second character", test : runU8 "abcd" (first  satisfyA any) == "a"}

secondT2 = {name : "(d) Use 'second' to recognize strings  beginning with 'a' followed by 'b' returning 'b'", test : runU8 "abcd" (second  satisfyA satisfyB) == "b"}
firstT2 = {name : "(e) Use 'first' to recognize strings beginning with 'a' followed by 'b' returning 'a'", test : runU8 "abcd" (first  satisfyA satisfyB) == "a"}


mapT = {name : "(f) Use map to shift output of parser: run \"abcd\" (map any (\\u -> u + 25)) == \"z\"", test : runU8 "abcd" (map any (\u -> u + 25)) == "z"  }

oneOfT1 = {name: "(g) test of oneOf combinator: recognize string beginning with 'a' or 'b', for 'abcd'", test: List.len ((oneOf [satisfyA, satisfyB]) [97, 98, 99, 100] ) == 1 }
oneOfT2 = {name: "(h) test of oneOf combinator: recognize string beginning with 'a' or 'b', for 'bcda'", test: List.len ((oneOf [satisfyA, satisfyB]) [98, 99, 100, 97] ) == 1 }
oneOfT3 = {name: "(i) test of oneOf combinator: recognize string beginning with 'a' or 'b', for 'cde' (fail)", test: List.len ((oneOf [satisfyA, satisfyB]) [99, 100, 101] ) == 0 }


# Test suites

suiteOneOf = {name: "OneOf", tests: [oneOfT1, oneOfT2, oneOfT3]}
suiteAll = {name: "All 7 combinators", tests: [anyT, satisfyT, andThenT, secondT, firstT, secondT2, firstT2, mapT, oneOfT1,oneOfT2, oneOfT3 ] }


main : Task.Task {} []
main =

Test.runSuites [suiteAll]
# Test.runSuites [suiteOneOf]

   |> Task.putLine





