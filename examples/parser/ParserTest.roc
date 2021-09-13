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

mapT = {name : "(f) Use map to shift output of parser: run \"abcd\" (map any (\\u -> u + 25)) == \"z\"", test : runU8 "abcd" (map any (\u -> u + 25)) == "z"  }

oneOfT1 = {name: "(g) test of oneOf combinator: recognize string beginning with 'a' or 'b', for 'abcd'", test: List.len ((oneOf [satisfyA, satisfyB]) [97, 98, 99, 100] ) == 1 }
oneOfT2 = {name: "(h) test of oneOf combinator: recognize string beginning with 'a' or 'b', for 'bcda'", test: List.len ((oneOf [satisfyA, satisfyB]) [98, 99, 100, 97] ) == 1 }
oneOfT3 = {name: "(i) test of oneOf combinator: recognize string beginning with 'a' or 'b', for 'cde' (fail)", test: List.len ((oneOf [satisfyA, satisfyB]) [99, 100, 101] ) == 0 }


oneOfResult = (oneOf [satisfyA, satisfyB]) [97, 98, 99, 100] 

# Test suites

suite1a =  {name: "1. anyT", tests: [anyT]}
suite1b =  {name: "2. satisfyT", tests: [satisfyT]}
suite1c =  {name: "3. andThen", tests: [andThenT]}
suite1d =  {name: "4. second", tests: [secondT]}
suite1e=  {name: "5. first", tests: [firstT]}
suite1f=  {name: "6. map", tests: [mapT]}

suite3 =  {name: "First three combinators", tests: [anyT, satisfyT, andThenT, ] }
suite4 = {name: "First four combinators", tests: [anyT, satisfyT, andThenT, secondT]}
suite5 =  {name: "First five combinators", tests: [anyT, satisfyT, andThenT, secondT, firstT] }
suite6 =  {name: "All six combinators", tests: [anyT, satisfyT, andThenT, secondT, firstT, mapT] }

suiteAll = {name: "All 7 combinators", tests: [anyT, satisfyT, andThenT, secondT, firstT, mapT, oneOfT1,oneOfT2, oneOfT3 ] }

# NOTE.  All of the functions imported from module Parser2 pass their inidividua respective
#        test. (Use suit1 with substitutions).  However, these tests do **not** necessarily
#        pass when used in combination with others.  Weird!


main : Task.Task {} []
main =
 
# Test.runSuite suite1a
# Test.runSuite suite1b
# Test.runSuite suite1c  ## Try this one (test (c) succeeds)
# Test.runSuite suite1d  ## Try this one (test (d) succeeds)
# Test.runSuite suite1e

# Test.runSuite suite1f

# Test.runSuites [suite1a, suite1b, suite1c, suite1d, suite1e, suite1f, suite4 ] ## Try this one (tests (c) and (d) fail)

# Test.runSuites [suite3] 
# Test.runSuites [suite4]
# Test.runSuites [suite5]
# Test.runSuites [suite6]
Test.runSuites [suiteAll]
   |> Task.putLine





