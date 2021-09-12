app "parseapp"
     packages { base: "platform" }
     imports [base.Task, 
        Parser2.{runU8, any, satisfy, andThen },
        Test]
     provides [ main ] to base


anyT = {name : "(1) run \"abcd any => \"a\"", test: runU8 "abcd" any == "a" }

satisfyT = {name : "(2) run \"abcd\" satisfy (\\u -> u == 97)) => \"a\"", test : runU8 "abcd" (satisfy (\u -> u == 97)) == "a" }

andThenT = { name: "(3) andThen: recognize strings with repeated first character", test: runU8 "aaxyz" (andThen any satisfyWhatCameBefore) == "a"}
satisfyWhatCameBefore = \u2 -> satisfy (\u3 -> u3 == u2)

satisfyA = satisfy (\u -> u == 97)
satisfyB = satisfy (\u -> u == 98)



suite1 = [andThenT]
suite2 = [anyT, satisfyT, andThenT]

main : Task.Task {} []
main =
 
suite2 |> Test.run "Parser test"
   |> Task.putLine





