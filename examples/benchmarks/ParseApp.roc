app "parseapp"
    packages { base: "platform" }
    imports [base.Task, Parser]
    provides [ main ] to base

main : Task.Task {} []
main =
    # Parser.runToString Parser.showU8 "abcd" Parser.any
    # Parser.runToString Parser.showU8 "abcd" (Parser.satisfy (\_ -> True))
    # Parser.runToString Parser.showU8 "abcd" (Parser.satisfy (\u -> u == 97))
    # Parser.runToString Parser.showU8 "abcd" (  Parser.andThen (Parser.satisfy (\u -> u == 97)) (\u2 -> Parser.satisfy (\u3 -> u3 == 98))  )
    # Recognize "a" followed by "b", returning "b"
    # Parser.runToString Parser.showU8 "abcd" (  Parser.second  (Parser.satisfy (\u -> u == 97)) (Parser.satisfy (\u -> u == 98))  )
    # Recognize "a" followed by "b", returning "a"
    Parser.runToString Parser.showU8 "abcd" (  Parser.first  (Parser.satisfy (\u -> u == 97)) (Parser.satisfy (\u -> u == 98))  )
    # TEST Parser.map
    # Parser.runToString Parser.showU8 "ABCD" (Parser.map Parser.any (\u -> 99))
       |> Task.putLine

