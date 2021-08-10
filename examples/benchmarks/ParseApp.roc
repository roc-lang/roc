app "parseapp"
    packages { base: "platform" }
    imports [base.Task, Parser]
    provides [ main ] to base

main : Task.Task {} []
main =
    # Parser.runToString Parser.showU8 "abcd" Parser.any
    # Parser.runToString Parser.showU8 "abcd" (Parser.satisfy (\_ -> True))
    # Parser.runToString Parser.showU8 "abcd" (Parser.satisfy (\u -> u == 97))
    Parser.runToString Parser.showU8 "abcd" (Parser.satisfy (\u -> u == 98))  
       |> Task.putLine

