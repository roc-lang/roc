interface Parser2 exposes [ 
    Parser, run, 
    tests
  ] imports [Pair, Utility]

## PARSER  

Parser a : List U8 -> List [Pair a (List U8)]


run : (List U8), Parser a -> List ([Pair a (List U8)])
run = 
  \input, parser -> parser input

##
dummy = {name: "1 + 1 == 2", test: 1 + 1 == 2}

tests = [dummy]