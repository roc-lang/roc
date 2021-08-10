# JIM README

These are notes on some roc things I am working on.


## Parser.roc

This is the main one, an experiment.  It will change a lot.

Type: `Parser a : List U8 -> List ([Pair a (List U8)])`


Functions:

    run, runToString,
    succeed, any,  satisfy, fail, 
    map, andThen, first, second
   

To test: cargo run examples/benchmarks/ParseApp.roc

## Pair

The beginning of a pair library.

Functions:

    first, second, 
    mapFirst, mapSecond


## Loop

Type: `Step state a : [ Loop state, Done a ]`

Functions: `loop : (state -> Step state a), state -> a`