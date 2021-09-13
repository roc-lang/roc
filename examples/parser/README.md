# JIM README

## Files

Console.roc
ListExtra.roc: filter : List a, (a -> Bool) -> List a
Loop.roc, LoopTest.roc: loop construct and tests for it.
Pair.roc, PairTest.roc: Pairs
StrExtra.roc, StrExtraTest.roc: repeat, concat, join
Test.roc, TestTest.roc: test library
Utility.roc

## Running Tests

See the file `Test.roc` for a small testing library.  To see how to use it, look
at the comments and also the file `TestRest.roc`.  The latter runs suites
of tests fo `Test.roc`.

### Dependencies

Test.roc: Console.roc

## Parser project

This is at a very early stage.  See files `Parser.roc` and `Parser2.roc`.
The latter is the one I am currently working on while I sort-out some bugs.
The active test suite is in `ParserTest.roc`.

### Dependencies

Parser2.roc: Pair.roc, Utility.roc

### Tests

`ParserTest.roc`: just run it.

 