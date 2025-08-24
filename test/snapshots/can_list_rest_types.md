# META
~~~ini
description=List rest patterns should have correct list types matching element types
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [first, .. as restNums] => restNums
    [] => []
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare CloseSquare OpFatArrow OpenSquare CloseSquare CloseCurly ~~~
# PARSE
~~~clojure
(match <10 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - can_list_rest_types.md:1:7:1:14
UNUSED VARIABLE - can_list_rest_types.md:2:6:2:11
# PROBLEMS
**Parse Error**
at 1:1 to 1:15

**Parse Error**
at 2:16 to 2:16

**Parse Error**
at 2:5 to 2:19

**Parse Error**
at 2:27 to 2:27

**Parse Error**
at 2:29 to 2:29

**Parse Error**
at 3:8 to 3:8

**Parse Error**
at 1:1 to 4:2

**Parse Error**
at 4:2 to 4:2

**Unsupported Node**
at 1:1 to 4:2

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
