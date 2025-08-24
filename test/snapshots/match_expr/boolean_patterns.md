# META
~~~ini
description=Match expression with boolean-like tag patterns
type=expr
~~~
# SOURCE
~~~roc
match isReady {
	True => "ready to go!"
	False => "not ready yet"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpFatArrow String UpperIdent OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match <6 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - boolean_patterns.md:1:7:1:14
# PROBLEMS
**Parse Error**
at 1:1 to 1:15

**Parse Error**
at 2:7 to 2:7

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
