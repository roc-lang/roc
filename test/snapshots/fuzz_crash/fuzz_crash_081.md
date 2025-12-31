# META
~~~ini
description=fuzz crash: 0.() incorrectly tokenized as float
type=snippet
~~~
# SOURCE
~~~roc
x = 0.()
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_081.md:1:6:1:7
PARSE ERROR - fuzz_crash_081.md:1:7:1:8
PARSE ERROR - fuzz_crash_081.md:1:8:1:9
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_081.md:1:6:1:7:**
```roc
x = 0.()
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_081.md:1:7:1:8:**
```roc
x = 0.()
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_081.md:1:8:1:9:**
```roc
x = 0.()
```
       ^


# TOKENS
~~~zig
LowerIdent,OpAssign,Int,Dot,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "0")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
x = 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-num (value "0"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
