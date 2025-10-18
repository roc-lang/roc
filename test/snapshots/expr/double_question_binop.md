# META
~~~ini
description=Double question default value
type=expr
~~~
# SOURCE
~~~roc
get_name!({}) ?? "Bob"
~~~
# EXPECTED
UNDEFINED VARIABLE - double_question_binop.md:1:1:1:10
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `get_name!` in this scope.
Is there an `import` or `exposing` missing up-top?

**double_question_binop.md:1:1:1:10:**
```roc
get_name!({}) ?? "Bob"
```
^^^^^^^^^


# TOKENS
~~~zig
LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,OpDoubleQuestion,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "??")
	(e-apply
		(e-ident (raw "get_name!"))
		(e-record))
	(e-string
		(e-string-part (raw "Bob"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-binop (op "null_coalesce")
	(e-call
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-empty_record))
	(e-string
		(e-literal (string "Bob"))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
