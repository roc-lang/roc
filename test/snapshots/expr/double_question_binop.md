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
NOT IMPLEMENTED - double_question_binop.md:1:1:1:23
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `get_name!` in this scope.
Is there an `import` or `exposing` missing up-top?

**double_question_binop.md:1:1:1:10:**
```roc
get_name!({}) ?? "Bob"
```
^^^^^^^^^


**NOT IMPLEMENTED**
This feature is not yet implemented: unsupported operator

**double_question_binop.md:1:1:1:23:**
```roc
get_name!({}) ?? "Bob"
```
^^^^^^^^^^^^^^^^^^^^^^

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!


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
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
