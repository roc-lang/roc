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
LowerIdent(1:1-1:10),NoSpaceOpenRound(1:10-1:11),OpenCurly(1:11-1:12),CloseCurly(1:12-1:13),CloseRound(1:13-1:14),OpDoubleQuestion(1:15-1:17),StringStart(1:18-1:19),StringPart(1:19-1:22),StringEnd(1:22-1:23),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.23 (op "??")
	(e-apply @1.1-1.14
		(e-ident @1.1-1.10 (raw "get_name!"))
		(e-record @1.11-1.13))
	(e-string @1.18-1.23
		(e-string-part @1.19-1.22 (raw "Bob"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.23 (op "null_coalesce")
	(e-call @1.1-1.14
		(e-empty_record @1.11-1.13))
	(e-string @1.18-1.23
		(e-literal @1.19-1.22 (string "Bob"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.23 (type "_a"))
~~~
