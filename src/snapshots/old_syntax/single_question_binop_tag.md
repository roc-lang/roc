# META
~~~ini
description=single_question_binop_tag
type=expr
~~~
# SOURCE
~~~roc
fallible!(args)   ? WrapOverErr
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `fallible!` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `args` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: binop
Let us know if you want to help!

# TOKENS
~~~zig
LowerIdent(1:1-1:10),NoSpaceOpenRound(1:10-1:11),LowerIdent(1:11-1:15),CloseRound(1:15-1:16),OpQuestion(1:19-1:20),UpperIdent(1:21-1:32),EndOfFile(1:32-1:32),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.32 (op "?")
	(e-apply @1.1-1.16
		(e-ident @1.1-1.10 (raw "fallible!"))
		(e-ident @1.11-1.15 (raw "args")))
	(e-tag @1.21-1.32 (raw "WrapOverErr")))
~~~
# FORMATTED
~~~roc
fallible!(args) ? WrapOverErr
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.32 (type "Error"))
~~~
