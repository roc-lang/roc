# META
~~~ini
description=single_question_binop_closure
type=expr
~~~
# SOURCE
~~~roc
fallible!(args)   ? |my_err|
    my_err * 2
~~~
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
LowerIdent(1:1-1:10),NoSpaceOpenRound(1:10-1:11),LowerIdent(1:11-1:15),CloseRound(1:15-1:16),OpQuestion(1:19-1:20),OpBar(1:21-1:22),LowerIdent(1:22-1:28),OpBar(1:28-1:29),Newline(1:1-1:1),
LowerIdent(2:5-2:11),OpStar(2:12-2:13),Int(2:14-2:15),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-3.2 (op "?")
	(e-apply @1.1-1.16
		(e-ident @1.1-1.10 (raw "fallible!"))
		(e-ident @1.11-1.15 (raw "args")))
	(e-lambda @1.21-3.2
		(args
			(p-ident @1.22-1.28 (raw "my_err")))
		(e-binop @2.5-3.2 (op "*")
			(e-ident @2.5-2.11 (raw "my_err"))
			(e-int @2.14-2.15 (raw "2")))))
~~~
# FORMATTED
~~~roc
fallible!(args) ? |my_err|
	my_err * 2
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "Error"))
~~~
