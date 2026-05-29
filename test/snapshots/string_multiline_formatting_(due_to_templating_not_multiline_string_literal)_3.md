# META
~~~ini
description=string_multiline_formatting_(due_to_templating_not_multiline_string_literal) (3)
type=expr
~~~
# SOURCE
~~~roc
"This is a string with ${
	some_func(
		a, # This is a comment
		b,
	)
} lines of text due to the template parts"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart,StringPart,OpenStringInterpolation,
LowerIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,
CloseStringInterpolation,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-string
	(e-string-part (raw "This is a string with "))
	(e-apply
		(e-ident (raw "some_func"))
		(e-ident (raw "a"))
		(e-ident (raw "b")))
	(e-string-part (raw " lines of text due to the template parts")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string
	(e-literal (string "This is a string with "))
	(e-call
		(e-lookup-local
			(p-assign (ident "some_func")))
		(e-lookup-local
			(p-assign (ident "a")))
		(e-lookup-local
			(p-assign (ident "b"))))
	(e-literal (string " lines of text due to the template parts")))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
