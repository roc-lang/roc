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
(e-block
	(s-let
		(p-assign (ident "#interp_0"))
		(e-call
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-runtime-error (tag "ident_not_in_scope"))))
	(e-interpolation
		(first
			(e-literal (string "This is a string with ")))
		(parts
			(e-lookup-local
				(p-assign (ident "#interp_0")))
			(e-literal (string " lines of text due to the template parts")))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
