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
UNDEFINED VARIABLE - string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_3.md:2:2:2:11
UNDEFINED VARIABLE - string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_3.md:3:3:3:4
UNDEFINED VARIABLE - string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_3.md:4:3:4:4
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:24),OpenStringInterpolation(1:24-1:26),Newline(1:1-1:1),
LowerIdent(2:2-2:11),NoSpaceOpenRound(2:11-2:12),Newline(1:1-1:1),
LowerIdent(3:3-3:4),Comma(3:4-3:5),Newline(3:7-3:25),
LowerIdent(4:3-4:4),Comma(4:4-4:5),Newline(1:1-1:1),
CloseRound(5:2-5:3),Newline(1:1-1:1),
CloseStringInterpolation(6:1-6:2),StringPart(6:2-6:42),StringEnd(6:42-6:43),EndOfFile(6:43-6:43),
~~~
# PARSE
~~~clojure
(e-string @1.1-6.43
	(e-string-part @1.2-1.24 (raw "This is a string with "))
	(e-apply @2.2-5.3
		(e-ident @2.2-2.11 (raw "some_func"))
		(e-ident @3.3-3.4 (raw "a"))
		(e-ident @4.3-4.4 (raw "b")))
	(e-string-part @6.2-6.42 (raw " lines of text due to the template parts")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-6.43
	(e-literal @1.2-1.24 (string "This is a string with "))
	(e-call @2.2-5.3
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(e-literal @6.2-6.42 (string " lines of text due to the template parts")))
~~~
# TYPES
~~~clojure
(expr @1.1-6.43 (type "Str"))
~~~
