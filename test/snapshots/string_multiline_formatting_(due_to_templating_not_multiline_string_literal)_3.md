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
**UNDEFINED VARIABLE**
Nothing is named `some_func` in this scope.
Is there an `import` or `exposing` missing up-top?

**string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_3.md:2:2:2:11:**
```roc
	some_func(
```
	^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `a` in this scope.
Is there an `import` or `exposing` missing up-top?

**string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_3.md:3:3:3:4:**
```roc
		a, # This is a comment
```
		^


**UNDEFINED VARIABLE**
Nothing is named `b` in this scope.
Is there an `import` or `exposing` missing up-top?

**string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_3.md:4:3:4:4:**
```roc
		b,
```
		^


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
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(e-literal (string " lines of text due to the template parts")))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
