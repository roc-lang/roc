# META
~~~ini
description=string_multiline_formatting_(due_to_templating_not_multiline_string_literal) (1)
type=expr
~~~
# SOURCE
~~~roc
"This is a string with ${some_func(a, #This is a comment
b)} lines of text due to the template parts"
~~~
# EXPECTED
UNDEFINED VARIABLE - string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_1.md:1:26:1:35
UNDEFINED VARIABLE - string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_1.md:1:36:1:37
UNDEFINED VARIABLE - string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_1.md:2:1:2:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `some_func` in this scope.
Is there an `import` or `exposing` missing up-top?

**string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_1.md:1:26:1:35:**
```roc
"This is a string with ${some_func(a, #This is a comment
```
                         ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `a` in this scope.
Is there an `import` or `exposing` missing up-top?

**string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_1.md:1:36:1:37:**
```roc
"This is a string with ${some_func(a, #This is a comment
```
                                   ^


**UNDEFINED VARIABLE**
Nothing is named `b` in this scope.
Is there an `import` or `exposing` missing up-top?

**string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_1.md:2:1:2:2:**
```roc
b)} lines of text due to the template parts"
```
^


# TOKENS
~~~zig
StringStart,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,
LowerIdent,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
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
"This is a string with ${
	some_func(
		a, # This is a comment
		b,
	)
} lines of text due to the template parts"
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
