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
StringStart(1:1-1:2),StringPart(1:2-1:24),OpenStringInterpolation(1:24-1:26),LowerIdent(1:26-1:35),NoSpaceOpenRound(1:35-1:36),LowerIdent(1:36-1:37),Comma(1:37-1:38),
LowerIdent(2:1-2:2),CloseRound(2:2-2:3),CloseStringInterpolation(2:3-2:4),StringPart(2:4-2:44),StringEnd(2:44-2:45),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(e-string @1.1-2.45
	(e-string-part @1.2-1.24 (raw "This is a string with "))
	(e-apply @1.26-2.3
		(e-ident @1.26-1.35 (raw "some_func"))
		(e-ident @1.36-1.37 (raw "a"))
		(e-ident @2.1-2.2 (raw "b")))
	(e-string-part @2.4-2.44 (raw " lines of text due to the template parts")))
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
(e-string @1.1-2.45
	(e-literal @1.2-1.24 (string "This is a string with "))
	(e-call @1.26-2.3
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(e-literal @2.4-2.44 (string " lines of text due to the template parts")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.45 (type "Str"))
~~~
