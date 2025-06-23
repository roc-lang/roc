# META
~~~ini
description=Simple string interpolation
type=expr
~~~
# SOURCE
~~~roc
"Hello ${name}!"
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named ``name`` in this scope.
Is there an `import` or `exposing` missing up-top?
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:8),OpenStringInterpolation(1:8-1:10),LowerIdent(1:10-1:14),CloseStringInterpolation(1:14-1:15),StringPart(1:15-1:16),StringEnd(1:16-1:17),EndOfFile(1:17-1:17),
~~~
# PARSE
~~~clojure
(string (1:1-1:17)
	(string_part (1:2-1:8) "Hello ")
	(ident (1:10-1:14) "" "name")
	(string_part (1:15-1:16) "!"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_string (1:1-1:17)
	(e_literal (1:2-1:8) "Hello ")
	(e_runtime_error (1:10-1:14) "ident_not_in_scope")
	(e_literal (1:15-1:16) "!"))
~~~
# TYPES
~~~clojure
(expr 16 (type "Str"))
~~~