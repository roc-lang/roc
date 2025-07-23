# META
~~~ini
description=Simple string interpolation
type=expr
~~~
# SOURCE
~~~roc
"Hello ${name}!"
~~~
# EXPECTED
UNDEFINED VARIABLE - string_interpolation_simple.md:1:10:1:14
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `name` in this scope.
Is there an `import` or `exposing` missing up-top?

**string_interpolation_simple.md:1:10:1:14:**
```roc
"Hello ${name}!"
```
         ^^^^


# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:8),OpenStringInterpolation(1:8-1:10),LowerIdent(1:10-1:14),CloseStringInterpolation(1:14-1:15),StringPart(1:15-1:16),StringEnd(1:16-1:17),EndOfFile(1:17-1:17),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.17
	(e-string-part @1.2-1.8 (raw "Hello "))
	(e-ident @1.10-1.14 (raw "name"))
	(e-string-part @1.15-1.16 (raw "!")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.17
	(e-literal @1.2-1.8 (string "Hello "))
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-literal @1.15-1.16 (string "!")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.17 (type "Str"))
~~~
