# META
~~~ini
description=multiple_fields
type=expr
~~~
# SOURCE
~~~roc
rec.abc.def.ghi
~~~
# EXPECTED
UNDEFINED VARIABLE - multiple_fields.md:1:1:1:4
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `rec` in this scope.
Is there an `import` or `exposing` missing up-top?

**multiple_fields.md:1:1:1:4:**
```roc
rec.abc.def.ghi
```
^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:4),NoSpaceDotLowerIdent(1:4-1:8),NoSpaceDotLowerIdent(1:8-1:12),NoSpaceDotLowerIdent(1:12-1:16),EndOfFile(1:16-1:16),
~~~
# PARSE
~~~clojure
(e-field-access @1.1-1.16
	(e-field-access @1.1-1.16
		(e-field-access @1.1-1.12
			(e-ident @1.1-1.4 (raw "rec"))
			(e-ident @1.4-1.8 (raw "abc")))
		(e-ident @1.8-1.12 (raw "def")))
	(e-ident @1.12-1.16 (raw "ghi")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access @1.1-1.16 (field "ghi")
	(receiver
		(e-dot-access @1.1-1.16 (field "def")
			(receiver
				(e-dot-access @1.1-1.12 (field "abc")
					(receiver
						(e-runtime-error (tag "ident_not_in_scope"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.16 (type "*"))
~~~
