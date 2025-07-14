# META
~~~ini
description=Record field access used in expressions (dot-access)
type=expr
~~~
# SOURCE
~~~roc
person.age + 5
~~~
# EXPECTED
UNDEFINED VARIABLE - record_access_in_expression.md:1:1:1:7
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_access_in_expression.md:1:1:1:7:**
```roc
person.age + 5
```
^^^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:7),NoSpaceDotLowerIdent(1:7-1:11),OpPlus(1:12-1:13),Int(1:14-1:15),EndOfFile(1:15-1:15),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.15 (op "+")
	(e-field-access @1.1-1.11
		(e-ident @1.1-1.7 (raw "person"))
		(e-ident @1.7-1.11 (raw "age")))
	(e-int @1.14-1.15 (raw "5")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.15 (op "add")
	(e-dot-access @1.1-1.11 (field "age")
		(receiver
			(e-runtime-error (tag "ident_not_in_scope"))))
	(e-int @1.14-1.15 (value "5")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.15 (type "_a"))
~~~
