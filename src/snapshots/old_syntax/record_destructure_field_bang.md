# META
~~~ini
description=record_destructure_field_bang
type=expr
~~~
# SOURCE
~~~roc
{launchTheNukes!, code} = config

launchTheNukes! code
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `launchTheNukes!` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `code` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:2-1:17),Comma(1:17-1:18),LowerIdent(1:19-1:23),CloseCurly(1:23-1:24),OpAssign(1:25-1:26),LowerIdent(1:27-1:33),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:16),LowerIdent(3:17-3:21),EndOfFile(3:21-3:21),
~~~
# PARSE
~~~clojure
(e-record @1.1-1.24
	(field (field "launchTheNukes!") (optional false))
	(field (field "code") (optional false)))
~~~
# FORMATTED
~~~roc
{launchTheNukes!, code}
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-1.24
	(fields
		(field (name "launchTheNukes!")
			(e-runtime-error (tag "ident_not_in_scope")))
		(field (name "code")
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.24 (type "{ launchTheNukes!: Error, code: Error }"))
~~~
