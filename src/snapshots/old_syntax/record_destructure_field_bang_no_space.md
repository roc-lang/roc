# META
~~~ini
description=record_destructure_field_bang_no_space fail
type=expr
~~~
# SOURCE
~~~roc
{launchTheNukes!wrong, code} = config

launchTheNukes! code
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `launchTheNukes!wrong` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `code` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:2-1:22),Comma(1:22-1:23),LowerIdent(1:24-1:28),CloseCurly(1:28-1:29),OpAssign(1:30-1:31),LowerIdent(1:32-1:38),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:16),LowerIdent(3:17-3:21),EndOfFile(3:21-3:21),
~~~
# PARSE
~~~clojure
(e-record @1.1-1.29
	(field (field "launchTheNukes!wrong") (optional false))
	(field (field "code") (optional false)))
~~~
# FORMATTED
~~~roc
{ launchTheNukes!wrong, code }
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-1.29 (id 79)
	(fields
		(field (name "launchTheNukes!wrong")
			(e-runtime-error (tag "ident_not_in_scope")))
		(field (name "code")
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(expr (id 79) (type "{ launchTheNukes!wrong: Error, code: Error }"))
~~~
