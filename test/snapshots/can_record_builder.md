# META
~~~ini
description=Test record builder expression (not yet implemented)
type=expr
~~~
# SOURCE
~~~roc
{build} foo bar
~~~
# EXPECTED
UNDEFINED VARIABLE - can_record_builder.md:1:2:1:7
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `build` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_record_builder.md:1:2:1:7:**
```roc
{build} foo bar
```
 ^^^^^


# TOKENS
~~~zig
OpenCurly,LowerIdent,CloseCurly,LowerIdent,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(e-ident (raw "build"))))
~~~
# FORMATTED
~~~roc
{
	build
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
