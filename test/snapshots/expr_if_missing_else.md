# META
~~~ini
description=
type=snippet
~~~
# SOURCE
~~~roc
foo = if tru 0
~~~
# EXPECTED
UNDEFINED VARIABLE - expr_if_missing_else.md:1:10:1:13
INCOMPATIBLE IF BRANCHES - expr_if_missing_else.md:1:7:1:7
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `tru` in this scope.
Is there an `import` or `exposing` missing up-top?

**expr_if_missing_else.md:1:10:1:13:**
```roc
foo = if tru 0
```
         ^^^


**INCOMPATIBLE IF BRANCHES**
This `if` has an `else` branch with a different type from it's `then` branch:
**expr_if_missing_else.md:1:7:**
```roc
foo = if tru 0
```
      ^^^^^^^^

The `else` branch has the type:
    _{}_

But the `then` branch has the type:
    _Num(_size)_

All branches in an `if` must have compatible types.

Note: You can wrap branches in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
LowerIdent,OpAssign,KwIf,LowerIdent,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-if-without-else
				(e-ident (raw "tru"))
				(e-int (raw "0"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-if
			(if-branches
				(if-branch
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-num (value "0"))))
			(if-else
				(e-empty_record)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
