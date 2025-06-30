# META
~~~ini
description=if_then_else (3)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	A
} else 2
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `bool` in this scope.
Is there an `import` or `exposing` missing up-top?

**INCOMPATIBLE IF BRANCHES**
This `if` has an `else` branch with a different type from it's `then` branch:
**if_then_else_3.md:1:1:**
```roc
if bool {
	A
} else 2
```
       ^

The `else` branch has the type:
    _Num(*)_

But the `then` branch has the type:
    _[A]*_

All branches in an `if` must have compatible types.

Note: You can wrap branches in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
KwIf(1:1-1:3),LowerIdent(1:4-1:8),OpenCurly(1:9-1:10),Newline(1:1-1:1),
UpperIdent(2:2-2:3),Newline(1:1-1:1),
CloseCurly(3:1-3:2),KwElse(3:3-3:7),Int(3:8-3:9),EndOfFile(3:9-3:9),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-3.9
	(e-ident @1.4-1.8 (qaul "") (raw "bool"))
	(e-block @1.9-3.2
		(statements
			(e-tag @2.2-2.3 (raw "A"))))
	(e-int @3.8-3.9 (raw "2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-3.9
	(if-branches
		(if-branch
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-block @1.9-3.2
				(e-tag @2.2-2.3 (name "A") (args "TODO")))))
	(if-else
		(e-int @3.8-3.9 (value "2"))))
~~~
# TYPES
~~~clojure
(expr @1.1-3.9 (type "Error"))
~~~
