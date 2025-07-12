# META
~~~ini
description=if_then_else (9)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	1
} else if 10 { # Comment after else open
	A
} else { # Comment after else open
	3
}
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_9.md:1:4:1:8
INVALID IF CONDITION - if_then_else_9.md:3:11:3:11
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `bool` in this scope.
Is there an `import` or `exposing` missing up-top?

**if_then_else_9.md:1:4:1:8:**
```roc
if bool {
```
   ^^^^


**INVALID IF CONDITION**
This `if` condition needs to be a _Bool_:
**if_then_else_9.md:3:11:**
```roc
} else if 10 { # Comment after else open
```
          ^^

Right now, it has the type:
    _Num(*)_

Every `if` condition must evaluate to a _Bool_–either `True` or `False`.

**INCOMPATIBLE IF BRANCHES**
The type of the second branch of this `if` does not match the previous branches:
**if_then_else_9.md:1:1:**
```roc
if bool {
	1
} else if 10 { # Comment after else open
	A
} else { # Comment after else open
	3
}
```
 ^

The second branch has this type:
    _[A]*_

But the previous branch has this type:
    _Num(*)_

All branches in an `if` must have compatible types.

Note: You can wrap branches in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
KwIf(1:1-1:3),LowerIdent(1:4-1:8),OpenCurly(1:9-1:10),
Int(2:2-2:3),
CloseCurly(3:1-3:2),KwElse(3:3-3:7),KwIf(3:8-3:10),Int(3:11-3:13),OpenCurly(3:14-3:15),
UpperIdent(4:2-4:3),
CloseCurly(5:1-5:2),KwElse(5:3-5:7),OpenCurly(5:8-5:9),
Int(6:2-6:3),
CloseCurly(7:1-7:2),EndOfFile(7:2-7:2),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-7.2
	(e-ident @1.4-1.8 (raw "bool"))
	(e-block @1.9-3.2
		(statements
			(e-int @2.2-2.3 (raw "1"))))
	(e-if-then-else @3.8-7.2
		(e-int @3.11-3.13 (raw "10"))
		(e-block @3.14-5.2
			(statements
				(e-tag @4.2-4.3 (raw "A"))))
		(e-block @5.8-7.2
			(statements
				(e-int @6.2-6.3 (raw "3"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-7.2
	(if-branches
		(if-branch
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-block @1.9-3.2
				(e-int @2.2-2.3 (value "1"))))
		(if-branch
			(e-int @3.11-3.13 (value "10"))
			(e-block @3.14-5.2
				(e-tag @4.2-4.3 (name "A")))))
	(if-else
		(e-block @5.8-7.2
			(e-int @6.2-6.3 (value "3")))))
~~~
# TYPES
~~~clojure
(expr @1.1-7.2 (type "Error"))
~~~
