# META
~~~ini
description=Example if-then-else statement
type=file
~~~
# SOURCE
~~~roc
foo = if 1 A

    else {
	"hello"
    }
~~~
# EXPECTED
MISSING MAIN! FUNCTION - if_then_else_simple_file.md:1:1:5:6
INVALID IF CONDITION - if_then_else_simple_file.md:1:10:1:10
INCOMPATIBLE IF BRANCHES - if_then_else_simple_file.md:1:7:1:7
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**if_then_else_simple_file.md:1:1:5:6:**
```roc
foo = if 1 A

    else {
	"hello"
    }
```


**INVALID IF CONDITION**
This `if` condition needs to be a _Bool_:
**if_then_else_simple_file.md:1:10:**
```roc
foo = if 1 A
```
         ^

Right now, it has the type:
    _Num(_size)_

Every `if` condition must evaluate to a _Bool_–either `True` or `False`.

**INCOMPATIBLE IF BRANCHES**
This `if` has an `else` branch with a different type from it's `then` branch:
**if_then_else_simple_file.md:1:7:**
```roc
foo = if 1 A

    else {
	"hello"
    }
```
 ^^^^^^^

The `else` branch has the type:
    _Str_

But the `then` branch has the type:
    _[A]_others_

All branches in an `if` must have compatible types.

Note: You can wrap branches in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpAssign(1:5-1:6),KwIf(1:7-1:9),Int(1:10-1:11),UpperIdent(1:12-1:13),
KwElse(3:5-3:9),OpenCurly(3:10-3:11),
StringStart(4:2-4:3),StringPart(4:3-4:8),StringEnd(4:8-4:9),
CloseCurly(5:5-5:6),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.6
	(type-module @1.1-1.4)
	(statements
		(s-decl @1.1-5.6
			(p-ident @1.1-1.4 (raw "foo"))
			(e-if-then-else @1.7-5.6
				(e-int @1.10-1.11 (raw "1"))
				(e-tag @1.12-1.13 (raw "A"))
				(e-block @3.10-5.6
					(statements
						(e-string @4.2-4.9
							(e-string-part @4.3-4.8 (raw "hello")))))))))
~~~
# FORMATTED
~~~roc
foo = if 1 A

	else {
		"hello"
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.4 (ident "foo"))
		(e-if @1.7-5.6
			(if-branches
				(if-branch
					(e-int @1.10-1.11 (value "1"))
					(e-tag @1.12-1.13 (name "A"))))
			(if-else
				(e-block @3.10-5.6
					(e-string @4.2-4.9
						(e-literal @4.3-4.8 (string "hello"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.4 (type "Error")))
	(expressions
		(expr @1.7-5.6 (type "Error"))))
~~~
