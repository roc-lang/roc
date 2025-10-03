# META
~~~ini
description=Example if-then-else statement
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo = if 1 A

    else {
	"hello"
    }
~~~
# EXPECTED
INVALID IF CONDITION - if_then_else_simple_file.md:3:10:3:10
INCOMPATIBLE IF BRANCHES - if_then_else_simple_file.md:3:7:3:7
# PROBLEMS
**INVALID IF CONDITION**
This `if` condition needs to be a _Bool_:
**if_then_else_simple_file.md:3:10:**
```roc
foo = if 1 A
```
         ^

Right now, it has the type:
    _Num(_size)_

Every `if` condition must evaluate to a _Bool_–either `True` or `False`.

**INCOMPATIBLE IF BRANCHES**
This `if` has an `else` branch with a different type from it's `then` branch:
**if_then_else_simple_file.md:3:7:**
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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),KwIf(3:7-3:9),Int(3:10-3:11),UpperIdent(3:12-3:13),
KwElse(5:5-5:9),OpenCurly(5:10-5:11),
StringStart(6:2-6:3),StringPart(6:3-6:8),StringEnd(6:8-6:9),
CloseCurly(7:5-7:6),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.6
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident @1.9-1.12
				(text "foo"))))
	(statements
		(s-decl @3.1-7.6
			(p-ident @3.1-3.4 (raw "foo"))
			(e-if-then-else @3.7-7.6
				(e-int @3.10-3.11 (raw "1"))
				(e-tag @3.12-3.13 (raw "A"))
				(e-block @5.10-7.6
					(statements
						(e-string @6.2-6.9
							(e-string-part @6.3-6.8 (raw "hello")))))))))
~~~
# FORMATTED
~~~roc
module [foo]

foo = if 1 A

	else {
		"hello"
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.4 (ident "foo"))
		(e-if @3.7-7.6
			(if-branches
				(if-branch
					(e-num @3.10-3.11 (value "1"))
					(e-tag @3.12-3.13 (name "A"))))
			(if-else
				(e-block @5.10-7.6
					(e-string @6.2-6.9
						(e-literal @6.3-6.8 (string "hello"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "Error")))
	(expressions
		(expr @3.7-7.6 (type "Error"))))
~~~
