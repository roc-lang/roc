# META
~~~ini
description=Example if-then-else statement
type=snippet
~~~
# SOURCE
~~~roc
foo = if 1 A

    else {
	"hello"
    }
~~~
# EXPECTED
INCOMPATIBLE IF BRANCHES - if_then_else_simple_file.md:1:7:1:7
# PROBLEMS
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
LowerIdent,OpAssign,KwIf,Int,UpperIdent,
KwElse,OpenCurly,
StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-if-then-else
				(e-int (raw "1"))
				(e-tag (raw "A"))
				(e-block
					(statements
						(e-string
							(e-string-part (raw "hello")))))))))
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
		(p-assign (ident "foo"))
		(e-if
			(if-branches
				(if-branch
					(e-num (value "1"))
					(e-tag (name "A"))))
			(if-else
				(e-block
					(e-string
						(e-literal (string "hello"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
