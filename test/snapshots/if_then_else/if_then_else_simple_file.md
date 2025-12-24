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
MISSING METHOD - if_then_else_simple_file.md:1:10:1:11
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

    Str

But the `then` branch has the type:

    [A, .._others]

All branches in an `if` must have compatible types.

Note: You can wrap branches in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**if_then_else_simple_file.md:1:10:1:11:**
```roc
foo = if 1 A
```
         ^

The value's type, which does not have a method named **from_numeral**, is:

    Bool

**Hint:** For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

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
