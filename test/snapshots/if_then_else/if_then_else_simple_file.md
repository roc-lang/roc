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
TYPE MISMATCH - if_then_else_simple_file.md:1:10:1:11
TYPE MISMATCH - if_then_else_simple_file.md:3:10:5:6
# PROBLEMS
**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**if_then_else_simple_file.md:1:10:1:11:**
```roc
foo = if 1 A
```
         ^

Other code expects this to have the type:

    Bool

**TYPE MISMATCH**
The second branch of this `if` does not match the previous branch :
**if_then_else_simple_file.md:3:10:5:6:**
```roc
    else {
	"hello"
    }
```

The second branch is:

    Str

But the previous branch results in:

    [A, ..]

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
