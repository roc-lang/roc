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
MISSING METHOD - if_then_else_simple_file.md:4:2:4:9
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This number is being used where a non-number type is ──────┐
└┬──────────────┘  needed.                                                    │
 │                                                                            │
 │  foo = if 1 A                                                              │
 │           ‾                                                                │
 └────────────────────────────────────────── if_then_else_simple_file.md:1:10 ┘

    Other code expects this to have the type:

        Bool


┌────────────────┐
│ MISSING METHOD ├─ This `from_quote` method is being called on a value ──────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  "hello"                                                                   │
 │  ‾‾‾‾‾‾‾                                                                   │
 └─────────────────────────────────────────── if_then_else_simple_file.md:4:2 ┘

    The value's type, which does not have a method named `from_quote`, is:

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
		(patt (type "[A, ..]")))
	(expressions
		(expr (type "[A, ..]"))))
~~~
