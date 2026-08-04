# META
~~~ini
description=Type annotation missing parentheses for type application
type=snippet
~~~
# SOURCE
~~~roc
nums : List U8
~~~
# EXPECTED
TYPE APPLICATION NEEDS PARENTHESES - type_annotation_missing_parens.md:2:1:2:1
TOO FEW ARGS - type_annotation_missing_parens.md:1:8:1:12
DECLARATION HAS NO VALUE - type_annotation_missing_parens.md:1:1:1:12
# PROBLEMS

┌────────────────────────────────────┐
│ TYPE APPLICATION NEEDS PARENTHESES ├─ I was parsing a type annotation, ─────┐
└┬───────────────────────────────────┘  and I found a type argument without   │
 │                                      parentheses.                          │
 │                                                                            │
 │                                                                            │
 │  ‾                                                                         │
 └───────────────────────────────────── type_annotation_missing_parens.md:2:1 ┘

    Roc type applications use parentheses around their arguments. Write
    `List(U8)`, not `List U8`.

    For example:
        List(U8)

    I reached the end of the file before this construct was complete.


┌──────────────┐
│ TOO FEW ARGS ├─ The type List expects 1 argument, but got 0 instead. ───────┐
└┬─────────────┘                                                              │
 │                                                                            │
 │  nums : List U8                                                            │
 │         ‾‾‾‾                                                               │
 └───────────────────────────────────── type_annotation_missing_parens.md:1:8 ┘



┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  nums : List U8                                                            │
 │  ‾‾‾‾‾‾‾‾‾‾‾                                                               │
 └───────────────────────────────────── type_annotation_missing_parens.md:1:1 ┘

    Add a value body here, or put hosted functions in a platform type mod so
    they are published through the host boundary.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "nums")
			(ty (name "List")))
		(s-malformed (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
nums : List
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "nums"))
		(e-anno-only)
		(annotation
			(ty-lookup (name "List") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
