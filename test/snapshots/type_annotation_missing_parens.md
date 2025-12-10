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
PARSE ERROR - type_annotation_missing_parens.md:2:1:2:1
TOO FEW ARGS - type_annotation_missing_parens.md:1:8:1:12
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**type_annotation_missing_parens.md:1:13:1:15:**
```roc
nums : List U8
```
            ^^


**TOO FEW ARGS**
The type _List_ expects 1 argument, but got 0 instead.
**type_annotation_missing_parens.md:1:8:1:12:**
```roc
nums : List U8
```
       ^^^^



# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "nums")
			(ty (name "List")))
		(s-malformed (tag "statement_unexpected_token"))))
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
