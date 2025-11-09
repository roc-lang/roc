# META
~~~ini
description=Multiline without comma formatting module
type=snippet
~~~
# SOURCE
~~~roc
	a,
	b
]

a = 'a'
b = 'a'
~~~
# EXPECTED
PARSE ERROR - module.md:1:2:1:3
PARSE ERROR - module.md:1:3:1:4
PARSE ERROR - module.md:2:2:2:3
PARSE ERROR - module.md:3:1:3:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**module.md:1:2:1:3:**
```roc
	a,
```
	^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**module.md:1:3:1:4:**
```roc
	a,
```
	 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**module.md:2:2:2:3:**
```roc
	b
```
	^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**module.md:3:1:3:2:**
```roc
]
```
^


# TOKENS
~~~zig
LowerIdent,Comma,
LowerIdent,
CloseSquare,
LowerIdent,OpAssign,SingleQuote,
LowerIdent,OpAssign,SingleQuote,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "a"))
			(e-single-quote (raw "'a'")))
		(s-decl
			(p-ident (raw "b"))
			(e-single-quote (raw "'a'")))))
~~~
# FORMATTED
~~~roc




a = 'a'
b = 'a'
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-num (value "97")))
	(d-let
		(p-assign (ident "b"))
		(e-num (value "97"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Int(_size))"))
		(patt (type "Num(Int(_size))")))
	(expressions
		(expr (type "Num(Int(_size))"))
		(expr (type "Num(Int(_size))"))))
~~~
