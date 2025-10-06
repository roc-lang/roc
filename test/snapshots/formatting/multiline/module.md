# META
~~~ini
description=Multiline formatting module
type=snippet
~~~
# SOURCE
~~~roc
	a,
	b,
]

a = 'a'
b = 'a'
~~~
# EXPECTED
PARSE ERROR - module.md:1:2:1:3
PARSE ERROR - module.md:1:3:1:4
PARSE ERROR - module.md:2:2:2:3
PARSE ERROR - module.md:2:3:2:4
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
	b,
```
	^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**module.md:2:3:2:4:**
```roc
	b,
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
LowerIdent(1:2-1:3),Comma(1:3-1:4),
LowerIdent(2:2-2:3),Comma(2:3-2:4),
CloseSquare(3:1-3:2),
LowerIdent(5:1-5:2),OpAssign(5:3-5:4),SingleQuote(5:5-5:8),
LowerIdent(6:1-6:2),OpAssign(6:3-6:4),SingleQuote(6:5-6:8),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.2-6.8
	(type-module @1.2-1.3)
	(statements
		(s-malformed @1.2-1.3 (tag "statement_unexpected_token"))
		(s-malformed @1.3-1.4 (tag "statement_unexpected_token"))
		(s-malformed @2.2-2.3 (tag "statement_unexpected_token"))
		(s-malformed @2.3-2.4 (tag "statement_unexpected_token"))
		(s-malformed @3.1-3.2 (tag "statement_unexpected_token"))
		(s-decl @5.1-5.8
			(p-ident @5.1-5.2 (raw "a"))
			(e-single-quote @5.5-5.8 (raw "'a'")))
		(s-decl @6.1-6.8
			(p-ident @6.1-6.2 (raw "b"))
			(e-single-quote @6.5-6.8 (raw "'a'")))))
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
		(p-assign @5.1-5.2 (ident "a"))
		(e-num @5.5-5.8 (value "97")))
	(d-let
		(p-assign @6.1-6.2 (ident "b"))
		(e-num @6.5-6.8 (value "97"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.2 (type "Num(Int(_size))"))
		(patt @6.1-6.2 (type "Num(Int(_size))")))
	(expressions
		(expr @5.5-5.8 (type "Num(Int(_size))"))
		(expr @6.5-6.8 (type "Num(Int(_size))"))))
~~~
