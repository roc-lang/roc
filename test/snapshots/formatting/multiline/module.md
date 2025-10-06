# META
~~~ini
description=Multiline formatting module
type=file:Module.roc
~~~
# SOURCE
~~~roc
Module := {}

	a,
	b,
]

a = 'a'
b = 'a'
~~~
# EXPECTED
PARSE ERROR - module.md:3:2:3:3
PARSE ERROR - module.md:3:3:3:4
PARSE ERROR - module.md:4:2:4:3
PARSE ERROR - module.md:4:3:4:4
PARSE ERROR - module.md:5:1:5:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**module.md:3:2:3:3:**
```roc
	a,
```
	^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**module.md:3:3:3:4:**
```roc
	a,
```
	 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**module.md:4:2:4:3:**
```roc
	b,
```
	^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**module.md:4:3:4:4:**
```roc
	b,
```
	 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**module.md:5:1:5:2:**
```roc
]
```
^


# TOKENS
~~~zig
UpperIdent(1:1-1:7),OpColonEqual(1:8-1:10),OpenCurly(1:11-1:12),CloseCurly(1:12-1:13),
LowerIdent(3:2-3:3),Comma(3:3-3:4),
LowerIdent(4:2-4:3),Comma(4:3-4:4),
CloseSquare(5:1-5:2),
LowerIdent(7:1-7:2),OpAssign(7:3-7:4),SingleQuote(7:5-7:8),
LowerIdent(8:1-8:2),OpAssign(8:3-8:4),SingleQuote(8:5-8:8),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(file @1.1-8.8
	(type-module @1.1-1.7)
	(statements
		(s-type-decl @1.1-1.13
			(header @1.1-1.7 (name "Module")
				(args))
			(ty-record @1.11-1.13))
		(s-malformed @3.2-3.3 (tag "statement_unexpected_token"))
		(s-malformed @3.3-3.4 (tag "statement_unexpected_token"))
		(s-malformed @4.2-4.3 (tag "statement_unexpected_token"))
		(s-malformed @4.3-4.4 (tag "statement_unexpected_token"))
		(s-malformed @5.1-5.2 (tag "statement_unexpected_token"))
		(s-decl @7.1-7.8
			(p-ident @7.1-7.2 (raw "a"))
			(e-single-quote @7.5-7.8 (raw "'a'")))
		(s-decl @8.1-8.8
			(p-ident @8.1-8.2 (raw "b"))
			(e-single-quote @8.5-8.8 (raw "'a'")))))
~~~
# FORMATTED
~~~roc
Module := {}





a = 'a'
b = 'a'
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @7.1-7.2 (ident "a"))
		(e-num @7.5-7.8 (value "97")))
	(d-let
		(p-assign @8.1-8.2 (ident "b"))
		(e-num @8.5-8.8 (value "97")))
	(s-nominal-decl @1.1-1.13
		(ty-header @1.1-1.7 (name "Module"))
		(ty-record @1.11-1.13)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.2 (type "Num(Int(_size))"))
		(patt @8.1-8.2 (type "Num(Int(_size))")))
	(type_decls
		(nominal @1.1-1.13 (type "Module")
			(ty-header @1.1-1.7 (name "Module"))))
	(expressions
		(expr @7.5-7.8 (type "Num(Int(_size))"))
		(expr @8.5-8.8 (type "Num(Int(_size))"))))
~~~
