# META
~~~ini
description=An empty module with multiline exposes and comments
type=file:ModuleMultilineWithComments.roc
~~~
# SOURCE
~~~roc
ModuleMultilineWithComments := {}

	[ # Comment After exposes open
		something, # Comment after exposed item
		SomeType, # Comment after final exposed item
	]
~~~
# EXPECTED
PARSE ERROR - module_multiline_with_comments.md:3:2:3:3
PARSE ERROR - module_multiline_with_comments.md:4:3:4:12
PARSE ERROR - module_multiline_with_comments.md:4:12:4:13
PARSE ERROR - module_multiline_with_comments.md:5:11:5:12
PARSE ERROR - module_multiline_with_comments.md:6:2:6:3
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**module_multiline_with_comments.md:3:2:3:3:**
```roc
	[ # Comment After exposes open
```
	^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**module_multiline_with_comments.md:4:3:4:12:**
```roc
		something, # Comment after exposed item
```
		^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**module_multiline_with_comments.md:4:12:4:13:**
```roc
		something, # Comment after exposed item
```
		         ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**module_multiline_with_comments.md:5:11:5:12:**
```roc
		SomeType, # Comment after final exposed item
```
		        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**module_multiline_with_comments.md:6:2:6:3:**
```roc
	]
```
	^


# TOKENS
~~~zig
UpperIdent(1:1-1:28),OpColonEqual(1:29-1:31),OpenCurly(1:32-1:33),CloseCurly(1:33-1:34),
OpenSquare(3:2-3:3),
LowerIdent(4:3-4:12),Comma(4:12-4:13),
UpperIdent(5:3-5:11),Comma(5:11-5:12),
CloseSquare(6:2-6:3),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.3
	(type-module @1.1-1.28)
	(statements
		(s-type-decl @1.1-1.34
			(header @1.1-1.28 (name "ModuleMultilineWithComments")
				(args))
			(ty-record @1.32-1.34))
		(s-malformed @3.2-3.3 (tag "statement_unexpected_token"))
		(s-malformed @4.3-4.12 (tag "statement_unexpected_token"))
		(s-malformed @4.12-4.13 (tag "statement_unexpected_token"))
		(s-malformed @5.11-5.12 (tag "expected_colon_after_type_annotation"))
		(s-malformed @6.2-6.3 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
ModuleMultilineWithComments := {}

# Comment After exposes open
# Comment after final exposed item
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.34
		(ty-header @1.1-1.28 (name "ModuleMultilineWithComments"))
		(ty-record @1.32-1.34)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.34 (type "ModuleMultilineWithComments")
			(ty-header @1.1-1.28 (name "ModuleMultilineWithComments"))))
	(expressions))
~~~
