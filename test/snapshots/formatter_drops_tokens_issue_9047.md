# META
~~~ini
description=Formatter should not silently drop tokens after unmatched closing paren (issue 9047)
type=snippet
~~~
# SOURCE
~~~roc
foo = {x: 5, y: 6}.Foo).bar()
~~~
# EXPECTED
PARSE ERROR - formatter_drops_tokens_issue_9047.md:1:23:1:24
PARSE ERROR - formatter_drops_tokens_issue_9047.md:1:24:1:28
PARSE ERROR - formatter_drops_tokens_issue_9047.md:1:28:1:29
PARSE ERROR - formatter_drops_tokens_issue_9047.md:1:29:1:30
RECORD BUILDER NOT SUPPORTED - formatter_drops_tokens_issue_9047.md:1:7:1:23
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**formatter_drops_tokens_issue_9047.md:1:23:1:24:**
```roc
foo = {x: 5, y: 6}.Foo).bar()
```
                      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**formatter_drops_tokens_issue_9047.md:1:24:1:28:**
```roc
foo = {x: 5, y: 6}.Foo).bar()
```
                       ^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**formatter_drops_tokens_issue_9047.md:1:28:1:29:**
```roc
foo = {x: 5, y: 6}.Foo).bar()
```
                           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**formatter_drops_tokens_issue_9047.md:1:29:1:30:**
```roc
foo = {x: 5, y: 6}.Foo).bar()
```
                            ^


**RECORD BUILDER NOT SUPPORTED**
The type `Foo` is used in a record builder expression, but does not implement `map2`:
**formatter_drops_tokens_issue_9047.md:1:7:1:23:**
```roc
foo = {x: 5, y: 6}.Foo).bar()
```
      ^^^^^^^^^^^^^^^^

Hint: To use `Foo` as a record builder, add a `map2` method to its type module.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,NoSpaceDotUpperIdent,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-record-builder
				(mapper (e-tag (raw "Foo")))
				(field (field "x")
					(e-int (raw "5")))
				(field (field "y")
					(e-int (raw "6")))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
foo = { x: 5, y: 6 }.Foo
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-runtime-error (tag "record_builder_map2_not_found"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
