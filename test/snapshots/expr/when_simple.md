# META
~~~ini
description=Simple when expression
type=snippet
~~~
# SOURCE
~~~roc
when x is
    Ok(value) -> value
    Err(msg) -> msg
~~~
# EXPECTED
PARSE ERROR - when_simple.md:1:1:1:5
PARSE ERROR - when_simple.md:1:6:1:7
PARSE ERROR - when_simple.md:1:8:1:10
PARSE ERROR - when_simple.md:2:15:2:17
PARSE ERROR - when_simple.md:2:18:2:23
PARSE ERROR - when_simple.md:3:14:3:16
PARSE ERROR - when_simple.md:3:17:3:20
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**when_simple.md:1:1:1:5:**
```roc
when x is
```
^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**when_simple.md:1:6:1:7:**
```roc
when x is
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**when_simple.md:1:8:1:10:**
```roc
when x is
```
       ^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**when_simple.md:2:15:2:17:**
```roc
    Ok(value) -> value
```
              ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**when_simple.md:2:18:2:23:**
```roc
    Ok(value) -> value
```
                 ^^^^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**when_simple.md:3:14:3:16:**
```roc
    Err(msg) -> msg
```
             ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**when_simple.md:3:17:3:20:**
```roc
    Err(msg) -> msg
```
                ^^^


# TOKENS
~~~zig
LowerIdent,LowerIdent,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,LowerIdent,
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
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
