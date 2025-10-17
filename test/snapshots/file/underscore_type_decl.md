# META
~~~ini
description=underscore_in_assignment_pattern
type=snippet
~~~
# SOURCE
~~~roc
import Module exposing [Pair]

Pair1(x, _) = Pair(0, 1)
Pair2(_, y) = Pair(0, 1)
Pair3(_, _) = Pair(0, 1)
~~~
# EXPECTED
PARSE ERROR - underscore_type_decl.md:3:13:3:14
PARSE ERROR - underscore_type_decl.md:3:20:3:21
PARSE ERROR - underscore_type_decl.md:3:23:3:24
PARSE ERROR - underscore_type_decl.md:4:1:4:6
PARSE ERROR - underscore_type_decl.md:4:6:4:7
PARSE ERROR - underscore_type_decl.md:4:7:4:8
PARSE ERROR - underscore_type_decl.md:4:8:4:9
PARSE ERROR - underscore_type_decl.md:4:10:4:11
PARSE ERROR - underscore_type_decl.md:4:11:4:12
PARSE ERROR - underscore_type_decl.md:4:13:4:14
PARSE ERROR - underscore_type_decl.md:4:20:4:21
PARSE ERROR - underscore_type_decl.md:4:23:4:24
PARSE ERROR - underscore_type_decl.md:5:1:5:6
PARSE ERROR - underscore_type_decl.md:5:6:5:7
PARSE ERROR - underscore_type_decl.md:5:7:5:8
PARSE ERROR - underscore_type_decl.md:5:8:5:9
PARSE ERROR - underscore_type_decl.md:5:10:5:11
PARSE ERROR - underscore_type_decl.md:5:11:5:12
PARSE ERROR - underscore_type_decl.md:5:13:5:14
PARSE ERROR - underscore_type_decl.md:5:20:5:21
PARSE ERROR - underscore_type_decl.md:5:23:5:24
PARSE ERROR - underscore_type_decl.md:6:1:6:1
MODULE NOT FOUND - underscore_type_decl.md:1:1:1:30
# PROBLEMS
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

**underscore_type_decl.md:3:13:3:14:**
```roc
Pair1(x, _) = Pair(0, 1)
```
            ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:3:20:3:21:**
```roc
Pair1(x, _) = Pair(0, 1)
```
                   ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:3:23:3:24:**
```roc
Pair1(x, _) = Pair(0, 1)
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

**underscore_type_decl.md:4:1:4:6:**
```roc
Pair2(_, y) = Pair(0, 1)
```
^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:6:4:7:**
```roc
Pair2(_, y) = Pair(0, 1)
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:7:4:8:**
```roc
Pair2(_, y) = Pair(0, 1)
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:8:4:9:**
```roc
Pair2(_, y) = Pair(0, 1)
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:10:4:11:**
```roc
Pair2(_, y) = Pair(0, 1)
```
         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:11:4:12:**
```roc
Pair2(_, y) = Pair(0, 1)
```
          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:13:4:14:**
```roc
Pair2(_, y) = Pair(0, 1)
```
            ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:20:4:21:**
```roc
Pair2(_, y) = Pair(0, 1)
```
                   ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:23:4:24:**
```roc
Pair2(_, y) = Pair(0, 1)
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

**underscore_type_decl.md:5:1:5:6:**
```roc
Pair3(_, _) = Pair(0, 1)
```
^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:6:5:7:**
```roc
Pair3(_, _) = Pair(0, 1)
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:7:5:8:**
```roc
Pair3(_, _) = Pair(0, 1)
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:8:5:9:**
```roc
Pair3(_, _) = Pair(0, 1)
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:10:5:11:**
```roc
Pair3(_, _) = Pair(0, 1)
```
         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:11:5:12:**
```roc
Pair3(_, _) = Pair(0, 1)
```
          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:13:5:14:**
```roc
Pair3(_, _) = Pair(0, 1)
```
            ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:20:5:21:**
```roc
Pair3(_, _) = Pair(0, 1)
```
                   ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:23:5:24:**
```roc
Pair3(_, _) = Pair(0, 1)
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

**underscore_type_decl.md:6:1:6:1:**
```roc

```
^


**MODULE NOT FOUND**
The module `Module` was not found in this Roc project.

You're attempting to use this module here:
**underscore_type_decl.md:1:1:1:30:**
```roc
import Module exposing [Pair]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport,UpperIdent,KwExposing,OpenSquare,UpperIdent,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,Underscore,CloseRound,OpAssign,UpperIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
UpperIdent,NoSpaceOpenRound,Underscore,Comma,LowerIdent,CloseRound,OpAssign,UpperIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
UpperIdent,NoSpaceOpenRound,Underscore,Comma,Underscore,CloseRound,OpAssign,UpperIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "Module")
			(exposing
				(exposed-upper-ident (text "Pair"))))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
import Module exposing [Pair]



~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (module "Module")
		(exposes
			(exposed (name "Pair") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
