# META
~~~ini
description=Open record type annotation
type=statement
~~~
# SOURCE
~~~roc
processUser! : { name : Str, age : U32, .. } => Str
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **.. }** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**type_open_record.md:1:41:1:45:**
```roc
processUser! : { name : Str, age : U32, .. } => Str
```


**PARSE ERROR**
A parsing error occurred: `expected_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_open_record.md:1:36:1:40:**
```roc
processUser! : { name : Str, age : U32, .. } => Str
```


**PARSE ERROR**
A parsing error occurred: `expected_ty_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_open_record.md:1:46:1:52:**
```roc
processUser! : { name : Str, age : U32, .. } => Str
```


# TOKENS
~~~zig
LowerIdent(1:1-1:13),OpColon(1:14-1:15),OpenCurly(1:16-1:17),LowerIdent(1:18-1:22),OpColon(1:23-1:24),UpperIdent(1:25-1:28),Comma(1:28-1:29),LowerIdent(1:30-1:33),OpColon(1:34-1:35),UpperIdent(1:36-1:39),Comma(1:39-1:40),DoubleDot(1:41-1:43),CloseCurly(1:44-1:45),OpFatArrow(1:46-1:48),UpperIdent(1:49-1:52),EndOfFile(1:52-1:52),
~~~
# PARSE
~~~clojure
(s-type-anno @1-1-1-52 (name "processUser!")
	(ty-malformed @1-46-1-52 (tag "expected_ty_close_curly_or_comma")))
~~~
# FORMATTED
~~~roc
processUser! : 
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~