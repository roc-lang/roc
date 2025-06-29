# META
~~~ini
description=Open record type annotation
type=statement
~~~
# SOURCE
~~~roc
process_user! : { name : Str, age : U32, .. } => Str
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **.. }** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**type_open_record.md:1:42:1:46:**
```roc
process_user! : { name : Str, age : U32, .. } => Str
```


**PARSE ERROR**
A parsing error occurred: `expected_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_open_record.md:1:37:1:41:**
```roc
process_user! : { name : Str, age : U32, .. } => Str
```


**PARSE ERROR**
A parsing error occurred: `expected_ty_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_open_record.md:1:47:1:53:**
```roc
process_user! : { name : Str, age : U32, .. } => Str
```


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

# TOKENS
~~~zig
LowerIdent(1:1-1:14),OpColon(1:15-1:16),OpenCurly(1:17-1:18),LowerIdent(1:19-1:23),OpColon(1:24-1:25),UpperIdent(1:26-1:29),Comma(1:29-1:30),LowerIdent(1:31-1:34),OpColon(1:35-1:36),UpperIdent(1:37-1:40),Comma(1:40-1:41),DoubleDot(1:42-1:44),CloseCurly(1:45-1:46),OpFatArrow(1:47-1:49),UpperIdent(1:50-1:53),EndOfFile(1:53-1:53),
~~~
# PARSE
~~~clojure
(s-type-anno @1-1-1-53 (name "process_user!")
	(ty-malformed @1-47-1-53 (tag "expected_ty_close_curly_or_comma")))
~~~
# FORMATTED
~~~roc
process_user! : 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno @1-1-1-53 (name "process_user!") (id 75)
		(ty-malformed @1-47-1-53)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
