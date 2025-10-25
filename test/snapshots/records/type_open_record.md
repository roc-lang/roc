# META
~~~ini
description=Open record type annotation
type=statement
~~~
# SOURCE
~~~roc
process_user! : { name : Str, age : U32, .. } => Str
~~~
# EXPECTED
UNEXPECTED TOKEN IN TYPE ANNOTATION - type_open_record.md:1:42:1:44
PARSE ERROR - type_open_record.md:1:37:1:40
PARSE ERROR - type_open_record.md:1:47:1:49
MALFORMED TYPE - type_open_record.md:1:47:1:49
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **..** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**type_open_record.md:1:42:1:44:**
```roc
process_user! : { name : Str, age : U32, .. } => Str
```
                                         ^^


**PARSE ERROR**
A parsing error occurred: `expected_arrow`
This is an unexpected parsing error. Please check your syntax.

**type_open_record.md:1:37:1:40:**
```roc
process_user! : { name : Str, age : U32, .. } => Str
```
                                    ^^^


**PARSE ERROR**
A parsing error occurred: `expected_ty_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

**type_open_record.md:1:47:1:49:**
```roc
process_user! : { name : Str, age : U32, .. } => Str
```
                                              ^^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**type_open_record.md:1:47:1:49:**
```roc
process_user! : { name : Str, age : U32, .. } => Str
```
                                              ^^


# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,DoubleDot,CloseCurly,OpFatArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-type-anno (name "process_user!")
	(ty-malformed (tag "expected_ty_close_curly_or_comma")))
~~~
# FORMATTED
~~~roc
process_user! : 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno (name "process_user!")
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
