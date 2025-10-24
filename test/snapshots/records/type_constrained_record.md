# META
~~~ini
description=Constrained record type annotation
type=statement
~~~
# SOURCE
~~~roc
process_user! : { name : Str, age : U32, ..a } => Str
~~~
# EXPECTED
UNEXPECTED TOKEN IN TYPE ANNOTATION - type_constrained_record.md:1:42:1:44
PARSE ERROR - type_constrained_record.md:1:37:1:40
MALFORMED TYPE - type_constrained_record.md:1:37:1:45
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **..** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**type_constrained_record.md:1:42:1:44:**
```roc
process_user! : { name : Str, age : U32, ..a } => Str
```
                                         ^^


**PARSE ERROR**
A parsing error occurred: `expected_arrow`
This is an unexpected parsing error. Please check your syntax.

**type_constrained_record.md:1:37:1:40:**
```roc
process_user! : { name : Str, age : U32, ..a } => Str
```
                                    ^^^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**type_constrained_record.md:1:37:1:45:**
```roc
process_user! : { name : Str, age : U32, ..a } => Str
```
                                    ^^^^^^^^


# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,DoubleDot,LowerIdent,CloseCurly,OpFatArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-type-anno (name "process_user!")
	(ty-fn
		(ty-record
			(anno-record-field (name "name")
				(ty (name "Str")))
			(anno-record-field (name "age")
				(ty-malformed (tag "expected_arrow"))))
		(ty (name "Str"))))
~~~
# FORMATTED
~~~roc
process_user! : { name : Str, age :  } => Str
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno (name "process_user!")
		(ty-fn (effectful true)
			(ty-record
				(field (field "name")
					(ty-lookup (name "Str") (external-module "Str")))
				(field (field "age")
					(ty-malformed)))
			(ty-lookup (name "Str") (external-module "Str")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
