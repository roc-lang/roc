# META
~~~ini
description=Constrained record type annotation
type=statement
~~~
# SOURCE
~~~roc
process_user! : { name : Str, age : U32, ..a } => Str
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **..a** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**type_constrained_record.md:1:42:1:45:**
```roc
process_user! : { name : Str, age : U32, ..a } => Str
```


**PARSE ERROR**
A parsing error occurred: `expected_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_constrained_record.md:1:37:1:41:**
```roc
process_user! : { name : Str, age : U32, ..a } => Str
```


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

# TOKENS
~~~zig
LowerIdent(1:1-1:14),OpColon(1:15-1:16),OpenCurly(1:17-1:18),LowerIdent(1:19-1:23),OpColon(1:24-1:25),UpperIdent(1:26-1:29),Comma(1:29-1:30),LowerIdent(1:31-1:34),OpColon(1:35-1:36),UpperIdent(1:37-1:40),Comma(1:40-1:41),DoubleDot(1:42-1:44),LowerIdent(1:44-1:45),CloseCurly(1:46-1:47),OpFatArrow(1:48-1:50),UpperIdent(1:51-1:54),EndOfFile(1:54-1:54),
~~~
# PARSE
~~~clojure
(s-type-anno @1-1-1-54 (name "process_user!")
	(ty-fn @1-17-1-54
		(ty-record @1-17-1-47
			(anno-record-field @1-19-1-30 (name "name")
				(ty (name "Str")))
			(anno-record-field @1-31-1-47 (name "age")
				(ty-malformed @1-37-1-47 (tag "expected_arrow"))))
		(ty (name "Str"))))
~~~
# FORMATTED
~~~roc
process_user! : { name : Str, age :  } => Str
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno @1-1-1-54 (name "process_user!") (id 80)
		(ty-fn @1-17-1-54 (effectful true)
			(ty-record @1-17-1-47
				(field (field "name")
					(ty @1-26-1-29 (name "Str")))
				(field (field "age")
					(ty-malformed @1-37-1-47)))
			(ty @1-51-1-54 (name "Str")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~