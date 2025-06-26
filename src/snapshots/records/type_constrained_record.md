# META
~~~ini
description=Constrained record type annotation
type=statement
~~~
# SOURCE
~~~roc
processUser! : { name : Str, age : U32, ..a } => Str
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **..a** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**type_constrained_record.md:1:41:1:44:**
```roc
processUser! : { name : Str, age : U32, ..a } => Str
```


**PARSE ERROR**
A parsing error occurred: `expected_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_constrained_record.md:1:36:1:40:**
```roc
processUser! : { name : Str, age : U32, ..a } => Str
```


# TOKENS
~~~zig
LowerIdent(1:1-1:13),OpColon(1:14-1:15),OpenCurly(1:16-1:17),LowerIdent(1:18-1:22),OpColon(1:23-1:24),UpperIdent(1:25-1:28),Comma(1:28-1:29),LowerIdent(1:30-1:33),OpColon(1:34-1:35),UpperIdent(1:36-1:39),Comma(1:39-1:40),DoubleDot(1:41-1:43),LowerIdent(1:43-1:44),CloseCurly(1:45-1:46),OpFatArrow(1:47-1:49),UpperIdent(1:50-1:53),EndOfFile(1:53-1:53),
~~~
# PARSE
~~~clojure
(s-type-anno @1-1-1-53 (name "processUser!")
	(ty-fn @1-16-1-53
		(ty-record @1-16-1-46
			(anno-record-field @1-18-1-29 (name "name")
				(ty (name "Str")))
			(anno-record-field @1-30-1-46 (name "age")
				(ty-malformed @1-36-1-46 (tag "expected_arrow"))))
		(ty (name "Str"))))
~~~
# FORMATTED
~~~roc
processUser! : { name : Str, age :  } => Str
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~