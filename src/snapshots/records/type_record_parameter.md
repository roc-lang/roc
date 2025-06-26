# META
~~~ini
description=Function type annotation with record parameter
type=statement
~~~
# SOURCE
~~~roc
process_things : { name : Str, age : U32, thing: a }, (a -> Str) -> Str
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:15),OpColon(1:16-1:17),OpenCurly(1:18-1:19),LowerIdent(1:20-1:24),OpColon(1:25-1:26),UpperIdent(1:27-1:30),Comma(1:30-1:31),LowerIdent(1:32-1:35),OpColon(1:36-1:37),UpperIdent(1:38-1:41),Comma(1:41-1:42),LowerIdent(1:43-1:48),OpColon(1:48-1:49),LowerIdent(1:50-1:51),CloseCurly(1:52-1:53),Comma(1:53-1:54),OpenRound(1:55-1:56),LowerIdent(1:56-1:57),OpArrow(1:58-1:60),UpperIdent(1:61-1:64),CloseRound(1:64-1:65),OpArrow(1:66-1:68),UpperIdent(1:69-1:72),EndOfFile(1:72-1:72),
~~~
# PARSE
~~~clojure
(s-type-anno @1-1-1-72 (name "process_things")
	(ty-fn @1-18-1-72
		(ty-record @1-18-1-53
			(anno-record-field @1-20-1-31 (name "name")
				(ty (name "Str")))
			(anno-record-field @1-32-1-42 (name "age")
				(ty (name "U32")))
			(anno-record-field @1-43-1-53 (name "thing")
				(ty-var @1-50-1-51 (raw "a"))))
		(ty-fn @1-56-1-64
			(ty-var @1-56-1-57 (raw "a"))
			(ty (name "Str")))
		(ty (name "Str"))))
~~~
# FORMATTED
~~~roc
process_things : { name : Str, age : U32, thing : a }, (a -> Str) -> Str
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~