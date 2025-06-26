# META
~~~ini
description=Function type annotation with record parameter
type=statement
~~~
# SOURCE
~~~roc
processUserThings : { name : Str, age : U32, thing: a }, (a -> Str) -> Str
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:18),OpColon(1:19-1:20),OpenCurly(1:21-1:22),LowerIdent(1:23-1:27),OpColon(1:28-1:29),UpperIdent(1:30-1:33),Comma(1:33-1:34),LowerIdent(1:35-1:38),OpColon(1:39-1:40),UpperIdent(1:41-1:44),Comma(1:44-1:45),LowerIdent(1:46-1:51),OpColon(1:51-1:52),LowerIdent(1:53-1:54),CloseCurly(1:55-1:56),Comma(1:56-1:57),OpenRound(1:58-1:59),LowerIdent(1:59-1:60),OpArrow(1:61-1:63),UpperIdent(1:64-1:67),CloseRound(1:67-1:68),OpArrow(1:69-1:71),UpperIdent(1:72-1:75),EndOfFile(1:75-1:75),
~~~
# PARSE
~~~clojure
(s-type-anno @1-1-1-75 (name "processUserThings")
	(ty-fn @1-21-1-75
		(ty-record @1-21-1-56
			(anno-record-field @1-23-1-34 (name "name")
				(ty (name "Str")))
			(anno-record-field @1-35-1-45 (name "age")
				(ty (name "U32")))
			(anno-record-field @1-46-1-56 (name "thing")
				(ty-var @1-53-1-54 (raw "a"))))
		(ty-fn @1-59-1-67
			(ty-var @1-59-1-60 (raw "a"))
			(ty (name "Str")))
		(ty (name "Str"))))
~~~
# FORMATTED
~~~roc
processUserThings : { name : Str, age : U32, thing : a }, (a -> Str) -> Str
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~