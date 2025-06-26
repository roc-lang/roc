# META
~~~ini
description=Function type annotation returning record
type=statement
~~~
# SOURCE
~~~roc
createUser! : Str, U32 => { name : Str, age : U32, id : U64, active : Bool }
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:12),OpColon(1:13-1:14),UpperIdent(1:15-1:18),Comma(1:18-1:19),UpperIdent(1:20-1:23),OpFatArrow(1:24-1:26),OpenCurly(1:27-1:28),LowerIdent(1:29-1:33),OpColon(1:34-1:35),UpperIdent(1:36-1:39),Comma(1:39-1:40),LowerIdent(1:41-1:44),OpColon(1:45-1:46),UpperIdent(1:47-1:50),Comma(1:50-1:51),LowerIdent(1:52-1:54),OpColon(1:55-1:56),UpperIdent(1:57-1:60),Comma(1:60-1:61),LowerIdent(1:62-1:68),OpColon(1:69-1:70),UpperIdent(1:71-1:75),CloseCurly(1:76-1:77),EndOfFile(1:77-1:77),
~~~
# PARSE
~~~clojure
(s-type-anno @1-1-1-77 (name "createUser!")
	(ty-fn @1-15-1-77
		(ty (name "Str"))
		(ty (name "U32"))
		(ty-record @1-27-1-77
			(anno-record-field @1-29-1-40 (name "name")
				(ty (name "Str")))
			(anno-record-field @1-41-1-51 (name "age")
				(ty (name "U32")))
			(anno-record-field @1-52-1-61 (name "id")
				(ty (name "U64")))
			(anno-record-field @1-62-1-77 (name "active")
				(ty (name "Bool"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~