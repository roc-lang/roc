# META
~~~ini
description=Open record type annotation with named extension
type=statement
~~~
# SOURCE
~~~roc
process_user! : { name : Str, age : U32, ..a } => Str
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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
				(ty (name "U32")))
			(ty-record-ext
				(ty-var (raw "a"))))
		(ty (name "Str"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-let
		(p-assign (ident "process_user!"))
		(e-anno-only)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
