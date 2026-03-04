# META
~~~ini
description=File import empty file as List(U8)
type=snippet
~~~
# SOURCE
~~~roc
import "test/snapshots/eval/file_import_empty_data.txt" as data : List(U8)

expect List.len(data) == 0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwImport,StringStart,StringPart,StringEnd,KwAs,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
KwExpect,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-file-import
			(path "test/snapshots/eval/file_import_empty_data.txt")
			(name "data")
			(type "List(U8)"))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "List.len"))
					(e-ident (raw "data")))
				(e-int (raw "0"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "data"))
		(e-bytes-literal (len "0")))
	(s-expect
		(e-binop (op "eq")
			(e-call
				(e-lookup-external
					(builtin))
				(e-lookup-local
					(p-assign (ident "data"))))
			(e-num (value "0")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(U8)")))
	(expressions
		(expr (type "List(U8)"))))
~~~
