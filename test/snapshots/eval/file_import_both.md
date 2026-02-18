# META
~~~ini
description=File import as both Str and List(U8)
type=snippet
~~~
# SOURCE
~~~roc
import "test/snapshots/eval/file_import_test_data.txt" as text : Str
import "test/snapshots/eval/file_import_test_data.txt" as bytes : List(U8)

expect text == "hello world"
expect List.len(bytes) == 11
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwImport,StringStart,StringPart,StringEnd,KwAs,LowerIdent,OpColon,UpperIdent,
KwImport,StringStart,StringPart,StringEnd,KwAs,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
KwExpect,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,
KwExpect,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-file-import
			(path "test/snapshots/eval/file_import_test_data.txt")
			(name "text")
			(type "Str"))
		(s-file-import
			(path "test/snapshots/eval/file_import_test_data.txt")
			(name "bytes")
			(type "List(U8)"))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "text"))
				(e-string
					(e-string-part (raw "hello world")))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "List.len"))
					(e-ident (raw "bytes")))
				(e-int (raw "11"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "text"))
		(e-literal (string "hello world")))
	(d-let
		(p-assign (ident "bytes"))
		(e-bytes-literal (len "11")))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "text")))
			(e-string
				(e-literal (string "hello world")))))
	(s-expect
		(e-binop (op "eq")
			(e-call
				(e-lookup-external
					(builtin))
				(e-lookup-local
					(p-assign (ident "bytes"))))
			(e-num (value "11")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str"))
		(patt (type "List(U8)")))
	(expressions
		(expr (type "Str"))
		(expr (type "List(U8)"))))
~~~
