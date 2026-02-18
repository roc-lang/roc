# META
~~~ini
description=File import as Str
type=snippet
~~~
# SOURCE
~~~roc
import "test/snapshots/eval/file_import_test_data.txt" as data : Str

expect data == "hello world"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwImport,StringStart,StringPart,StringEnd,KwAs,LowerIdent,OpColon,UpperIdent,
KwExpect,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-file-import
			(path "test/snapshots/eval/file_import_test_data.txt")
			(name "data")
			(type "Str"))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "data"))
				(e-string
					(e-string-part (raw "hello world")))))))
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
		(e-literal (string "hello world")))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "data")))
			(e-string
				(e-literal (string "hello world"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str")))
	(expressions
		(expr (type "Str"))))
~~~
