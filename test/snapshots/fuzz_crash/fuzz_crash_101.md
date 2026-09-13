# META
~~~ini
description=Parser formatting non-stable roundtrip
type=file
~~~
# SOURCE
~~~roc
r:(),(->c),(->d)->(c,)
r=|()|(()())
a={
}
~~~
# EXPECTED
EMPTY TUPLE NOT ALLOWED - fuzz_crash_101.md:2:8:2:10
TYPE MISMATCH - fuzz_crash_101.md:2:3:2:13
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Empty Tuple Not Allowed")
		(region (start 2 8) (end 2 10))
		(headline
			(reflow "I am part way through parsing this tuple, but it is empty."))
		(document
			(source-region (file "fuzz_crash_101.md") (start 2 8) (end 2 10) (annotation error) (line-text "r=|()|(()())"))
			(line-break)
			(reflow "If you want to represent nothing, try using an empty record: ")
			(annotated code "{}")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 3) (end 2 13))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "fuzz_crash_101.md") (start 2 3) (end 2 13) (annotation error) (line-text "r=|()|(()())"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "() -> Error")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(), (({}) -> c), (({}) -> d) -> c")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This function expects")
			(reflow " ")
			(reflow "3")
			(reflow " ")
			(reflow "arguments")
			(reflow " ")
			(reflow "but got")
			(reflow " ")
			(reflow "1")
			(reflow "."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,NoSpaceOpenRound,CloseRound,Comma,NoSpaceOpenRound,OpArrow,LowerIdent,CloseRound,Comma,NoSpaceOpenRound,OpArrow,LowerIdent,CloseRound,OpArrow,NoSpaceOpenRound,LowerIdent,Comma,CloseRound,
LowerIdent,OpAssign,OpBar,NoSpaceOpenRound,CloseRound,OpBar,NoSpaceOpenRound,NoSpaceOpenRound,CloseRound,NoSpaceOpenRound,CloseRound,CloseRound,
LowerIdent,OpAssign,OpenCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "r")
			(ty-fn
				(ty-tuple)
				(ty-fn
					(ty-var (raw "c")))
				(ty-fn
					(ty-var (raw "d")))
				(ty-tuple
					(ty-var (raw "c")))))
		(s-decl
			(p-ident (raw "r"))
			(e-lambda
				(args
					(p-tuple))
				(e-tuple
					(e-apply
						(e-tuple)))))
		(s-decl
			(p-ident (raw "a"))
			(e-record))))
~~~
# FORMATTED
~~~roc
r : (),
(() -> c),
(() -> d) -> (
	c,
)
r = |()| (()())

a = {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-tuple)
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var (name "c"))))
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var (name "d"))))
				(ty-rigid-var-lookup (ty-rigid-var (name "c"))))))
	(d-let
		(p-assign (ident "a"))
		(e-empty_record)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(), (({}) -> c), (({}) -> d) -> c"))
		(patt (type "{}")))
	(expressions
		(expr (type "(), (({}) -> c), (({}) -> d) -> c"))
		(expr (type "{}"))))
~~~
