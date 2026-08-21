# META
~~~ini
description=Multiline without comma formatting everything
type=snippet
~~~
# SOURCE
~~~roc
# Import exposing
import I1 exposing [
	I11,
	I12
]
import I2 exposing [
	I21 as Ias1,
	I22 as Ias2
]

# Where constraint
A(a) : a
	where [
		a.a1 : (
			a,
			a
		) -> Str,
		a.a2 : (
			a,
			a
		) -> Str
	]
B(b) : b
	where [
		b.b1 : (
			b,
			b
		) -> Str,
		b.b2 : (
			b,
			b
		) -> Str
	]

C(
	a,
	b
) : (
	a,
	b
)
D(
	a,
	b
) : C(
	a,
	b
)
E : {
	a : Str,
	b : Str
}
F : [
	A,
	B
]

g : e -> e
	where [
		e.A,
		e.B
	]

h = |x, y| {
	h1 = {
		h11: x,
		h12: x,
		h13: {
			h131: x,
			h132: y
		}
	}
	h2 = h(
		x,
		y
	)
	h3 = A(
		x,
		y
	)
	h4 = [
		x,
		y
	]
	h5 = (
		x,
		y
	)

	match x {
		Z1(
			(
				a,
				b
			)
		) => a
		Z2(
			a,
			b
		) => a
		Z3(
			{
				a,
				b
			}
		) => a
		Z4(
			[
				a,
				b
			]
		) => a
	}
}
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:12:1:22:3
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:23:1:33:3
UNUSED VARIABLE - everything.md:94:5:94:6
UNUSED VARIABLE - everything.md:99:4:99:5
UNUSED VARIABLE - everything.md:104:5:104:6
UNUSED VARIABLE - everything.md:110:5:110:6
UNUSED VARIABLE - everything.md:65:2:65:4
UNUSED VARIABLE - everything.md:73:2:73:4
UNUSED VARIABLE - everything.md:77:2:77:4
UNUSED VARIABLE - everything.md:81:2:81:4
UNUSED VARIABLE - everything.md:85:2:85:4
NOT A WHERE ALIAS - everything.md:60:4:60:6
NOT A WHERE ALIAS - everything.md:61:4:61:6
DECLARATION HAS NO VALUE - everything.md:58:1:62:3
NON EXHAUSTIVE MATCH - everything.md:90:2:113:3
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Where Clause Not Allowed In Type Declaration")
		(region (start 12 1) (end 22 3))
		(headline
			(text "You cannot define a ")
			(annotated code "where")
			(reflow " clause inside a type declaration."))
		(document
			(source-region (file "everything.md") (start 12 1) (end 22 3) (annotation error) (line-text "A(a) : a\n\twhere [\n\t\ta.a1 : (\n\t\t\ta,\n\t\t\ta\n\t\t) -> Str,\n\t\ta.a2 : (\n\t\t\ta,\n\t\t\ta\n\t\t) -> Str\n\t]"))))
	(report
		(severity runtime_error)
		(title "Where Clause Not Allowed In Type Declaration")
		(region (start 23 1) (end 33 3))
		(headline
			(text "You cannot define a ")
			(annotated code "where")
			(reflow " clause inside a type declaration."))
		(document
			(source-region (file "everything.md") (start 23 1) (end 33 3) (annotation error) (line-text "B(b) : b\n\twhere [\n\t\tb.b1 : (\n\t\t\tb,\n\t\t\tb\n\t\t) -> Str,\n\t\tb.b2 : (\n\t\t\tb,\n\t\t\tb\n\t\t) -> Str\n\t]"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 94 5) (end 94 6))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "b")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_b")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "everything.md") (start 94 5) (end 94 6) (annotation error) (line-text "\t\t\t\tb"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 99 4) (end 99 5))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "b")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_b")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "everything.md") (start 99 4) (end 99 5) (annotation error) (line-text "\t\t\tb"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 104 5) (end 104 6))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "b")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_b")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "everything.md") (start 104 5) (end 104 6) (annotation error) (line-text "\t\t\t\tb"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 110 5) (end 110 6))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "b")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_b")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "everything.md") (start 110 5) (end 110 6) (annotation error) (line-text "\t\t\t\tb"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 65 2) (end 65 4))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "h1")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_h1")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "everything.md") (start 65 2) (end 65 4) (annotation error) (line-text "\th1 = {"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 73 2) (end 73 4))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "h2")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_h2")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "everything.md") (start 73 2) (end 73 4) (annotation error) (line-text "\th2 = h("))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 77 2) (end 77 4))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "h3")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_h3")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "everything.md") (start 77 2) (end 77 4) (annotation error) (line-text "\th3 = A("))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 81 2) (end 81 4))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "h4")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_h4")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "everything.md") (start 81 2) (end 81 4) (annotation error) (line-text "\th4 = ["))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 85 2) (end 85 4))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "h5")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_h5")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "everything.md") (start 85 2) (end 85 4) (annotation error) (line-text "\th5 = ("))))
	(report
		(severity runtime_error)
		(title "Not a Where Alias")
		(region (start 60 4) (end 60 6))
		(headline
			(reflow "A where clause can only name a where alias, but")
			(reflow " ")
			(annotated type "A")
			(reflow " ")
			(reflow "is a type."))
		(document
			(source-region (file "everything.md") (start 60 4) (end 60 6) (annotation error) (line-text "\t\te.A,"))
			(line-break)
			(reflow "A where alias names a set of method constraints, declared like")
			(reflow " ")
			(annotated code "a.Sortable : where [a.compare : a -> [LT, EQ, GT]]")
			(reflow " ")
			(reflow "and written in a where clause as")
			(reflow " ")
			(annotated code "where [a.Sortable]")))
	(report
		(severity runtime_error)
		(title "Not a Where Alias")
		(region (start 61 4) (end 61 6))
		(headline
			(reflow "A where clause can only name a where alias, but")
			(reflow " ")
			(annotated type "B")
			(reflow " ")
			(reflow "is a type."))
		(document
			(source-region (file "everything.md") (start 61 4) (end 61 6) (annotation error) (line-text "\t\te.B"))
			(line-break)
			(reflow "A where alias names a set of method constraints, declared like")
			(reflow " ")
			(annotated code "a.Sortable : where [a.compare : a -> [LT, EQ, GT]]")
			(reflow " ")
			(reflow "and written in a where clause as")
			(reflow " ")
			(annotated code "where [a.Sortable]")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 58 1) (end 62 3))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "everything.md") (start 58 1) (end 62 3) (annotation error) (line-text "g : e -> e\n\twhere [\n\t\te.A,\n\t\te.B\n\t]"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Non Exhaustive Match")
		(region (start 90 2) (end 113 3))
		(headline
			(reflow "This match expression doesn't cover all possible cases."))
		(document
			(source-region (file "everything.md") (start 90 2) (end 113 3) (annotation error) (line-text "\tmatch x {\n\t\tZ1(\n\t\t\t(\n\t\t\t\ta,\n\t\t\t\tb\n\t\t\t)\n\t\t) => a\n\t\tZ2(\n\t\t\ta,\n\t\t\tb\n\t\t) => a\n\t\tZ3(\n\t\t\t{\n\t\t\t\ta,\n\t\t\t\tb\n\t\t\t}\n\t\t) => a\n\t\tZ4(\n\t\t\t[\n\t\t\t\ta,\n\t\t\t\tb\n\t\t\t]\n\t\t) => a\n\t}"))
			(line-break)
			(reflow "The value being matched on has type:")
			(line-break)
			(text "        ")
			(annotated type "[Z1((c, _field)), Z2(c, _d), Z3({ a: c, b: _field }), Z4(List(c))]")
			(line-break)
			(line-break)
			(reflow "Missing patterns:")
			(line-break)
			(text "    ")
			(annotation-start code-block)
			(indent 1)
			(text "Z4 []")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Hint: Add branches to handle these cases, or use")
			(reflow " ")
			(annotated keyword "_")
			(reflow " ")
			(reflow "to match anything."))))
~~~
# TOKENS
~~~zig
KwImport,UpperIdent,KwExposing,OpenSquare,
UpperIdent,Comma,
UpperIdent,
CloseSquare,
KwImport,UpperIdent,KwExposing,OpenSquare,
UpperIdent,KwAs,UpperIdent,Comma,
UpperIdent,KwAs,UpperIdent,
CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,LowerIdent,
KwWhere,OpenSquare,
LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpArrow,UpperIdent,Comma,
LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpArrow,UpperIdent,
CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,LowerIdent,
KwWhere,OpenSquare,
LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpArrow,UpperIdent,Comma,
LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpArrow,UpperIdent,
CloseSquare,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,
UpperIdent,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
LowerIdent,OpColon,UpperIdent,
CloseCurly,
UpperIdent,OpColon,OpenSquare,
UpperIdent,Comma,
UpperIdent,
CloseSquare,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
KwWhere,OpenSquare,
LowerIdent,NoSpaceDotUpperIdent,Comma,
LowerIdent,NoSpaceDotUpperIdent,
CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,
LowerIdent,OpAssign,OpenSquare,
LowerIdent,Comma,
LowerIdent,
CloseSquare,
LowerIdent,OpAssign,OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,
OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,
CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,
OpenCurly,
LowerIdent,Comma,
LowerIdent,
CloseCurly,
CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,
OpenSquare,
LowerIdent,Comma,
LowerIdent,
CloseSquare,
CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "I1")
			(exposing
				(exposed-upper-ident (text "I11"))
				(exposed-upper-ident (text "I12"))))
		(s-import (raw "I2")
			(exposing
				(exposed-upper-ident (text "I21") (as "Ias1"))
				(exposed-upper-ident (text "I22") (as "Ias2"))))
		(s-type-decl
			(header (name "A")
				(args
					(ty-var (raw "a"))))
			(ty-var (raw "a"))
			(where
				(method (mod-of "a") (name "a1")
					(args
						(ty-tuple
							(ty-var (raw "a"))
							(ty-var (raw "a"))))
					(ty (name "Str")))
				(method (mod-of "a") (name "a2")
					(args
						(ty-tuple
							(ty-var (raw "a"))
							(ty-var (raw "a"))))
					(ty (name "Str")))))
		(s-type-decl
			(header (name "B")
				(args
					(ty-var (raw "b"))))
			(ty-var (raw "b"))
			(where
				(method (mod-of "b") (name "b1")
					(args
						(ty-tuple
							(ty-var (raw "b"))
							(ty-var (raw "b"))))
					(ty (name "Str")))
				(method (mod-of "b") (name "b2")
					(args
						(ty-tuple
							(ty-var (raw "b"))
							(ty-var (raw "b"))))
					(ty (name "Str")))))
		(s-type-decl
			(header (name "C")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-tuple
				(ty-var (raw "a"))
				(ty-var (raw "b"))))
		(s-type-decl
			(header (name "D")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-apply
				(ty (name "C"))
				(ty-var (raw "a"))
				(ty-var (raw "b"))))
		(s-type-decl
			(header (name "E")
				(args))
			(ty-record
				(anno-record-field (name "a")
					(ty (name "Str")))
				(anno-record-field (name "b")
					(ty (name "Str")))))
		(s-type-decl
			(header (name "F")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))
					(ty (name "B")))))
		(s-type-anno (name "g")
			(ty-fn
				(ty-var (raw "e"))
				(ty-var (raw "e")))
			(where
				(alias (mod-of "e")
					(ty (name "A")))
				(alias (mod-of "e")
					(ty (name "B")))))
		(s-decl
			(p-ident (raw "h"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "h1"))
							(e-record
								(field (field "h11")
									(e-ident (raw "x")))
								(field (field "h12")
									(e-ident (raw "x")))
								(field (field "h13")
									(e-record
										(field (field "h131")
											(e-ident (raw "x")))
										(field (field "h132")
											(e-ident (raw "y")))))))
						(s-decl
							(p-ident (raw "h2"))
							(e-apply
								(e-ident (raw "h"))
								(e-ident (raw "x"))
								(e-ident (raw "y"))))
						(s-decl
							(p-ident (raw "h3"))
							(e-apply
								(e-tag (raw "A"))
								(e-ident (raw "x"))
								(e-ident (raw "y"))))
						(s-decl
							(p-ident (raw "h4"))
							(e-list
								(e-ident (raw "x"))
								(e-ident (raw "y"))))
						(s-decl
							(p-ident (raw "h5"))
							(e-tuple
								(e-ident (raw "x"))
								(e-ident (raw "y"))))
						(e-match
							(e-ident (raw "x"))
							(branches
								(branch
									(p-tag (raw "Z1")
										(p-tuple
											(p-ident (raw "a"))
											(p-ident (raw "b"))))
									(e-ident (raw "a")))
								(branch
									(p-tag (raw "Z2")
										(p-ident (raw "a"))
										(p-ident (raw "b")))
									(e-ident (raw "a")))
								(branch
									(p-tag (raw "Z3")
										(p-record
											(field (name "a") (rest false))
											(field (name "b") (rest false))))
									(e-ident (raw "a")))
								(branch
									(p-tag (raw "Z4")
										(p-list
											(p-ident (raw "a"))
											(p-ident (raw "b"))))
									(e-ident (raw "a")))))))))))
~~~
# FORMATTED
~~~roc
# Import exposing
import I1 exposing [I11, I12]
import I2 exposing [I21 as Ias1, I22 as Ias2]

# Where constraint
A(a) : a
	where [
		a.a1 : (a, a) -> Str,
		a.a2 : (a, a) -> Str,
	]

B(b) : b
	where [
		b.b1 : (b, b) -> Str,
		b.b2 : (b, b) -> Str,
	]

C(a, b) : (a, b)

D(a, b) : C(a, b)

E : { a : Str, b : Str }

F : [A, B]

g : e -> e
	where [e.A, e.B]

h = |x, y| {
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y } }
	h2 = h(x, y)
	h3 = A(x, y)
	h4 = [x, y]
	h5 = (x, y)

	match x {
		Z1((a, b)) => a
		Z2(a, b) => a
		Z3({ a, b }) => a
		Z4([a, b]) => a
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "g"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "e"))
				(ty-rigid-var-lookup (ty-rigid-var (name "e"))))
			(where
				(alias
					(ty-rigid-var-lookup (ty-rigid-var (name "e")))
					(ty-lookup (name "A") (local)))
				(alias
					(ty-rigid-var-lookup (ty-rigid-var (name "e")))
					(ty-lookup (name "B") (local))))))
	(d-let
		(p-assign (ident "h"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-block
				(s-let
					(p-assign (ident "h1"))
					(e-record
						(fields
							(field (name "h11")
								(e-lookup-local
									(p-assign (ident "x"))))
							(field (name "h12")
								(e-lookup-local
									(p-assign (ident "x"))))
							(field (name "h13")
								(e-record
									(fields
										(field (name "h131")
											(e-lookup-local
												(p-assign (ident "x"))))
										(field (name "h132")
											(e-lookup-local
												(p-assign (ident "y"))))))))))
				(s-let
					(p-assign (ident "h2"))
					(e-call (constraint-fn-var 359)
						(e-lookup-local
							(p-assign (ident "h")))
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y")))))
				(s-let
					(p-assign (ident "h3"))
					(e-tag (name "A")
						(args
							(e-lookup-local
								(p-assign (ident "x")))
							(e-lookup-local
								(p-assign (ident "y"))))))
				(s-let
					(p-assign (ident "h4"))
					(e-list
						(elems
							(e-lookup-local
								(p-assign (ident "x")))
							(e-lookup-local
								(p-assign (ident "y"))))))
				(s-let
					(p-assign (ident "h5"))
					(e-tuple
						(elems
							(e-lookup-local
								(p-assign (ident "x")))
							(e-lookup-local
								(p-assign (ident "y"))))))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "x"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-lookup-local
										(p-assign (ident "a")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-lookup-local
										(p-assign (ident "a")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-lookup-local
										(p-assign (ident "a")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-lookup-local
										(p-assign (ident "a")))))))))))
	(s-import (mod "I1")
		(exposes
			(exposed (name "I11") (wildcard false))
			(exposed (name "I12") (wildcard false))))
	(s-import (mod "I2")
		(exposes
			(exposed (name "I21") (alias "Ias1") (wildcard false))
			(exposed (name "I22") (alias "Ias2") (wildcard false))))
	(s-alias-decl
		(ty-header (name "A")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
	(s-alias-decl
		(ty-header (name "B")
			(ty-args
				(ty-rigid-var (name "b"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "b"))))
	(s-alias-decl
		(ty-header (name "C")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-tuple
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var (name "b")))))
	(s-alias-decl
		(ty-header (name "D")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-apply (name "C") (local)
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var (name "b")))))
	(s-alias-decl
		(ty-header (name "E"))
		(ty-record
			(field (field "a")
				(ty-lookup (name "Str") (builtin)))
			(field (field "b")
				(ty-lookup (name "Str") (builtin)))))
	(s-alias-decl
		(ty-header (name "F"))
		(ty-tag-union
			(ty-tag-name (name "A"))
			(ty-tag-name (name "B")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Error"))
		(patt (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))], [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))] -> c")))
	(type_decls
		(alias (type "A(a)")
			(ty-header (name "A")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "B(b)")
			(ty-header (name "B")
				(ty-args
					(ty-rigid-var (name "b")))))
		(alias (type "C(a, b)")
			(ty-header (name "C")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "D(a, b)")
			(ty-header (name "D")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "E")
			(ty-header (name "E")))
		(alias (type "F")
			(ty-header (name "F"))))
	(expressions
		(expr (type "Error -> Error"))
		(expr (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))], [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))] -> c"))))
~~~
