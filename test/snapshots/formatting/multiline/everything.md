# META
~~~ini
description=Multiline formatting everything
type=snippet
~~~
# SOURCE
~~~roc
# Import exposing
import I1 exposing [
	I11,
	I12,
]
import I2 exposing [
	I21 as Ias1,
	I22 as Ias2,
]

# Where constraint
A(a) : a
	where [
		a.a1 : (
			a,
			a,
		) -> Str,
		a.a2 : (
			a,
			a,
		) -> Str,
	]
B(b) : b
	where [
		b.b1 : (
			b,
			b,
		) -> Str,
		b.b2 : (
			b,
			b,
		) -> Str,
	]

C(
	a,
	b,
) : (
	a,
	b,
)
D(
	a,
	b,
) : C(
	a,
	b,
)
E : {
	a : Str,
	b : Str,
}
F : [
	A,
	B,
]

g : e -> e
	where [
		e.A,
		e.B,
	]

h = |x, y| {
	h1 = {
		h11: x,
		h12: x,
		h13: {
			h131: x,
			h132: y,
		},
	}
	h2 = h(
		x,
		y,
	)
	h3 = A(
		x,
		y,
	)
	h4 = [
		x,
		y,
	]
	h5 = (
		x,
		y,
	)

	match x {
		Z1(
			(
				a,
				b,
			),
		) => a
		Z2(
			a,
			b,
		) => a
		Z3(
			{
				a,
				b,
			},
		) => a
		Z4(
			[
				a,
				b,
			],
		) => a
	}
}
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:12:1:22:3
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:23:1:33:3
MODULE NOT FOUND - everything.md:2:1:5:2
MODULE NOT FOUND - everything.md:6:1:9:2
UNUSED VARIABLE - everything.md:94:5:94:6
UNUSED VARIABLE - everything.md:99:4:99:5
UNUSED VARIABLE - everything.md:104:5:104:6
UNUSED VARIABLE - everything.md:110:5:110:6
UNUSED VARIABLE - everything.md:65:2:65:4
UNUSED VARIABLE - everything.md:73:2:73:4
UNUSED VARIABLE - everything.md:77:2:77:4
UNUSED VARIABLE - everything.md:81:2:81:4
UNUSED VARIABLE - everything.md:85:2:85:4
# PROBLEMS
**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**everything.md:12:1:22:3:**
```roc
A(a) : a
	where [
		a.a1 : (
			a,
			a,
		) -> Str,
		a.a2 : (
			a,
			a,
		) -> Str,
	]
```


**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**everything.md:23:1:33:3:**
```roc
B(b) : b
	where [
		b.b1 : (
			b,
			b,
		) -> Str,
		b.b2 : (
			b,
			b,
		) -> Str,
	]
```


**MODULE NOT FOUND**
The module `I1` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:2:1:5:2:**
```roc
import I1 exposing [
	I11,
	I12,
]
```


**MODULE NOT FOUND**
The module `I2` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:6:1:9:2:**
```roc
import I2 exposing [
	I21 as Ias1,
	I22 as Ias2,
]
```


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:94:5:94:6:**
```roc
				b,
```
				^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:99:4:99:5:**
```roc
			b,
```
			^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:104:5:104:6:**
```roc
				b,
```
				^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:110:5:110:6:**
```roc
				b,
```
				^


**UNUSED VARIABLE**
Variable `h1` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h1` to suppress this warning.
The unused variable is declared here:
**everything.md:65:2:65:4:**
```roc
	h1 = {
```
	^^


**UNUSED VARIABLE**
Variable `h2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h2` to suppress this warning.
The unused variable is declared here:
**everything.md:73:2:73:4:**
```roc
	h2 = h(
```
	^^


**UNUSED VARIABLE**
Variable `h3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h3` to suppress this warning.
The unused variable is declared here:
**everything.md:77:2:77:4:**
```roc
	h3 = A(
```
	^^


**UNUSED VARIABLE**
Variable `h4` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h4` to suppress this warning.
The unused variable is declared here:
**everything.md:81:2:81:4:**
```roc
	h4 = [
```
	^^


**UNUSED VARIABLE**
Variable `h5` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h5` to suppress this warning.
The unused variable is declared here:
**everything.md:85:2:85:4:**
```roc
	h5 = (
```
	^^


# TOKENS
~~~zig
KwImport,UpperIdent,KwExposing,OpenSquare,
UpperIdent,Comma,
UpperIdent,Comma,
CloseSquare,
KwImport,UpperIdent,KwExposing,OpenSquare,
UpperIdent,KwAs,UpperIdent,Comma,
UpperIdent,KwAs,UpperIdent,Comma,
CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,LowerIdent,
KwWhere,OpenSquare,
LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,OpArrow,UpperIdent,Comma,
LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,OpArrow,UpperIdent,Comma,
CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,LowerIdent,
KwWhere,OpenSquare,
LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,OpArrow,UpperIdent,Comma,
LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,OpArrow,UpperIdent,Comma,
CloseSquare,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,
UpperIdent,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
LowerIdent,OpColon,UpperIdent,Comma,
CloseCurly,
UpperIdent,OpColon,OpenSquare,
UpperIdent,Comma,
UpperIdent,Comma,
CloseSquare,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
KwWhere,OpenSquare,
LowerIdent,NoSpaceDotUpperIdent,Comma,
LowerIdent,NoSpaceDotUpperIdent,Comma,
CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
CloseCurly,Comma,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,
LowerIdent,OpAssign,OpenSquare,
LowerIdent,Comma,
LowerIdent,Comma,
CloseSquare,
LowerIdent,OpAssign,OpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,
OpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,Comma,
CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,
OpenCurly,
LowerIdent,Comma,
LowerIdent,Comma,
CloseCurly,Comma,
CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,
OpenSquare,
LowerIdent,Comma,
LowerIdent,Comma,
CloseSquare,Comma,
CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
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
			(ty-var (raw "a")))
		(s-type-decl
			(header (name "B")
				(args
					(ty-var (raw "b"))))
			(ty-var (raw "b")))
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
				(alias (module-of "e") (name "A"))
				(alias (module-of "e") (name "B"))))
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
import I1 exposing [
	I11,
	I12,
]
import I2 exposing [
	I21 as Ias1,
	I22 as Ias2,
]

# Where constraint
A(a) : a
	where
		[
			a.a1 : (
			a,
			a,
		) -> Str,
		
			a.a2 : (
			a,
			a,
		) -> Str
	]
B(b) : b
	where
		[
			b.b1 : (
			b,
			b,
		) -> Str,
		
			b.b2 : (
			b,
			b,
		) -> Str
	]

C(
	a,
	b,
) : (
	a,
	b,
)
D(
	a,
	b,
) : C(
	a,
	b,
)
E : {
	a : Str,
	b : Str,
}
F : [
	A,
	B,
]

g : e -> e
	where
		[
		e.A,
		e.B
	]

h = |x, y| {
	h1 = {
		h11: x,
		h12: x,
		h13: {
			h131: x,
			h132: y,
		},
	}
	h2 = h(
		x,
		y,
	)
	h3 = A(
		x,
		y,
	)
	h4 = [
		x,
		y,
	]
	h5 = (
		x,
		y,
	)

	match x {
		Z1(
			(
				a,
				b,
			),
		) => a
		Z2(
			a,
			b,
		) => a
		Z3(
			{
				a,
				b,
			},
		) => a
		Z4(
			[
				a,
				b,
			],
		) => a
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "h"))
		(e-closure
			(captures
				(capture (ident "h"))
				(capture (ident "a"))
				(capture (ident "a"))
				(capture (ident "a"))
				(capture (ident "a")))
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
						(e-call
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
											(p-assign (ident "a"))))))))))))
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
			(ty-tag-name (name "B"))))
	(s-import (module "I1")
		(exposes
			(exposed (name "I11") (wildcard false))
			(exposed (name "I12") (wildcard false))))
	(s-import (module "I2")
		(exposes
			(exposed (name "I21") (alias "Ias1") (wildcard false))
			(exposed (name "I22") (alias "Ias2") (wildcard false))))
	(s-type-anno (name "g")
		(ty-fn (effectful false)
			(ty-rigid-var (name "e"))
			(ty-rigid-var-lookup (ty-rigid-var (name "e"))))
		(where
			(alias (module-of "e") (ident "A"))
			(alias (module-of "e") (ident "B"))))
	(ext-decl (ident "e.A") (kind "type"))
	(ext-decl (ident "e.B") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j, [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j -> c")))
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
		(expr (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j, [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j -> c"))))
~~~
