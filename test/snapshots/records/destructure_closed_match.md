# META
~~~ini
description=Record pattern in a `match` branch without `..` is closed: matching a record with an extra field is a type mismatch
type=snippet
~~~
# SOURCE
~~~roc
describe : { x : U64, y : U64, z : U64 } -> U64
describe = |rec| match rec {
    { x, y } => x + y
}
~~~
# EXPECTED
TYPE MISMATCH - destructure_closed_match.md:2:18:2:18
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ The first pattern in this `match` is incompatible. ────────┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  describe = |rec| match rec {                                              │
 │      { x, y } => x + y                                                     │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────────────────────── destructure_closed_match.md:2:5 ┘

    The first pattern is trying to match:

        { x: _field, y: U64 }

    But the expression between the `match` parenthesis has the type:

        { x: U64, y: U64, z: U64 }

    These can never match! Either the pattern or expression has a problem.
    Hint: This pattern doesn't bind the `z` field. Match it explicitly with `z:
    _`, or add `..` to match all the remaining fields.

# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "describe")
			(ty-fn
				(ty-record
					(anno-record-field (name "x")
						(ty (name "U64")))
					(anno-record-field (name "y")
						(ty (name "U64")))
					(anno-record-field (name "z")
						(ty (name "U64"))))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "describe"))
			(e-lambda
				(args
					(p-ident (raw "rec")))
				(e-match
					(e-ident (raw "rec"))
					(branches
						(branch
							(p-record
								(field (name "x") (rest false))
								(field (name "y") (rest false)))
							(e-binop (op "+")
								(e-ident (raw "x"))
								(e-ident (raw "y"))))))))))
~~~
# FORMATTED
~~~roc
describe : { x : U64, y : U64, z : U64 } -> U64
describe = |rec| match rec {
	{ x, y } => x + y
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "describe"))
		(e-lambda
			(args
				(p-assign (ident "rec")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "x")
						(ty-lookup (name "U64") (builtin)))
					(field (field "y")
						(ty-lookup (name "U64") (builtin)))
					(field (field "z")
						(ty-lookup (name "U64") (builtin))))
				(ty-lookup (name "U64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ x: U64, y: U64, z: U64 } -> U64")))
	(expressions
		(expr (type "{ x: U64, y: U64, z: U64 } -> U64"))))
~~~
