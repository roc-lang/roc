# META
~~~ini
description=Implicit equality on transparent structural type aliases (tag-union, record, and nested record-of-aliases) should type-check (issue 9500)
type=snippet
~~~
# SOURCE
~~~roc
Color : [Red, Green, Blue]

pick : U8 -> Color
pick = |n| match n {
    0 => Red
    1 => Green
    _ => Blue
}

is_red : Bool
is_red = pick(0) == Red

not_red : Bool
not_red = pick(0) != Red

Point : { x : U8, y : U8 }

origin : Point
origin = { x: 0, y: 0 }

is_origin : Bool
is_origin = origin == { x: 0, y: 0 }

Palette : [None, Color1, Color2, Color3, Color4]

DrawColors : {
    primary : Palette,
    secondary : Palette,
}

from_flags : U8 -> DrawColors
from_flags = |flags| match flags {
    0 => { primary: None, secondary: None }
    _ => { primary: Color2, secondary: Color4 }
}

is_match : Bool
is_match = from_flags(1) == { primary: Color2, secondary: Color4 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
Int,OpFatArrow,UpperIdent,
Int,OpFatArrow,UpperIdent,
Underscore,OpFatArrow,UpperIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpEquals,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpNotEquals,UpperIdent,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,OpEquals,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
UpperIdent,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
LowerIdent,OpColon,UpperIdent,Comma,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
Int,OpFatArrow,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
Underscore,OpFatArrow,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpEquals,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Color")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Red"))
					(ty (name "Green"))
					(ty (name "Blue")))))
		(s-type-anno (name "pick")
			(ty-fn
				(ty (name "U8"))
				(ty (name "Color"))))
		(s-decl
			(p-ident (raw "pick"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-match
					(e-ident (raw "n"))
					(branches
						(branch
							(p-int (raw "0"))
							(e-tag (raw "Red")))
						(branch
							(p-int (raw "1"))
							(e-tag (raw "Green")))
						(branch
							(p-underscore)
							(e-tag (raw "Blue")))))))
		(s-type-anno (name "is_red")
			(ty (name "Bool")))
		(s-decl
			(p-ident (raw "is_red"))
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "pick"))
					(e-int (raw "0")))
				(e-tag (raw "Red"))))
		(s-type-anno (name "not_red")
			(ty (name "Bool")))
		(s-decl
			(p-ident (raw "not_red"))
			(e-binop (op "!=")
				(e-apply
					(e-ident (raw "pick"))
					(e-int (raw "0")))
				(e-tag (raw "Red"))))
		(s-type-decl
			(header (name "Point")
				(args))
			(ty-record
				(anno-record-field (name "x")
					(ty (name "U8")))
				(anno-record-field (name "y")
					(ty (name "U8")))))
		(s-type-anno (name "origin")
			(ty (name "Point")))
		(s-decl
			(p-ident (raw "origin"))
			(e-record
				(field (field "x")
					(e-int (raw "0")))
				(field (field "y")
					(e-int (raw "0")))))
		(s-type-anno (name "is_origin")
			(ty (name "Bool")))
		(s-decl
			(p-ident (raw "is_origin"))
			(e-binop (op "==")
				(e-ident (raw "origin"))
				(e-record
					(field (field "x")
						(e-int (raw "0")))
					(field (field "y")
						(e-int (raw "0"))))))
		(s-type-decl
			(header (name "Palette")
				(args))
			(ty-tag-union
				(tags
					(ty (name "None"))
					(ty (name "Color1"))
					(ty (name "Color2"))
					(ty (name "Color3"))
					(ty (name "Color4")))))
		(s-type-decl
			(header (name "DrawColors")
				(args))
			(ty-record
				(anno-record-field (name "primary")
					(ty (name "Palette")))
				(anno-record-field (name "secondary")
					(ty (name "Palette")))))
		(s-type-anno (name "from_flags")
			(ty-fn
				(ty (name "U8"))
				(ty (name "DrawColors"))))
		(s-decl
			(p-ident (raw "from_flags"))
			(e-lambda
				(args
					(p-ident (raw "flags")))
				(e-match
					(e-ident (raw "flags"))
					(branches
						(branch
							(p-int (raw "0"))
							(e-record
								(field (field "primary")
									(e-tag (raw "None")))
								(field (field "secondary")
									(e-tag (raw "None")))))
						(branch
							(p-underscore)
							(e-record
								(field (field "primary")
									(e-tag (raw "Color2")))
								(field (field "secondary")
									(e-tag (raw "Color4")))))))))
		(s-type-anno (name "is_match")
			(ty (name "Bool")))
		(s-decl
			(p-ident (raw "is_match"))
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "from_flags"))
					(e-int (raw "1")))
				(e-record
					(field (field "primary")
						(e-tag (raw "Color2")))
					(field (field "secondary")
						(e-tag (raw "Color4"))))))))
~~~
# FORMATTED
~~~roc
Color : [Red, Green, Blue]

pick : U8 -> Color
pick = |n| match n {
	0 => Red
	1 => Green
	_ => Blue
}

is_red : Bool
is_red = pick(0) == Red

not_red : Bool
not_red = pick(0) != Red

Point : { x : U8, y : U8 }

origin : Point
origin = { x: 0, y: 0 }

is_origin : Bool
is_origin = origin == { x: 0, y: 0 }

Palette : [None, Color1, Color2, Color3, Color4]

DrawColors : {
	primary : Palette,
	secondary : Palette,
}

from_flags : U8 -> DrawColors
from_flags = |flags| match flags {
	0 => { primary: None, secondary: None }
	_ => { primary: Color2, secondary: Color4 }
}

is_match : Bool
is_match = from_flags(1) == { primary: Color2, secondary: Color4 }
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "pick"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "n"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-num (value "0"))))
							(value
								(e-tag (name "Red"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-num (value "1"))))
							(value
								(e-tag (name "Green"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-tag (name "Blue"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "Color") (local)))))
	(d-let
		(p-assign (ident "is_red"))
		(e-structural-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 457)
					(e-lookup-local
						(p-assign (ident "pick")))
					(e-num (value "0"))))
			(rhs
				(e-tag (name "Red"))))
		(annotation
			(ty-lookup (name "Bool") (builtin))))
	(d-let
		(p-assign (ident "not_red"))
		(e-structural-eq (negated "true")
			(lhs
				(e-call (constraint-fn-var 578)
					(e-lookup-local
						(p-assign (ident "pick")))
					(e-num (value "0"))))
			(rhs
				(e-tag (name "Red"))))
		(annotation
			(ty-lookup (name "Bool") (builtin))))
	(d-let
		(p-assign (ident "origin"))
		(e-record
			(fields
				(field (name "x")
					(e-num (value "0")))
				(field (name "y")
					(e-num (value "0")))))
		(annotation
			(ty-lookup (name "Point") (local))))
	(d-let
		(p-assign (ident "is_origin"))
		(e-structural-eq (negated "false")
			(lhs
				(e-lookup-local
					(p-assign (ident "origin"))))
			(rhs
				(e-record
					(fields
						(field (name "x")
							(e-num (value "0")))
						(field (name "y")
							(e-num (value "0")))))))
		(annotation
			(ty-lookup (name "Bool") (builtin))))
	(d-let
		(p-assign (ident "from_flags"))
		(e-lambda
			(args
				(p-assign (ident "flags")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "flags"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-num (value "0"))))
							(value
								(e-record
									(fields
										(field (name "primary")
											(e-tag (name "None")))
										(field (name "secondary")
											(e-tag (name "None")))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-record
									(fields
										(field (name "primary")
											(e-tag (name "Color2")))
										(field (name "secondary")
											(e-tag (name "Color4")))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "DrawColors") (local)))))
	(d-let
		(p-assign (ident "is_match"))
		(e-structural-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 1319)
					(e-lookup-local
						(p-assign (ident "from_flags")))
					(e-num (value "1"))))
			(rhs
				(e-record
					(fields
						(field (name "primary")
							(e-tag (name "Color2")))
						(field (name "secondary")
							(e-tag (name "Color4")))))))
		(annotation
			(ty-lookup (name "Bool") (builtin))))
	(s-alias-decl
		(ty-header (name "Color"))
		(ty-tag-union
			(ty-tag-name (name "Red"))
			(ty-tag-name (name "Green"))
			(ty-tag-name (name "Blue"))))
	(s-alias-decl
		(ty-header (name "Point"))
		(ty-record
			(field (field "x")
				(ty-lookup (name "U8") (builtin)))
			(field (field "y")
				(ty-lookup (name "U8") (builtin)))))
	(s-alias-decl
		(ty-header (name "Palette"))
		(ty-tag-union
			(ty-tag-name (name "None"))
			(ty-tag-name (name "Color1"))
			(ty-tag-name (name "Color2"))
			(ty-tag-name (name "Color3"))
			(ty-tag-name (name "Color4"))))
	(s-alias-decl
		(ty-header (name "DrawColors"))
		(ty-record
			(field (field "primary")
				(ty-lookup (name "Palette") (local)))
			(field (field "secondary")
				(ty-lookup (name "Palette") (local))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U8 -> Color"))
		(patt (type "Bool"))
		(patt (type "Bool"))
		(patt (type "Point"))
		(patt (type "Bool"))
		(patt (type "U8 -> DrawColors"))
		(patt (type "Bool")))
	(type_decls
		(alias (type "Color")
			(ty-header (name "Color")))
		(alias (type "Point")
			(ty-header (name "Point")))
		(alias (type "Palette")
			(ty-header (name "Palette")))
		(alias (type "DrawColors")
			(ty-header (name "DrawColors"))))
	(expressions
		(expr (type "U8 -> Color"))
		(expr (type "Bool"))
		(expr (type "Bool"))
		(expr (type "Point"))
		(expr (type "Bool"))
		(expr (type "U8 -> DrawColors"))
		(expr (type "Bool"))))
~~~
