# META
~~~ini
description=Test == and != operators with custom user-defined types that have is_eq methods
type=snippet
~~~
# SOURCE
~~~roc
# Define a simple Color type with custom equality
Color := [Red, Green, Blue].{
    is_eq : Color, Color -> Bool
    is_eq = |a, b| match a {
        Red => match b {
            Red => True
            Green => False
            Blue => False
        }
        Green => match b {
            Red => False
            Green => True
            Blue => False
        }
        Blue => match b {
            Red => False
            Green => False
            Blue => True
        }
    }
}

c1 : Color
c1 = Color.Red

c2 : Color
c2 = Color.Red

c3 : Color
c3 = Color.Blue

# Test equality - same values should be equal
expect c1 == c2

# Test inequality - different values should not be equal
expect c1 != c3
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,UpperIdent,
UpperIdent,OpFatArrow,UpperIdent,
UpperIdent,OpFatArrow,UpperIdent,
CloseCurly,
UpperIdent,OpFatArrow,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,UpperIdent,
UpperIdent,OpFatArrow,UpperIdent,
UpperIdent,OpFatArrow,UpperIdent,
CloseCurly,
UpperIdent,OpFatArrow,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,UpperIdent,
UpperIdent,OpFatArrow,UpperIdent,
UpperIdent,OpFatArrow,UpperIdent,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
KwExpect,LowerIdent,OpEquals,LowerIdent,
KwExpect,LowerIdent,OpNotEquals,LowerIdent,
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
					(ty (name "Blue"))))
			(associated
				(s-type-anno (name "is_eq")
					(ty-fn
						(ty (name "Color"))
						(ty (name "Color"))
						(ty (name "Bool"))))
				(s-decl
					(p-ident (raw "is_eq"))
					(e-lambda
						(args
							(p-ident (raw "a"))
							(p-ident (raw "b")))
						(e-match
							(e-ident (raw "a"))
							(branches
								(branch
									(p-tag (raw "Red"))
									(e-match
										(e-ident (raw "b"))
										(branches
											(branch
												(p-tag (raw "Red"))
												(e-tag (raw "True")))
											(branch
												(p-tag (raw "Green"))
												(e-tag (raw "False")))
											(branch
												(p-tag (raw "Blue"))
												(e-tag (raw "False"))))))
								(branch
									(p-tag (raw "Green"))
									(e-match
										(e-ident (raw "b"))
										(branches
											(branch
												(p-tag (raw "Red"))
												(e-tag (raw "False")))
											(branch
												(p-tag (raw "Green"))
												(e-tag (raw "True")))
											(branch
												(p-tag (raw "Blue"))
												(e-tag (raw "False"))))))
								(branch
									(p-tag (raw "Blue"))
									(e-match
										(e-ident (raw "b"))
										(branches
											(branch
												(p-tag (raw "Red"))
												(e-tag (raw "False")))
											(branch
												(p-tag (raw "Green"))
												(e-tag (raw "False")))
											(branch
												(p-tag (raw "Blue"))
												(e-tag (raw "True"))))))))))))
		(s-type-anno (name "c1")
			(ty (name "Color")))
		(s-decl
			(p-ident (raw "c1"))
			(e-tag (raw "Color.Red")))
		(s-type-anno (name "c2")
			(ty (name "Color")))
		(s-decl
			(p-ident (raw "c2"))
			(e-tag (raw "Color.Red")))
		(s-type-anno (name "c3")
			(ty (name "Color")))
		(s-decl
			(p-ident (raw "c3"))
			(e-tag (raw "Color.Blue")))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "c1"))
				(e-ident (raw "c2"))))
		(s-expect
			(e-binop (op "!=")
				(e-ident (raw "c1"))
				(e-ident (raw "c3"))))))
~~~
# FORMATTED
~~~roc
# Define a simple Color type with custom equality
Color := [Red, Green, Blue].{
	is_eq : Color, Color -> Bool
	is_eq = |a, b| match a {
		Red => match b {
			Red => True
			Green => False
			Blue => False
		}
		Green => match b {
			Red => False
			Green => True
			Blue => False
		}
		Blue => match b {
			Red => False
			Green => False
			Blue => True
		}
	}
}

c1 : Color
c1 = Color.Red

c2 : Color
c2 = Color.Red

c3 : Color
c3 = Color.Blue

# Test equality - same values should be equal
expect c1 == c2

# Test inequality - different values should not be equal
expect c1 != c3
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "custom_type_equality.Color.is_eq"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "a"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-match
									(match
										(cond
											(e-lookup-local
												(p-assign (ident "b"))))
										(branches
											(branch
												(patterns
													(pattern (degenerate false)
														(p-applied-tag)))
												(value
													(e-tag (name "True"))))
											(branch
												(patterns
													(pattern (degenerate false)
														(p-applied-tag)))
												(value
													(e-tag (name "False"))))
											(branch
												(patterns
													(pattern (degenerate false)
														(p-applied-tag)))
												(value
													(e-tag (name "False")))))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-match
									(match
										(cond
											(e-lookup-local
												(p-assign (ident "b"))))
										(branches
											(branch
												(patterns
													(pattern (degenerate false)
														(p-applied-tag)))
												(value
													(e-tag (name "False"))))
											(branch
												(patterns
													(pattern (degenerate false)
														(p-applied-tag)))
												(value
													(e-tag (name "True"))))
											(branch
												(patterns
													(pattern (degenerate false)
														(p-applied-tag)))
												(value
													(e-tag (name "False")))))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-match
									(match
										(cond
											(e-lookup-local
												(p-assign (ident "b"))))
										(branches
											(branch
												(patterns
													(pattern (degenerate false)
														(p-applied-tag)))
												(value
													(e-tag (name "False"))))
											(branch
												(patterns
													(pattern (degenerate false)
														(p-applied-tag)))
												(value
													(e-tag (name "False"))))
											(branch
												(patterns
													(pattern (degenerate false)
														(p-applied-tag)))
												(value
													(e-tag (name "True")))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Color") (local))
				(ty-lookup (name "Color") (local))
				(ty-lookup (name "Bool") (builtin)))))
	(d-let
		(p-assign (ident "c1"))
		(e-nominal (nominal "Color")
			(e-tag (name "Red")))
		(annotation
			(ty-lookup (name "Color") (local))))
	(d-let
		(p-assign (ident "c2"))
		(e-nominal (nominal "Color")
			(e-tag (name "Red")))
		(annotation
			(ty-lookup (name "Color") (local))))
	(d-let
		(p-assign (ident "c3"))
		(e-nominal (nominal "Color")
			(e-tag (name "Blue")))
		(annotation
			(ty-lookup (name "Color") (local))))
	(s-nominal-decl
		(ty-header (name "Color"))
		(ty-tag-union
			(ty-tag-name (name "Red"))
			(ty-tag-name (name "Green"))
			(ty-tag-name (name "Blue"))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "c1")))
			(e-lookup-local
				(p-assign (ident "c2")))))
	(s-expect
		(e-binop (op "ne")
			(e-lookup-local
				(p-assign (ident "c1")))
			(e-lookup-local
				(p-assign (ident "c3"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Color, Color -> Bool"))
		(patt (type "Color"))
		(patt (type "Color"))
		(patt (type "Color")))
	(type_decls
		(nominal (type "Color")
			(ty-header (name "Color"))))
	(expressions
		(expr (type "Color, Color -> Bool"))
		(expr (type "Color"))
		(expr (type "Color"))
		(expr (type "Color"))))
~~~
