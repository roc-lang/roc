# META
~~~ini
description=Test == and != operators with payload-carrying nominal types that have is_eq methods
type=snippet
~~~
# SOURCE
~~~roc
# Define a UserId type that wraps an I64 with custom equality
UserId := [Id(I64)].{
    is_eq : UserId, UserId -> Bool
    is_eq = |a, b| match a {
        Id(id_a) => match b {
            Id(id_b) => id_a == id_b
        }
    }
}

user1 : UserId
user1 = UserId.Id(100)

user2 : UserId
user2 = UserId.Id(100)

user3 : UserId
user3 = UserId.Id(200)

# Test equality - same IDs should be equal
expect user1 == user2

# Test inequality - different IDs should not be equal
expect user1 != user3
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**custom_wrapper_equality.md:12:19:12:22:**
```roc
user1 = UserId.Id(100)
```
                  ^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**custom_wrapper_equality.md:12:19:12:22:**
```roc
user1 = UserId.Id(100)
```
                  ^^^

It has the type:
    _Try(I64, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(I64, [InvalidNumeral(Str)])_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**custom_wrapper_equality.md:15:19:15:22:**
```roc
user2 = UserId.Id(100)
```
                  ^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**custom_wrapper_equality.md:15:19:15:22:**
```roc
user2 = UserId.Id(100)
```
                  ^^^

It has the type:
    _Try(I64, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(I64, [InvalidNumeral(Str)])_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**custom_wrapper_equality.md:18:19:18:22:**
```roc
user3 = UserId.Id(200)
```
                  ^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**custom_wrapper_equality.md:18:19:18:22:**
```roc
user3 = UserId.Id(200)
```
                  ^^^

It has the type:
    _Try(I64, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(I64, [InvalidNumeral(Str)])_

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpEquals,LowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,
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
			(header (name "UserId")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Id"))
						(ty (name "I64")))))
			(associated
				(s-type-anno (name "is_eq")
					(ty-fn
						(ty (name "UserId"))
						(ty (name "UserId"))
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
									(p-tag (raw "Id")
										(p-ident (raw "id_a")))
									(e-match
										(e-ident (raw "b"))
										(branches
											(branch
												(p-tag (raw "Id")
													(p-ident (raw "id_b")))
												(e-binop (op "==")
													(e-ident (raw "id_a"))
													(e-ident (raw "id_b")))))))))))))
		(s-type-anno (name "user1")
			(ty (name "UserId")))
		(s-decl
			(p-ident (raw "user1"))
			(e-apply
				(e-tag (raw "UserId.Id"))
				(e-int (raw "100"))))
		(s-type-anno (name "user2")
			(ty (name "UserId")))
		(s-decl
			(p-ident (raw "user2"))
			(e-apply
				(e-tag (raw "UserId.Id"))
				(e-int (raw "100"))))
		(s-type-anno (name "user3")
			(ty (name "UserId")))
		(s-decl
			(p-ident (raw "user3"))
			(e-apply
				(e-tag (raw "UserId.Id"))
				(e-int (raw "200"))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "user1"))
				(e-ident (raw "user2"))))
		(s-expect
			(e-binop (op "!=")
				(e-ident (raw "user1"))
				(e-ident (raw "user3"))))))
~~~
# FORMATTED
~~~roc
# Define a UserId type that wraps an I64 with custom equality
UserId := [Id(I64)].{
	is_eq : UserId, UserId -> Bool
	is_eq = |a, b| match a {
		Id(id_a) => match b {
			Id(id_b) => id_a == id_b
		}
	}
}

user1 : UserId
user1 = UserId.Id(100)

user2 : UserId
user2 = UserId.Id(100)

user3 : UserId
user3 = UserId.Id(200)

# Test equality - same IDs should be equal
expect user1 == user2

# Test inequality - different IDs should not be equal
expect user1 != user3
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "custom_wrapper_equality.UserId.is_eq"))
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
													(e-binop (op "eq")
														(e-lookup-local
															(p-assign (ident "id_a")))
														(e-lookup-local
															(p-assign (ident "id_b")))))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "UserId") (local))
				(ty-lookup (name "UserId") (local))
				(ty-lookup (name "Bool") (builtin)))))
	(d-let
		(p-assign (ident "user1"))
		(e-nominal (nominal "UserId")
			(e-tag (name "Id")
				(args
					(e-num (value "100")))))
		(annotation
			(ty-lookup (name "UserId") (local))))
	(d-let
		(p-assign (ident "user2"))
		(e-nominal (nominal "UserId")
			(e-tag (name "Id")
				(args
					(e-num (value "100")))))
		(annotation
			(ty-lookup (name "UserId") (local))))
	(d-let
		(p-assign (ident "user3"))
		(e-nominal (nominal "UserId")
			(e-tag (name "Id")
				(args
					(e-num (value "200")))))
		(annotation
			(ty-lookup (name "UserId") (local))))
	(s-nominal-decl
		(ty-header (name "UserId"))
		(ty-tag-union
			(ty-tag-name (name "Id")
				(ty-lookup (name "I64") (builtin)))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "user1")))
			(e-lookup-local
				(p-assign (ident "user2")))))
	(s-expect
		(e-binop (op "ne")
			(e-lookup-local
				(p-assign (ident "user1")))
			(e-lookup-local
				(p-assign (ident "user3"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "UserId, UserId -> Bool"))
		(patt (type "UserId"))
		(patt (type "UserId"))
		(patt (type "UserId")))
	(type_decls
		(nominal (type "UserId")
			(ty-header (name "UserId"))))
	(expressions
		(expr (type "UserId, UserId -> Bool"))
		(expr (type "UserId"))
		(expr (type "UserId"))
		(expr (type "UserId"))))
~~~
