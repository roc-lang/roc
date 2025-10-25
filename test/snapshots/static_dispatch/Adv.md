# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
Adv := [Val(U64, Str)].{
  to_str : Adv -> Str
  to_str = |Adv.Val(_, s)| s

  to_u64 : Adv -> U64
  to_u64 = |Adv.Val(u, _)| u

  update_str : Adv, Str -> Adv
  update_str = |Adv.Val(u64, _), next_str| Adv.Val(u64, next_str)

  update_u64 : Adv, U64 -> Adv
  update_u64 = |Adv.Val(_, str), next_u64| Adv.Val(next_u64, str)
}

mismatch = {
	val = Adv.Val(10, "hello")
	next_val = val.update_str(100)
	next_val
}

mismatch2 = {
	val = Adv.Val(10, "hello")
	next_val = val.update_strr(100)
	next_val
}

mismatch3 = {
	next_val = "Hello".update(100)
	next_val
}

main : (Str, U64)
main = {
	val = Adv.Val(10, "hello")
	next_val = val.update_str("world").update_u64(20)
	(next_val.to_str(), next_val.to_u64)
}
~~~
# EXPECTED
MISSING METHOD - Adv.md:23:13:23:33
TYPE DOES NOT HAVE METHODS - Adv.md:28:13:28:32
# PROBLEMS
**MISSING METHOD**
The **Adv** type does not have a **update_strr** method:
**Adv.md:23:13:23:33:**
```roc
	next_val = val.update_strr(100)
```
	           ^^^^^^^^^^^^^^^^^^^^


**Hint:** Did you forget to define **update_strr** in the type's method block?

**TYPE DOES NOT HAVE METHODS**
You're trying to call the `update` method on a `Str`:
**Adv.md:28:13:28:32:**
```roc
	next_val = "Hello".update(100)
```
	           ^^^^^^^^^^^^^^^^^^^

But `Str` doesn't support methods.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Underscore,Comma,LowerIdent,CloseRound,OpBar,LowerIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,Comma,Underscore,CloseRound,OpBar,LowerIdent,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,Comma,Underscore,CloseRound,Comma,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Underscore,Comma,LowerIdent,CloseRound,Comma,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,Comma,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,Comma,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,
CloseCurly,
LowerIdent,OpColon,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,Comma,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,
OpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,Comma,LowerIdent,NoSpaceDotLowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Adv")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Val"))
						(ty (name "U64"))
						(ty (name "Str")))))
			(associated
				(s-type-anno (name "to_str")
					(ty-fn
						(ty (name "Adv"))
						(ty (name "Str"))))
				(s-decl
					(p-ident (raw "to_str"))
					(e-lambda
						(args
							(p-tag (raw ".Val")
								(p-underscore)
								(p-ident (raw "s"))))
						(e-ident (raw "s"))))
				(s-type-anno (name "to_u64")
					(ty-fn
						(ty (name "Adv"))
						(ty (name "U64"))))
				(s-decl
					(p-ident (raw "to_u64"))
					(e-lambda
						(args
							(p-tag (raw ".Val")
								(p-ident (raw "u"))
								(p-underscore)))
						(e-ident (raw "u"))))
				(s-type-anno (name "update_str")
					(ty-fn
						(ty (name "Adv"))
						(ty (name "Str"))
						(ty (name "Adv"))))
				(s-decl
					(p-ident (raw "update_str"))
					(e-lambda
						(args
							(p-tag (raw ".Val")
								(p-ident (raw "u64"))
								(p-underscore))
							(p-ident (raw "next_str")))
						(e-apply
							(e-tag (raw "Adv.Val"))
							(e-ident (raw "u64"))
							(e-ident (raw "next_str")))))
				(s-type-anno (name "update_u64")
					(ty-fn
						(ty (name "Adv"))
						(ty (name "U64"))
						(ty (name "Adv"))))
				(s-decl
					(p-ident (raw "update_u64"))
					(e-lambda
						(args
							(p-tag (raw ".Val")
								(p-underscore)
								(p-ident (raw "str")))
							(p-ident (raw "next_u64")))
						(e-apply
							(e-tag (raw "Adv.Val"))
							(e-ident (raw "next_u64"))
							(e-ident (raw "str")))))))
		(s-decl
			(p-ident (raw "mismatch"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "val"))
						(e-apply
							(e-tag (raw "Adv.Val"))
							(e-int (raw "10"))
							(e-string
								(e-string-part (raw "hello")))))
					(s-decl
						(p-ident (raw "next_val"))
						(e-field-access
							(e-ident (raw "val"))
							(e-apply
								(e-ident (raw "update_str"))
								(e-int (raw "100")))))
					(e-ident (raw "next_val")))))
		(s-decl
			(p-ident (raw "mismatch2"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "val"))
						(e-apply
							(e-tag (raw "Adv.Val"))
							(e-int (raw "10"))
							(e-string
								(e-string-part (raw "hello")))))
					(s-decl
						(p-ident (raw "next_val"))
						(e-field-access
							(e-ident (raw "val"))
							(e-apply
								(e-ident (raw "update_strr"))
								(e-int (raw "100")))))
					(e-ident (raw "next_val")))))
		(s-decl
			(p-ident (raw "mismatch3"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "next_val"))
						(e-field-access
							(e-string
								(e-string-part (raw "Hello")))
							(e-apply
								(e-ident (raw "update"))
								(e-int (raw "100")))))
					(e-ident (raw "next_val")))))
		(s-type-anno (name "main")
			(ty-tuple
				(ty (name "Str"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "val"))
						(e-apply
							(e-tag (raw "Adv.Val"))
							(e-int (raw "10"))
							(e-string
								(e-string-part (raw "hello")))))
					(s-decl
						(p-ident (raw "next_val"))
						(e-field-access
							(e-field-access
								(e-ident (raw "val"))
								(e-apply
									(e-ident (raw "update_str"))
									(e-string
										(e-string-part (raw "world")))))
							(e-apply
								(e-ident (raw "update_u64"))
								(e-int (raw "20")))))
					(e-tuple
						(e-field-access
							(e-ident (raw "next_val"))
							(e-apply
								(e-ident (raw "to_str"))))
						(e-field-access
							(e-ident (raw "next_val"))
							(e-ident (raw "to_u64")))))))))
~~~
# FORMATTED
~~~roc
Adv := [Val(U64, Str)].{
	to_str : Adv -> Str
	to_str = |Adv.Val(_, s)| s
	to_u64 : Adv -> U64
	to_u64 = |Adv.Val(u, _)| u
	update_str : Adv, Str -> Adv
	update_str = |Adv.Val(u64, _), next_str| Adv.Val(u64, next_str)
	update_u64 : Adv, U64 -> Adv
	update_u64 = |Adv.Val(_, str), next_u64| Adv.Val(next_u64, str)
}

mismatch = {
	val = Adv.Val(10, "hello")
	next_val = val.update_str(100)
	next_val
}

mismatch2 = {
	val = Adv.Val(10, "hello")
	next_val = val.update_strr(100)
	next_val
}

mismatch3 = {
	next_val = "Hello".update(100)
	next_val
}

main : (Str, U64)
main = {
	val = Adv.Val(10, "hello")
	next_val = val.update_str("world").update_u64(20)
	(next_val.to_str(), next_val.to_u64)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "mismatch"))
		(e-block
			(s-let
				(p-assign (ident "val"))
				(e-nominal (nominal "Adv")
					(e-tag (name "Val")
						(args
							(e-num (value "10"))
							(e-string
								(e-literal (string "hello")))))))
			(s-let
				(p-assign (ident "next_val"))
				(e-dot-access (field "update_str")
					(receiver
						(e-lookup-local
							(p-assign (ident "val"))))
					(args
						(e-num (value "100")))))
			(e-lookup-local
				(p-assign (ident "next_val")))))
	(d-let
		(p-assign (ident "mismatch2"))
		(e-block
			(s-let
				(p-assign (ident "val"))
				(e-nominal (nominal "Adv")
					(e-tag (name "Val")
						(args
							(e-num (value "10"))
							(e-string
								(e-literal (string "hello")))))))
			(s-let
				(p-assign (ident "next_val"))
				(e-dot-access (field "update_strr")
					(receiver
						(e-lookup-local
							(p-assign (ident "val"))))
					(args
						(e-num (value "100")))))
			(e-lookup-local
				(p-assign (ident "next_val")))))
	(d-let
		(p-assign (ident "mismatch3"))
		(e-block
			(s-let
				(p-assign (ident "next_val"))
				(e-dot-access (field "update")
					(receiver
						(e-string
							(e-literal (string "Hello"))))
					(args
						(e-num (value "100")))))
			(e-lookup-local
				(p-assign (ident "next_val")))))
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-assign (ident "val"))
				(e-nominal (nominal "Adv")
					(e-tag (name "Val")
						(args
							(e-num (value "10"))
							(e-string
								(e-literal (string "hello")))))))
			(s-let
				(p-assign (ident "next_val"))
				(e-dot-access (field "update_u64")
					(receiver
						(e-dot-access (field "update_str")
							(receiver
								(e-lookup-local
									(p-assign (ident "val"))))
							(args
								(e-string
									(e-literal (string "world"))))))
					(args
						(e-num (value "20")))))
			(e-tuple
				(elems
					(e-dot-access (field "to_str")
						(receiver
							(e-lookup-local
								(p-assign (ident "next_val"))))
						(args))
					(e-dot-access (field "to_u64")
						(receiver
							(e-lookup-local
								(p-assign (ident "next_val"))))))))
		(annotation
			(ty-tuple
				(ty-lookup (name "Str") (external-module "Str"))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "Adv.to_str"))
		(e-closure
			(captures
				(capture (ident "s")))
			(e-lambda
				(args
					(p-nominal
						(p-applied-tag)))
				(e-lookup-local
					(p-assign (ident "s")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Adv") (local))
				(ty-lookup (name "Str") (external-module "Str")))))
	(d-let
		(p-assign (ident "Adv.to_u64"))
		(e-closure
			(captures
				(capture (ident "u")))
			(e-lambda
				(args
					(p-nominal
						(p-applied-tag)))
				(e-lookup-local
					(p-assign (ident "u")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Adv") (local))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "Adv.update_str"))
		(e-closure
			(captures
				(capture (ident "u64")))
			(e-lambda
				(args
					(p-nominal
						(p-applied-tag))
					(p-assign (ident "next_str")))
				(e-nominal (nominal "Adv")
					(e-tag (name "Val")
						(args
							(e-lookup-local
								(p-assign (ident "u64")))
							(e-lookup-local
								(p-assign (ident "next_str"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Adv") (local))
				(ty-lookup (name "Str") (external-module "Str"))
				(ty-lookup (name "Adv") (local)))))
	(d-let
		(p-assign (ident "Adv.update_u64"))
		(e-closure
			(captures
				(capture (ident "str")))
			(e-lambda
				(args
					(p-nominal
						(p-applied-tag))
					(p-assign (ident "next_u64")))
				(e-nominal (nominal "Adv")
					(e-tag (name "Val")
						(args
							(e-lookup-local
								(p-assign (ident "next_u64")))
							(e-lookup-local
								(p-assign (ident "str"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Adv") (local))
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "Adv") (local)))))
	(s-nominal-decl
		(ty-header (name "Adv"))
		(ty-tag-union
			(ty-tag-name (name "Val")
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "Str") (external-module "Str"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "(Error, Num(Int(Unsigned64)))"))
		(patt (type "Adv -> Error"))
		(patt (type "Adv -> Num(Int(Unsigned64))"))
		(patt (type "Adv, Error -> Adv"))
		(patt (type "Adv, Num(Int(Unsigned64)) -> Adv")))
	(type_decls
		(nominal (type "Adv")
			(ty-header (name "Adv"))))
	(expressions
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "(Error, Num(Int(Unsigned64)))"))
		(expr (type "Adv -> Error"))
		(expr (type "Adv -> Num(Int(Unsigned64))"))
		(expr (type "Adv, Error -> Adv"))
		(expr (type "Adv, Num(Int(Unsigned64)) -> Adv"))))
~~~
