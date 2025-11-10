# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
Basic := [Val(Str)].{
  to_str : Basic -> Str
  to_str = |Basic.Val(s)| s

  to_str2 : Basic -> Str
  to_str2 = |test| test.to_str()
}

helper1 : a -> b where [ a.to_str : a -> b ]
helper1 = |x| x.to_str()

helper2 : a -> b where [ a.to_str2 : a -> b ]
helper2 = |x| x.to_str2()

val : Basic
val = Basic.Val("hello")

main : (Str, Str)
main = (helper1(val), helper2(val))
~~~
# EXPECTED
UNUSED VARIABLE - Basic.md:3:3:3:9
UNUSED VARIABLE - Basic.md:6:3:6:10
MISSING METHOD - Basic.md:6:20:6:33
MISSING METHOD - Basic.md:10:15:10:25
MISSING METHOD - Basic.md:13:15:13:26
# PROBLEMS
**UNUSED VARIABLE**
Variable `Basic.to_str` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Basic.to_str` to suppress this warning.
The unused variable is declared here:
**Basic.md:3:3:3:9:**
```roc
  to_str = |Basic.Val(s)| s
```
  ^^^^^^


**UNUSED VARIABLE**
Variable `Basic.to_str2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Basic.to_str2` to suppress this warning.
The unused variable is declared here:
**Basic.md:6:3:6:10:**
```roc
  to_str2 = |test| test.to_str()
```
  ^^^^^^^


**MISSING METHOD**
The **Basic** type does not have a **to_str** method:
**Basic.md:6:20:6:33:**
```roc
  to_str2 = |test| test.to_str()
```
                   ^^^^^^^^^^^^^


**Hint:** Did you forget to define **to_str** in the type's method block?

**MISSING METHOD**
The **Basic** type does not have a **to_str** method:
**Basic.md:10:15:10:25:**
```roc
helper1 = |x| x.to_str()
```
              ^^^^^^^^^^


**Hint:** Did you forget to define **to_str** in the type's method block?

**MISSING METHOD**
The **Basic** type does not have a **to_str2** method:
**Basic.md:13:15:13:26:**
```roc
helper2 = |x| x.to_str2()
```
              ^^^^^^^^^^^


**Hint:** Did you forget to define **to_str2** in the type's method block?

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,LowerIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpColon,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenRound,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Basic")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Val"))
						(ty (name "Str")))))
			(associated
				(s-type-anno (name "to_str")
					(ty-fn
						(ty (name "Basic"))
						(ty (name "Str"))))
				(s-decl
					(p-ident (raw "to_str"))
					(e-lambda
						(args
							(p-tag (raw ".Val")
								(p-ident (raw "s"))))
						(e-ident (raw "s"))))
				(s-type-anno (name "to_str2")
					(ty-fn
						(ty (name "Basic"))
						(ty (name "Str"))))
				(s-decl
					(p-ident (raw "to_str2"))
					(e-lambda
						(args
							(p-ident (raw "test")))
						(e-field-access
							(e-ident (raw "test"))
							(e-apply
								(e-ident (raw "to_str"))))))))
		(s-type-anno (name "helper1")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b")))
			(where
				(method (module-of "a") (name "to_str")
					(args
						(ty-var (raw "a")))
					(ty-var (raw "b")))))
		(s-decl
			(p-ident (raw "helper1"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-field-access
					(e-ident (raw "x"))
					(e-apply
						(e-ident (raw "to_str"))))))
		(s-type-anno (name "helper2")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b")))
			(where
				(method (module-of "a") (name "to_str2")
					(args
						(ty-var (raw "a")))
					(ty-var (raw "b")))))
		(s-decl
			(p-ident (raw "helper2"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-field-access
					(e-ident (raw "x"))
					(e-apply
						(e-ident (raw "to_str2"))))))
		(s-type-anno (name "val")
			(ty (name "Basic")))
		(s-decl
			(p-ident (raw "val"))
			(e-apply
				(e-tag (raw "Basic.Val"))
				(e-string
					(e-string-part (raw "hello")))))
		(s-type-anno (name "main")
			(ty-tuple
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "main"))
			(e-tuple
				(e-apply
					(e-ident (raw "helper1"))
					(e-ident (raw "val")))
				(e-apply
					(e-ident (raw "helper2"))
					(e-ident (raw "val")))))))
~~~
# FORMATTED
~~~roc
Basic := [Val(Str)].{
	to_str : Basic -> Str
	to_str = |Basic.Val(s)| s
	to_str2 : Basic -> Str
	to_str2 = |test| test.to_str()
}

helper1 : a -> b where [a.to_str : a -> b]
helper1 = |x| x.to_str()

helper2 : a -> b where [a.to_str2 : a -> b]
helper2 = |x| x.to_str2()

val : Basic
val = Basic.Val("hello")

main : (Str, Str)
main = (helper1(val), helper2(val))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Basic.to_str"))
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
				(ty-lookup (name "Basic") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "Basic.to_str2"))
		(e-lambda
			(args
				(p-assign (ident "test")))
			(e-dot-access (field "to_str")
				(receiver
					(e-lookup-local
						(p-assign (ident "test"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Basic") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "helper1"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-dot-access (field "to_str")
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_str")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))))))
	(d-let
		(p-assign (ident "helper2"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-dot-access (field "to_str2")
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_str2")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))))))
	(d-let
		(p-assign (ident "val"))
		(e-nominal (nominal "Basic")
			(e-tag (name "Val")
				(args
					(e-string
						(e-literal (string "hello"))))))
		(annotation
			(ty-lookup (name "Basic") (local))))
	(d-let
		(p-assign (ident "main"))
		(e-tuple
			(elems
				(e-call
					(e-lookup-local
						(p-assign (ident "helper1")))
					(e-lookup-local
						(p-assign (ident "val"))))
				(e-call
					(e-lookup-local
						(p-assign (ident "helper2")))
					(e-lookup-local
						(p-assign (ident "val"))))))
		(annotation
			(ty-tuple
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Basic"))
		(ty-tag-union
			(ty-tag-name (name "Val")
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Basic -> Str"))
		(patt (type "Basic -> Error"))
		(patt (type "a -> b where [a.to_str : a -> b]"))
		(patt (type "a -> b where [a.to_str2 : a -> b]"))
		(patt (type "Basic"))
		(patt (type "(Error, Error)")))
	(type_decls
		(nominal (type "Basic")
			(ty-header (name "Basic"))))
	(expressions
		(expr (type "Basic -> Str"))
		(expr (type "Basic -> Error"))
		(expr (type "a -> b where [a.to_str : a -> b]"))
		(expr (type "a -> b where [a.to_str2 : a -> b]"))
		(expr (type "Basic"))
		(expr (type "(Error, Error)"))))
~~~
