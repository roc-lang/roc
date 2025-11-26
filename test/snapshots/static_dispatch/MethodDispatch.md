# META
~~~ini
description=Static dispatch with method calls on nominal types
type=file:Container.roc
~~~
# SOURCE
~~~roc
Container := [Box(Str)].{
	get_value : Container -> Str
	get_value = |Container.Box(s)| s
	transform : Container, (Str -> Str) -> Container
	transform = |Container.Box(s), fn| Container.Box(fn(s))
}

# Generic function that works with any type that has a get_value method
extract : a -> Str where [a.get_value : a -> Str]
extract = |x| x.get_value()

# Generic function that works with any type that has a transform method
modify : a, (Str -> Str) -> a where [a.transform : a, (Str -> Str) -> a]
modify = |x, fn| x.transform(fn)

# Test values
container : Container
container = Container.Box("hello")

# Another container value with direct method call
myContainer : Container
myContainer = Container.Box("world")

directCall : Str
directCall = myContainer.get_value()

# Use generic functions with Container type
result1 : Str
result1 = extract(container)

result2 : Container
result2 = modify(
	container,
	|s| "${s} world",
)

main : (Str, Str, Str)
main = (directCall, result1, extract(result2))
~~~
# EXPECTED
MISSING METHOD - MethodDispatch.md:25:14:25:37
MISSING METHOD - MethodDispatch.md:10:15:10:28
MISSING METHOD - MethodDispatch.md:14:18:14:33
# PROBLEMS
**MISSING METHOD**
This **get_value** method is being called on the type **Container**, which has no method with that name:
**MethodDispatch.md:25:14:25:37:**
```roc
directCall = myContainer.get_value()
```
             ^^^^^^^^^^^^^^^^^^^^^^^


**Hint: **For this to work, the type would need to have a method named **get_value** associated with it in the type's declaration.

**MISSING METHOD**
This **get_value** method is being called on the type **Container**, which has no method with that name:
**MethodDispatch.md:10:15:10:28:**
```roc
extract = |x| x.get_value()
```
              ^^^^^^^^^^^^^


**Hint: **For this to work, the type would need to have a method named **get_value** associated with it in the type's declaration.

**MISSING METHOD**
This **transform** method is being called on the type **Container**, which has no method with that name:
**MethodDispatch.md:14:18:14:33:**
```roc
modify = |x, fn| x.transform(fn)
```
                 ^^^^^^^^^^^^^^^


**Hint: **For this to work, the type would need to have a method named **transform** associated with it in the type's declaration.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,LowerIdent,
LowerIdent,OpColon,UpperIdent,Comma,OpenRound,UpperIdent,OpArrow,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpColon,LowerIdent,Comma,OpenRound,UpperIdent,OpArrow,UpperIdent,CloseRound,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,Comma,OpenRound,UpperIdent,OpArrow,UpperIdent,CloseRound,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,
LowerIdent,Comma,
OpBar,LowerIdent,OpBar,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,Comma,
CloseRound,
LowerIdent,OpColon,OpenRound,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Container")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Box"))
						(ty (name "Str")))))
			(associated
				(s-type-anno (name "get_value")
					(ty-fn
						(ty (name "Container"))
						(ty (name "Str"))))
				(s-decl
					(p-ident (raw "get_value"))
					(e-lambda
						(args
							(p-tag (raw ".Box")
								(p-ident (raw "s"))))
						(e-ident (raw "s"))))
				(s-type-anno (name "transform")
					(ty-fn
						(ty (name "Container"))
						(ty-fn
							(ty (name "Str"))
							(ty (name "Str")))
						(ty (name "Container"))))
				(s-decl
					(p-ident (raw "transform"))
					(e-lambda
						(args
							(p-tag (raw ".Box")
								(p-ident (raw "s")))
							(p-ident (raw "fn")))
						(e-apply
							(e-tag (raw "Container.Box"))
							(e-apply
								(e-ident (raw "fn"))
								(e-ident (raw "s"))))))))
		(s-type-anno (name "extract")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str")))
			(where
				(method (module-of "a") (name "get_value")
					(args
						(ty-var (raw "a")))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "extract"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-field-access
					(e-ident (raw "x"))
					(e-apply
						(e-ident (raw "get_value"))))))
		(s-type-anno (name "modify")
			(ty-fn
				(ty-var (raw "a"))
				(ty-fn
					(ty (name "Str"))
					(ty (name "Str")))
				(ty-var (raw "a")))
			(where
				(method (module-of "a") (name "transform")
					(args
						(ty-var (raw "a"))
						(ty-fn
							(ty (name "Str"))
							(ty (name "Str"))))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "modify"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "fn")))
				(e-field-access
					(e-ident (raw "x"))
					(e-apply
						(e-ident (raw "transform"))
						(e-ident (raw "fn"))))))
		(s-type-anno (name "container")
			(ty (name "Container")))
		(s-decl
			(p-ident (raw "container"))
			(e-apply
				(e-tag (raw "Container.Box"))
				(e-string
					(e-string-part (raw "hello")))))
		(s-type-anno (name "myContainer")
			(ty (name "Container")))
		(s-decl
			(p-ident (raw "myContainer"))
			(e-apply
				(e-tag (raw "Container.Box"))
				(e-string
					(e-string-part (raw "world")))))
		(s-type-anno (name "directCall")
			(ty (name "Str")))
		(s-decl
			(p-ident (raw "directCall"))
			(e-field-access
				(e-ident (raw "myContainer"))
				(e-apply
					(e-ident (raw "get_value")))))
		(s-type-anno (name "result1")
			(ty (name "Str")))
		(s-decl
			(p-ident (raw "result1"))
			(e-apply
				(e-ident (raw "extract"))
				(e-ident (raw "container"))))
		(s-type-anno (name "result2")
			(ty (name "Container")))
		(s-decl
			(p-ident (raw "result2"))
			(e-apply
				(e-ident (raw "modify"))
				(e-ident (raw "container"))
				(e-lambda
					(args
						(p-ident (raw "s")))
					(e-string
						(e-string-part (raw ""))
						(e-ident (raw "s"))
						(e-string-part (raw " world"))))))
		(s-type-anno (name "main")
			(ty-tuple
				(ty (name "Str"))
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "main"))
			(e-tuple
				(e-ident (raw "directCall"))
				(e-ident (raw "result1"))
				(e-apply
					(e-ident (raw "extract"))
					(e-ident (raw "result2")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Container.get_value"))
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
				(ty-lookup (name "Container") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "Container.transform"))
		(e-closure
			(captures
				(capture (ident "s")))
			(e-lambda
				(args
					(p-nominal
						(p-applied-tag))
					(p-assign (ident "fn")))
				(e-nominal (nominal "Container")
					(e-tag (name "Box")
						(args
							(e-call
								(e-lookup-local
									(p-assign (ident "fn")))
								(e-lookup-local
									(p-assign (ident "s")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Container") (local))
				(ty-parens
					(ty-fn (effectful false)
						(ty-lookup (name "Str") (builtin))
						(ty-lookup (name "Str") (builtin))))
				(ty-lookup (name "Container") (local)))))
	(d-let
		(p-assign (ident "extract"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-dot-access (field "get_value")
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "Str") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "get_value")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "modify"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "fn")))
			(e-dot-access (field "transform")
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args
					(e-lookup-local
						(p-assign (ident "fn"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-parens
					(ty-fn (effectful false)
						(ty-lookup (name "Str") (builtin))
						(ty-lookup (name "Str") (builtin))))
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "transform")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-parens
							(ty-fn (effectful false)
								(ty-lookup (name "Str") (builtin))
								(ty-lookup (name "Str") (builtin)))))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "container"))
		(e-nominal (nominal "Container")
			(e-tag (name "Box")
				(args
					(e-string
						(e-literal (string "hello"))))))
		(annotation
			(ty-lookup (name "Container") (local))))
	(d-let
		(p-assign (ident "myContainer"))
		(e-nominal (nominal "Container")
			(e-tag (name "Box")
				(args
					(e-string
						(e-literal (string "world"))))))
		(annotation
			(ty-lookup (name "Container") (local))))
	(d-let
		(p-assign (ident "directCall"))
		(e-dot-access (field "get_value")
			(receiver
				(e-lookup-local
					(p-assign (ident "myContainer"))))
			(args))
		(annotation
			(ty-lookup (name "Str") (builtin))))
	(d-let
		(p-assign (ident "result1"))
		(e-call
			(e-lookup-local
				(p-assign (ident "extract")))
			(e-lookup-local
				(p-assign (ident "container"))))
		(annotation
			(ty-lookup (name "Str") (builtin))))
	(d-let
		(p-assign (ident "result2"))
		(e-call
			(e-lookup-local
				(p-assign (ident "modify")))
			(e-lookup-local
				(p-assign (ident "container")))
			(e-lambda
				(args
					(p-assign (ident "s")))
				(e-string
					(e-literal (string ""))
					(e-lookup-local
						(p-assign (ident "s")))
					(e-literal (string " world")))))
		(annotation
			(ty-lookup (name "Container") (local))))
	(d-let
		(p-assign (ident "main"))
		(e-tuple
			(elems
				(e-lookup-local
					(p-assign (ident "directCall")))
				(e-lookup-local
					(p-assign (ident "result1")))
				(e-call
					(e-lookup-local
						(p-assign (ident "extract")))
					(e-lookup-local
						(p-assign (ident "result2"))))))
		(annotation
			(ty-tuple
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Container"))
		(ty-tag-union
			(ty-tag-name (name "Box")
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Container -> Str"))
		(patt (type "Container, (Str -> Str) -> Container"))
		(patt (type "a -> Str where [a.get_value : a -> Str]"))
		(patt (type "a, (Str -> Str) -> a where [a.transform : a, (Str -> Str) -> a]"))
		(patt (type "Container"))
		(patt (type "Container"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "(Error, Error, Error)")))
	(type_decls
		(nominal (type "Container")
			(ty-header (name "Container"))))
	(expressions
		(expr (type "Container -> Str"))
		(expr (type "Container, (Str -> Str) -> Container"))
		(expr (type "a -> Str where [a.get_value : a -> Str]"))
		(expr (type "a, (Str -> Str) -> a where [a.transform : a, (Str -> Str) -> a]"))
		(expr (type "Container"))
		(expr (type "Container"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "(Error, Error, Error)"))))
~~~
