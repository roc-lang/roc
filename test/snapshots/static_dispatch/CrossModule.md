# META
~~~ini
description=Cross-module static dispatch with imported nominal types
type=file
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
extract : a -> Str where [ a.get_value : a -> Str ]
extract = |x| x.get_value()

# Generic function that works with any type that has a transform method
modify : a, (Str -> Str) -> a where [ a.transform : a, (Str -> Str) -> a ]
modify = |x, fn| x.transform(fn)

# Test values
container : Container
container = Container.Box("hello")

# Use generic functions with Container type
result1 : Str
result1 = extract(container)

result2 : Container
result2 = modify(container, |s| "#{s} world")

main : (Str, Str)
main = (result1, extract(result2))
~~~
# EXPECTED
UNUSED VARIABLE - CrossModule.md:26:30:26:31
UNUSED VARIABLE - CrossModule.md:3:3:3:35
UNUSED VARIABLE - CrossModule.md:6:3:6:58
TYPE MODULE MISSING MATCHING TYPE - CrossModule.md:1:1:29:35
MISSING METHOD - CrossModule.md:10:28:10:50
MISSING METHOD - CrossModule.md:14:39:14:73
# PROBLEMS
**UNUSED VARIABLE**
Variable `s` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_s` to suppress this warning.
The unused variable is declared here:
**CrossModule.md:26:30:26:31:**
```roc
result2 = modify(container, |s| "#{s} world")
```
                             ^


**UNUSED VARIABLE**
Variable `get_value` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_get_value` to suppress this warning.
The unused variable is declared here:
**CrossModule.md:3:3:3:35:**
```roc
  get_value = |Container.Box(s)| s
```
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `transform` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_transform` to suppress this warning.
The unused variable is declared here:
**CrossModule.md:6:3:6:58:**
```roc
  transform = |Container.Box(s), fn| Container.Box(fn(s))
```
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `CrossModule`.roc, but no top-level type declaration named `CrossModule` was found.

Add either:
`CrossModule := ...` (nominal type)
or:
`CrossModule : ...` (type alias)
**CrossModule.md:1:1:29:35:**
```roc
Container := [Box(Str)].{
  get_value : Container -> Str
  get_value = |Container.Box(s)| s

  transform : Container, (Str -> Str) -> Container
  transform = |Container.Box(s), fn| Container.Box(fn(s))
}

# Generic function that works with any type that has a get_value method
extract : a -> Str where [ a.get_value : a -> Str ]
extract = |x| x.get_value()

# Generic function that works with any type that has a transform method
modify : a, (Str -> Str) -> a where [ a.transform : a, (Str -> Str) -> a ]
modify = |x, fn| x.transform(fn)

# Test values
container : Container
container = Container.Box("hello")

# Use generic functions with Container type
result1 : Str
result1 = extract(container)

result2 : Container
result2 = modify(container, |s| "#{s} world")

main : (Str, Str)
main = (result1, extract(result2))
```


**MISSING METHOD**
The **Container** type does not have a **get_value** method:
**CrossModule.md:10:28:10:50:**
```roc
extract : a -> Str where [ a.get_value : a -> Str ]
```
                           ^^^^^^^^^^^^^^^^^^^^^^


**Hint:** Did you forget to define **get_value** in the type's method block?

**MISSING METHOD**
The **Container** type does not have a **transform** method:
**CrossModule.md:14:39:14:73:**
```roc
modify : a, (Str -> Str) -> a where [ a.transform : a, (Str -> Str) -> a ]
```
                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**Hint:** Did you forget to define **transform** in the type's method block?

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
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,OpBar,LowerIdent,OpBar,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpColon,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenRound,LowerIdent,Comma,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
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
						(e-string-part (raw "#{s} world"))))))
		(s-type-anno (name "main")
			(ty-tuple
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "main"))
			(e-tuple
				(e-ident (raw "result1"))
				(e-apply
					(e-ident (raw "extract"))
					(e-ident (raw "result2")))))))
~~~
# FORMATTED
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

# Use generic functions with Container type
result1 : Str
result1 = extract(container)

result2 : Container
result2 = modify(
	container,
	|s| "#{s} world",
)

main : (Str, Str)
main = (result1, extract(result2))
~~~
# CANONICALIZE
~~~clojure
(can-ir
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
					(e-literal (string "#{s} world")))))
		(annotation
			(ty-lookup (name "Container") (local))))
	(d-let
		(p-assign (ident "main"))
		(e-tuple
			(elems
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
				(ty-lookup (name "Str") (builtin)))))
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
		(patt (type "a -> Str where [a.get_value : a -> Str]"))
		(patt (type "a, Str -> Str -> a where [a.transform : a, Str -> Str -> aa.transform : a, Str -> Str -> a]"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "(Error, Error)"))
		(patt (type "Container -> Str"))
		(patt (type "Container, Str -> Str -> Container")))
	(type_decls
		(nominal (type "Container")
			(ty-header (name "Container"))))
	(expressions
		(expr (type "a -> Str where [a.get_value : a -> Str]"))
		(expr (type "a, Str -> Str -> a where [a.transform : a, Str -> Str -> aa.transform : a, Str -> Str -> a]"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "(Error, Error)"))
		(expr (type "Container -> Str"))
		(expr (type "Container, Str -> Str -> Container"))))
~~~
