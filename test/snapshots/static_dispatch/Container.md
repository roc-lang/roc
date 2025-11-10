# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
# Define a polymorphic container with static dispatch
Container(a) := [Empty, Value(a)].{
  # Method with annotation
  map : Container(a), (a -> b) -> Container(b)
  map = |container, f| {
    match container {
      Value(val) => Value(f(val))
      Empty => Empty
    }
  }
  
  # Method without annotation (inferred)
  get_or = |container, default| {
    match container {
      Value(val) => val
      Empty => default
    }
  }
  
  # Chained method dispatch
  flat_map : Container(a), (a -> Container(b)) -> Container(b)
  flat_map = |container, f| {
    match container {
      Value(val) => f(val)
      Empty => Empty
    }
  }
}

func = {
  num_container = Container.Value(100)

  chained = num_container
    .map(|x| x + 1)
    .flat_map(|x| Container.Value(x + 2))
    .get_or(0)

  chained
}
~~~
# EXPECTED
UNUSED VARIABLE - Container.md:13:3:13:9
MISSING METHOD - Container.md:33:13:34:20
# PROBLEMS
**UNUSED VARIABLE**
Variable `Container.get_or` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Container.get_or` to suppress this warning.
The unused variable is declared here:
**Container.md:13:3:13:9:**
```roc
  get_or = |container, default| {
```
  ^^^^^^


**MISSING METHOD**
The **Container(Num(_size))** type does not have a **map** method:
**Container.md:33:13:34:20:**
```roc
  chained = num_container
    .map(|x| x + 1)
```


**Hint:** Did you forget to define **map** in the type's method block?

# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
UpperIdent,OpFatArrow,UpperIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,OpFatArrow,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,OpFatArrow,UpperIdent,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,
DotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,CloseRound,
DotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,OpPlus,Int,CloseRound,CloseRound,
DotLowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Container")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty (name "Empty"))
					(ty-apply
						(ty (name "Value"))
						(ty-var (raw "a")))))
			(associated
				(s-type-anno (name "map")
					(ty-fn
						(ty-apply
							(ty (name "Container"))
							(ty-var (raw "a")))
						(ty-fn
							(ty-var (raw "a"))
							(ty-var (raw "b")))
						(ty-apply
							(ty (name "Container"))
							(ty-var (raw "b")))))
				(s-decl
					(p-ident (raw "map"))
					(e-lambda
						(args
							(p-ident (raw "container"))
							(p-ident (raw "f")))
						(e-block
							(statements
								(e-match
									(e-ident (raw "container"))
									(branches
										(branch
											(p-tag (raw "Value")
												(p-ident (raw "val")))
											(e-apply
												(e-tag (raw "Value"))
												(e-apply
													(e-ident (raw "f"))
													(e-ident (raw "val")))))
										(branch
											(p-tag (raw "Empty"))
											(e-tag (raw "Empty")))))))))
				(s-decl
					(p-ident (raw "get_or"))
					(e-lambda
						(args
							(p-ident (raw "container"))
							(p-ident (raw "default")))
						(e-block
							(statements
								(e-match
									(e-ident (raw "container"))
									(branches
										(branch
											(p-tag (raw "Value")
												(p-ident (raw "val")))
											(e-ident (raw "val")))
										(branch
											(p-tag (raw "Empty"))
											(e-ident (raw "default")))))))))
				(s-type-anno (name "flat_map")
					(ty-fn
						(ty-apply
							(ty (name "Container"))
							(ty-var (raw "a")))
						(ty-fn
							(ty-var (raw "a"))
							(ty-apply
								(ty (name "Container"))
								(ty-var (raw "b"))))
						(ty-apply
							(ty (name "Container"))
							(ty-var (raw "b")))))
				(s-decl
					(p-ident (raw "flat_map"))
					(e-lambda
						(args
							(p-ident (raw "container"))
							(p-ident (raw "f")))
						(e-block
							(statements
								(e-match
									(e-ident (raw "container"))
									(branches
										(branch
											(p-tag (raw "Value")
												(p-ident (raw "val")))
											(e-apply
												(e-ident (raw "f"))
												(e-ident (raw "val"))))
										(branch
											(p-tag (raw "Empty"))
											(e-tag (raw "Empty")))))))))))
		(s-decl
			(p-ident (raw "func"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "num_container"))
						(e-apply
							(e-tag (raw "Container.Value"))
							(e-int (raw "100"))))
					(s-decl
						(p-ident (raw "chained"))
						(e-field-access
							(e-field-access
								(e-field-access
									(e-ident (raw "num_container"))
									(e-apply
										(e-ident (raw ".map"))
										(e-lambda
											(args
												(p-ident (raw "x")))
											(e-binop (op "+")
												(e-ident (raw "x"))
												(e-int (raw "1"))))))
								(e-apply
									(e-ident (raw ".flat_map"))
									(e-lambda
										(args
											(p-ident (raw "x")))
										(e-apply
											(e-tag (raw "Container.Value"))
											(e-binop (op "+")
												(e-ident (raw "x"))
												(e-int (raw "2")))))))
							(e-apply
								(e-ident (raw ".get_or"))
								(e-int (raw "0")))))
					(e-ident (raw "chained")))))))
~~~
# FORMATTED
~~~roc
# Define a polymorphic container with static dispatch
Container(a) := [Empty, Value(a)].{
	map : Container(a), (a -> b) -> Container(b)
	map = |container, f| {
		match container {
			Value(val) => Value(f(val))
			Empty => Empty
		}
	}
	get_or = |container, default| {
		match container {
			Value(val) => val
			Empty => default
		}
	}
	flat_map : Container(a), (a -> Container(b)) -> Container(b)
	flat_map = |container, f| {
		match container {
			Value(val) => f(val)
			Empty => Empty
		}
	}
}

func = {
	num_container = Container.Value(100)

	chained = num_container
		.map(|x| x + 1)
		.flat_map(|x| Container.Value(x + 2))
		.get_or(0)

	chained
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Container.map"))
		(e-closure
			(captures
				(capture (ident "val")))
			(e-lambda
				(args
					(p-assign (ident "container"))
					(p-assign (ident "f")))
				(e-block
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "container"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-tag (name "Value")
											(args
												(e-call
													(e-lookup-local
														(p-assign (ident "f")))
													(e-lookup-local
														(p-assign (ident "val"))))))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-tag (name "Empty"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Container") (local)
					(ty-rigid-var (name "a")))
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-rigid-var (name "b"))))
				(ty-apply (name "Container") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))))))
	(d-let
		(p-assign (ident "Container.get_or"))
		(e-closure
			(captures
				(capture (ident "val")))
			(e-lambda
				(args
					(p-assign (ident "container"))
					(p-assign (ident "default")))
				(e-block
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "container"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-lookup-local
											(p-assign (ident "val")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-lookup-local
											(p-assign (ident "default"))))))))))))
	(d-let
		(p-assign (ident "Container.flat_map"))
		(e-closure
			(captures
				(capture (ident "val")))
			(e-lambda
				(args
					(p-assign (ident "container"))
					(p-assign (ident "f")))
				(e-block
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "container"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-call
											(e-lookup-local
												(p-assign (ident "f")))
											(e-lookup-local
												(p-assign (ident "val"))))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-tag (name "Empty"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Container") (local)
					(ty-rigid-var (name "a")))
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-apply (name "Container") (local)
							(ty-rigid-var (name "b")))))
				(ty-apply (name "Container") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))))))
	(d-let
		(p-assign (ident "func"))
		(e-block
			(s-let
				(p-assign (ident "num_container"))
				(e-nominal (nominal "Container")
					(e-tag (name "Value")
						(args
							(e-num (value "100"))))))
			(s-let
				(p-assign (ident "chained"))
				(e-dot-access (field "get_or")
					(receiver
						(e-dot-access (field "flat_map")
							(receiver
								(e-dot-access (field "map")
									(receiver
										(e-lookup-local
											(p-assign (ident "num_container"))))
									(args
										(e-lambda
											(args
												(p-assign (ident "x")))
											(e-binop (op "add")
												(e-lookup-local
													(p-assign (ident "x")))
												(e-num (value "1")))))))
							(args
								(e-lambda
									(args
										(p-assign (ident "x")))
									(e-nominal (nominal "Container")
										(e-tag (name "Value")
											(args
												(e-binop (op "add")
													(e-lookup-local
														(p-assign (ident "x")))
													(e-num (value "2"))))))))))
					(args
						(e-num (value "0")))))
			(e-lookup-local
				(p-assign (ident "chained")))))
	(s-nominal-decl
		(ty-header (name "Container")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Empty"))
			(ty-tag-name (name "Value")
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Container(a), (a -> b) -> Container(b)"))
		(patt (type "[Value(c), Empty]_others, c -> c"))
		(patt (type "Container(a), (a -> Container(b)) -> Container(b)"))
		(patt (type "_c")))
	(type_decls
		(nominal (type "Container(a)")
			(ty-header (name "Container")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "Container(a), (a -> b) -> Container(b)"))
		(expr (type "[Value(c), Empty]_others, c -> c"))
		(expr (type "Container(a), (a -> Container(b)) -> Container(b)"))
		(expr (type "_c"))))
~~~
