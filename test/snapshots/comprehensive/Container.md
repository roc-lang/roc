# META
~~~ini
description=Color module from package
type=package
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
      Container.Value(val) => val
      Container.Empty => default
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

# First layer: polymorphic helper with annotation
compose : (b -> c), (a -> b), a -> c
compose = |g, f, x| g(f(x))

# Second layer: inferred polymorphic function using compose
transform_twice = |f, x| {
  first = compose(f, f, x)
  second = compose(f, f, first)
  second
}

# Third layer: curried function (multiple lambda layers)
make_processor : (a -> b) -> ((b -> c) -> (a -> c))
make_processor = |f1| |f2| |x| {
  step1 = f1(x)
  step2 = f2(step1)
  step2
}

# Fourth layer: polymorphic function using static dispatch
process_with_method : a, c -> d where [a.map : a, (b -> c) -> d]
process_with_method = |container, value| {
  # Multiple nested lambdas with let-polymorphism
  id = |x| x
  
  # Use id polymorphically
  _test1 = id(42)
  _test2 = id("test")
  
  # Static dispatch with method call - just replace values
  result = container.map(|_| value)
  result
}

# Fifth layer: combine everything
main = {
  # Let-polymorphism layer 1
  # TODO INLINE ANNOS
  # id : a -> a 
  id = |x| x
  
  # Let-polymorphism layer 2 with nested lambdas
  _apply_to_container = |f| |container| |default| {
    mapped = container.map(f)
    mapped.get_or(default)
  }
  
  # Create containers
  num_container = Container.Value(100)
  str_container = Container.Value("hello")
  _empty_container = Container.Empty
  
  # Use id polymorphically on different types
  id_num = id(42)
  id_str = id("world")
  id_bool = id(True)
  
  # Multiple layers of curried application
  add_ten = |x| x + 10
  processor = make_processor(add_ten)(add_ten)
  processed = processor(5)
  
  # Static dispatch with polymorphic methods
  num_result = num_container.map(|x| x + 1)
  _str_result = str_container.map(|s| s)
  
  # Chain method calls with static dispatch
  chained = num_container
    .map(|x| x + 1)
    .flat_map(|x| Container.Value(x + 2))
    .get_or(0)
  
  # Use transform_twice with let-polymorphism
  double_fn = |x| x + x
  transformed = transform_twice(double_fn, 3)
  
  # Final result combining all techniques
  {
    id_results: (id_num, id_str, id_bool),
    processed: processed,
    chained: chained,
    transformed: transformed,
    final: num_result.get_or(0),
  }
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceDotUpperIdent,OpFatArrow,LowerIdent,
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
LowerIdent,OpColon,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,Comma,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,
CloseCurly,
LowerIdent,OpColon,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,OpenRound,NoSpaceOpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,
CloseCurly,
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,Comma,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,Underscore,OpBar,LowerIdent,CloseRound,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
NamedUnderscore,OpAssign,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
NamedUnderscore,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,CloseRound,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,
DotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,CloseRound,
DotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,OpPlus,Int,CloseRound,CloseRound,
DotLowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,Int,CloseRound,
OpenCurly,
LowerIdent,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,Comma,
CloseCurly,
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
											(p-tag (raw ".Value")
												(p-ident (raw "val")))
											(e-ident (raw "val")))
										(branch
											(p-tag (raw ".Empty"))
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
		(s-type-anno (name "compose")
			(ty-fn
				(ty-fn
					(ty-var (raw "b"))
					(ty-var (raw "c")))
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-var (raw "a"))
				(ty-var (raw "c"))))
		(s-decl
			(p-ident (raw "compose"))
			(e-lambda
				(args
					(p-ident (raw "g"))
					(p-ident (raw "f"))
					(p-ident (raw "x")))
				(e-apply
					(e-ident (raw "g"))
					(e-apply
						(e-ident (raw "f"))
						(e-ident (raw "x"))))))
		(s-decl
			(p-ident (raw "transform_twice"))
			(e-lambda
				(args
					(p-ident (raw "f"))
					(p-ident (raw "x")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "first"))
							(e-apply
								(e-ident (raw "compose"))
								(e-ident (raw "f"))
								(e-ident (raw "f"))
								(e-ident (raw "x"))))
						(s-decl
							(p-ident (raw "second"))
							(e-apply
								(e-ident (raw "compose"))
								(e-ident (raw "f"))
								(e-ident (raw "f"))
								(e-ident (raw "first"))))
						(e-ident (raw "second"))))))
		(s-type-anno (name "make_processor")
			(ty-fn
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-fn
					(ty-fn
						(ty-var (raw "b"))
						(ty-var (raw "c")))
					(ty-fn
						(ty-var (raw "a"))
						(ty-var (raw "c"))))))
		(s-decl
			(p-ident (raw "make_processor"))
			(e-lambda
				(args
					(p-ident (raw "f1")))
				(e-lambda
					(args
						(p-ident (raw "f2")))
					(e-lambda
						(args
							(p-ident (raw "x")))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "step1"))
									(e-apply
										(e-ident (raw "f1"))
										(e-ident (raw "x"))))
								(s-decl
									(p-ident (raw "step2"))
									(e-apply
										(e-ident (raw "f2"))
										(e-ident (raw "step1"))))
								(e-ident (raw "step2"))))))))
		(s-type-anno (name "process_with_method")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "c"))
				(ty-var (raw "d")))
			(where
				(method (module-of "a") (name "map")
					(args
						(ty-var (raw "a"))
						(ty-fn
							(ty-var (raw "b"))
							(ty-var (raw "c"))))
					(ty-var (raw "d")))))
		(s-decl
			(p-ident (raw "process_with_method"))
			(e-lambda
				(args
					(p-ident (raw "container"))
					(p-ident (raw "value")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "id"))
							(e-lambda
								(args
									(p-ident (raw "x")))
								(e-ident (raw "x"))))
						(s-decl
							(p-ident (raw "_test1"))
							(e-apply
								(e-ident (raw "id"))
								(e-int (raw "42"))))
						(s-decl
							(p-ident (raw "_test2"))
							(e-apply
								(e-ident (raw "id"))
								(e-string
									(e-string-part (raw "test")))))
						(s-decl
							(p-ident (raw "result"))
							(e-field-access
								(e-ident (raw "container"))
								(e-apply
									(e-ident (raw "map"))
									(e-lambda
										(args
											(p-underscore))
										(e-ident (raw "value"))))))
						(e-ident (raw "result"))))))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "id"))
						(e-lambda
							(args
								(p-ident (raw "x")))
							(e-ident (raw "x"))))
					(s-decl
						(p-ident (raw "_apply_to_container"))
						(e-lambda
							(args
								(p-ident (raw "f")))
							(e-lambda
								(args
									(p-ident (raw "container")))
								(e-lambda
									(args
										(p-ident (raw "default")))
									(e-block
										(statements
											(s-decl
												(p-ident (raw "mapped"))
												(e-field-access
													(e-ident (raw "container"))
													(e-apply
														(e-ident (raw "map"))
														(e-ident (raw "f")))))
											(e-field-access
												(e-ident (raw "mapped"))
												(e-apply
													(e-ident (raw "get_or"))
													(e-ident (raw "default"))))))))))
					(s-decl
						(p-ident (raw "num_container"))
						(e-apply
							(e-tag (raw "Container.Value"))
							(e-int (raw "100"))))
					(s-decl
						(p-ident (raw "str_container"))
						(e-apply
							(e-tag (raw "Container.Value"))
							(e-string
								(e-string-part (raw "hello")))))
					(s-decl
						(p-ident (raw "_empty_container"))
						(e-tag (raw "Container.Empty")))
					(s-decl
						(p-ident (raw "id_num"))
						(e-apply
							(e-ident (raw "id"))
							(e-int (raw "42"))))
					(s-decl
						(p-ident (raw "id_str"))
						(e-apply
							(e-ident (raw "id"))
							(e-string
								(e-string-part (raw "world")))))
					(s-decl
						(p-ident (raw "id_bool"))
						(e-apply
							(e-ident (raw "id"))
							(e-tag (raw "True"))))
					(s-decl
						(p-ident (raw "add_ten"))
						(e-lambda
							(args
								(p-ident (raw "x")))
							(e-binop (op "+")
								(e-ident (raw "x"))
								(e-int (raw "10")))))
					(s-decl
						(p-ident (raw "processor"))
						(e-apply
							(e-apply
								(e-ident (raw "make_processor"))
								(e-ident (raw "add_ten")))
							(e-ident (raw "add_ten"))))
					(s-decl
						(p-ident (raw "processed"))
						(e-apply
							(e-ident (raw "processor"))
							(e-int (raw "5"))))
					(s-decl
						(p-ident (raw "num_result"))
						(e-field-access
							(e-ident (raw "num_container"))
							(e-apply
								(e-ident (raw "map"))
								(e-lambda
									(args
										(p-ident (raw "x")))
									(e-binop (op "+")
										(e-ident (raw "x"))
										(e-int (raw "1")))))))
					(s-decl
						(p-ident (raw "_str_result"))
						(e-field-access
							(e-ident (raw "str_container"))
							(e-apply
								(e-ident (raw "map"))
								(e-lambda
									(args
										(p-ident (raw "s")))
									(e-ident (raw "s"))))))
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
					(s-decl
						(p-ident (raw "double_fn"))
						(e-lambda
							(args
								(p-ident (raw "x")))
							(e-binop (op "+")
								(e-ident (raw "x"))
								(e-ident (raw "x")))))
					(s-decl
						(p-ident (raw "transformed"))
						(e-apply
							(e-ident (raw "transform_twice"))
							(e-ident (raw "double_fn"))
							(e-int (raw "3"))))
					(e-record
						(field (field "id_results")
							(e-tuple
								(e-ident (raw "id_num"))
								(e-ident (raw "id_str"))
								(e-ident (raw "id_bool"))))
						(field (field "processed")
							(e-ident (raw "processed")))
						(field (field "chained")
							(e-ident (raw "chained")))
						(field (field "transformed")
							(e-ident (raw "transformed")))
						(field (field "final")
							(e-field-access
								(e-ident (raw "num_result"))
								(e-apply
									(e-ident (raw "get_or"))
									(e-int (raw "0")))))))))))
~~~
# FORMATTED
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
			Container.Value(val) => val
			Container.Empty => default
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

# First layer: polymorphic helper with annotation
compose : (b -> c), (a -> b), a -> c
compose = |g, f, x| g(f(x))

# Second layer: inferred polymorphic function using compose
transform_twice = |f, x| {
	first = compose(f, f, x)
	second = compose(f, f, first)
	second
}

# Third layer: curried function (multiple lambda layers)
make_processor : (a -> b) -> ((b -> c) -> (a -> c))
make_processor = |f1| |f2| |x| {
	step1 = f1(x)
	step2 = f2(step1)
	step2
}

# Fourth layer: polymorphic function using static dispatch
process_with_method : a, c -> d where [a.map : a, (b -> c) -> d]
process_with_method = |container, value| {
	# Multiple nested lambdas with let-polymorphism
	id = |x| x

	# Use id polymorphically
	_test1 = id(42)
	_test2 = id("test")

	# Static dispatch with method call - just replace values
	result = container.map(|_| value)
	result
}

# Fifth layer: combine everything
main = {
	# Let-polymorphism layer 1
	# TODO INLINE ANNOS
	# id : a -> a 
	id = |x| x

	# Let-polymorphism layer 2 with nested lambdas
	_apply_to_container = |f| |container| |default| {
		mapped = container.map(f)
		mapped.get_or(default)
	}

	# Create containers
	num_container = Container.Value(100)
	str_container = Container.Value("hello")
	_empty_container = Container.Empty

	# Use id polymorphically on different types
	id_num = id(42)
	id_str = id("world")
	id_bool = id(True)

	# Multiple layers of curried application
	add_ten = |x| x + 10
	processor = make_processor(add_ten)(add_ten)
	processed = processor(5)

	# Static dispatch with polymorphic methods
	num_result = num_container.map(|x| x + 1)
	_str_result = str_container.map(|s| s)

	# Chain method calls with static dispatch
	chained = num_container
		.map(|x| x + 1)
		.flat_map(|x| Container.Value(x + 2))
		.get_or(0)

	# Use transform_twice with let-polymorphism
	double_fn = |x| x + x
	transformed = transform_twice(double_fn, 3)

	# Final result combining all techniques
	{
		id_results: (id_num, id_str, id_bool),
		processed: processed,
		chained: chained,
		transformed: transformed,
		final: num_result.get_or(0),
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Container.map"))
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
									(e-tag (name "Empty")))))))))
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
										(p-nominal
											(p-applied-tag))))
								(value
									(e-lookup-local
										(p-assign (ident "val")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal
											(p-applied-tag))))
								(value
									(e-lookup-local
										(p-assign (ident "default")))))))))))
	(d-let
		(p-assign (ident "Container.flat_map"))
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
									(e-tag (name "Empty")))))))))
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
		(p-assign (ident "compose"))
		(e-lambda
			(args
				(p-assign (ident "g"))
				(p-assign (ident "f"))
				(p-assign (ident "x")))
			(e-call
				(e-lookup-local
					(p-assign (ident "g")))
				(e-call
					(e-lookup-local
						(p-assign (ident "f")))
					(e-lookup-local
						(p-assign (ident "x"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var (name "b"))
						(ty-rigid-var (name "c"))))
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var (name "a"))
						(ty-rigid-var-lookup (ty-rigid-var (name "b")))))
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))
				(ty-rigid-var-lookup (ty-rigid-var (name "c"))))))
	(d-let
		(p-assign (ident "transform_twice"))
		(e-closure
			(captures
				(capture (ident "compose")))
			(e-lambda
				(args
					(p-assign (ident "f"))
					(p-assign (ident "x")))
				(e-block
					(s-let
						(p-assign (ident "first"))
						(e-call
							(e-lookup-local
								(p-assign (ident "compose")))
							(e-lookup-local
								(p-assign (ident "f")))
							(e-lookup-local
								(p-assign (ident "f")))
							(e-lookup-local
								(p-assign (ident "x")))))
					(s-let
						(p-assign (ident "second"))
						(e-call
							(e-lookup-local
								(p-assign (ident "compose")))
							(e-lookup-local
								(p-assign (ident "f")))
							(e-lookup-local
								(p-assign (ident "f")))
							(e-lookup-local
								(p-assign (ident "first")))))
					(e-lookup-local
						(p-assign (ident "second")))))))
	(d-let
		(p-assign (ident "make_processor"))
		(e-lambda
			(args
				(p-assign (ident "f1")))
			(e-closure
				(captures
					(capture (ident "f1")))
				(e-lambda
					(args
						(p-assign (ident "f2")))
					(e-closure
						(captures
							(capture (ident "f1"))
							(capture (ident "f2")))
						(e-lambda
							(args
								(p-assign (ident "x")))
							(e-block
								(s-let
									(p-assign (ident "step1"))
									(e-call
										(e-lookup-local
											(p-assign (ident "f1")))
										(e-lookup-local
											(p-assign (ident "x")))))
								(s-let
									(p-assign (ident "step2"))
									(e-call
										(e-lookup-local
											(p-assign (ident "f2")))
										(e-lookup-local
											(p-assign (ident "step1")))))
								(e-lookup-local
									(p-assign (ident "step2")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var (name "a"))
						(ty-rigid-var (name "b"))))
				(ty-parens
					(ty-fn (effectful false)
						(ty-parens
							(ty-fn (effectful false)
								(ty-rigid-var-lookup (ty-rigid-var (name "b")))
								(ty-rigid-var (name "c"))))
						(ty-parens
							(ty-fn (effectful false)
								(ty-rigid-var-lookup (ty-rigid-var (name "a")))
								(ty-rigid-var-lookup (ty-rigid-var (name "c"))))))))))
	(d-let
		(p-assign (ident "process_with_method"))
		(e-lambda
			(args
				(p-assign (ident "container"))
				(p-assign (ident "value")))
			(e-block
				(s-let
					(p-assign (ident "id"))
					(e-lambda
						(args
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "x")))))
				(s-let
					(p-assign (ident "_test1"))
					(e-call
						(e-lookup-local
							(p-assign (ident "id")))
						(e-num (value "42"))))
				(s-let
					(p-assign (ident "_test2"))
					(e-call
						(e-lookup-local
							(p-assign (ident "id")))
						(e-string
							(e-literal (string "test")))))
				(s-let
					(p-assign (ident "result"))
					(e-dot-access (field "map")
						(receiver
							(e-lookup-local
								(p-assign (ident "container"))))
						(args
							(e-closure
								(captures
									(capture (ident "value")))
								(e-lambda
									(args
										(p-underscore))
									(e-lookup-local
										(p-assign (ident "value"))))))))
				(e-lookup-local
					(p-assign (ident "result")))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "c"))
				(ty-rigid-var (name "d")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "map")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-parens
							(ty-fn (effectful false)
								(ty-rigid-var (name "b"))
								(ty-rigid-var-lookup (ty-rigid-var (name "c"))))))
					(ty-rigid-var-lookup (ty-rigid-var (name "d")))))))
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-assign (ident "id"))
				(e-lambda
					(args
						(p-assign (ident "x")))
					(e-lookup-local
						(p-assign (ident "x")))))
			(s-let
				(p-assign (ident "_apply_to_container"))
				(e-lambda
					(args
						(p-assign (ident "f")))
					(e-closure
						(captures
							(capture (ident "f")))
						(e-lambda
							(args
								(p-assign (ident "container")))
							(e-closure
								(captures
									(capture (ident "container"))
									(capture (ident "f")))
								(e-lambda
									(args
										(p-assign (ident "default")))
									(e-block
										(s-let
											(p-assign (ident "mapped"))
											(e-dot-access (field "map")
												(receiver
													(e-lookup-local
														(p-assign (ident "container"))))
												(args
													(e-lookup-local
														(p-assign (ident "f"))))))
										(e-dot-access (field "get_or")
											(receiver
												(e-lookup-local
													(p-assign (ident "mapped"))))
											(args
												(e-lookup-local
													(p-assign (ident "default"))))))))))))
			(s-let
				(p-assign (ident "num_container"))
				(e-nominal (nominal "Container")
					(e-tag (name "Value")
						(args
							(e-num (value "100"))))))
			(s-let
				(p-assign (ident "str_container"))
				(e-nominal (nominal "Container")
					(e-tag (name "Value")
						(args
							(e-string
								(e-literal (string "hello")))))))
			(s-let
				(p-assign (ident "_empty_container"))
				(e-nominal (nominal "Container")
					(e-tag (name "Empty"))))
			(s-let
				(p-assign (ident "id_num"))
				(e-call
					(e-lookup-local
						(p-assign (ident "id")))
					(e-num (value "42"))))
			(s-let
				(p-assign (ident "id_str"))
				(e-call
					(e-lookup-local
						(p-assign (ident "id")))
					(e-string
						(e-literal (string "world")))))
			(s-let
				(p-assign (ident "id_bool"))
				(e-call
					(e-lookup-local
						(p-assign (ident "id")))
					(e-tag (name "True"))))
			(s-let
				(p-assign (ident "add_ten"))
				(e-lambda
					(args
						(p-assign (ident "x")))
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-num (value "10")))))
			(s-let
				(p-assign (ident "processor"))
				(e-call
					(e-call
						(e-lookup-local
							(p-assign (ident "make_processor")))
						(e-lookup-local
							(p-assign (ident "add_ten"))))
					(e-lookup-local
						(p-assign (ident "add_ten")))))
			(s-let
				(p-assign (ident "processed"))
				(e-call
					(e-lookup-local
						(p-assign (ident "processor")))
					(e-num (value "5"))))
			(s-let
				(p-assign (ident "num_result"))
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
			(s-let
				(p-assign (ident "_str_result"))
				(e-dot-access (field "map")
					(receiver
						(e-lookup-local
							(p-assign (ident "str_container"))))
					(args
						(e-lambda
							(args
								(p-assign (ident "s")))
							(e-lookup-local
								(p-assign (ident "s")))))))
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
			(s-let
				(p-assign (ident "double_fn"))
				(e-lambda
					(args
						(p-assign (ident "x")))
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "x"))))))
			(s-let
				(p-assign (ident "transformed"))
				(e-call
					(e-lookup-local
						(p-assign (ident "transform_twice")))
					(e-lookup-local
						(p-assign (ident "double_fn")))
					(e-num (value "3"))))
			(e-record
				(fields
					(field (name "id_results")
						(e-tuple
							(elems
								(e-lookup-local
									(p-assign (ident "id_num")))
								(e-lookup-local
									(p-assign (ident "id_str")))
								(e-lookup-local
									(p-assign (ident "id_bool"))))))
					(field (name "processed")
						(e-lookup-local
							(p-assign (ident "processed"))))
					(field (name "chained")
						(e-lookup-local
							(p-assign (ident "chained"))))
					(field (name "transformed")
						(e-lookup-local
							(p-assign (ident "transformed"))))
					(field (name "final")
						(e-dot-access (field "get_or")
							(receiver
								(e-lookup-local
									(p-assign (ident "num_result"))))
							(args
								(e-num (value "0")))))))))
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
		(patt (type "Container(a), a -> a"))
		(patt (type "Container(a), (a -> Container(b)) -> Container(b)"))
		(patt (type "(b -> c), (a -> b), a -> c"))
		(patt (type "(a -> a), a -> a"))
		(patt (type "(a -> b) -> ((b -> c) -> (a -> c))"))
		(patt (type "a, c -> d where [a.map : a, (b -> c) -> d]"))
		(patt (type "{ chained: a, final: a, id_results: (e, Str, [True, .._others]), processed: c, transformed: a } where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, h -> a, a.plus : a, i -> a, a.plus : a, a -> a, c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), c.plus : c, j -> c, e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)]), h.from_numeral : Numeral -> Try(h, [InvalidNumeral(Str)]), i.from_numeral : Numeral -> Try(i, [InvalidNumeral(Str)]), j.from_numeral : Numeral -> Try(j, [InvalidNumeral(Str)])]")))
	(type_decls
		(nominal (type "Container(a)")
			(ty-header (name "Container")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "Container(a), (a -> b) -> Container(b)"))
		(expr (type "Container(a), a -> a"))
		(expr (type "Container(a), (a -> Container(b)) -> Container(b)"))
		(expr (type "(b -> c), (a -> b), a -> c"))
		(expr (type "(a -> a), a -> a"))
		(expr (type "(a -> b) -> ((b -> c) -> (a -> c))"))
		(expr (type "a, c -> d where [a.map : a, (b -> c) -> d]"))
		(expr (type "{ chained: a, final: a, id_results: (e, Str, [True, .._others]), processed: c, transformed: a } where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, h -> a, a.plus : a, i -> a, a.plus : a, a -> a, c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), c.plus : c, j -> c, e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)]), h.from_numeral : Numeral -> Try(h, [InvalidNumeral(Str)]), i.from_numeral : Numeral -> Try(i, [InvalidNumeral(Str)]), j.from_numeral : Numeral -> Try(j, [InvalidNumeral(Str)])]"))))
~~~
