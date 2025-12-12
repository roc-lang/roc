# META
~~~ini
description=Type variable alias for static dispatch on type parameters
type=snippet
~~~
# SOURCE
~~~roc
# Type var alias allows static dispatch on type variables
# When a function has a type parameter, you can bind it to an uppercase
# alias and call methods on that type.

# Simple example: calling a method on a type variable
call_default : {} -> thing where [thing.default : () -> thing]
call_default = |_| {
    Thing : thing
    Thing.default()
}

# Multiple type variables with different constraints
combine : a, b -> a where [a.from_b : b -> a]
combine = |_first, second| {
    A : a
    A.from_b(second)
}

# Multiple methods on same type variable
process_value : val -> val where [val.transform : val -> val, val.validate : val -> Bool]
process_value = |input| {
    V : val
    if V.validate(input) {
        V.transform(input)
    } else {
        input
    }
}

# Chaining method results - pass result of one dispatch to another
chain_methods : x -> x where [x.second : x -> x, x.first : () -> x]
chain_methods = |_| {
    X : x
    initial = X.first()
    X.second(initial)
}

# Multiple type var aliases in same scope with different type vars
multi_alias : a, b -> (a, b) where [a.convert : a -> a, b.convert : b -> b]
multi_alias = |x, y| {
    A : a
    B : b
    (A.convert(x), B.convert(y))
}

# Method taking multiple arguments
method_with_args : t, Str -> t where [t.create : Str, U64 -> t]
method_with_args = |_, name| {
    T : t
    T.create(name, 42)
}

# Method that takes a function argument and returns the type variable
from_str : Str -> thing where [thing.from_str : Str -> thing]
from_str = |str| {
    Thing : thing
    Thing.from_str(str)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenRound,CloseRound,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,NamedUnderscore,Comma,LowerIdent,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,Comma,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
KwIf,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpenCurly,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,KwElse,OpenCurly,
LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,Comma,LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenRound,CloseRound,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,Comma,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
UpperIdent,OpColon,LowerIdent,
OpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
CloseCurly,
LowerIdent,OpColon,LowerIdent,Comma,UpperIdent,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,Comma,LowerIdent,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,Int,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "call_default")
			(ty-fn
				(ty-record)
				(ty-var (raw "thing")))
			(where
				(method (module-of "thing") (name "default")
					(args)
					(ty-var (raw "thing")))))
		(s-decl
			(p-ident (raw "call_default"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-type-decl
							(header (name "Thing")
								(args))
							(ty-var (raw "thing")))
						(e-apply
							(e-ident (raw "Thing.default")))))))
		(s-type-anno (name "combine")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b"))
				(ty-var (raw "a")))
			(where
				(method (module-of "a") (name "from_b")
					(args
						(ty-var (raw "b")))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "combine"))
			(e-lambda
				(args
					(p-ident (raw "_first"))
					(p-ident (raw "second")))
				(e-block
					(statements
						(s-type-decl
							(header (name "A")
								(args))
							(ty-var (raw "a")))
						(e-apply
							(e-ident (raw "A.from_b"))
							(e-ident (raw "second")))))))
		(s-type-anno (name "process_value")
			(ty-fn
				(ty-var (raw "val"))
				(ty-var (raw "val")))
			(where
				(method (module-of "val") (name "transform")
					(args
						(ty-var (raw "val")))
					(ty-var (raw "val")))
				(method (module-of "val") (name "validate")
					(args
						(ty-var (raw "val")))
					(ty (name "Bool")))))
		(s-decl
			(p-ident (raw "process_value"))
			(e-lambda
				(args
					(p-ident (raw "input")))
				(e-block
					(statements
						(s-type-decl
							(header (name "V")
								(args))
							(ty-var (raw "val")))
						(e-if-then-else
							(e-apply
								(e-ident (raw "V.validate"))
								(e-ident (raw "input")))
							(e-block
								(statements
									(e-apply
										(e-ident (raw "V.transform"))
										(e-ident (raw "input")))))
							(e-block
								(statements
									(e-ident (raw "input")))))))))
		(s-type-anno (name "chain_methods")
			(ty-fn
				(ty-var (raw "x"))
				(ty-var (raw "x")))
			(where
				(method (module-of "x") (name "second")
					(args
						(ty-var (raw "x")))
					(ty-var (raw "x")))
				(method (module-of "x") (name "first")
					(args)
					(ty-var (raw "x")))))
		(s-decl
			(p-ident (raw "chain_methods"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-type-decl
							(header (name "X")
								(args))
							(ty-var (raw "x")))
						(s-decl
							(p-ident (raw "initial"))
							(e-apply
								(e-ident (raw "X.first"))))
						(e-apply
							(e-ident (raw "X.second"))
							(e-ident (raw "initial")))))))
		(s-type-anno (name "multi_alias")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b"))
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(where
				(method (module-of "a") (name "convert")
					(args
						(ty-var (raw "a")))
					(ty-var (raw "a")))
				(method (module-of "b") (name "convert")
					(args
						(ty-var (raw "b")))
					(ty-var (raw "b")))))
		(s-decl
			(p-ident (raw "multi_alias"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-block
					(statements
						(s-type-decl
							(header (name "A")
								(args))
							(ty-var (raw "a")))
						(s-type-decl
							(header (name "B")
								(args))
							(ty-var (raw "b")))
						(e-tuple
							(e-apply
								(e-ident (raw "A.convert"))
								(e-ident (raw "x")))
							(e-apply
								(e-ident (raw "B.convert"))
								(e-ident (raw "y"))))))))
		(s-type-anno (name "method_with_args")
			(ty-fn
				(ty-var (raw "t"))
				(ty (name "Str"))
				(ty-var (raw "t")))
			(where
				(method (module-of "t") (name "create")
					(args
						(ty (name "Str"))
						(ty (name "U64")))
					(ty-var (raw "t")))))
		(s-decl
			(p-ident (raw "method_with_args"))
			(e-lambda
				(args
					(p-underscore)
					(p-ident (raw "name")))
				(e-block
					(statements
						(s-type-decl
							(header (name "T")
								(args))
							(ty-var (raw "t")))
						(e-apply
							(e-ident (raw "T.create"))
							(e-ident (raw "name"))
							(e-int (raw "42")))))))
		(s-type-anno (name "from_str")
			(ty-fn
				(ty (name "Str"))
				(ty-var (raw "thing")))
			(where
				(method (module-of "thing") (name "from_str")
					(args
						(ty (name "Str")))
					(ty-var (raw "thing")))))
		(s-decl
			(p-ident (raw "from_str"))
			(e-lambda
				(args
					(p-ident (raw "str")))
				(e-block
					(statements
						(s-type-decl
							(header (name "Thing")
								(args))
							(ty-var (raw "thing")))
						(e-apply
							(e-ident (raw "Thing.from_str"))
							(e-ident (raw "str")))))))))
~~~
# FORMATTED
~~~roc
# Type var alias allows static dispatch on type variables
# When a function has a type parameter, you can bind it to an uppercase
# alias and call methods on that type.

# Simple example: calling a method on a type variable
call_default : {} -> thing where [thing.default : thing]
call_default = |_| {
	Thing : thing
	Thing.default()
}

# Multiple type variables with different constraints
combine : a, b -> a where [a.from_b : b -> a]
combine = |_first, second| {
	A : a
	A.from_b(second)
}

# Multiple methods on same type variable
process_value : val -> val where [val.transform : val -> val, val.validate : val -> Bool]
process_value = |input| {
	V : val
	if V.validate(input) {
		V.transform(input)
	} else {
		input
	}
}

# Chaining method results - pass result of one dispatch to another
chain_methods : x -> x where [x.second : x -> x, x.first : x]
chain_methods = |_| {
	X : x
	initial = X.first()
	X.second(initial)
}

# Multiple type var aliases in same scope with different type vars
multi_alias : a, b -> (a, b) where [a.convert : a -> a, b.convert : b -> b]
multi_alias = |x, y| {
	A : a
	B : b
	(A.convert(x), B.convert(y))
}

# Method taking multiple arguments
method_with_args : t, Str -> t where [t.create : Str, U64 -> t]
method_with_args = |_, name| {
	T : t
	T.create(name, 42)
}

# Method that takes a function argument and returns the type variable
from_str : Str -> thing where [thing.from_str : Str -> thing]
from_str = |str| {
	Thing : thing
	Thing.from_str(str)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "call_default"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-type-var-alias (alias "Thing") (type-var "thing")
					(ty-rigid-var (name "thing")))
				(e-type-var-dispatch (method "default"))))
		(annotation
			(ty-fn (effectful false)
				(ty-record)
				(ty-rigid-var (name "thing")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "thing"))) (name "default")
					(args)
					(ty-rigid-var-lookup (ty-rigid-var (name "thing")))))))
	(d-let
		(p-assign (ident "combine"))
		(e-lambda
			(args
				(p-assign (ident "_first"))
				(p-assign (ident "second")))
			(e-block
				(s-type-var-alias (alias "A") (type-var "a")
					(ty-rigid-var (name "a")))
				(e-type-var-dispatch (method "from_b")
					(e-lookup-local
						(p-assign (ident "second"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "from_b")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "b"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "process_value"))
		(e-lambda
			(args
				(p-assign (ident "input")))
			(e-block
				(s-type-var-alias (alias "V") (type-var "val")
					(ty-rigid-var (name "val")))
				(e-if
					(if-branches
						(if-branch
							(e-type-var-dispatch (method "validate")
								(e-lookup-local
									(p-assign (ident "input"))))
							(e-block
								(e-type-var-dispatch (method "transform")
									(e-lookup-local
										(p-assign (ident "input")))))))
					(if-else
						(e-block
							(e-lookup-local
								(p-assign (ident "input"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "val"))
				(ty-rigid-var-lookup (ty-rigid-var (name "val"))))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "val"))) (name "transform")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "val"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "val"))))
				(method (ty-rigid-var-lookup (ty-rigid-var (name "val"))) (name "validate")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "val"))))
					(ty-lookup (name "Bool") (builtin))))))
	(d-let
		(p-assign (ident "chain_methods"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-type-var-alias (alias "X") (type-var "x")
					(ty-rigid-var (name "x")))
				(s-let
					(p-assign (ident "initial"))
					(e-type-var-dispatch (method "first")))
				(e-type-var-dispatch (method "second")
					(e-lookup-local
						(p-assign (ident "initial"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "x"))
				(ty-rigid-var-lookup (ty-rigid-var (name "x"))))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "x"))) (name "second")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "x"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "x"))))
				(method (ty-rigid-var-lookup (ty-rigid-var (name "x"))) (name "first")
					(args)
					(ty-rigid-var-lookup (ty-rigid-var (name "x")))))))
	(d-let
		(p-assign (ident "multi_alias"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-block
				(s-type-var-alias (alias "A") (type-var "a")
					(ty-rigid-var (name "a")))
				(s-type-var-alias (alias "B") (type-var "b")
					(ty-rigid-var (name "b")))
				(e-tuple
					(elems
						(e-type-var-dispatch (method "convert")
							(e-lookup-local
								(p-assign (ident "x"))))
						(e-type-var-dispatch (method "convert")
							(e-lookup-local
								(p-assign (ident "y"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))
				(ty-tuple
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "convert")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
				(method (ty-rigid-var-lookup (ty-rigid-var (name "b"))) (name "convert")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "b"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))))))
	(d-let
		(p-assign (ident "method_with_args"))
		(e-lambda
			(args
				(p-underscore)
				(p-assign (ident "name")))
			(e-block
				(s-type-var-alias (alias "T") (type-var "t")
					(ty-rigid-var (name "t")))
				(e-type-var-dispatch (method "create")
					(e-lookup-local
						(p-assign (ident "name")))
					(e-num (value "42")))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "t"))
				(ty-lookup (name "Str") (builtin))
				(ty-rigid-var-lookup (ty-rigid-var (name "t"))))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "t"))) (name "create")
					(args
						(ty-lookup (name "Str") (builtin))
						(ty-lookup (name "U64") (builtin)))
					(ty-rigid-var-lookup (ty-rigid-var (name "t")))))))
	(d-let
		(p-assign (ident "from_str"))
		(e-lambda
			(args
				(p-assign (ident "str")))
			(e-block
				(s-type-var-alias (alias "Thing") (type-var "thing")
					(ty-rigid-var (name "thing")))
				(e-type-var-dispatch (method "from_str")
					(e-lookup-local
						(p-assign (ident "str"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-rigid-var (name "thing")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "thing"))) (name "from_str")
					(args
						(ty-lookup (name "Str") (builtin)))
					(ty-rigid-var-lookup (ty-rigid-var (name "thing"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{  } -> thing where [thing.default : ({}) -> thing]"))
		(patt (type "a, b -> a where [a.from_b : b -> a]"))
		(patt (type "val -> val where [val.transform : val -> val, val.validate : val -> Bool]"))
		(patt (type "x -> x where [x.second : x -> x, x.first : ({}) -> x]"))
		(patt (type "a, b -> (a, b) where [a.convert : a -> a, b.convert : b -> b]"))
		(patt (type "t, Str -> t where [t.create : Str, U64 -> t]"))
		(patt (type "Str -> thing where [thing.from_str : Str -> thing]")))
	(expressions
		(expr (type "{  } -> thing where [thing.default : ({}) -> thing]"))
		(expr (type "a, b -> a where [a.from_b : b -> a]"))
		(expr (type "val -> val where [val.transform : val -> val, val.validate : val -> Bool]"))
		(expr (type "x -> x where [x.second : x -> x, x.first : ({}) -> x]"))
		(expr (type "a, b -> (a, b) where [a.convert : a -> a, b.convert : b -> b]"))
		(expr (type "t, Str -> t where [t.create : Str, U64 -> t]"))
		(expr (type "Str -> thing where [thing.from_str : Str -> thing]"))))
~~~
