# META
~~~ini
description=Let-polymorphism with numbers
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic number polymorphism
num = 42
frac = 4.2

# Using polymorphic values in different contexts
int_use = num
float_use = frac

# num used as Int
int_add = num + 10
int_multiply = num * 2

# num used as Float
float_add = num + 3.14
float_multiply = num * 2.5

# Polymorphic function with numeric types
double = |x| x * 2

# Used with different numeric types
int_doubled = double(5)
float_doubled = double(2.5)

main = |_| {
    # Combine results
    int_add + int_multiply
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Float,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
LowerIdent,OpAssign,LowerIdent,OpPlus,Float,
LowerIdent,OpAssign,LowerIdent,OpStar,Float,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpStar,Int,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Float,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-decl
			(p-ident (raw "num"))
			(e-int (raw "42")))
		(s-decl
			(p-ident (raw "frac"))
			(e-frac (raw "4.2")))
		(s-decl
			(p-ident (raw "int_use"))
			(e-ident (raw "num")))
		(s-decl
			(p-ident (raw "float_use"))
			(e-ident (raw "frac")))
		(s-decl
			(p-ident (raw "int_add"))
			(e-binop (op "+")
				(e-ident (raw "num"))
				(e-int (raw "10"))))
		(s-decl
			(p-ident (raw "int_multiply"))
			(e-binop (op "*")
				(e-ident (raw "num"))
				(e-int (raw "2"))))
		(s-decl
			(p-ident (raw "float_add"))
			(e-binop (op "+")
				(e-ident (raw "num"))
				(e-frac (raw "3.14"))))
		(s-decl
			(p-ident (raw "float_multiply"))
			(e-binop (op "*")
				(e-ident (raw "num"))
				(e-frac (raw "2.5"))))
		(s-decl
			(p-ident (raw "double"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "*")
					(e-ident (raw "x"))
					(e-int (raw "2")))))
		(s-decl
			(p-ident (raw "int_doubled"))
			(e-apply
				(e-ident (raw "double"))
				(e-int (raw "5"))))
		(s-decl
			(p-ident (raw "float_doubled"))
			(e-apply
				(e-ident (raw "double"))
				(e-frac (raw "2.5"))))
		(s-decl
			(p-ident (raw "main"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(e-binop (op "+")
							(e-ident (raw "int_add"))
							(e-ident (raw "int_multiply")))))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic number polymorphism
num = 42
frac = 4.2

# Using polymorphic values in different contexts
int_use = num
float_use = frac

# num used as Int
int_add = num + 10
int_multiply = num * 2

# num used as Float
float_add = num + 3.14
float_multiply = num * 2.5

# Polymorphic function with numeric types
double = |x| x * 2

# Used with different numeric types
int_doubled = double(5)
float_doubled = double(2.5)

main = |_| {
	# Combine results
	int_add + int_multiply
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "num"))
		(e-num (value "42")))
	(d-let
		(p-assign (ident "frac"))
		(e-dec-small (numerator "42") (denominator-power-of-ten "1") (value "4.2")))
	(d-let
		(p-assign (ident "int_use"))
		(e-lookup-local
			(p-assign (ident "num"))))
	(d-let
		(p-assign (ident "float_use"))
		(e-lookup-local
			(p-assign (ident "frac"))))
	(d-let
		(p-assign (ident "int_add"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "num")))
			(e-num (value "10"))))
	(d-let
		(p-assign (ident "int_multiply"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "num")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "float_add"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "num")))
			(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
	(d-let
		(p-assign (ident "float_multiply"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "num")))
			(e-dec-small (numerator "25") (denominator-power-of-ten "1") (value "2.5"))))
	(d-let
		(p-assign (ident "double"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "mul")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-num (value "2")))))
	(d-let
		(p-assign (ident "int_doubled"))
		(e-call
			(e-lookup-local
				(p-assign (ident "double")))
			(e-num (value "5"))))
	(d-let
		(p-assign (ident "float_doubled"))
		(e-call
			(e-lookup-local
				(p-assign (ident "double")))
			(e-dec-small (numerator "25") (denominator-power-of-ten "1") (value "2.5"))))
	(d-let
		(p-assign (ident "main"))
		(e-closure
			(captures
				(capture (ident "int_multiply"))
				(capture (ident "int_add")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "int_add")))
						(e-lookup-local
							(p-assign (ident "int_multiply")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Frac(_size))"))
		(patt (type "Num(Frac(_size))"))
		(patt (type "Num(Frac(_size))"))
		(patt (type "Num(Frac(_size))"))
		(patt (type "Num(Frac(_size))"))
		(patt (type "Num(Frac(_size))"))
		(patt (type "Num(Frac(_size))"))
		(patt (type "Num(Frac(_size))"))
		(patt (type "Num(_size) -> Num(_size2)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(Frac(_size))"))
		(patt (type "_arg -> Num(Frac(_size))")))
	(expressions
		(expr (type "Num(Frac(_size))"))
		(expr (type "Num(Frac(_size))"))
		(expr (type "Num(Frac(_size))"))
		(expr (type "Num(Frac(_size))"))
		(expr (type "Num(Frac(_size))"))
		(expr (type "Num(Frac(_size))"))
		(expr (type "Num(Frac(_size))"))
		(expr (type "Num(Frac(_size))"))
		(expr (type "Num(_size) -> Num(_size2)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(Frac(_size))"))
		(expr (type "_arg -> Num(Frac(_size))"))))
~~~
