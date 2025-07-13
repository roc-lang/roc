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
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),Int(4:7-4:9),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),Float(5:8-5:11),
LowerIdent(8:1-8:8),OpAssign(8:9-8:10),LowerIdent(8:11-8:14),
LowerIdent(9:1-9:10),OpAssign(9:11-9:12),LowerIdent(9:13-9:17),
LowerIdent(12:1-12:8),OpAssign(12:9-12:10),LowerIdent(12:11-12:14),OpPlus(12:15-12:16),Int(12:17-12:19),
LowerIdent(13:1-13:13),OpAssign(13:14-13:15),LowerIdent(13:16-13:19),OpStar(13:20-13:21),Int(13:22-13:23),
LowerIdent(16:1-16:10),OpAssign(16:11-16:12),LowerIdent(16:13-16:16),OpPlus(16:17-16:18),Float(16:19-16:23),
LowerIdent(17:1-17:15),OpAssign(17:16-17:17),LowerIdent(17:18-17:21),OpStar(17:22-17:23),Float(17:24-17:27),
LowerIdent(20:1-20:7),OpAssign(20:8-20:9),OpBar(20:10-20:11),LowerIdent(20:11-20:12),OpBar(20:12-20:13),LowerIdent(20:14-20:15),OpStar(20:16-20:17),Int(20:18-20:19),
LowerIdent(23:1-23:12),OpAssign(23:13-23:14),LowerIdent(23:15-23:21),NoSpaceOpenRound(23:21-23:22),Int(23:22-23:23),CloseRound(23:23-23:24),
LowerIdent(24:1-24:14),OpAssign(24:15-24:16),LowerIdent(24:17-24:23),NoSpaceOpenRound(24:23-24:24),Float(24:24-24:27),CloseRound(24:27-24:28),
LowerIdent(26:1-26:5),OpAssign(26:6-26:7),OpBar(26:8-26:9),Underscore(26:9-26:10),OpBar(26:10-26:11),OpenCurly(26:12-26:13),
LowerIdent(28:5-28:12),OpPlus(28:13-28:14),LowerIdent(28:15-28:27),
CloseCurly(29:1-29:2),EndOfFile(29:2-29:2),
~~~
# PARSE
~~~clojure
(file @1.1-29.2
	(app @1.1-1.56
		(provides @1.5-1.11
			(exposed-lower-ident @1.6-1.10 (text "main")))
		(record-field @1.14-1.54 (name "pf")
			(e-string @1.27-1.54
				(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))
		(packages @1.12-1.56
			(record-field @1.14-1.54 (name "pf")
				(e-string @1.27-1.54
					(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-decl @4.1-4.9
			(p-ident @4.1-4.4 (raw "num"))
			(e-int @4.7-4.9 (raw "42")))
		(s-decl @5.1-5.11
			(p-ident @5.1-5.5 (raw "frac"))
			(e-frac @5.8-5.11 (raw "4.2")))
		(s-decl @8.1-8.14
			(p-ident @8.1-8.8 (raw "int_use"))
			(e-ident @8.11-8.14 (raw "num")))
		(s-decl @9.1-9.17
			(p-ident @9.1-9.10 (raw "float_use"))
			(e-ident @9.13-9.17 (raw "frac")))
		(s-decl @12.1-12.19
			(p-ident @12.1-12.8 (raw "int_add"))
			(e-binop @12.11-12.19 (op "+")
				(e-ident @12.11-12.14 (raw "num"))
				(e-int @12.17-12.19 (raw "10"))))
		(s-decl @13.1-13.23
			(p-ident @13.1-13.13 (raw "int_multiply"))
			(e-binop @13.16-13.23 (op "*")
				(e-ident @13.16-13.19 (raw "num"))
				(e-int @13.22-13.23 (raw "2"))))
		(s-decl @16.1-16.23
			(p-ident @16.1-16.10 (raw "float_add"))
			(e-binop @16.13-16.23 (op "+")
				(e-ident @16.13-16.16 (raw "num"))
				(e-frac @16.19-16.23 (raw "3.14"))))
		(s-decl @17.1-17.27
			(p-ident @17.1-17.15 (raw "float_multiply"))
			(e-binop @17.18-17.27 (op "*")
				(e-ident @17.18-17.21 (raw "num"))
				(e-frac @17.24-17.27 (raw "2.5"))))
		(s-decl @20.1-20.19
			(p-ident @20.1-20.7 (raw "double"))
			(e-lambda @20.10-20.19
				(args
					(p-ident @20.11-20.12 (raw "x")))
				(e-binop @20.14-20.19 (op "*")
					(e-ident @20.14-20.15 (raw "x"))
					(e-int @20.18-20.19 (raw "2")))))
		(s-decl @23.1-23.24
			(p-ident @23.1-23.12 (raw "int_doubled"))
			(e-apply @23.15-23.24
				(e-ident @23.15-23.21 (raw "double"))
				(e-int @23.22-23.23 (raw "5"))))
		(s-decl @24.1-24.28
			(p-ident @24.1-24.14 (raw "float_doubled"))
			(e-apply @24.17-24.28
				(e-ident @24.17-24.23 (raw "double"))
				(e-frac @24.24-24.27 (raw "2.5"))))
		(s-decl @26.1-29.2
			(p-ident @26.1-26.5 (raw "main"))
			(e-lambda @26.8-29.2
				(args
					(p-underscore))
				(e-block @26.12-29.2
					(statements
						(e-binop @28.5-28.27 (op "+")
							(e-ident @28.5-28.12 (raw "int_add"))
							(e-ident @28.15-28.27 (raw "int_multiply")))))))))
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
		(p-assign @4.1-4.4 (ident "num"))
		(e-int @4.7-4.9 (value "42")))
	(d-let
		(p-assign @5.1-5.5 (ident "frac"))
		(e-dec-small @5.8-5.11 (numerator "42") (denominator-power-of-ten "1") (value "4.2")))
	(d-let
		(p-assign @8.1-8.8 (ident "int_use"))
		(e-lookup-local @8.11-8.14
			(p-assign @4.1-4.4 (ident "num"))))
	(d-let
		(p-assign @9.1-9.10 (ident "float_use"))
		(e-lookup-local @9.13-9.17
			(p-assign @5.1-5.5 (ident "frac"))))
	(d-let
		(p-assign @12.1-12.8 (ident "int_add"))
		(e-binop @12.11-12.19 (op "add")
			(e-lookup-local @12.11-12.14
				(p-assign @4.1-4.4 (ident "num")))
			(e-int @12.17-12.19 (value "10"))))
	(d-let
		(p-assign @13.1-13.13 (ident "int_multiply"))
		(e-binop @13.16-13.23 (op "mul")
			(e-lookup-local @13.16-13.19
				(p-assign @4.1-4.4 (ident "num")))
			(e-int @13.22-13.23 (value "2"))))
	(d-let
		(p-assign @16.1-16.10 (ident "float_add"))
		(e-binop @16.13-16.23 (op "add")
			(e-lookup-local @16.13-16.16
				(p-assign @4.1-4.4 (ident "num")))
			(e-dec-small @16.19-16.23 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
	(d-let
		(p-assign @17.1-17.15 (ident "float_multiply"))
		(e-binop @17.18-17.27 (op "mul")
			(e-lookup-local @17.18-17.21
				(p-assign @4.1-4.4 (ident "num")))
			(e-dec-small @17.24-17.27 (numerator "25") (denominator-power-of-ten "1") (value "2.5"))))
	(d-let
		(p-assign @20.1-20.7 (ident "double"))
		(e-lambda @20.10-20.19
			(args
				(p-assign @20.11-20.12 (ident "x")))
			(e-binop @20.14-20.19 (op "mul")
				(e-lookup-local @20.14-20.15
					(p-assign @20.11-20.12 (ident "x")))
				(e-int @20.18-20.19 (value "2")))))
	(d-let
		(p-assign @23.1-23.12 (ident "int_doubled"))
		(e-call @23.15-23.24
			(e-lookup-local @23.15-23.21
				(p-assign @20.1-20.7 (ident "double")))
			(e-int @23.22-23.23 (value "5"))))
	(d-let
		(p-assign @24.1-24.14 (ident "float_doubled"))
		(e-call @24.17-24.28
			(e-lookup-local @24.17-24.23
				(p-assign @20.1-20.7 (ident "double")))
			(e-dec-small @24.24-24.27 (numerator "25") (denominator-power-of-ten "1") (value "2.5"))))
	(d-let
		(p-assign @26.1-26.5 (ident "main"))
		(e-lambda @26.8-29.2
			(args
				(p-underscore @26.9-26.10))
			(e-block @26.12-29.2
				(e-binop @28.5-28.27 (op "add")
					(e-lookup-local @28.5-28.12
						(p-assign @12.1-12.8 (ident "int_add")))
					(e-lookup-local @28.15-28.27
						(p-assign @13.1-13.13 (ident "int_multiply"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "Num(size)"))
		(patt @5.1-5.5 (type "Frac(size)"))
		(patt @8.1-8.8 (type "Num(size)"))
		(patt @9.1-9.10 (type "Frac(size)"))
		(patt @12.1-12.8 (type "a"))
		(patt @13.1-13.13 (type "b"))
		(patt @16.1-16.10 (type "c"))
		(patt @17.1-17.15 (type "d"))
		(patt @20.1-20.7 (type "arg -> ret"))
		(patt @23.1-23.12 (type "e"))
		(patt @24.1-24.14 (type "f"))
		(patt @26.1-26.5 (type "arg2 -> ret2")))
	(expressions
		(expr @4.7-4.9 (type "Num(size)"))
		(expr @5.8-5.11 (type "Frac(size)"))
		(expr @8.11-8.14 (type "Num(size)"))
		(expr @9.13-9.17 (type "Frac(size)"))
		(expr @12.11-12.19 (type "a"))
		(expr @13.16-13.23 (type "b"))
		(expr @16.13-16.23 (type "c"))
		(expr @17.18-17.27 (type "d"))
		(expr @20.10-20.19 (type "arg -> ret"))
		(expr @23.15-23.24 (type "e"))
		(expr @24.17-24.28 (type "f"))
		(expr @26.8-29.2 (type "arg2 -> ret2"))))
~~~
