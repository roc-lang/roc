# META
~~~ini
description=Type variable names avoid collision with existing identifiers in module
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Use 'a' as a local variable name to force type variable generation to skip it
a = 42

# This should get type 'b -> b' since 'a' is taken
identity = |x| x

# Use more names to test the sequence
b = "hello"
c = 3.14
d = True
e = False

# This should get type 'f -> f' since a,b,c,d,e are taken
anotherIdentity = |y| y

# Test with a function that has multiple type variables
# Should get types like 'f, g -> (f, g)' or similar
combine = |first, second| (first, second)

# Use even more names to test wraparound behavior
f = 1
g = 2
h = 3
i = 4
j = 5
k = 6
l = 7
m = 8
n = 9
o = 10
p = 11
q = 12
r = 13
s = 14
t = 15
u = 16
v = 17
w = 18
x = 19
y = 20
z = 21

# This should get type 'aa -> aa' since a-z are taken
yetAnotherIdentity = |arg| arg

# Test that we still avoid collisions even with two-letter names
aa = 100
ab = 200

# This should skip 'aa' and 'ab' and use 'ac -> ac'
finalIdentity = |param| param

main! = |_| {
    # Use some of our functions to avoid unused warnings
    result1 = identity(123)
    result2 = anotherIdentity("test")
    result3 = combine(result1, result2)
    result4 = yetAnotherIdentity(True)
    result5 = finalIdentity(3.14)
    
    # Return something to complete the function
    a + f  # Just use some of our variables
}
~~~
# EXPECTED
UNUSED VARIABLE - type_var_name_avoids_collision.md:59:5:59:12
UNUSED VARIABLE - type_var_name_avoids_collision.md:60:5:60:12
UNUSED VARIABLE - type_var_name_avoids_collision.md:61:5:61:12
# PROBLEMS
**UNUSED VARIABLE**
Variable `result3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result3` to suppress this warning.
The unused variable is declared here:
**type_var_name_avoids_collision.md:59:5:59:12:**
```roc
    result3 = combine(result1, result2)
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable `result4` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result4` to suppress this warning.
The unused variable is declared here:
**type_var_name_avoids_collision.md:60:5:60:12:**
```roc
    result4 = yetAnotherIdentity(True)
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable `result5` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result5` to suppress this warning.
The unused variable is declared here:
**type_var_name_avoids_collision.md:61:5:61:12:**
```roc
    result5 = finalIdentity(3.14)
```
    ^^^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,Float,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Float,CloseRound,
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
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-int (raw "42")))
		(s-decl
			(p-ident (raw "identity"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-decl
			(p-ident (raw "b"))
			(e-string
				(e-string-part (raw "hello"))))
		(s-decl
			(p-ident (raw "c"))
			(e-frac (raw "3.14")))
		(s-decl
			(p-ident (raw "d"))
			(e-tag (raw "True")))
		(s-decl
			(p-ident (raw "e"))
			(e-tag (raw "False")))
		(s-decl
			(p-ident (raw "anotherIdentity"))
			(e-lambda
				(args
					(p-ident (raw "y")))
				(e-ident (raw "y"))))
		(s-decl
			(p-ident (raw "combine"))
			(e-lambda
				(args
					(p-ident (raw "first"))
					(p-ident (raw "second")))
				(e-tuple
					(e-ident (raw "first"))
					(e-ident (raw "second")))))
		(s-decl
			(p-ident (raw "f"))
			(e-int (raw "1")))
		(s-decl
			(p-ident (raw "g"))
			(e-int (raw "2")))
		(s-decl
			(p-ident (raw "h"))
			(e-int (raw "3")))
		(s-decl
			(p-ident (raw "i"))
			(e-int (raw "4")))
		(s-decl
			(p-ident (raw "j"))
			(e-int (raw "5")))
		(s-decl
			(p-ident (raw "k"))
			(e-int (raw "6")))
		(s-decl
			(p-ident (raw "l"))
			(e-int (raw "7")))
		(s-decl
			(p-ident (raw "m"))
			(e-int (raw "8")))
		(s-decl
			(p-ident (raw "n"))
			(e-int (raw "9")))
		(s-decl
			(p-ident (raw "o"))
			(e-int (raw "10")))
		(s-decl
			(p-ident (raw "p"))
			(e-int (raw "11")))
		(s-decl
			(p-ident (raw "q"))
			(e-int (raw "12")))
		(s-decl
			(p-ident (raw "r"))
			(e-int (raw "13")))
		(s-decl
			(p-ident (raw "s"))
			(e-int (raw "14")))
		(s-decl
			(p-ident (raw "t"))
			(e-int (raw "15")))
		(s-decl
			(p-ident (raw "u"))
			(e-int (raw "16")))
		(s-decl
			(p-ident (raw "v"))
			(e-int (raw "17")))
		(s-decl
			(p-ident (raw "w"))
			(e-int (raw "18")))
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "19")))
		(s-decl
			(p-ident (raw "y"))
			(e-int (raw "20")))
		(s-decl
			(p-ident (raw "z"))
			(e-int (raw "21")))
		(s-decl
			(p-ident (raw "yetAnotherIdentity"))
			(e-lambda
				(args
					(p-ident (raw "arg")))
				(e-ident (raw "arg"))))
		(s-decl
			(p-ident (raw "aa"))
			(e-int (raw "100")))
		(s-decl
			(p-ident (raw "ab"))
			(e-int (raw "200")))
		(s-decl
			(p-ident (raw "finalIdentity"))
			(e-lambda
				(args
					(p-ident (raw "param")))
				(e-ident (raw "param"))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "result1"))
							(e-apply
								(e-ident (raw "identity"))
								(e-int (raw "123"))))
						(s-decl
							(p-ident (raw "result2"))
							(e-apply
								(e-ident (raw "anotherIdentity"))
								(e-string
									(e-string-part (raw "test")))))
						(s-decl
							(p-ident (raw "result3"))
							(e-apply
								(e-ident (raw "combine"))
								(e-ident (raw "result1"))
								(e-ident (raw "result2"))))
						(s-decl
							(p-ident (raw "result4"))
							(e-apply
								(e-ident (raw "yetAnotherIdentity"))
								(e-tag (raw "True"))))
						(s-decl
							(p-ident (raw "result5"))
							(e-apply
								(e-ident (raw "finalIdentity"))
								(e-frac (raw "3.14"))))
						(e-binop (op "+")
							(e-ident (raw "a"))
							(e-ident (raw "f")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Use 'a' as a local variable name to force type variable generation to skip it
a = 42

# This should get type 'b -> b' since 'a' is taken
identity = |x| x

# Use more names to test the sequence
b = "hello"
c = 3.14
d = True
e = False

# This should get type 'f -> f' since a,b,c,d,e are taken
anotherIdentity = |y| y

# Test with a function that has multiple type variables
# Should get types like 'f, g -> (f, g)' or similar
combine = |first, second| (first, second)

# Use even more names to test wraparound behavior
f = 1
g = 2
h = 3
i = 4
j = 5
k = 6
l = 7
m = 8
n = 9
o = 10
p = 11
q = 12
r = 13
s = 14
t = 15
u = 16
v = 17
w = 18
x = 19
y = 20
z = 21

# This should get type 'aa -> aa' since a-z are taken
yetAnotherIdentity = |arg| arg

# Test that we still avoid collisions even with two-letter names
aa = 100
ab = 200

# This should skip 'aa' and 'ab' and use 'ac -> ac'
finalIdentity = |param| param

main! = |_| {
	# Use some of our functions to avoid unused warnings
	result1 = identity(123)
	result2 = anotherIdentity("test")
	result3 = combine(result1, result2)
	result4 = yetAnotherIdentity(True)
	result5 = finalIdentity(3.14)

	# Return something to complete the function
	a + f # Just use some of our variables
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-num (value "42")))
	(d-let
		(p-assign (ident "identity"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x")))))
	(d-let
		(p-assign (ident "b"))
		(e-string
			(e-literal (string "hello"))))
	(d-let
		(p-assign (ident "c"))
		(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
	(d-let
		(p-assign (ident "d"))
		(e-nominal (nominal "Bool")
			(e-tag (name "True"))))
	(d-let
		(p-assign (ident "e"))
		(e-nominal (nominal "Bool")
			(e-tag (name "False"))))
	(d-let
		(p-assign (ident "anotherIdentity"))
		(e-lambda
			(args
				(p-assign (ident "y")))
			(e-lookup-local
				(p-assign (ident "y")))))
	(d-let
		(p-assign (ident "combine"))
		(e-lambda
			(args
				(p-assign (ident "first"))
				(p-assign (ident "second")))
			(e-tuple
				(elems
					(e-lookup-local
						(p-assign (ident "first")))
					(e-lookup-local
						(p-assign (ident "second")))))))
	(d-let
		(p-assign (ident "f"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "g"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "h"))
		(e-num (value "3")))
	(d-let
		(p-assign (ident "i"))
		(e-num (value "4")))
	(d-let
		(p-assign (ident "j"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "k"))
		(e-num (value "6")))
	(d-let
		(p-assign (ident "l"))
		(e-num (value "7")))
	(d-let
		(p-assign (ident "m"))
		(e-num (value "8")))
	(d-let
		(p-assign (ident "n"))
		(e-num (value "9")))
	(d-let
		(p-assign (ident "o"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "p"))
		(e-num (value "11")))
	(d-let
		(p-assign (ident "q"))
		(e-num (value "12")))
	(d-let
		(p-assign (ident "r"))
		(e-num (value "13")))
	(d-let
		(p-assign (ident "s"))
		(e-num (value "14")))
	(d-let
		(p-assign (ident "t"))
		(e-num (value "15")))
	(d-let
		(p-assign (ident "u"))
		(e-num (value "16")))
	(d-let
		(p-assign (ident "v"))
		(e-num (value "17")))
	(d-let
		(p-assign (ident "w"))
		(e-num (value "18")))
	(d-let
		(p-assign (ident "x"))
		(e-num (value "19")))
	(d-let
		(p-assign (ident "y"))
		(e-num (value "20")))
	(d-let
		(p-assign (ident "z"))
		(e-num (value "21")))
	(d-let
		(p-assign (ident "yetAnotherIdentity"))
		(e-lambda
			(args
				(p-assign (ident "arg")))
			(e-lookup-local
				(p-assign (ident "arg")))))
	(d-let
		(p-assign (ident "aa"))
		(e-num (value "100")))
	(d-let
		(p-assign (ident "ab"))
		(e-num (value "200")))
	(d-let
		(p-assign (ident "finalIdentity"))
		(e-lambda
			(args
				(p-assign (ident "param")))
			(e-lookup-local
				(p-assign (ident "param")))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "anotherIdentity"))
				(capture (ident "f"))
				(capture (ident "a"))
				(capture (ident "finalIdentity"))
				(capture (ident "identity"))
				(capture (ident "combine"))
				(capture (ident "yetAnotherIdentity")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "result1"))
						(e-call
							(e-lookup-local
								(p-assign (ident "identity")))
							(e-num (value "123"))))
					(s-let
						(p-assign (ident "result2"))
						(e-call
							(e-lookup-local
								(p-assign (ident "anotherIdentity")))
							(e-string
								(e-literal (string "test")))))
					(s-let
						(p-assign (ident "result3"))
						(e-call
							(e-lookup-local
								(p-assign (ident "combine")))
							(e-lookup-local
								(p-assign (ident "result1")))
							(e-lookup-local
								(p-assign (ident "result2")))))
					(s-let
						(p-assign (ident "result4"))
						(e-call
							(e-lookup-local
								(p-assign (ident "yetAnotherIdentity")))
							(e-nominal (nominal "Bool")
								(e-tag (name "True")))))
					(s-let
						(p-assign (ident "result5"))
						(e-call
							(e-lookup-local
								(p-assign (ident "finalIdentity")))
							(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "a")))
						(e-lookup-local
							(p-assign (ident "f")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size)"))
		(patt (type "ac -> ac"))
		(patt (type "Str"))
		(patt (type "Num(Frac(_size))"))
		(patt (type "Bool"))
		(patt (type "Bool"))
		(patt (type "ac -> ac"))
		(patt (type "ac, ad -> (ac, ad)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "ac -> ac"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "ac -> ac"))
		(patt (type "_arg2 -> Num(_size)")))
	(expressions
		(expr (type "Num(_size)"))
		(expr (type "ac -> ac"))
		(expr (type "Str"))
		(expr (type "Num(Frac(_size))"))
		(expr (type "Bool"))
		(expr (type "Bool"))
		(expr (type "ac -> ac"))
		(expr (type "ac, ad -> (ac, ad)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "ac -> ac"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "ac -> ac"))
		(expr (type "_arg2 -> Num(_size)"))))
~~~
