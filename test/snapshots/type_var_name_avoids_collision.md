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
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Int(4:5-4:7),
LowerIdent(7:1-7:9),OpAssign(7:10-7:11),OpBar(7:12-7:13),LowerIdent(7:13-7:14),OpBar(7:14-7:15),LowerIdent(7:16-7:17),
LowerIdent(10:1-10:2),OpAssign(10:3-10:4),StringStart(10:5-10:6),StringPart(10:6-10:11),StringEnd(10:11-10:12),
LowerIdent(11:1-11:2),OpAssign(11:3-11:4),Float(11:5-11:9),
LowerIdent(12:1-12:2),OpAssign(12:3-12:4),UpperIdent(12:5-12:9),
LowerIdent(13:1-13:2),OpAssign(13:3-13:4),UpperIdent(13:5-13:10),
LowerIdent(16:1-16:16),OpAssign(16:17-16:18),OpBar(16:19-16:20),LowerIdent(16:20-16:21),OpBar(16:21-16:22),LowerIdent(16:23-16:24),
LowerIdent(20:1-20:8),OpAssign(20:9-20:10),OpBar(20:11-20:12),LowerIdent(20:12-20:17),Comma(20:17-20:18),LowerIdent(20:19-20:25),OpBar(20:25-20:26),OpenRound(20:27-20:28),LowerIdent(20:28-20:33),Comma(20:33-20:34),LowerIdent(20:35-20:41),CloseRound(20:41-20:42),
LowerIdent(23:1-23:2),OpAssign(23:3-23:4),Int(23:5-23:6),
LowerIdent(24:1-24:2),OpAssign(24:3-24:4),Int(24:5-24:6),
LowerIdent(25:1-25:2),OpAssign(25:3-25:4),Int(25:5-25:6),
LowerIdent(26:1-26:2),OpAssign(26:3-26:4),Int(26:5-26:6),
LowerIdent(27:1-27:2),OpAssign(27:3-27:4),Int(27:5-27:6),
LowerIdent(28:1-28:2),OpAssign(28:3-28:4),Int(28:5-28:6),
LowerIdent(29:1-29:2),OpAssign(29:3-29:4),Int(29:5-29:6),
LowerIdent(30:1-30:2),OpAssign(30:3-30:4),Int(30:5-30:6),
LowerIdent(31:1-31:2),OpAssign(31:3-31:4),Int(31:5-31:6),
LowerIdent(32:1-32:2),OpAssign(32:3-32:4),Int(32:5-32:7),
LowerIdent(33:1-33:2),OpAssign(33:3-33:4),Int(33:5-33:7),
LowerIdent(34:1-34:2),OpAssign(34:3-34:4),Int(34:5-34:7),
LowerIdent(35:1-35:2),OpAssign(35:3-35:4),Int(35:5-35:7),
LowerIdent(36:1-36:2),OpAssign(36:3-36:4),Int(36:5-36:7),
LowerIdent(37:1-37:2),OpAssign(37:3-37:4),Int(37:5-37:7),
LowerIdent(38:1-38:2),OpAssign(38:3-38:4),Int(38:5-38:7),
LowerIdent(39:1-39:2),OpAssign(39:3-39:4),Int(39:5-39:7),
LowerIdent(40:1-40:2),OpAssign(40:3-40:4),Int(40:5-40:7),
LowerIdent(41:1-41:2),OpAssign(41:3-41:4),Int(41:5-41:7),
LowerIdent(42:1-42:2),OpAssign(42:3-42:4),Int(42:5-42:7),
LowerIdent(43:1-43:2),OpAssign(43:3-43:4),Int(43:5-43:7),
LowerIdent(46:1-46:19),OpAssign(46:20-46:21),OpBar(46:22-46:23),LowerIdent(46:23-46:26),OpBar(46:26-46:27),LowerIdent(46:28-46:31),
LowerIdent(49:1-49:3),OpAssign(49:4-49:5),Int(49:6-49:9),
LowerIdent(50:1-50:3),OpAssign(50:4-50:5),Int(50:6-50:9),
LowerIdent(53:1-53:14),OpAssign(53:15-53:16),OpBar(53:17-53:18),LowerIdent(53:18-53:23),OpBar(53:23-53:24),LowerIdent(53:25-53:30),
LowerIdent(55:1-55:6),OpAssign(55:7-55:8),OpBar(55:9-55:10),Underscore(55:10-55:11),OpBar(55:11-55:12),OpenCurly(55:13-55:14),
LowerIdent(57:5-57:12),OpAssign(57:13-57:14),LowerIdent(57:15-57:23),NoSpaceOpenRound(57:23-57:24),Int(57:24-57:27),CloseRound(57:27-57:28),
LowerIdent(58:5-58:12),OpAssign(58:13-58:14),LowerIdent(58:15-58:30),NoSpaceOpenRound(58:30-58:31),StringStart(58:31-58:32),StringPart(58:32-58:36),StringEnd(58:36-58:37),CloseRound(58:37-58:38),
LowerIdent(59:5-59:12),OpAssign(59:13-59:14),LowerIdent(59:15-59:22),NoSpaceOpenRound(59:22-59:23),LowerIdent(59:23-59:30),Comma(59:30-59:31),LowerIdent(59:32-59:39),CloseRound(59:39-59:40),
LowerIdent(60:5-60:12),OpAssign(60:13-60:14),LowerIdent(60:15-60:33),NoSpaceOpenRound(60:33-60:34),UpperIdent(60:34-60:38),CloseRound(60:38-60:39),
LowerIdent(61:5-61:12),OpAssign(61:13-61:14),LowerIdent(61:15-61:28),NoSpaceOpenRound(61:28-61:29),Float(61:29-61:33),CloseRound(61:33-61:34),
LowerIdent(64:5-64:6),OpPlus(64:7-64:8),LowerIdent(64:9-64:10),
CloseCurly(65:1-65:2),
EndOfFile(66:1-66:1),
~~~
# PARSE
~~~clojure
(file @1.1-65.2
	(app @1.1-1.53
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11
				(text "main!")))
		(record-field @1.15-1.51 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.51 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-decl @4.1-4.7
			(p-ident @4.1-4.2 (raw "a"))
			(e-int @4.5-4.7 (raw "42")))
		(s-decl @7.1-7.17
			(p-ident @7.1-7.9 (raw "identity"))
			(e-lambda @7.12-7.17
				(args
					(p-ident @7.13-7.14 (raw "x")))
				(e-ident @7.16-7.17 (raw "x"))))
		(s-decl @10.1-10.12
			(p-ident @10.1-10.2 (raw "b"))
			(e-string @10.5-10.12
				(e-string-part @10.6-10.11 (raw "hello"))))
		(s-decl @11.1-11.9
			(p-ident @11.1-11.2 (raw "c"))
			(e-frac @11.5-11.9 (raw "3.14")))
		(s-decl @12.1-12.9
			(p-ident @12.1-12.2 (raw "d"))
			(e-tag @12.5-12.9 (raw "True")))
		(s-decl @13.1-13.10
			(p-ident @13.1-13.2 (raw "e"))
			(e-tag @13.5-13.10 (raw "False")))
		(s-decl @16.1-16.24
			(p-ident @16.1-16.16 (raw "anotherIdentity"))
			(e-lambda @16.19-16.24
				(args
					(p-ident @16.20-16.21 (raw "y")))
				(e-ident @16.23-16.24 (raw "y"))))
		(s-decl @20.1-20.42
			(p-ident @20.1-20.8 (raw "combine"))
			(e-lambda @20.11-20.42
				(args
					(p-ident @20.12-20.17 (raw "first"))
					(p-ident @20.19-20.25 (raw "second")))
				(e-tuple @20.27-20.42
					(e-ident @20.28-20.33 (raw "first"))
					(e-ident @20.35-20.41 (raw "second")))))
		(s-decl @23.1-23.6
			(p-ident @23.1-23.2 (raw "f"))
			(e-int @23.5-23.6 (raw "1")))
		(s-decl @24.1-24.6
			(p-ident @24.1-24.2 (raw "g"))
			(e-int @24.5-24.6 (raw "2")))
		(s-decl @25.1-25.6
			(p-ident @25.1-25.2 (raw "h"))
			(e-int @25.5-25.6 (raw "3")))
		(s-decl @26.1-26.6
			(p-ident @26.1-26.2 (raw "i"))
			(e-int @26.5-26.6 (raw "4")))
		(s-decl @27.1-27.6
			(p-ident @27.1-27.2 (raw "j"))
			(e-int @27.5-27.6 (raw "5")))
		(s-decl @28.1-28.6
			(p-ident @28.1-28.2 (raw "k"))
			(e-int @28.5-28.6 (raw "6")))
		(s-decl @29.1-29.6
			(p-ident @29.1-29.2 (raw "l"))
			(e-int @29.5-29.6 (raw "7")))
		(s-decl @30.1-30.6
			(p-ident @30.1-30.2 (raw "m"))
			(e-int @30.5-30.6 (raw "8")))
		(s-decl @31.1-31.6
			(p-ident @31.1-31.2 (raw "n"))
			(e-int @31.5-31.6 (raw "9")))
		(s-decl @32.1-32.7
			(p-ident @32.1-32.2 (raw "o"))
			(e-int @32.5-32.7 (raw "10")))
		(s-decl @33.1-33.7
			(p-ident @33.1-33.2 (raw "p"))
			(e-int @33.5-33.7 (raw "11")))
		(s-decl @34.1-34.7
			(p-ident @34.1-34.2 (raw "q"))
			(e-int @34.5-34.7 (raw "12")))
		(s-decl @35.1-35.7
			(p-ident @35.1-35.2 (raw "r"))
			(e-int @35.5-35.7 (raw "13")))
		(s-decl @36.1-36.7
			(p-ident @36.1-36.2 (raw "s"))
			(e-int @36.5-36.7 (raw "14")))
		(s-decl @37.1-37.7
			(p-ident @37.1-37.2 (raw "t"))
			(e-int @37.5-37.7 (raw "15")))
		(s-decl @38.1-38.7
			(p-ident @38.1-38.2 (raw "u"))
			(e-int @38.5-38.7 (raw "16")))
		(s-decl @39.1-39.7
			(p-ident @39.1-39.2 (raw "v"))
			(e-int @39.5-39.7 (raw "17")))
		(s-decl @40.1-40.7
			(p-ident @40.1-40.2 (raw "w"))
			(e-int @40.5-40.7 (raw "18")))
		(s-decl @41.1-41.7
			(p-ident @41.1-41.2 (raw "x"))
			(e-int @41.5-41.7 (raw "19")))
		(s-decl @42.1-42.7
			(p-ident @42.1-42.2 (raw "y"))
			(e-int @42.5-42.7 (raw "20")))
		(s-decl @43.1-43.7
			(p-ident @43.1-43.2 (raw "z"))
			(e-int @43.5-43.7 (raw "21")))
		(s-decl @46.1-46.31
			(p-ident @46.1-46.19 (raw "yetAnotherIdentity"))
			(e-lambda @46.22-46.31
				(args
					(p-ident @46.23-46.26 (raw "arg")))
				(e-ident @46.28-46.31 (raw "arg"))))
		(s-decl @49.1-49.9
			(p-ident @49.1-49.3 (raw "aa"))
			(e-int @49.6-49.9 (raw "100")))
		(s-decl @50.1-50.9
			(p-ident @50.1-50.3 (raw "ab"))
			(e-int @50.6-50.9 (raw "200")))
		(s-decl @53.1-53.30
			(p-ident @53.1-53.14 (raw "finalIdentity"))
			(e-lambda @53.17-53.30
				(args
					(p-ident @53.18-53.23 (raw "param")))
				(e-ident @53.25-53.30 (raw "param"))))
		(s-decl @55.1-65.2
			(p-ident @55.1-55.6 (raw "main!"))
			(e-lambda @55.9-65.2
				(args
					(p-underscore))
				(e-block @55.13-65.2
					(statements
						(s-decl @57.5-57.28
							(p-ident @57.5-57.12 (raw "result1"))
							(e-apply @57.15-57.28
								(e-ident @57.15-57.23 (raw "identity"))
								(e-int @57.24-57.27 (raw "123"))))
						(s-decl @58.5-58.38
							(p-ident @58.5-58.12 (raw "result2"))
							(e-apply @58.15-58.38
								(e-ident @58.15-58.30 (raw "anotherIdentity"))
								(e-string @58.31-58.37
									(e-string-part @58.32-58.36 (raw "test")))))
						(s-decl @59.5-59.40
							(p-ident @59.5-59.12 (raw "result3"))
							(e-apply @59.15-59.40
								(e-ident @59.15-59.22 (raw "combine"))
								(e-ident @59.23-59.30 (raw "result1"))
								(e-ident @59.32-59.39 (raw "result2"))))
						(s-decl @60.5-60.39
							(p-ident @60.5-60.12 (raw "result4"))
							(e-apply @60.15-60.39
								(e-ident @60.15-60.33 (raw "yetAnotherIdentity"))
								(e-tag @60.34-60.38 (raw "True"))))
						(s-decl @61.5-61.34
							(p-ident @61.5-61.12 (raw "result5"))
							(e-apply @61.15-61.34
								(e-ident @61.15-61.28 (raw "finalIdentity"))
								(e-frac @61.29-61.33 (raw "3.14"))))
						(e-binop @64.5-64.10 (op "+")
							(e-ident @64.5-64.6 (raw "a"))
							(e-ident @64.9-64.10 (raw "f")))))))))
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
		(p-assign @4.1-4.2 (ident "a"))
		(e-num @4.5-4.7 (value "42")))
	(d-let
		(p-assign @7.1-7.9 (ident "identity"))
		(e-lambda @7.12-7.17
			(args
				(p-assign @7.13-7.14 (ident "x")))
			(e-lookup-local @7.16-7.17
				(p-assign @7.13-7.14 (ident "x")))))
	(d-let
		(p-assign @10.1-10.2 (ident "b"))
		(e-string @10.5-10.12
			(e-literal @10.6-10.11 (string "hello"))))
	(d-let
		(p-assign @11.1-11.2 (ident "c"))
		(e-dec-small @11.5-11.9 (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
	(d-let
		(p-assign @12.1-12.2 (ident "d"))
		(e-nominal @12.5-12.9 (nominal "Bool")
			(e-tag @12.5-12.9 (name "True"))))
	(d-let
		(p-assign @13.1-13.2 (ident "e"))
		(e-nominal @13.5-13.10 (nominal "Bool")
			(e-tag @13.5-13.10 (name "False"))))
	(d-let
		(p-assign @16.1-16.16 (ident "anotherIdentity"))
		(e-lambda @16.19-16.24
			(args
				(p-assign @16.20-16.21 (ident "y")))
			(e-lookup-local @16.23-16.24
				(p-assign @16.20-16.21 (ident "y")))))
	(d-let
		(p-assign @20.1-20.8 (ident "combine"))
		(e-lambda @20.11-20.42
			(args
				(p-assign @20.12-20.17 (ident "first"))
				(p-assign @20.19-20.25 (ident "second")))
			(e-tuple @20.27-20.42
				(elems
					(e-lookup-local @20.28-20.33
						(p-assign @20.12-20.17 (ident "first")))
					(e-lookup-local @20.35-20.41
						(p-assign @20.19-20.25 (ident "second")))))))
	(d-let
		(p-assign @23.1-23.2 (ident "f"))
		(e-num @23.5-23.6 (value "1")))
	(d-let
		(p-assign @24.1-24.2 (ident "g"))
		(e-num @24.5-24.6 (value "2")))
	(d-let
		(p-assign @25.1-25.2 (ident "h"))
		(e-num @25.5-25.6 (value "3")))
	(d-let
		(p-assign @26.1-26.2 (ident "i"))
		(e-num @26.5-26.6 (value "4")))
	(d-let
		(p-assign @27.1-27.2 (ident "j"))
		(e-num @27.5-27.6 (value "5")))
	(d-let
		(p-assign @28.1-28.2 (ident "k"))
		(e-num @28.5-28.6 (value "6")))
	(d-let
		(p-assign @29.1-29.2 (ident "l"))
		(e-num @29.5-29.6 (value "7")))
	(d-let
		(p-assign @30.1-30.2 (ident "m"))
		(e-num @30.5-30.6 (value "8")))
	(d-let
		(p-assign @31.1-31.2 (ident "n"))
		(e-num @31.5-31.6 (value "9")))
	(d-let
		(p-assign @32.1-32.2 (ident "o"))
		(e-num @32.5-32.7 (value "10")))
	(d-let
		(p-assign @33.1-33.2 (ident "p"))
		(e-num @33.5-33.7 (value "11")))
	(d-let
		(p-assign @34.1-34.2 (ident "q"))
		(e-num @34.5-34.7 (value "12")))
	(d-let
		(p-assign @35.1-35.2 (ident "r"))
		(e-num @35.5-35.7 (value "13")))
	(d-let
		(p-assign @36.1-36.2 (ident "s"))
		(e-num @36.5-36.7 (value "14")))
	(d-let
		(p-assign @37.1-37.2 (ident "t"))
		(e-num @37.5-37.7 (value "15")))
	(d-let
		(p-assign @38.1-38.2 (ident "u"))
		(e-num @38.5-38.7 (value "16")))
	(d-let
		(p-assign @39.1-39.2 (ident "v"))
		(e-num @39.5-39.7 (value "17")))
	(d-let
		(p-assign @40.1-40.2 (ident "w"))
		(e-num @40.5-40.7 (value "18")))
	(d-let
		(p-assign @41.1-41.2 (ident "x"))
		(e-num @41.5-41.7 (value "19")))
	(d-let
		(p-assign @42.1-42.2 (ident "y"))
		(e-num @42.5-42.7 (value "20")))
	(d-let
		(p-assign @43.1-43.2 (ident "z"))
		(e-num @43.5-43.7 (value "21")))
	(d-let
		(p-assign @46.1-46.19 (ident "yetAnotherIdentity"))
		(e-lambda @46.22-46.31
			(args
				(p-assign @46.23-46.26 (ident "arg")))
			(e-lookup-local @46.28-46.31
				(p-assign @46.23-46.26 (ident "arg")))))
	(d-let
		(p-assign @49.1-49.3 (ident "aa"))
		(e-num @49.6-49.9 (value "100")))
	(d-let
		(p-assign @50.1-50.3 (ident "ab"))
		(e-num @50.6-50.9 (value "200")))
	(d-let
		(p-assign @53.1-53.14 (ident "finalIdentity"))
		(e-lambda @53.17-53.30
			(args
				(p-assign @53.18-53.23 (ident "param")))
			(e-lookup-local @53.25-53.30
				(p-assign @53.18-53.23 (ident "param")))))
	(d-let
		(p-assign @55.1-55.6 (ident "main!"))
		(e-closure @55.9-65.2
			(captures
				(capture @46.1-46.19 (ident "yetAnotherIdentity"))
				(capture @23.1-23.2 (ident "f"))
				(capture @4.1-4.2 (ident "a"))
				(capture @53.1-53.14 (ident "finalIdentity"))
				(capture @7.1-7.9 (ident "identity"))
				(capture @16.1-16.16 (ident "anotherIdentity"))
				(capture @20.1-20.8 (ident "combine")))
			(e-lambda @55.9-65.2
				(args
					(p-underscore @55.10-55.11))
				(e-block @55.13-65.2
					(s-let @57.5-57.28
						(p-assign @57.5-57.12 (ident "result1"))
						(e-call @57.15-57.28
							(e-lookup-local @57.15-57.23
								(p-assign @7.1-7.9 (ident "identity")))
							(e-num @57.24-57.27 (value "123"))))
					(s-let @58.5-58.38
						(p-assign @58.5-58.12 (ident "result2"))
						(e-call @58.15-58.38
							(e-lookup-local @58.15-58.30
								(p-assign @16.1-16.16 (ident "anotherIdentity")))
							(e-string @58.31-58.37
								(e-literal @58.32-58.36 (string "test")))))
					(s-let @59.5-59.40
						(p-assign @59.5-59.12 (ident "result3"))
						(e-call @59.15-59.40
							(e-lookup-local @59.15-59.22
								(p-assign @20.1-20.8 (ident "combine")))
							(e-lookup-local @59.23-59.30
								(p-assign @57.5-57.12 (ident "result1")))
							(e-lookup-local @59.32-59.39
								(p-assign @58.5-58.12 (ident "result2")))))
					(s-let @60.5-60.39
						(p-assign @60.5-60.12 (ident "result4"))
						(e-call @60.15-60.39
							(e-lookup-local @60.15-60.33
								(p-assign @46.1-46.19 (ident "yetAnotherIdentity")))
							(e-nominal @60.34-60.38 (nominal "Bool")
								(e-tag @60.34-60.38 (name "True")))))
					(s-let @61.5-61.34
						(p-assign @61.5-61.12 (ident "result5"))
						(e-call @61.15-61.34
							(e-lookup-local @61.15-61.28
								(p-assign @53.1-53.14 (ident "finalIdentity")))
							(e-dec-small @61.29-61.33 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
					(e-binop @64.5-64.10 (op "add")
						(e-lookup-local @64.5-64.6
							(p-assign @4.1-4.2 (ident "a")))
						(e-lookup-local @64.9-64.10
							(p-assign @23.1-23.2 (ident "f")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.2 (type "Num(_size)"))
		(patt @7.1-7.9 (type "ac -> ac"))
		(patt @10.1-10.2 (type "Str"))
		(patt @11.1-11.2 (type "Num(Frac(_size))"))
		(patt @12.1-12.2 (type "Bool"))
		(patt @13.1-13.2 (type "Bool"))
		(patt @16.1-16.16 (type "ac -> ac"))
		(patt @20.1-20.8 (type "ac, ad -> (ac, ad)"))
		(patt @23.1-23.2 (type "Num(_size)"))
		(patt @24.1-24.2 (type "Num(_size)"))
		(patt @25.1-25.2 (type "Num(_size)"))
		(patt @26.1-26.2 (type "Num(_size)"))
		(patt @27.1-27.2 (type "Num(_size)"))
		(patt @28.1-28.2 (type "Num(_size)"))
		(patt @29.1-29.2 (type "Num(_size)"))
		(patt @30.1-30.2 (type "Num(_size)"))
		(patt @31.1-31.2 (type "Num(_size)"))
		(patt @32.1-32.2 (type "Num(_size)"))
		(patt @33.1-33.2 (type "Num(_size)"))
		(patt @34.1-34.2 (type "Num(_size)"))
		(patt @35.1-35.2 (type "Num(_size)"))
		(patt @36.1-36.2 (type "Num(_size)"))
		(patt @37.1-37.2 (type "Num(_size)"))
		(patt @38.1-38.2 (type "Num(_size)"))
		(patt @39.1-39.2 (type "Num(_size)"))
		(patt @40.1-40.2 (type "Num(_size)"))
		(patt @41.1-41.2 (type "Num(_size)"))
		(patt @42.1-42.2 (type "Num(_size)"))
		(patt @43.1-43.2 (type "Num(_size)"))
		(patt @46.1-46.19 (type "ac -> ac"))
		(patt @49.1-49.3 (type "Num(_size)"))
		(patt @50.1-50.3 (type "Num(_size)"))
		(patt @53.1-53.14 (type "ac -> ac"))
		(patt @55.1-55.6 (type "_arg2 -> Num(_size)")))
	(expressions
		(expr @4.5-4.7 (type "Num(_size)"))
		(expr @7.12-7.17 (type "ac -> ac"))
		(expr @10.5-10.12 (type "Str"))
		(expr @11.5-11.9 (type "Num(Frac(_size))"))
		(expr @12.5-12.9 (type "Bool"))
		(expr @13.5-13.10 (type "Bool"))
		(expr @16.19-16.24 (type "ac -> ac"))
		(expr @20.11-20.42 (type "ac, ad -> (ac, ad)"))
		(expr @23.5-23.6 (type "Num(_size)"))
		(expr @24.5-24.6 (type "Num(_size)"))
		(expr @25.5-25.6 (type "Num(_size)"))
		(expr @26.5-26.6 (type "Num(_size)"))
		(expr @27.5-27.6 (type "Num(_size)"))
		(expr @28.5-28.6 (type "Num(_size)"))
		(expr @29.5-29.6 (type "Num(_size)"))
		(expr @30.5-30.6 (type "Num(_size)"))
		(expr @31.5-31.6 (type "Num(_size)"))
		(expr @32.5-32.7 (type "Num(_size)"))
		(expr @33.5-33.7 (type "Num(_size)"))
		(expr @34.5-34.7 (type "Num(_size)"))
		(expr @35.5-35.7 (type "Num(_size)"))
		(expr @36.5-36.7 (type "Num(_size)"))
		(expr @37.5-37.7 (type "Num(_size)"))
		(expr @38.5-38.7 (type "Num(_size)"))
		(expr @39.5-39.7 (type "Num(_size)"))
		(expr @40.5-40.7 (type "Num(_size)"))
		(expr @41.5-41.7 (type "Num(_size)"))
		(expr @42.5-42.7 (type "Num(_size)"))
		(expr @43.5-43.7 (type "Num(_size)"))
		(expr @46.22-46.31 (type "ac -> ac"))
		(expr @49.6-49.9 (type "Num(_size)"))
		(expr @50.6-50.9 (type "Num(_size)"))
		(expr @53.17-53.30 (type "ac -> ac"))
		(expr @55.9-65.2 (type "_arg2 -> Num(_size)"))))
~~~
