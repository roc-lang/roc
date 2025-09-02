# META
~~~ini
description=Type variable names avoid collision with existing identifiers in module
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

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
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpAssign String LowerIdent OpAssign Float LowerIdent OpAssign UpperIdent LowerIdent OpAssign UpperIdent BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LineComment LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpAssign Int LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound Float CloseRound BlankLine LineComment LowerIdent OpPlus LowerIdent LineComment CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

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
	result3 = combine((result1, result2))
	result4 = yetAnotherIdentity(True)
	result5 = finalIdentity(3.14)

	# Return something to complete the function
	a + f
}

# Just use some of our variables
~~~
# EXPECTED
UNUSED VARIABLE - type_var_name_avoids_collision.md:61:5:61:12
UNUSED VARIABLE - type_var_name_avoids_collision.md:59:5:59:12
UNUSED VARIABLE - type_var_name_avoids_collision.md:60:5:60:12
# PROBLEMS
**UNUSED VARIABLE**
Variable **result5** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result5` to suppress this warning.
The unused variable is declared here:

**type_var_name_avoids_collision.md:61:5:61:12:**
```roc
    result5 = finalIdentity(3.14)
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable **result3** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result3` to suppress this warning.
The unused variable is declared here:

**type_var_name_avoids_collision.md:59:5:59:12:**
```roc
    result3 = combine(result1, result2)
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable **result4** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result4` to suppress this warning.
The unused variable is declared here:

**type_var_name_avoids_collision.md:60:5:60:12:**
```roc
    result4 = yetAnotherIdentity(True)
```
    ^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "a"))
    (Expr.num_literal_i32 42)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "b"))
    (Expr.str_literal_big)
  )
  (Stmt.assign
    (pattern (Patt.ident "c"))
    (Expr.frac_literal_small 3.14)
  )
  (Stmt.assign
    (pattern (Patt.ident "d"))
    (Expr.apply_tag)
  )
  (Stmt.assign
    (pattern (Patt.ident "e"))
    (Expr.apply_tag)
  )
  (Stmt.assign
    (pattern (Patt.ident "anotherIdentity"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "combine"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "f"))
    (Expr.num_literal_i32 1)
  )
  (Stmt.assign
    (pattern (Patt.ident "g"))
    (Expr.num_literal_i32 2)
  )
  (Stmt.assign
    (pattern (Patt.ident "h"))
    (Expr.num_literal_i32 3)
  )
  (Stmt.assign
    (pattern (Patt.ident "i"))
    (Expr.num_literal_i32 4)
  )
  (Stmt.assign
    (pattern (Patt.ident "j"))
    (Expr.num_literal_i32 5)
  )
  (Stmt.assign
    (pattern (Patt.ident "k"))
    (Expr.num_literal_i32 6)
  )
  (Stmt.assign
    (pattern (Patt.ident "l"))
    (Expr.num_literal_i32 7)
  )
  (Stmt.assign
    (pattern (Patt.ident "m"))
    (Expr.num_literal_i32 8)
  )
  (Stmt.assign
    (pattern (Patt.ident "n"))
    (Expr.num_literal_i32 9)
  )
  (Stmt.assign
    (pattern (Patt.ident "o"))
    (Expr.num_literal_i32 10)
  )
  (Stmt.assign
    (pattern (Patt.ident "p"))
    (Expr.num_literal_i32 11)
  )
  (Stmt.assign
    (pattern (Patt.ident "q"))
    (Expr.num_literal_i32 12)
  )
  (Stmt.assign
    (pattern (Patt.ident "r"))
    (Expr.num_literal_i32 13)
  )
  (Stmt.assign
    (pattern (Patt.ident "s"))
    (Expr.num_literal_i32 14)
  )
  (Stmt.assign
    (pattern (Patt.ident "t"))
    (Expr.num_literal_i32 15)
  )
  (Stmt.assign
    (pattern (Patt.ident "u"))
    (Expr.num_literal_i32 16)
  )
  (Stmt.assign
    (pattern (Patt.ident "v"))
    (Expr.num_literal_i32 17)
  )
  (Stmt.assign
    (pattern (Patt.ident "w"))
    (Expr.num_literal_i32 18)
  )
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.num_literal_i32 19)
  )
  (Stmt.assign
    (pattern (Patt.ident "y"))
    (Expr.num_literal_i32 20)
  )
  (Stmt.assign
    (pattern (Patt.ident "z"))
    (Expr.num_literal_i32 21)
  )
  (Stmt.assign
    (pattern (Patt.ident "yetAnotherIdentity"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "aa"))
    (Expr.num_literal_i32 100)
  )
  (Stmt.assign
    (pattern (Patt.ident "ab"))
    (Expr.num_literal_i32 200)
  )
  (Stmt.assign
    (pattern (Patt.ident "finalIdentity"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
