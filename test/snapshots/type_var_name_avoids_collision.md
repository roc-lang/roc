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

a = 42
identity = |x| x
b = "hello"
c = 3.14
d = True
e = False
anotherIdentity = |y| y
combine = |first, second| (first, second)
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
yetAnotherIdentity = |arg| arg
aa = 100
ab = 200
finalIdentity = |param| param
main! = |_| {
	result1 = identity(123)
	result2 = anotherIdentity("test")
	result3 = combine((result1, result2))
	result4 = yetAnotherIdentity(True)
	result5 = finalIdentity(3.14)
	a + f
}

# Use 'a' as a local variable name to force type variable generation to skip it
# This should get type 'b -> b' since 'a' is taken
# Use more names to test the sequence
# This should get type 'f -> f' since a,b,c,d,e are taken
# Test with a function that has multiple type variables
# Should get types like 'f, g -> (f, g)' or similar
# Use even more names to test wraparound behavior
# This should get type 'aa -> aa' since a-z are taken
# Test that we still avoid collisions even with two-letter names
# This should skip 'aa' and 'ab' and use 'ac -> ac'
# Use some of our functions to avoid unused warnings
# Return something to complete the function
# Just use some of our variables
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "a")
    (Expr.num_literal_i32 42)
  )
  (Expr.binop_equals
    (Expr.lookup "identity")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "b")
    (Expr.str_literal_big)
  )
  (Expr.binop_equals
    (Expr.lookup "c")
    (Expr.frac_literal_small 3.14)
  )
  (Expr.binop_equals
    (Expr.lookup "d")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "e")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "anotherIdentity")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "combine")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "f")
    (Expr.num_literal_i32 1)
  )
  (Expr.binop_equals
    (Expr.lookup "g")
    (Expr.num_literal_i32 2)
  )
  (Expr.binop_equals
    (Expr.lookup "h")
    (Expr.num_literal_i32 3)
  )
  (Expr.binop_equals
    (Expr.lookup "i")
    (Expr.num_literal_i32 4)
  )
  (Expr.binop_equals
    (Expr.lookup "j")
    (Expr.num_literal_i32 5)
  )
  (Expr.binop_equals
    (Expr.lookup "k")
    (Expr.num_literal_i32 6)
  )
  (Expr.binop_equals
    (Expr.lookup "l")
    (Expr.num_literal_i32 7)
  )
  (Expr.binop_equals
    (Expr.lookup "m")
    (Expr.num_literal_i32 8)
  )
  (Expr.binop_equals
    (Expr.lookup "n")
    (Expr.num_literal_i32 9)
  )
  (Expr.binop_equals
    (Expr.lookup "o")
    (Expr.num_literal_i32 10)
  )
  (Expr.binop_equals
    (Expr.lookup "p")
    (Expr.num_literal_i32 11)
  )
  (Expr.binop_equals
    (Expr.lookup "q")
    (Expr.num_literal_i32 12)
  )
  (Expr.binop_equals
    (Expr.lookup "r")
    (Expr.num_literal_i32 13)
  )
  (Expr.binop_equals
    (Expr.lookup "s")
    (Expr.num_literal_i32 14)
  )
  (Expr.binop_equals
    (Expr.lookup "t")
    (Expr.num_literal_i32 15)
  )
  (Expr.binop_equals
    (Expr.lookup "u")
    (Expr.num_literal_i32 16)
  )
  (Expr.binop_equals
    (Expr.lookup "v")
    (Expr.num_literal_i32 17)
  )
  (Expr.binop_equals
    (Expr.lookup "w")
    (Expr.num_literal_i32 18)
  )
  (Expr.binop_equals
    (Expr.lookup "x")
    (Expr.num_literal_i32 19)
  )
  (Expr.binop_equals
    (Expr.lookup "y")
    (Expr.num_literal_i32 20)
  )
  (Expr.binop_equals
    (Expr.lookup "z")
    (Expr.num_literal_i32 21)
  )
  (Expr.binop_equals
    (Expr.lookup "yetAnotherIdentity")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "aa")
    (Expr.num_literal_i32 100)
  )
  (Expr.binop_equals
    (Expr.lookup "ab")
    (Expr.num_literal_i32 200)
  )
  (Expr.binop_equals
    (Expr.lookup "finalIdentity")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.not_lookup)
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_ac")
~~~
# TYPES
~~~roc
a : Num(_size)
identity : _ac
b : Str
c : F64
d : []_others
e : []_others
anotherIdentity : _ac
combine : _ac
f : Num(_size)
g : Num(_size)
h : Num(_size)
i : Num(_size)
j : Num(_size)
k : Num(_size)
l : Num(_size)
m : Num(_size)
n : Num(_size)
o : Num(_size)
p : Num(_size)
q : Num(_size)
r : Num(_size)
s : Num(_size)
t : Num(_size)
u : Num(_size)
v : Num(_size)
w : Num(_size)
x : Num(_size)
y : Num(_size)
z : Num(_size)
yetAnotherIdentity : _ac
aa : Num(_size)
ab : Num(_size)
finalIdentity : _ac
~~~
