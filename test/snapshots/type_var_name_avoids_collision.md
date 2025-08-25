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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpAssign Int LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpAssign String LowerIdent OpAssign Float LowerIdent OpAssign UpperIdent LowerIdent OpAssign UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound Float CloseRound LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "a")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "identity")
    (lambda
      (body
        (lc "x")
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_equals
    (lc "b")
    (str_literal_big "hello")
  )
  (binop_equals
    (lc "c")
    (frac_literal_small 3.14)
  )
  (binop_equals
    (lc "d")
    (uc "True")
  )
  (binop_equals
    (lc "e")
    (uc "False")
  )
  (binop_equals
    (lc "anotherIdentity")
    (lambda
      (body
        (lc "y")
      )
      (args
        (lc "y")
      )
    )
  )
  (binop_equals
    (lc "combine")
    (lambda
      (body
        (tuple_literal
          (lc "first")
          (lc "second")
        )
      )
      (args
        (tuple_literal
          (lc "first")
          (lc "second")
        )
      )
    )
  )
  (binop_equals
    (lc "f")
    (num_literal_i32 1)
  )
  (binop_equals
    (lc "g")
    (num_literal_i32 2)
  )
  (binop_equals
    (lc "h")
    (num_literal_i32 3)
  )
  (binop_equals
    (lc "i")
    (num_literal_i32 4)
  )
  (binop_equals
    (lc "j")
    (num_literal_i32 5)
  )
  (binop_equals
    (lc "k")
    (num_literal_i32 6)
  )
  (binop_equals
    (lc "l")
    (num_literal_i32 7)
  )
  (binop_equals
    (lc "m")
    (num_literal_i32 8)
  )
  (binop_equals
    (lc "n")
    (num_literal_i32 9)
  )
  (binop_equals
    (lc "o")
    (num_literal_i32 10)
  )
  (binop_equals
    (lc "p")
    (num_literal_i32 11)
  )
  (binop_equals
    (lc "q")
    (num_literal_i32 12)
  )
  (binop_equals
    (lc "r")
    (num_literal_i32 13)
  )
  (binop_equals
    (lc "s")
    (num_literal_i32 14)
  )
  (binop_equals
    (lc "t")
    (num_literal_i32 15)
  )
  (binop_equals
    (lc "u")
    (num_literal_i32 16)
  )
  (binop_equals
    (lc "v")
    (num_literal_i32 17)
  )
  (binop_equals
    (lc "w")
    (num_literal_i32 18)
  )
  (binop_equals
    (lc "x")
    (num_literal_i32 19)
  )
  (binop_equals
    (lc "y")
    (num_literal_i32 20)
  )
  (binop_equals
    (lc "z")
    (num_literal_i32 21)
  )
  (binop_equals
    (lc "yetAnotherIdentity")
    (lambda
      (body
        (lc "arg")
      )
      (args
        (lc "arg")
      )
    )
  )
  (binop_equals
    (lc "aa")
    (num_literal_i32 100)
  )
  (binop_equals
    (lc "ab")
    (num_literal_i32 200)
  )
  (binop_equals
    (lc "finalIdentity")
    (lambda
      (body
        (lc "param")
      )
      (args
        (lc "param")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (block
          (binop_equals
            (lc "result1")
            (apply_lc
              (lc "identity")
              (num_literal_i32 123)
            )
          )
          (binop_equals
            (lc "result2")
            (apply_lc
              (lc "anotherIdentity")
              (str_literal_small "test")
            )
          )
          (binop_equals
            (lc "result3")
            (apply_lc
              (lc "combine")
              (tuple_literal
                (lc "result1")
                (lc "result2")
              )
            )
          )
          (binop_equals
            (lc "result4")
            (apply_lc
              (lc "yetAnotherIdentity")
              (uc "True")
            )
          )
          (binop_equals
            (lc "result5")
            (apply_lc
              (lc "finalIdentity")
              (frac_literal_small 3.14)
            )
          )
          (binop_plus
            (lc "a")
            (lc "f")
          )
        )
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app
{
	pf: "../basic-cli/main.roc" platform [
		main,
	],
}

a = 42

# This should get type 'b -> b' since 'a' is taken
identity = \x -> x

# Use more names to test the sequence
b = "hello"
c = 3.14
d = True
e = False

# This should get type 'f -> f' since a,b,c,d,e are taken
anotherIdentity = \y -> y

# Test with a function that has multiple type variables
# Should get types like 'f, g -> (f, g)' or similar
combine = \(
	first,
	second
) -> (first, second)
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
yetAnotherIdentity = \arg -> arg
aa = 100
ab = 200
finalIdentity = \param -> param
main! = \_ -> {
	result1 = identity(123)
	result2 = anotherIdentity("test")
	result3 = combine((result1, result2))
	result4 = yetAnotherIdentity(True)
	result5 = finalIdentity(3.14)
	a + f
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 7:12 to 7:16

**Unsupported Node**
at 16:19 to 16:23

**Unsupported Node**
at 20:11 to 20:27

**Unsupported Node**
at 46:22 to 46:28

**Unsupported Node**
at 53:17 to 53:25

**Unsupported Node**
at 55:1 to 55:6

**Unsupported Node**
at 55:9 to 55:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
