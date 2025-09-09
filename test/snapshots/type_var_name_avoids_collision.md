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
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpAssign String LowerIdent OpAssign Float LowerIdent OpAssign UpperIdent LowerIdent OpAssign UpperIdent BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LineComment LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpAssign Int LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound Float CloseRound BlankLine LineComment LowerIdent OpPlus LowerIdent LineComment CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block)
      )
    )
))
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
        (lc "first")
        (lc "second")
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
app [main!] { pf: "../basic-cli/main.roc" platform [] }

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
UNUSED VARIABLE - type_var_name_avoids_collision.md:59:5:59:12
UNUSED VARIABLE - type_var_name_avoids_collision.md:60:5:60:12
UNUSED VARIABLE - type_var_name_avoids_collision.md:61:5:61:12
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:4:1:4:2:**
```roc
a = 42
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:7:13:7:14:**
```roc
identity = |x| x
```
            ^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:7:1:7:9:**
```roc
identity = |x| x
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:10:1:10:2:**
```roc
b = "hello"
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:11:1:11:2:**
```roc
c = 3.14
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:12:1:12:2:**
```roc
d = True
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:13:1:13:2:**
```roc
e = False
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:16:20:16:21:**
```roc
anotherIdentity = |y| y
```
                   ^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:16:1:16:16:**
```roc
anotherIdentity = |y| y
```
^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:20:1:20:8:**
```roc
combine = |first, second| (first, second)
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:23:1:23:2:**
```roc
f = 1
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:24:1:24:2:**
```roc
g = 2
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:25:1:25:2:**
```roc
h = 3
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:26:1:26:2:**
```roc
i = 4
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:27:1:27:2:**
```roc
j = 5
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:28:1:28:2:**
```roc
k = 6
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:29:1:29:2:**
```roc
l = 7
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:30:1:30:2:**
```roc
m = 8
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:31:1:31:2:**
```roc
n = 9
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:32:1:32:2:**
```roc
o = 10
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:33:1:33:2:**
```roc
p = 11
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:34:1:34:2:**
```roc
q = 12
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:35:1:35:2:**
```roc
r = 13
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:36:1:36:2:**
```roc
s = 14
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:37:1:37:2:**
```roc
t = 15
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:38:1:38:2:**
```roc
u = 16
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:39:1:39:2:**
```roc
v = 17
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:40:1:40:2:**
```roc
w = 18
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:41:1:41:2:**
```roc
x = 19
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:42:1:42:2:**
```roc
y = 20
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:43:1:43:2:**
```roc
z = 21
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:46:1:46:19:**
```roc
yetAnotherIdentity = |arg| arg
```
^^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:49:1:49:3:**
```roc
aa = 100
```
^^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:50:1:50:3:**
```roc
ab = 200
```
^^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:53:1:53:14:**
```roc
finalIdentity = |param| param
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_name_avoids_collision.md:55:1:55:6:**
```roc
main! = |_| {
```
^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



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
    (Expr.tag_no_args)
  )
  (Stmt.assign
    (pattern (Patt.ident "e"))
    (Expr.tag_no_args)
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
; Total type variables: 176
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 -> #8)
(var #8 Num *)
(var #9 _)
(var #10 -> #156)
(var #11 _)
(var #12 _)
(var #13 -> #156)
(var #14 _)
(var #15 -> #16)
(var #16 Str)
(var #17 _)
(var #18 -> #19)
(var #19 F64)
(var #20 _)
(var #21 -> #22)
(var #22 _)
(var #23 _)
(var #24 -> #25)
(var #25 _)
(var #26 _)
(var #27 -> #158)
(var #28 _)
(var #29 _)
(var #30 -> #158)
(var #31 _)
(var #32 -> #163)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 -> #161)
(var #38 -> #163)
(var #39 _)
(var #40 -> #41)
(var #41 Num *)
(var #42 _)
(var #43 -> #44)
(var #44 Num *)
(var #45 _)
(var #46 -> #47)
(var #47 Num *)
(var #48 _)
(var #49 -> #50)
(var #50 Num *)
(var #51 _)
(var #52 -> #53)
(var #53 Num *)
(var #54 _)
(var #55 -> #56)
(var #56 Num *)
(var #57 _)
(var #58 -> #59)
(var #59 Num *)
(var #60 _)
(var #61 -> #62)
(var #62 Num *)
(var #63 _)
(var #64 -> #65)
(var #65 Num *)
(var #66 _)
(var #67 -> #68)
(var #68 Num *)
(var #69 _)
(var #70 -> #71)
(var #71 Num *)
(var #72 _)
(var #73 -> #74)
(var #74 Num *)
(var #75 _)
(var #76 -> #77)
(var #77 Num *)
(var #78 _)
(var #79 -> #80)
(var #80 Num *)
(var #81 _)
(var #82 -> #83)
(var #83 Num *)
(var #84 _)
(var #85 -> #86)
(var #86 Num *)
(var #87 _)
(var #88 -> #89)
(var #89 Num *)
(var #90 _)
(var #91 -> #92)
(var #92 Num *)
(var #93 _)
(var #94 -> #95)
(var #95 Num *)
(var #96 _)
(var #97 -> #98)
(var #98 Num *)
(var #99 _)
(var #100 -> #101)
(var #101 Num *)
(var #102 _)
(var #103 -> #165)
(var #104 _)
(var #105 _)
(var #106 -> #165)
(var #107 _)
(var #108 -> #109)
(var #109 Num *)
(var #110 _)
(var #111 -> #112)
(var #112 Num *)
(var #113 _)
(var #114 -> #167)
(var #115 _)
(var #116 _)
(var #117 -> #167)
(var #118 _)
(var #119 -> #175)
(var #120 _)
(var #121 -> #124)
(var #122 -> #169)
(var #123 Num *)
(var #124 _)
(var #125 _)
(var #126 -> #129)
(var #127 -> #170)
(var #128 Str)
(var #129 _)
(var #130 _)
(var #131 -> #136)
(var #132 -> #172)
(var #133 _)
(var #134 _)
(var #135 -> #171)
(var #136 _)
(var #137 _)
(var #138 -> #141)
(var #139 -> #173)
(var #140 _)
(var #141 _)
(var #142 _)
(var #143 -> #146)
(var #144 -> #174)
(var #145 F64)
(var #146 _)
(var #147 _)
(var #148 -> #149)
(var #149 -> #150)
(var #150 _)
(var #151 _)
(var #152 -> #175)
(var #153 _)
(var #154 _)
(var #155 _)
(var #156 fn_pure)
(var #157 _)
(var #158 fn_pure)
(var #159 _)
(var #160 _)
(var #161 tuple)
(var #162 fn_pure)
(var #163 fn_pure)
(var #164 _)
(var #165 fn_pure)
(var #166 _)
(var #167 fn_pure)
(var #168 _)
(var #169 fn_pure)
(var #170 fn_pure)
(var #171 tuple)
(var #172 fn_pure)
(var #173 fn_pure)
(var #174 fn_pure)
(var #175 fn_pure)
~~~
# TYPES
~~~roc
b : Str
n : Num(_size)
u : Num(_size)
anotherIdentity : _arg2 -> _ret
f : Num(_size)
y : Num(_size)
a : Num(_size)
g : Num(_size)
o : Num(_size)
second : _ac
r : Num(_size)
first : _ac
identity : _arg2 -> _ret
x : Num(_size)
e : _ac
d : _ac
ab : Num(_size)
aa : Num(_size)
finalIdentity : _arg2 -> _ret
l : Num(_size)
p : Num(_size)
q : Num(_size)
i : Num(_size)
k : Num(_size)
s : Num(_size)
t : Num(_size)
arg : _ac
j : Num(_size)
c : F64
w : Num(_size)
z : Num(_size)
combine : _arg2 -> _arg3 -> (_field, _field2)
h : Num(_size)
main : _arg2 -> _ret
param : _ac
m : Num(_size)
yetAnotherIdentity : _arg2 -> _ret
v : Num(_size)
~~~
