# META
~~~ini
description=Block with multiple assignments of different types
type=expr
~~~
# SOURCE
~~~roc
{
    x = 42
    name = "Alice"
    pi = 3.14159
    isActive = Bool.true
    add = |a, b| a + b
    pair = (1, "hello")
    record = { age: 30, city: "NYC" }
    list = [1, 2, 3]
    y = x + 10
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign String LowerIdent OpAssign Float LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpPlus LowerIdent LowerIdent OpAssign OpenRound Int Comma String CloseRound LowerIdent OpAssign OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon String CloseCurly LowerIdent OpAssign OpenSquare Int Comma Int Comma Int CloseSquare LowerIdent OpAssign LowerIdent OpPlus Int CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "name")
    (str_literal_big "Alice")
  )
  (binop_equals
    (lc "pi")
    (frac_literal_big big:<idx:30>)
  )
  (binop_equals
    (lc "isActive")
    (binop_pipe
      (uc "Bool")
      (dot_lc "true")
    )
  )
  (binop_equals
    (lc "add")
    (lambda
      (body
        (binop_plus
          (lc "a")
          (lc "b")
        )
      )
      (args
        (lc "a")
        (lc "b")
      )
    )
  )
  (binop_equals
    (lc "pair")
    (tuple_literal
      (num_literal_i32 1)
      (str_literal_big "hello")
    )
  )
  (binop_equals
    (lc "record")
    (record_literal
      (binop_colon
        (lc "age")
        (num_literal_i32 30)
      )
      (binop_colon
        (lc "city")
        (str_literal_small "NYC")
      )
    )
  )
  (binop_equals
    (lc "list")
    (list_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
      (num_literal_i32 3)
    )
  )
  (binop_equals
    (lc "y")
    (binop_plus
      (lc "x")
      (num_literal_i32 10)
    )
  )
)
~~~
# FORMATTED
~~~roc
x = 42
name = "Alice"
pi = 3.14159
isActive = Bool.true
add = |a, b| a + b
pair = (1, "hello")
record = { age : 30, city : "NYC" }
list = [1, 2, 3]
y = x + 10
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **age** in this scope.
Is there an **import** or **exposing** missing up-top?

**block_with_assignments.md:8:16:8:19:**
```roc
    record = { age: 30, city: "NYC" }
```
               ^^^


**UNDEFINED VARIABLE**
Nothing is named **city** in this scope.
Is there an **import** or **exposing** missing up-top?

**block_with_assignments.md:8:25:8:29:**
```roc
    record = { age: 30, city: "NYC" }
```
                        ^^^^


**UNUSED VARIABLE**
Variable **record** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_record` to suppress this warning.
The unused variable is declared here:

**block_with_assignments.md:8:5:8:11:**
```roc
    record = { age: 30, city: "NYC" }
```
    ^^^^^^


**UNUSED VARIABLE**
Variable **isActive** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_isActive` to suppress this warning.
The unused variable is declared here:

**block_with_assignments.md:5:5:5:13:**
```roc
    isActive = Bool.true
```
    ^^^^^^^^


**UNUSED VARIABLE**
Variable **add** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_add` to suppress this warning.
The unused variable is declared here:

**block_with_assignments.md:6:5:6:8:**
```roc
    add = |a, b| a + b
```
    ^^^


**UNUSED VARIABLE**
Variable **pair** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_pair` to suppress this warning.
The unused variable is declared here:

**block_with_assignments.md:7:5:7:9:**
```roc
    pair = (1, "hello")
```
    ^^^^


**UNUSED VARIABLE**
Variable **y** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:

**block_with_assignments.md:10:5:10:6:**
```roc
    y = x + 10
```
    ^


**UNUSED VARIABLE**
Variable **list** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:

**block_with_assignments.md:9:5:9:9:**
```roc
    list = [1, 2, 3]
```
    ^^^^


**UNUSED VARIABLE**
Variable **name** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_name` to suppress this warning.
The unused variable is declared here:

**block_with_assignments.md:3:5:3:9:**
```roc
    name = "Alice"
```
    ^^^^


**UNUSED VARIABLE**
Variable **pi** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_pi` to suppress this warning.
The unused variable is declared here:

**block_with_assignments.md:4:5:4:7:**
```roc
    pi = 3.14159
```
    ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.num_literal_i32 42)
  )
  (Stmt.assign
    (pattern (Patt.ident "name"))
    (Expr.str_literal_big)
  )
  (Stmt.assign
    (pattern (Patt.ident "pi"))
    (Expr.frac_literal_big big:<idx:30>)
  )
  (Stmt.assign
    (pattern (Patt.ident "isActive"))
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "add"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "pair"))
    (Expr.tuple_literal
      (Expr.num_literal_i32 1)
      (Expr.str_literal_big)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "record"))
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "age")
        (Expr.num_literal_i32 30)
      )
      (Expr.binop_colon
        (Expr.lookup "city")
        (Expr.str_literal_small)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "list"))
    (Expr.list_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "y"))
    (Expr.binop_plus
      (Expr.lookup "x")
      (Expr.num_literal_i32 10)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
x : Num(_size)
name : Str
pi : F64
isActive : _c
add : _c
pair : _c
record : {}
list : List(_elem)
y : Num(_size)
~~~
