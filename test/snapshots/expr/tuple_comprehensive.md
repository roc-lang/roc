# META
~~~ini
description=Comprehensive tuple expression tests
type=expr
~~~
# SOURCE
~~~roc
{
    # define these to avoid runtime errors
    add_one = |_| {}
    x = 10
    y = 20
    z = 30

    # example tuples
	empty = ()
	single = (42)
	pair = (1, 2)
	triple = (1, "hello", True)
	nested = ((1, 2), (3, 4))
	mixed = (add_one(5), "world", [1, 2, 3])
	with_vars = (x, y, z)
	with_lambda = (|n| n + 1, 42)

	empty
}
~~~
# TOKENS
~~~text
OpenCurly LineComment LowerIdent OpAssign OpBar Underscore OpBar OpenCurly CloseCurly LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign OpenRound CloseRound LowerIdent OpAssign OpenRound Int CloseRound LowerIdent OpAssign OpenRound Int Comma Int CloseRound LowerIdent OpAssign OpenRound Int Comma String Comma UpperIdent CloseRound LowerIdent OpAssign OpenRound OpenRound Int Comma Int CloseRound Comma OpenRound Int Comma Int CloseRound CloseRound LowerIdent OpAssign OpenRound LowerIdent OpenRound Int CloseRound Comma String Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpAssign OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpenRound OpBar LowerIdent OpBar LowerIdent OpPlus Int Comma Int CloseRound BlankLine LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
  (binop_equals
    (lc "add_one")
    (lambda
      (body
        (record_literal)
      )
      (args
        (underscore)
      )
    )
  )
  (binop_equals
    (lc "x")
    (num_literal_i32 10)
  )
  (binop_equals
    (lc "y")
    (num_literal_i32 20)
  )
  (binop_equals
    (lc "z")
    (num_literal_i32 30)
  )
  (binop_equals
    (lc "empty")
    (tuple_literal)
  )
  (binop_equals
    (lc "single")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "pair")
    (tuple_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
    )
  )
  (binop_equals
    (lc "triple")
    (tuple_literal
      (num_literal_i32 1)
      (str_literal_big "hello")
      (uc "True")
    )
  )
  (binop_equals
    (lc "nested")
    (tuple_literal
      (tuple_literal
        (num_literal_i32 1)
        (num_literal_i32 2)
      )
      (tuple_literal
        (num_literal_i32 3)
        (num_literal_i32 4)
      )
    )
  )
  (binop_equals
    (lc "mixed")
    (tuple_literal
      (apply_lc
        (lc "add_one")
        (num_literal_i32 5)
      )
      (str_literal_big "world")
      (list_literal
        (num_literal_i32 1)
        (num_literal_i32 2)
        (num_literal_i32 3)
      )
    )
  )
  (binop_equals
    (lc "with_vars")
    (tuple_literal
      (lc "x")
      (lc "y")
      (lc "z")
    )
  )
  (binop_equals
    (lc "with_lambda")
    (lambda
      (body
        (tuple_literal
          (binop_plus
            (lc "n")
            (num_literal_i32 1)
          )
          (num_literal_i32 42)
        )
      )
      (args
        (lc "n")
      )
    )
  )
  (lc "empty")
)
~~~
# FORMATTED
~~~roc
# define these to avoid runtime errors
add_one = |_| {}
x = 10
y = 20
z = 30
# example tuples
empty = ()
single = 42
pair = (1, 2)
triple = (1, "hello", True)
nested = ((1, 2), (3, 4))
mixed = (add_one(5), "world", [1, 2, 3])
with_vars = (x, y, z)
with_lambda = |n| (n + 1, 42)
empty
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **# define these to avoid runtime errors
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**tuple_comprehensive.md:2:5:3:5:**
```roc
    # define these to avoid runtime errors
    add_one = |_| {}
```


**UNUSED VARIABLE**
Variable **pair** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_pair` to suppress this warning.
The unused variable is declared here:

**tuple_comprehensive.md:11:2:11:6:**
```roc
	pair = (1, 2)
```
	^^^^


**UNUSED VARIABLE**
Variable **with_vars** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_with_vars` to suppress this warning.
The unused variable is declared here:

**tuple_comprehensive.md:15:2:15:11:**
```roc
	with_vars = (x, y, z)
```
	^^^^^^^^^


**UNUSED VARIABLE**
Variable **triple** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_triple` to suppress this warning.
The unused variable is declared here:

**tuple_comprehensive.md:12:2:12:8:**
```roc
	triple = (1, "hello", True)
```
	^^^^^^


**UNUSED VARIABLE**
Variable **single** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_single` to suppress this warning.
The unused variable is declared here:

**tuple_comprehensive.md:10:2:10:8:**
```roc
	single = (42)
```
	^^^^^^


**UNUSED VARIABLE**
Variable **nested** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_nested` to suppress this warning.
The unused variable is declared here:

**tuple_comprehensive.md:13:2:13:8:**
```roc
	nested = ((1, 2), (3, 4))
```
	^^^^^^


**UNUSED VARIABLE**
Variable **with_lambda** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_with_lambda` to suppress this warning.
The unused variable is declared here:

**tuple_comprehensive.md:16:2:16:13:**
```roc
	with_lambda = (|n| n + 1, 42)
```
	^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **mixed** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_mixed` to suppress this warning.
The unused variable is declared here:

**tuple_comprehensive.md:14:2:14:7:**
```roc
	mixed = (add_one(5), "world", [1, 2, 3])
```
	^^^^^


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
  (Expr.lookup "empty")
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
add_one : _a
x : Num(_size)
y : Num(_size)
z : Num(_size)
empty : _a
single : Num(_size)
pair : _a
triple : _a
nested : _a
mixed : _a
with_vars : _a
with_lambda : _a
~~~
