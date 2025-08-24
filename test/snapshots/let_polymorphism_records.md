# META
~~~ini
description=Let-polymorphism with records
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic values for polymorphism testing
num = 42
frac = 4.2
str = "hello"
my_empty_list = []
my_nonempty_list = [num, frac]

# Record with polymorphic field
make_container = |value| { data: value, count: 1 }

# Used with different types
int_container = make_container(num)
str_container = make_container(str)
list_container = make_container(my_empty_list)

# Polymorphic record update
update_data = |container, new_value| { container & data: new_value }

# Used with different record types
updated_int = update_data(int_container, 100)
updated_str = update_data(str_container, "world")

# Function returning polymorphic record
identity_record = |x| { value: x }

# Used at different types
int_record = identity_record(42)
str_record = identity_record("test")
list_record = identity_record([1, 2, 3])

main = |_| {
    # Access polymorphic fields
    int_container.count + str_container.count
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpAssign Int LowerIdent OpAssign Float LowerIdent OpAssign String LowerIdent OpAssign OpenSquare CloseSquare LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpAmpersand LowerIdent OpColon LowerIdent CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma Int CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma String CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent CloseCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LowerIdent Dot LowerIdent OpPlus LowerIdent Dot LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "num")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "frac")
    (frac_literal_small 4.2)
  )
  (binop_equals
    (lc "str")
    (str_literal_big "hello")
  )
  (binop_equals
    (lc "my_empty_list")
    (list_literal)
  )
  (binop_equals
    (lc "my_nonempty_list")
    (list_literal
      (tuple_literal
        (lc "num")
        (lc "frac")
      )
    )
  )
  (binop_equals
    (lc "make_container")
    (lambda
      (body
        (block
          (binop_colon
            (lc "data")
            (binop_colon
              (tuple_literal
                (lc "value")
                (lc "count")
              )
              (num_literal_i32 1)
            )
          )
        )
      )
      (args
        (lc "value")
      )
    )
  )
  (binop_equals
    (lc "int_container")
    (apply_lc
      (lc "make_container")
      (lc "num")
    )
  )
  (binop_equals
    (lc "str_container")
    (apply_lc
      (lc "make_container")
      (lc "str")
    )
  )
  (binop_equals
    (lc "list_container")
    (apply_lc
      (lc "make_container")
      (lc "my_empty_list")
    )
  )
  (binop_equals
    (lc "update_data")
    (lambda
      (body
        (block
          (binop_colon
            (lc "container")
            (lc "container")
          )
          (malformed malformed:expr_unexpected_token)
          (binop_colon
            (lc "data")
            (lc "new_value")
          )
        )
      )
      (args
        (tuple_literal
          (lc "container")
          (lc "new_value")
        )
      )
    )
  )
  (binop_equals
    (lc "updated_int")
    (apply_lc
      (lc "update_data")
      (tuple_literal
        (lc "int_container")
        (num_literal_i32 100)
      )
    )
  )
  (binop_equals
    (lc "updated_str")
    (apply_lc
      (lc "update_data")
      (tuple_literal
        (lc "str_container")
        (str_literal_big "world")
      )
    )
  )
  (binop_equals
    (lc "identity_record")
    (lambda
      (body
        (block
          (binop_colon
            (lc "value")
            (lc "x")
          )
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_equals
    (lc "int_record")
    (apply_lc
      (lc "identity_record")
      (num_literal_i32 42)
    )
  )
  (binop_equals
    (lc "str_record")
    (apply_lc
      (lc "identity_record")
      (str_literal_small "test")
    )
  )
  (binop_equals
    (lc "list_record")
    (apply_lc
      (lc "identity_record")
      (list_literal
        (tuple_literal
          (num_literal_i32 1)
          (num_literal_i32 2)
          (num_literal_i32 3)
        )
      )
    )
  )
  (binop_equals
    (lc "main")
    (lambda
      (body
        (block
          (binop_plus
            (binop_pipe
              (lc "int_container")
              (dot_lc "count")
            )
            (binop_pipe
              (lc "str_container")
              (dot_lc "count")
            )
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
NO CHANGE
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - let_polymorphism_records.md:19:50:19:51
UNUSED VARIABLE - let_polymorphism_records.md:19:27:19:36
# PROBLEMS
**Parse Error**
at 19:50 to 19:50

**Unsupported Node**
at 7:17 to 7:18

**Unsupported Node**
at 8:20 to 9:1

**Unsupported Node**
at 11:18 to 11:26

**Unsupported Node**
at 19:15 to 19:38

**Unsupported Node**
at 26:19 to 26:23

**Unsupported Node**
at 31:31 to 31:41

**Unsupported Node**
at 33:8 to 33:12

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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
