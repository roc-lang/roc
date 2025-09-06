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
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpAssign Int LowerIdent OpAssign Float LowerIdent OpAssign String LowerIdent OpAssign OpenSquare CloseSquare LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon Int CloseCurly BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpAmpersand LowerIdent OpColon LowerIdent CloseCurly BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma Int CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma String CloseRound BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent CloseCurly BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound OpenSquare Int Comma Int Comma Int CloseSquare CloseRound BlankLine LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent Dot LowerIdent OpPlus LowerIdent Dot LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block)
      )
    )
))
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
      (lc "num")
      (lc "frac")
    )
  )
  (binop_equals
    (lc "make_container")
    (lambda
      (body
        (record_literal
          (binop_colon
            (lc "data")
            (lc "value")
          )
          (binop_colon
            (lc "count")
            (num_literal_i32 1)
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
          (lc "container")
          (malformed)
          (binop_colon
            (lc "data")
            (lc "new_value")
          )
        )
      )
      (args
        (lc "container")
        (lc "new_value")
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
        (num_literal_i32 1)
        (num_literal_i32 2)
        (num_literal_i32 3)
      )
    )
  )
  (binop_equals
    (lc "main")
    (lambda
      (body
        (block
          (binop_plus
            (binop_dot
              (lc "int_container")
              (dot_lc "count")
            )
            (binop_dot
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
app [main] { pf: "../basic-cli/platform.roc" platform [] }

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
update_data = |container, new_value| {
	container
	& 
	data : new_value
}

# Used with different record types
updated_int = update_data((int_container, 100))
updated_str = update_data((str_container, "world"))
# Function returning polymorphic record
identity_record = |x| {
	value : x
}

# Used at different types
int_record = identity_record(42)
str_record = identity_record("test")
list_record = identity_record([1, 2, 3])
main = |_| {
	# Access polymorphic fields
	(int_container..count) + (str_container..count)
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - let_polymorphism_records.md:19:50:19:51
UNRECOGNIZED SYNTAX - let_polymorphism_records.md:19:50:19:51
UNUSED VARIABLE - let_polymorphism_records.md:19:27:19:36
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **& ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**let_polymorphism_records.md:19:50:19:52:**
```roc
update_data = |container, new_value| { container & data: new_value }
```
                                                 ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "num"))
    (Expr.num_literal_i32 42)
  )
  (Stmt.assign
    (pattern (Patt.ident "frac"))
    (Expr.frac_literal_small 4.2)
  )
  (Stmt.assign
    (pattern (Patt.ident "str"))
    (Expr.str_literal_big)
  )
  (Stmt.assign
    (pattern (Patt.ident "my_empty_list"))
    (Expr.list_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "my_nonempty_list"))
    (Expr.list_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "make_container"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "int_container"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "str_container"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "list_container"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "update_data"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "updated_int"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "updated_str"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity_record"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "int_record"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "str_record"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "list_record"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 136
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
(var #10 -> #11)
(var #11 F64)
(var #12 _)
(var #13 -> #14)
(var #14 Str)
(var #15 _)
(var #16 -> #17)
(var #17 _)
(var #18 _)
(var #19 -> #22)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 -> #116)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 Num *)
(var #31 _)
(var #32 -> #115)
(var #33 -> #116)
(var #34 _)
(var #35 -> #38)
(var #36 -> #117)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 -> #43)
(var #41 -> #118)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 -> #48)
(var #46 -> #119)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 -> #123)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 -> #123)
(var #60 _)
(var #61 -> #66)
(var #62 -> #125)
(var #63 _)
(var #64 Num *)
(var #65 -> #124)
(var #66 _)
(var #67 _)
(var #68 -> #73)
(var #69 -> #127)
(var #70 _)
(var #71 Str)
(var #72 -> #126)
(var #73 _)
(var #74 _)
(var #75 -> #130)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 -> #129)
(var #81 -> #130)
(var #82 _)
(var #83 -> #86)
(var #84 -> #131)
(var #85 Num *)
(var #86 _)
(var #87 _)
(var #88 -> #91)
(var #89 -> #132)
(var #90 Str)
(var #91 _)
(var #92 _)
(var #93 -> #99)
(var #94 -> #133)
(var #95 Num *)
(var #96 Num *)
(var #97 Num *)
(var #98 _)
(var #99 _)
(var #100 _)
(var #101 -> #135)
(var #102 _)
(var #103 _)
(var #104 _)
(var #105 -> #108)
(var #106 _)
(var #107 _)
(var #108 -> #109)
(var #109 _)
(var #110 _)
(var #111 -> #135)
(var #112 _)
(var #113 _)
(var #114 _)
(var #115 {})
(var #116 fn_pure)
(var #117 fn_pure)
(var #118 fn_pure)
(var #119 fn_pure)
(var #120 _)
(var #121 _)
(var #122 _)
(var #123 fn_pure)
(var #124 tuple)
(var #125 fn_pure)
(var #126 tuple)
(var #127 fn_pure)
(var #128 _)
(var #129 {})
(var #130 fn_pure)
(var #131 fn_pure)
(var #132 fn_pure)
(var #133 fn_pure)
(var #134 _)
(var #135 fn_pure)
~~~
# TYPES
~~~roc
my_empty_list : _a
identity_record : _arg -> {}
container : _a
data : _a
update_data : _arg, _arg2 -> _ret
updated_int : _a
new_value : _a
num : Num(_size)
int_container : _a
int_record : _a
my_nonempty_list : _a
frac : F64
list_record : _a
str_record : _a
x : _a
make_container : _arg -> {}
updated_str : _a
str : Str
list_container : _a
main : _arg -> _ret
str_container : _a
value : _a
~~~
