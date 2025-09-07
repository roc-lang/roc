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


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:4:1:4:4:**
```roc
num = 42
```
^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:5:1:5:5:**
```roc
frac = 4.2
```
^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:6:1:6:4:**
```roc
str = "hello"
```
^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:7:1:7:14:**
```roc
my_empty_list = []
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:8:1:8:17:**
```roc
my_nonempty_list = [num, frac]
```
^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:11:1:11:15:**
```roc
make_container = |value| { data: value, count: 1 }
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:14:1:14:14:**
```roc
int_container = make_container(num)
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:15:1:15:14:**
```roc
str_container = make_container(str)
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:16:1:16:15:**
```roc
list_container = make_container(my_empty_list)
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:19:1:19:12:**
```roc
update_data = |container, new_value| { container & data: new_value }
```
^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:22:1:22:12:**
```roc
updated_int = update_data(int_container, 100)
```
^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:23:1:23:12:**
```roc
updated_str = update_data(str_container, "world")
```
^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:26:1:26:16:**
```roc
identity_record = |x| { value: x }
```
^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:29:1:29:11:**
```roc
int_record = identity_record(42)
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:30:1:30:11:**
```roc
str_record = identity_record("test")
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_records.md:31:1:31:12:**
```roc
list_record = identity_record([1, 2, 3])
```
^^^^^^^^^^^


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
; Total type variables: 143
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
(var #16 -> #115)
(var #17 -> #115)
(var #18 _)
(var #19 -> #116)
(var #20 _)
(var #21 -> #20)
(var #22 -> #116)
(var #23 _)
(var #24 -> #120)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 Num *)
(var #31 _)
(var #32 -> #119)
(var #33 -> #120)
(var #34 _)
(var #35 -> #38)
(var #36 -> #121)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 -> #43)
(var #41 -> #122)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 -> #48)
(var #46 -> #123)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 -> #128)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 -> #128)
(var #60 _)
(var #61 -> #66)
(var #62 -> #130)
(var #63 _)
(var #64 Num *)
(var #65 -> #129)
(var #66 _)
(var #67 _)
(var #68 -> #73)
(var #69 -> #132)
(var #70 _)
(var #71 Str)
(var #72 -> #131)
(var #73 _)
(var #74 _)
(var #75 -> #136)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 -> #135)
(var #81 -> #136)
(var #82 _)
(var #83 -> #86)
(var #84 -> #137)
(var #85 Num *)
(var #86 _)
(var #87 _)
(var #88 -> #91)
(var #89 -> #138)
(var #90 Str)
(var #91 _)
(var #92 _)
(var #93 -> #99)
(var #94 -> #140)
(var #95 Num *)
(var #96 -> #95)
(var #97 -> #95)
(var #98 -> #139)
(var #99 _)
(var #100 _)
(var #101 -> #142)
(var #102 _)
(var #103 _)
(var #104 _)
(var #105 -> #108)
(var #106 _)
(var #107 _)
(var #108 -> #109)
(var #109 _)
(var #110 _)
(var #111 -> #142)
(var #112 _)
(var #113 _)
(var #114 _)
(var #115 List #114)
(var #116 List #20)
(var #117 _)
(var #118 {})
(var #119 record)
(var #120 fn_pure)
(var #121 fn_pure)
(var #122 fn_pure)
(var #123 fn_pure)
(var #124 _)
(var #125 _)
(var #126 _)
(var #127 fn_pure)
(var #128 fn_pure)
(var #129 tuple)
(var #130 fn_pure)
(var #131 tuple)
(var #132 fn_pure)
(var #133 _)
(var #134 {})
(var #135 record)
(var #136 fn_pure)
(var #137 fn_pure)
(var #138 fn_pure)
(var #139 List #95)
(var #140 fn_pure)
(var #141 _)
(var #142 fn_pure)
~~~
# TYPES
~~~roc
my_empty_list : List(_elem)
identity_record : _arg -> { value: _field }
data : _a
update_data : _arg -> _arg2 -> _ret
updated_int : _a
new_value : _a
num : Num(_size)
int_container : _a
int_record : _a
my_nonempty_list : List(_elem)
frac : F64
list_record : _a
str_record : _a
x : _a
make_container : _arg -> { data: _field, count: Num(_size) }
updated_str : _a
str : Str
list_container : _a
value : _a
str_container : _a
container : _a
~~~
