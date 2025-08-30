# META
~~~ini
description=Let-polymorphism with records
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly LowerIdent OpAssign Int LowerIdent OpAssign Float LowerIdent OpAssign String LowerIdent OpAssign OpenSquare CloseSquare LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpAmpersand LowerIdent OpColon LowerIdent CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma Int CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma String CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent CloseCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LowerIdent Dot LowerIdent OpPlus LowerIdent Dot LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

num = 42
frac = 4.2
str = "hello"
my_empty_list = []
my_nonempty_list = [num, frac]
make_container = |value| { data : value, count : 1 }
int_container = make_container(num)
str_container = make_container(str)
list_container = make_container(my_empty_list)
update_data = |container, new_value| {
	container : container
	& 
	data : new_value
}
updated_int = update_data((int_container, 100))
updated_str = update_data((str_container, "world"))
identity_record = |x| {
	value : x
}
int_record = identity_record(42)
str_record = identity_record("test")
list_record = identity_record([1, 2, 3])
main = |_| {
	int_container.count + str_container.count
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 19:50 to 19:52

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "num")
    (Expr.num_literal_i32 42)
  )
  (Expr.binop_equals
    (Expr.lookup "frac")
    (Expr.frac_literal_small 4.2)
  )
  (Expr.binop_equals
    (Expr.lookup "str")
    (Expr.str_literal_big)
  )
  (Expr.binop_equals
    (Expr.lookup "my_empty_list")
    (Expr.list_literal)
  )
  (Expr.binop_equals
    (Expr.lookup "my_nonempty_list")
    (Expr.list_literal)
  )
  (Expr.binop_equals
    (Expr.lookup "make_container")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "int_container")
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "str_container")
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "list_container")
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "update_data")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "updated_int")
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "updated_str")
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "identity_record")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "int_record")
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "str_record")
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "list_record")
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "main")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
num : Num(_size)
frac : F64
str : Str
my_empty_list : List(_elem)
my_nonempty_list : List(_elem)
make_container : _a
int_container : _a
str_container : _a
list_container : _a
update_data : _a
updated_int : _a
updated_str : _a
identity_record : _a
int_record : _a
str_record : _a
list_record : _a
main : _a
~~~
