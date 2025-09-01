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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly BlankLine LineComment LowerIdent OpAssign Int LowerIdent OpAssign Float LowerIdent OpAssign String LowerIdent OpAssign OpenSquare CloseSquare LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon Int CloseCurly BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpAmpersand LowerIdent OpColon LowerIdent CloseCurly BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma Int CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma String CloseRound BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent CloseCurly BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound OpenSquare Int Comma Int Comma Int CloseSquare CloseRound BlankLine LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent Dot LowerIdent OpPlus LowerIdent Dot LowerIdent CloseCurly ~~~
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

# Basic values for polymorphism testing
num = 42
frac = 4.2
str = "hello"
my_empty_list = []
my_nonempty_list = [num, frac]

# Record with polymorphic field
make_container = |value| { data : value, count : 1 }

# Used with different types
int_container = make_container(num)
str_container = make_container(str)
list_container = make_container(my_empty_list)

# Polymorphic record update
update_data = |container, new_value| {
	container : container
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
	int_container.count + str_container.count
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **& ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**let_polymorphism_records.md:19:50:19:52:**
```roc
update_data = |container, new_value| { container & data: new_value }
```
                                                 ^^


**UNUSED VARIABLE**
Variable **data** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_data` to suppress this warning.
The unused variable is declared here:

**let_polymorphism_records.md:19:52:19:56:**
```roc
update_data = |container, new_value| { container & data: new_value }
```
                                                   ^^^^


**UNUSED VARIABLE**
Variable **container** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_container` to suppress this warning.
The unused variable is declared here:

**let_polymorphism_records.md:19:40:19:49:**
```roc
update_data = |container, new_value| { container & data: new_value }
```
                                       ^^^^^^^^^


**UNUSED VARIABLE**
Variable **new_value** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_new_value` to suppress this warning.
The unused variable is declared here:

**let_polymorphism_records.md:19:27:19:36:**
```roc
update_data = |container, new_value| { container & data: new_value }
```
                          ^^^^^^^^^


**UNUSED VARIABLE**
Variable **container** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_container` to suppress this warning.
The unused variable is declared here:

**let_polymorphism_records.md:19:16:19:25:**
```roc
update_data = |container, new_value| { container & data: new_value }
```
               ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **value** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_records.md:26:25:26:30:**
```roc
identity_record = |x| { value: x }
```
                        ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**let_polymorphism_records.md:35:5:35:24:**
```roc
    int_container.count + str_container.count
```
    ^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**let_polymorphism_records.md:35:27:35:46:**
```roc
    int_container.count + str_container.count
```
                          ^^^^^^^^^^^^^^^^^^^


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
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "str_container"))
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "list_container"))
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "update_data"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "updated_int"))
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "updated_str"))
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity_record"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "int_record"))
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "str_record"))
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "list_record"))
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
