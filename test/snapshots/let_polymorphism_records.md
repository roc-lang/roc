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


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_records.md:11:28:11:39:**
```roc
make_container = |value| { data: value, count: 1 }
```
                           ^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_records.md:11:41:11:49:**
```roc
make_container = |value| { data: value, count: 1 }
```
                                        ^^^^^^^^


**UNUSED VARIABLE**
Variable **value** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_value` to suppress this warning.
The unused variable is declared here:

**let_polymorphism_records.md:11:19:11:24:**
```roc
make_container = |value| { data: value, count: 1 }
```
                  ^^^^^


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


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_records.md:26:25:26:33:**
```roc
identity_record = |x| { value: x }
```
                        ^^^^^^^^


**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**let_polymorphism_records.md:26:20:26:21:**
```roc
identity_record = |x| { value: x }
```
                   ^


**UNUSED VARIABLE**
Variable **int_container** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_int_container` to suppress this warning.
The unused variable is declared here:

**let_polymorphism_records.md:35:5:35:18:**
```roc
    int_container.count + str_container.count
```
    ^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **str_container** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_str_container` to suppress this warning.
The unused variable is declared here:

**let_polymorphism_records.md:35:27:35:40:**
```roc
    int_container.count + str_container.count
```
                          ^^^^^^^^^^^^^


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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
