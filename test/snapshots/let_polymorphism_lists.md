# META
~~~ini
description=Let-polymorphism with lists
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic empty list polymorphism
my_empty_list = []

# Empty list used in different contexts
int_list = [1, 2, 3]
str_list = ["hello", "world"]
float_list = [1.1, 2.2, 3.3]

# Append empty list (polymorphic use)
all_int_list = int_list ++ my_empty_list
all_str_list = str_list ++ my_empty_list
all_float_list = float_list ++ my_empty_list

# Function returning empty list
get_empty = |_| []

# Used at different types
empty_int_list = get_empty(42)
empty_str_list = get_empty("test")

main = |_| {
    # Type inference should work correctly
    len1 = List.len(all_int_list)
    len2 = List.len(all_str_list)
    len3 = List.len(all_float_list)
    len1 + len2 + len3
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpAssign OpenSquare CloseSquare BlankLine LineComment LowerIdent OpAssign OpenSquare Int Comma Int Comma Int CloseSquare LowerIdent OpAssign OpenSquare String Comma String CloseSquare LowerIdent OpAssign OpenSquare Float Comma Float Comma Float CloseSquare BlankLine LineComment LowerIdent OpAssign LowerIdent OpPlus OpPlus LowerIdent LowerIdent OpAssign LowerIdent OpPlus OpPlus LowerIdent LowerIdent OpAssign LowerIdent OpPlus OpPlus LowerIdent BlankLine LineComment LowerIdent OpAssign OpBar Underscore OpBar OpenSquare CloseSquare BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound BlankLine LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
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
    (lc "my_empty_list")
    (list_literal)
  )
  (binop_equals
    (lc "int_list")
    (list_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
      (num_literal_i32 3)
    )
  )
  (binop_equals
    (lc "str_list")
    (list_literal
      (str_literal_big "hello")
      (str_literal_big "world")
    )
  )
  (binop_equals
    (lc "float_list")
    (list_literal
      (frac_literal_small 1.1)
      (frac_literal_small 2.2)
      (frac_literal_small 3.3)
    )
  )
  (binop_equals
    (lc "all_int_list")
    (binop_plus
      (lc "int_list")
      (malformed)
    )
  )
  (lc "my_empty_list")
  (binop_equals
    (lc "all_str_list")
    (binop_plus
      (lc "str_list")
      (malformed)
    )
  )
  (lc "my_empty_list")
  (binop_equals
    (lc "all_float_list")
    (binop_plus
      (lc "float_list")
      (malformed)
    )
  )
  (lc "my_empty_list")
  (binop_equals
    (lc "get_empty")
    (lambda
      (body
        (list_literal)
      )
      (args
        (underscore)
      )
    )
  )
  (binop_equals
    (lc "empty_int_list")
    (apply_lc
      (lc "get_empty")
      (num_literal_i32 42)
    )
  )
  (binop_equals
    (lc "empty_str_list")
    (apply_lc
      (lc "get_empty")
      (str_literal_small "test")
    )
  )
  (binop_equals
    (lc "main")
    (lambda
      (body
        (block
          (binop_equals
            (lc "len1")
            (apply_anon
              (binop_dot
                (uc "List")
                (dot_lc "len")
              )
              (lc "all_int_list")
            )
          )
          (binop_equals
            (lc "len2")
            (apply_anon
              (binop_dot
                (uc "List")
                (dot_lc "len")
              )
              (lc "all_str_list")
            )
          )
          (binop_equals
            (lc "len3")
            (apply_anon
              (binop_dot
                (uc "List")
                (dot_lc "len")
              )
              (lc "all_float_list")
            )
          )
          (binop_plus
            (binop_plus
              (lc "len1")
              (lc "len2")
            )
            (lc "len3")
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

# Basic empty list polymorphism
my_empty_list = []
# Empty list used in different contexts
int_list = [1, 2, 3]
str_list = ["hello", "world"]
float_list = [1.1, 2.2, 3.3]
# Append empty list (polymorphic use)
all_int_list = int_list + + 
my_empty_list
all_str_list = str_list + + 
my_empty_list
all_float_list = float_list + + 
my_empty_list
# Function returning empty list
get_empty = |_| []
# Used at different types
empty_int_list = get_empty(42)
empty_str_list = get_empty("test")
main = |_| {
	# Type inference should work correctly
	len1 = List..len(all_int_list)
	len2 = List..len(all_str_list)
	len3 = List..len(all_float_list)
	(len1 + len2) + len3
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - let_polymorphism_lists.md:12:26:12:27
PARSE ERROR - let_polymorphism_lists.md:12:28:12:41
UNEXPECTED TOKEN IN EXPRESSION - let_polymorphism_lists.md:13:26:13:27
PARSE ERROR - let_polymorphism_lists.md:13:28:13:41
UNEXPECTED TOKEN IN EXPRESSION - let_polymorphism_lists.md:14:30:14:31
PARSE ERROR - let_polymorphism_lists.md:14:32:14:45
UNRECOGNIZED SYNTAX - let_polymorphism_lists.md:12:16:12:27
UNRECOGNIZED SYNTAX - let_polymorphism_lists.md:13:16:13:27
UNRECOGNIZED SYNTAX - let_polymorphism_lists.md:14:18:14:31
UNDEFINED VARIABLE - let_polymorphism_lists.md:25:12:25:20
UNDEFINED VARIABLE - let_polymorphism_lists.md:26:12:26:20
UNDEFINED VARIABLE - let_polymorphism_lists.md:27:12:27:20
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **+ ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**let_polymorphism_lists.md:12:26:12:28:**
```roc
all_int_list = int_list ++ my_empty_list
```
                         ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **+ ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**let_polymorphism_lists.md:13:26:13:28:**
```roc
all_str_list = str_list ++ my_empty_list
```
                         ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **+ ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**let_polymorphism_lists.md:14:30:14:32:**
```roc
all_float_list = float_list ++ my_empty_list
```
                             ^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_lists.md:4:1:4:14:**
```roc
my_empty_list = []
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_lists.md:7:1:7:9:**
```roc
int_list = [1, 2, 3]
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_lists.md:8:1:8:9:**
```roc
str_list = ["hello", "world"]
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_lists.md:9:1:9:11:**
```roc
float_list = [1.1, 2.2, 3.3]
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_lists.md:12:1:12:13:**
```roc
all_int_list = int_list ++ my_empty_list
```
^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_lists.md:13:1:13:13:**
```roc
all_str_list = str_list ++ my_empty_list
```
^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_lists.md:14:1:14:15:**
```roc
all_float_list = float_list ++ my_empty_list
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_lists.md:17:1:17:10:**
```roc
get_empty = |_| []
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_lists.md:20:1:20:15:**
```roc
empty_int_list = get_empty(42)
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_lists.md:21:1:21:15:**
```roc
empty_str_list = get_empty("test")
```
^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "my_empty_list"))
    (Expr.list_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "int_list"))
    (Expr.list_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "str_list"))
    (Expr.list_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "float_list"))
    (Expr.list_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "all_int_list"))
    (Expr.binop_plus
      (Expr.lookup "int_list")
      (Expr.malformed)
    )
  )
  (Expr.lookup "my_empty_list")
  (Stmt.assign
    (pattern (Patt.ident "all_str_list"))
    (Expr.binop_plus
      (Expr.lookup "str_list")
      (Expr.malformed)
    )
  )
  (Expr.lookup "my_empty_list")
  (Stmt.assign
    (pattern (Patt.ident "all_float_list"))
    (Expr.binop_plus
      (Expr.lookup "float_list")
      (Expr.malformed)
    )
  )
  (Expr.lookup "my_empty_list")
  (Stmt.assign
    (pattern (Patt.ident "get_empty"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "empty_int_list"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "empty_str_list"))
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
; Total type variables: 111
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 -> #93)
(var #8 -> #93)
(var #9 _)
(var #10 -> #94)
(var #11 Num *)
(var #12 -> #11)
(var #13 -> #11)
(var #14 -> #94)
(var #15 _)
(var #16 -> #95)
(var #17 Str)
(var #18 -> #17)
(var #19 -> #95)
(var #20 _)
(var #21 -> #96)
(var #22 F64)
(var #23 -> #22)
(var #24 -> #22)
(var #25 -> #96)
(var #26 _)
(var #27 -> #30)
(var #28 -> #97)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 -> #36)
(var #34 -> #98)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 -> #42)
(var #40 -> #99)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 -> #103)
(var #46 _)
(var #47 -> #102)
(var #48 -> #103)
(var #49 _)
(var #50 -> #53)
(var #51 -> #104)
(var #52 Num *)
(var #53 _)
(var #54 _)
(var #55 -> #58)
(var #56 -> #105)
(var #57 Str)
(var #58 _)
(var #59 _)
(var #60 -> #110)
(var #61 _)
(var #62 -> #67)
(var #63 _)
(var #64 _)
(var #65 -> #107)
(var #66 _)
(var #67 _)
(var #68 _)
(var #69 -> #74)
(var #70 _)
(var #71 _)
(var #72 -> #108)
(var #73 _)
(var #74 _)
(var #75 _)
(var #76 -> #81)
(var #77 _)
(var #78 _)
(var #79 -> #109)
(var #80 _)
(var #81 _)
(var #82 _)
(var #83 -> #84)
(var #84 -> #85)
(var #85 -> #86)
(var #86 -> #87)
(var #87 _)
(var #88 _)
(var #89 -> #110)
(var #90 _)
(var #91 _)
(var #92 _)
(var #93 List #92)
(var #94 List #11)
(var #95 List #17)
(var #96 List #22)
(var #97 -> #30)
(var #98 -> #36)
(var #99 -> #42)
(var #100 _)
(var #101 _)
(var #102 List #101)
(var #103 fn_pure)
(var #104 fn_pure)
(var #105 fn_pure)
(var #106 _)
(var #107 fn_pure)
(var #108 fn_pure)
(var #109 fn_pure)
(var #110 fn_pure)
~~~
# TYPES
~~~roc
int_list : List(Num(_size))
float_list : List(F64)
get_empty : _arg -> List(_elem)
my_empty_list : List(_elem)
all_str_list : _a
str_list : List(Str)
all_float_list : _a
empty_int_list : _a
empty_str_list : _a
all_int_list : _a
~~~
