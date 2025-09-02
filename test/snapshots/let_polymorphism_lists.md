# META
~~~ini
description=Let-polymorphism with lists
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly BlankLine LineComment LowerIdent OpAssign OpenSquare CloseSquare BlankLine LineComment LowerIdent OpAssign OpenSquare Int Comma Int Comma Int CloseSquare LowerIdent OpAssign OpenSquare String Comma String CloseSquare LowerIdent OpAssign OpenSquare Float Comma Float Comma Float CloseSquare BlankLine LineComment LowerIdent OpAssign LowerIdent OpPlus OpPlus LowerIdent LowerIdent OpAssign LowerIdent OpPlus OpPlus LowerIdent LowerIdent OpAssign LowerIdent OpPlus OpPlus LowerIdent BlankLine LineComment LowerIdent OpAssign OpBar Underscore OpBar OpenSquare CloseSquare BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound BlankLine LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
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
	len1 = List.len(all_int_list)
	len2 = List.len(all_str_list)
	len3 = List.len(all_float_list)
	(len1 + len2) + len3
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - let_polymorphism_lists.md:12:26:12:28
UNEXPECTED TOKEN IN EXPRESSION - let_polymorphism_lists.md:13:26:13:28
UNEXPECTED TOKEN IN EXPRESSION - let_polymorphism_lists.md:14:30:14:32
EXPRESSION IN STATEMENT CONTEXT - let_polymorphism_lists.md:12:28:12:41
EXPRESSION IN STATEMENT CONTEXT - let_polymorphism_lists.md:13:28:13:41
EXPRESSION IN STATEMENT CONTEXT - let_polymorphism_lists.md:14:32:14:45
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


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**let_polymorphism_lists.md:12:28:12:41:**
```roc
all_int_list = int_list ++ my_empty_list
```
                           ^^^^^^^^^^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**let_polymorphism_lists.md:13:28:13:41:**
```roc
all_str_list = str_list ++ my_empty_list
```
                           ^^^^^^^^^^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**let_polymorphism_lists.md:14:32:14:45:**
```roc
all_float_list = float_list ++ my_empty_list
```
                               ^^^^^^^^^^^^^


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
  (Stmt.malformed)
  (Stmt.assign
    (pattern (Patt.ident "all_str_list"))
    (Expr.binop_plus
      (Expr.lookup "str_list")
      (Expr.malformed)
    )
  )
  (Stmt.malformed)
  (Stmt.assign
    (pattern (Patt.ident "all_float_list"))
    (Expr.binop_plus
      (Expr.lookup "float_list")
      (Expr.malformed)
    )
  )
  (Stmt.malformed)
  (Stmt.assign
    (pattern (Patt.ident "get_empty"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "empty_int_list"))
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "empty_str_list"))
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
~~~
# TYPES
~~~roc
~~~
