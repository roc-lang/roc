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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly LowerIdent OpAssign OpenSquare CloseSquare LowerIdent OpAssign OpenSquare Int Comma Int Comma Int CloseSquare LowerIdent OpAssign OpenSquare String Comma String CloseSquare LowerIdent OpAssign OpenSquare Float Comma Float Comma Float CloseSquare LowerIdent OpAssign LowerIdent OpPlus OpPlus LowerIdent LowerIdent OpAssign LowerIdent OpPlus OpPlus LowerIdent LowerIdent OpAssign LowerIdent OpPlus OpPlus LowerIdent LowerIdent OpAssign OpBar Underscore OpBar OpenSquare CloseSquare LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
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
      (malformed malformed:expr_unexpected_token)
    )
  )
  (lc "my_empty_list")
  (binop_equals
    (lc "all_str_list")
    (binop_plus
      (lc "str_list")
      (malformed malformed:expr_unexpected_token)
    )
  )
  (lc "my_empty_list")
  (binop_equals
    (lc "all_float_list")
    (binop_plus
      (lc "float_list")
      (malformed malformed:expr_unexpected_token)
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
              (binop_pipe
                (uc "List")
                (dot_lc "len")
              )
              (lc "all_int_list")
            )
          )
          (binop_equals
            (lc "len2")
            (apply_anon
              (binop_pipe
                (uc "List")
                (dot_lc "len")
              )
              (lc "all_str_list")
            )
          )
          (binop_equals
            (lc "len3")
            (apply_anon
              (binop_pipe
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
app { pf: "../basic-cli/platform.roc" platform [main] }

my_empty_list = []
int_list = [1, 2, 3]
str_list = ["hello", "world"]
float_list = [1.1, 2.2, 3.3]
all_int_list = int_list + + 
my_empty_list
all_str_list = str_list + + 
my_empty_list
all_float_list = float_list + + 
my_empty_list
get_empty = |_| []
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
NIL
# PROBLEMS
**Parse Error**
at 12:26 to 12:28

**Parse Error**
at 13:26 to 13:28

**Parse Error**
at 14:30 to 14:32

**Unsupported Node**
at 25:12 to 25:16

**Unsupported Node**
at 26:12 to 26:16

**Unsupported Node**
at 27:12 to 27:16

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "my_empty_list")
    (Expr.list_literal)
  )
  (Expr.binop_equals
    (Expr.lookup "int_list")
    (Expr.list_literal)
  )
  (Expr.binop_equals
    (Expr.lookup "str_list")
    (Expr.list_literal)
  )
  (Expr.binop_equals
    (Expr.lookup "float_list")
    (Expr.list_literal)
  )
  (Expr.binop_equals
    (Expr.lookup "all_int_list")
    (Expr.binop_plus
      (Expr.lookup "int_list")
      (Expr.malformed)
    )
  )
  (Expr.lookup "my_empty_list")
  (Expr.binop_equals
    (Expr.lookup "all_str_list")
    (Expr.binop_plus
      (Expr.lookup "str_list")
      (Expr.malformed)
    )
  )
  (Expr.lookup "my_empty_list")
  (Expr.binop_equals
    (Expr.lookup "all_float_list")
    (Expr.binop_plus
      (Expr.lookup "float_list")
      (Expr.malformed)
    )
  )
  (Expr.lookup "my_empty_list")
  (Expr.binop_equals
    (Expr.lookup "get_empty")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "empty_int_list")
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "empty_str_list")
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
my_empty_list : List(_elem)
int_list : List(_elem)
str_list : List(_elem)
float_list : List(_elem)
all_int_list : Num(_size)
all_str_list : Num(_size)
all_float_list : Num(_size)
get_empty : _a
empty_int_list : _a
empty_str_list : _a
main : _a
~~~
