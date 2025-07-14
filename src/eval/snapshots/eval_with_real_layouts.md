# META

description = "Test that eval function uses real layouts from type checker"
type = "expr"

# SOURCE

5 + 3

# EXPECTED

NIL

# TOKENS

0:1 Num("5") 5
1:3 BinOp(Plus) +
3:4 Num("3") 3

# PARSE

e_binop#0:
  op:
    add
  left:
    e_int#0: 5i128
  right:
    e_int#3: 3i128

# CANONICALIZE

e_binop#0:
  op: add
  left: 0 e_int#0
  right: 3 e_int#3
  whole_expr_idx: 0 e_binop#0

# TYPES

e_int#0:
  int_precision: u64

e_int#3:
  int_precision: u64

e_binop#0:
  int_precision: u64

# PROBLEMS

NIL