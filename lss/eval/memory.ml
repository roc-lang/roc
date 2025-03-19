type memory_cell =
  | Word of int
  | Block of memory_cell list
  | Label of Symbol.symbol

type memory = (string * memory_cell) list
