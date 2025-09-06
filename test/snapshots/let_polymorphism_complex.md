# META
~~~ini
description=Complex let-polymorphism interactions
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic polymorphic values
num = 42
frac = 4.2
str = "hello"
bool = True

# Polymorphic empty collections
empty_list = []
empty_record = {}

# Using empty list in multiple contexts
int_list = [1, 2, 3]
str_list = ["a", "b", "c"]
bool_list = [True, False]

# Nested empty lists
nested_empty = [empty_list, empty_list, empty_list]
mixed_nested = [empty_list, [1, 2], empty_list, [3, 4]]

# Polymorphic record with empty list
poly_record = { items: empty_list, count: 0 }
use_poly_record1 = { items: [1, 2, 3], count: 0 }
use_poly_record2 = { items: ["x", "y", "z"], count: 0 }

# Complex nested structure with multiple polymorphic uses
base_config = {
    data: empty_list,
    metadata: {
        version: num,
        ratio: frac,
        description: str,
    },
}

# Different instantiations of base_config
config1 = {
    data: [1, 2, 3, 4, 5],
    metadata: {
        version: num,
        ratio: frac,
        description: str,
    },
    name: "integers",
}

config2 = { # Test comment 1
    data: ["apple", "banana", "cherry"], # Test comment 2
    metadata: { # Test comment 3
        version: num, # Test comment 4
        ratio: frac, # Test comment 5
        description: str, # Test comment 6
    }, # Test comment 7
    name: "fruits", # Test comment 8
} # Test comment 9

# Polymorphic function-like structures
make_container = |val| { value: val, wrapper: [val] }
container1 = make_container(num)
container2 = make_container(str)
container3 = make_container(frac)

# Deeply nested polymorphism
deep = {
    level1: {
        level2: {
            level3: {
                data: empty_list,
                value: num,
            },
            items: [num, num * 2, num * 3],
        },
        collection: empty_list,
    },
    results: [
        { data: [1], tag: "single" },
        { data: [1, 2], tag: "ints" },
        { data: [1, 2, 3], tag: "more" },
    ],
}

# Polymorphic values used in computations
compute1 = num + 10
compute2 = num * 2
compute3 = [num, num]
compute4 = { base: num, derived: [num, num + 1, num + 2] }

# Mixed polymorphic structures
mixed = {
    numbers: { value: num, list: [num, num], float: frac },
    strings: { value: str, list: [str, str] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list },
    },
    computations: {
        from_num: num * 100,
        from_frac: frac * 10.0,
        list_from_num: [num, num, num],
    },
}

main = |_| {
    # Just type-check everything
    container1.value + 10
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpAssign Int LowerIdent OpAssign Float LowerIdent OpAssign String LowerIdent OpAssign UpperIdent BlankLine LineComment LowerIdent OpAssign OpenSquare CloseSquare LowerIdent OpAssign OpenCurly CloseCurly BlankLine LineComment LowerIdent OpAssign OpenSquare Int Comma Int Comma Int CloseSquare LowerIdent OpAssign OpenSquare String Comma String Comma String CloseSquare LowerIdent OpAssign OpenSquare UpperIdent Comma UpperIdent CloseSquare BlankLine LineComment LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign OpenSquare LowerIdent Comma OpenSquare Int Comma Int CloseSquare Comma LowerIdent Comma OpenSquare Int Comma Int CloseSquare CloseSquare BlankLine LineComment LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenSquare Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenSquare String Comma String Comma String CloseSquare Comma LowerIdent OpColon Int CloseCurly BlankLine LineComment LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma CloseCurly BlankLine LineComment LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenSquare Int Comma Int Comma Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma LowerIdent OpColon String Comma CloseCurly BlankLine LowerIdent OpAssign OpenCurly LineComment LowerIdent OpColon OpenSquare String Comma String Comma String CloseSquare Comma LineComment LowerIdent OpColon OpenCurly LineComment LowerIdent OpColon LowerIdent Comma LineComment LowerIdent OpColon LowerIdent Comma LineComment LowerIdent OpColon LowerIdent Comma LineComment CloseCurly Comma LineComment LowerIdent OpColon String Comma LineComment CloseCurly LineComment BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent CloseSquare CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound BlankLine LineComment LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent OpStar Int Comma LowerIdent OpStar Int CloseSquare Comma CloseCurly Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma LowerIdent OpColon OpenSquare OpenCurly LowerIdent OpColon OpenSquare Int CloseSquare Comma LowerIdent OpColon String CloseCurly Comma OpenCurly LowerIdent OpColon OpenSquare Int Comma Int CloseSquare Comma LowerIdent OpColon String CloseCurly Comma OpenCurly LowerIdent OpColon OpenSquare Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon String CloseCurly Comma CloseSquare Comma CloseCurly BlankLine LineComment LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent OpAssign LowerIdent OpStar Int LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent OpPlus Int Comma LowerIdent OpPlus Int CloseSquare CloseCurly BlankLine LineComment LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent CloseSquare Comma LowerIdent OpColon LowerIdent CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent CloseSquare CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent CloseSquare Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent CloseCurly Comma CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent OpStar Int Comma LowerIdent OpColon LowerIdent OpStar Float Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare Comma CloseCurly Comma CloseCurly BlankLine LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent Dot LowerIdent OpPlus Int CloseCurly ~~~
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
~~~
# FORMATTED
~~~roc
app [main] { pf: "../basic-cli/platform.roc" platform [] }

# Basic polymorphic values
num = 42
frac = 4.2
str = "hello"
bool = True

# Polymorphic empty collections
empty_list = []
empty_record = {}

# Using empty list in multiple contexts
int_list = [1, 2, 3]
str_list = ["a", "b", "c"]
bool_list = [True, False]

# Nested empty lists
nested_empty = [empty_list, empty_list, empty_list]
mixed_nested = [empty_list, [1, 2], empty_list, [3, 4]]

# Polymorphic record with empty list
poly_record = { items: empty_list, count: 0 }
use_poly_record1 = { items: [1, 2, 3], count: 0 }
use_poly_record2 = { items: ["x", "y", "z"], count: 0 }

# Complex nested structure with multiple polymorphic uses
base_config = {
	data: empty_list,
	metadata: {
		version: num,
		ratio: frac,
		description: str,
	},
}

# Different instantiations of base_config
config1 = { data: [1, 2, 3, 4, 5], metadata: {
	version: num,
	ratio: frac,
	description: str,
}, name: "integers" }

config2 = { # Test comment 1
	data: ["apple", "banana", "cherry"], # Test comment 2
	metadata: { # Test comment 3
		version: num, # Test comment 4
		ratio: frac, # Test comment 5
		description: str, # Test comment 6
	}, # Test comment 7
	name: "fruits", # Test comment 8
}

# Test comment 9

# Polymorphic function-like structures
make_container = |val| { value: val, wrapper: [val] }
container1 = make_container(num)
container2 = make_container(str)
container3 = make_container(frac)

# Deeply nested polymorphism
deep = {
	level1: {
		level2: {
			level3: {
				data: empty_list,
				value: num,
			},
			items: [num, num * 2, num * 3],
		},
		collection: empty_list,
	},
	results: [{ data: [1], tag: "single" }, { data: [1, 2], tag: "ints" }, { data: [1, 2, 3], tag: "more" }],
}

# Polymorphic values used in computations
compute1 = num + 10
compute2 = num * 2
compute3 = [num, num]
compute4 = { base: num, derived: [num, num + 1, num + 2] }

# Mixed polymorphic structures
mixed = { numbers: { value: num, list: [num, num], float: frac }, strings: { value: str, list: [str, str] }, empty_lists: { raw: empty_list, in_list: [empty_list], in_record: {
	data : empty_list
} }, computations: { from_num: num * 100, from_frac: frac * 10.0, list_from_num: [num, num, num] } }

main = |_| {
	# Just type-check everything
	container1.value + 10
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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
    (pattern (Patt.ident "bool"))
    (Expr.lookup "True")
  )
  (Stmt.assign
    (pattern (Patt.ident "empty_list"))
    (Expr.list_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "empty_record"))
    (Expr.record_literal
    )
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
    (pattern (Patt.ident "bool_list"))
    (Expr.list_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "nested_empty"))
    (Expr.list_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "mixed_nested"))
    (Expr.list_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "poly_record"))
    (Expr.record_literal
      (Expr.binop_colon
        (lc "items")
        (Expr.lookup "empty_list")
      )
      (Expr.binop_colon
        (lc "count")
        (Expr.num_literal_i32 0)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "use_poly_record1"))
    (Expr.record_literal
      (Expr.binop_colon
        (lc "items")
        (Expr.list_literal)
      )
      (Expr.binop_colon
        (lc "count")
        (Expr.num_literal_i32 0)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "use_poly_record2"))
    (Expr.record_literal
      (Expr.binop_colon
        (lc "items")
        (Expr.list_literal)
      )
      (Expr.binop_colon
        (lc "count")
        (Expr.num_literal_i32 0)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "base_config"))
    (Expr.record_literal
      (Expr.binop_colon
        (lc "data")
        (Expr.lookup "empty_list")
      )
      (Expr.binop_colon
        (lc "metadata")
        (Expr.record_literal
          (Expr.binop_colon
            (lc "version")
            (Expr.lookup "num")
          )
          (Expr.binop_colon
            (lc "ratio")
            (Expr.lookup "frac")
          )
          (Expr.binop_colon
            (lc "description")
            (Expr.lookup "str")
          )
        )
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "config1"))
    (Expr.record_literal
      (Expr.binop_colon
        (lc "data")
        (Expr.list_literal)
      )
      (Expr.binop_colon
        (lc "metadata")
        (Expr.record_literal
          (Expr.binop_colon
            (lc "version")
            (Expr.lookup "num")
          )
          (Expr.binop_colon
            (lc "ratio")
            (Expr.lookup "frac")
          )
          (Expr.binop_colon
            (lc "description")
            (Expr.lookup "str")
          )
        )
      )
      (Expr.binop_colon
        (lc "name")
        (Expr.str_literal_big)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "config2"))
    (Expr.record_literal
      (Expr.binop_colon
        (lc "data")
        (Expr.list_literal)
      )
      (Expr.binop_colon
        (lc "metadata")
        (Expr.record_literal
          (Expr.binop_colon
            (lc "version")
            (Expr.lookup "num")
          )
          (Expr.binop_colon
            (lc "ratio")
            (Expr.lookup "frac")
          )
          (Expr.binop_colon
            (lc "description")
            (Expr.lookup "str")
          )
        )
      )
      (Expr.binop_colon
        (lc "name")
        (Expr.str_literal_big)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "make_container"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "container1"))
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "container2"))
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "container3"))
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "deep"))
    (Expr.record_literal
      (Expr.binop_colon
        (lc "level1")
        (Expr.record_literal
          (Expr.binop_colon
            (lc "level2")
            (Expr.record_literal
              (Expr.binop_colon
                (lc "level3")
                (Expr.record_literal
                  (Expr.binop_colon
                    (lc "data")
                    (Expr.lookup "empty_list")
                  )
                  (Expr.binop_colon
                    (lc "value")
                    (Expr.lookup "num")
                  )
                )
              )
              (Expr.binop_colon
                (lc "items")
                (Expr.list_literal)
              )
            )
          )
          (Expr.binop_colon
            (lc "collection")
            (Expr.lookup "empty_list")
          )
        )
      )
      (Expr.binop_colon
        (lc "results")
        (Expr.list_literal)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "compute1"))
    (Expr.binop_plus
      (Expr.lookup "num")
      (Expr.num_literal_i32 10)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "compute2"))
    (Expr.binop_star
      (Expr.lookup "num")
      (Expr.num_literal_i32 2)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "compute3"))
    (Expr.list_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "compute4"))
    (Expr.record_literal
      (Expr.binop_colon
        (lc "base")
        (Expr.lookup "num")
      )
      (Expr.binop_colon
        (lc "derived")
        (Expr.list_literal)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "mixed"))
    (Expr.record_literal
      (Expr.binop_colon
        (lc "numbers")
        (Expr.record_literal
          (Expr.binop_colon
            (lc "value")
            (Expr.lookup "num")
          )
          (Expr.binop_colon
            (lc "list")
            (Expr.list_literal)
          )
          (Expr.binop_colon
            (lc "float")
            (Expr.lookup "frac")
          )
        )
      )
      (Expr.binop_colon
        (lc "strings")
        (Expr.record_literal
          (Expr.binop_colon
            (lc "value")
            (Expr.lookup "str")
          )
          (Expr.binop_colon
            (lc "list")
            (Expr.list_literal)
          )
        )
      )
      (Expr.binop_colon
        (lc "empty_lists")
        (Expr.record_literal
          (Expr.binop_colon
            (lc "raw")
            (Expr.lookup "empty_list")
          )
          (Expr.binop_colon
            (lc "in_list")
            (Expr.list_literal)
          )
          (Expr.binop_colon
            (lc "in_record")
            (Expr.record_literal
              (Expr.malformed)
            )
          )
        )
      )
      (Expr.binop_colon
        (lc "computations")
        (Expr.record_literal
          (Expr.binop_colon
            (lc "from_num")
            (Expr.binop_star
              (Expr.lookup "num")
              (Expr.num_literal_i32 100)
            )
          )
          (Expr.binop_colon
            (lc "from_frac")
            (Expr.binop_star
              (Expr.lookup "frac")
              (Expr.frac_literal_small 10)
            )
          )
          (Expr.binop_colon
            (lc "list_from_num")
            (Expr.list_literal)
          )
        )
      )
    )
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
