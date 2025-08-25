# META
~~~ini
description=Complex let-polymorphism interactions
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly LowerIdent OpAssign Int LowerIdent OpAssign Float LowerIdent OpAssign String LowerIdent OpAssign UpperIdent LowerIdent OpAssign OpenSquare CloseSquare LowerIdent OpAssign OpenCurly CloseCurly LowerIdent OpAssign OpenSquare Int Comma Int Comma Int CloseSquare LowerIdent OpAssign OpenSquare String Comma String Comma String CloseSquare LowerIdent OpAssign OpenSquare UpperIdent Comma UpperIdent CloseSquare LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign OpenSquare LowerIdent Comma OpenSquare Int Comma Int CloseSquare Comma LowerIdent Comma OpenSquare Int Comma Int CloseSquare CloseSquare LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenSquare Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenSquare String Comma String Comma String CloseSquare Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenSquare Int Comma Int Comma Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma LowerIdent OpColon String Comma CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenSquare String Comma String Comma String CloseSquare Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma LowerIdent OpColon String Comma CloseCurly LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent CloseSquare CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent OpStar Int Comma LowerIdent OpStar Int CloseSquare Comma CloseCurly Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma LowerIdent OpColon OpenSquare OpenCurly LowerIdent OpColon OpenSquare Int CloseSquare Comma LowerIdent OpColon String CloseCurly Comma OpenCurly LowerIdent OpColon OpenSquare Int Comma Int CloseSquare Comma LowerIdent OpColon String CloseCurly Comma OpenCurly LowerIdent OpColon OpenSquare Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon String CloseCurly Comma CloseSquare Comma CloseCurly LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent OpAssign LowerIdent OpStar Int LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent OpPlus Int Comma LowerIdent OpPlus Int CloseSquare CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent CloseSquare Comma LowerIdent OpColon LowerIdent CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent CloseSquare CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent CloseSquare Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent CloseCurly Comma CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent OpStar Int Comma LowerIdent OpColon LowerIdent OpStar Float Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare Comma CloseCurly Comma CloseCurly LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LowerIdent Dot LowerIdent OpPlus Int CloseCurly ~~~
# PARSE
~~~clojure
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
    (lc "bool")
    (uc "True")
  )
  (binop_equals
    (lc "empty_list")
    (list_literal)
  )
  (binop_equals
    (lc "empty_record")
    (record_literal)
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
      (str_literal_small "a")
      (str_literal_small "b")
      (str_literal_small "c")
    )
  )
  (binop_equals
    (lc "bool_list")
    (list_literal
      (uc "True")
      (uc "False")
    )
  )
  (binop_equals
    (lc "nested_empty")
    (list_literal
      (lc "empty_list")
      (lc "empty_list")
      (lc "empty_list")
    )
  )
  (binop_equals
    (lc "mixed_nested")
    (list_literal
      (lc "empty_list")
      (list_literal
        (num_literal_i32 1)
        (num_literal_i32 2)
      )
      (lc "empty_list")
      (list_literal
        (num_literal_i32 3)
        (num_literal_i32 4)
      )
    )
  )
  (binop_equals
    (lc "poly_record")
    (record_literal
      (binop_colon
        (lc "items")
        (lc "empty_list")
      )
      (binop_colon
        (lc "count")
        (num_literal_i32 0)
      )
    )
  )
  (binop_equals
    (lc "use_poly_record1")
    (record_literal
      (binop_colon
        (lc "items")
        (list_literal
          (num_literal_i32 1)
          (num_literal_i32 2)
          (num_literal_i32 3)
        )
      )
      (binop_colon
        (lc "count")
        (num_literal_i32 0)
      )
    )
  )
  (binop_equals
    (lc "use_poly_record2")
    (record_literal
      (binop_colon
        (lc "items")
        (list_literal
          (str_literal_small "x")
          (str_literal_small "y")
          (str_literal_small "z")
        )
      )
      (binop_colon
        (lc "count")
        (num_literal_i32 0)
      )
    )
  )
  (binop_equals
    (lc "base_config")
    (record_literal
      (binop_colon
        (lc "data")
        (lc "empty_list")
      )
      (binop_colon
        (lc "metadata")
        (record_literal
          (binop_colon
            (lc "version")
            (lc "num")
          )
          (binop_colon
            (lc "ratio")
            (lc "frac")
          )
          (binop_colon
            (lc "description")
            (lc "str")
          )
        )
      )
    )
  )
  (binop_equals
    (lc "config1")
    (record_literal
      (binop_colon
        (lc "data")
        (list_literal
          (num_literal_i32 1)
          (num_literal_i32 2)
          (num_literal_i32 3)
          (num_literal_i32 4)
          (num_literal_i32 5)
        )
      )
      (binop_colon
        (lc "metadata")
        (record_literal
          (binop_colon
            (lc "version")
            (lc "num")
          )
          (binop_colon
            (lc "ratio")
            (lc "frac")
          )
          (binop_colon
            (lc "description")
            (lc "str")
          )
        )
      )
      (binop_colon
        (lc "name")
        (str_literal_big "integers")
      )
    )
  )
  (binop_equals
    (lc "config2")
    (record_literal
      (binop_colon
        (lc "data")
        (list_literal
          (str_literal_big "apple")
          (str_literal_big "banana")
          (str_literal_big "cherry")
        )
      )
      (binop_colon
        (lc "metadata")
        (record_literal
          (binop_colon
            (lc "version")
            (lc "num")
          )
          (binop_colon
            (lc "ratio")
            (lc "frac")
          )
          (binop_colon
            (lc "description")
            (lc "str")
          )
        )
      )
      (binop_colon
        (lc "name")
        (str_literal_big "fruits")
      )
    )
  )
  (binop_equals
    (lc "make_container")
    (lambda
      (body
        (record_literal
          (binop_colon
            (lc "value")
            (lc "val")
          )
          (binop_colon
            (lc "wrapper")
            (list_literal
              (lc "val")
            )
          )
        )
      )
      (args
        (lc "val")
      )
    )
  )
  (binop_equals
    (lc "container1")
    (apply_lc
      (lc "make_container")
      (lc "num")
    )
  )
  (binop_equals
    (lc "container2")
    (apply_lc
      (lc "make_container")
      (lc "str")
    )
  )
  (binop_equals
    (lc "container3")
    (apply_lc
      (lc "make_container")
      (lc "frac")
    )
  )
  (binop_equals
    (lc "deep")
    (record_literal
      (binop_colon
        (lc "level1")
        (record_literal
          (binop_colon
            (lc "level2")
            (record_literal
              (binop_colon
                (lc "level3")
                (record_literal
                  (binop_colon
                    (lc "data")
                    (lc "empty_list")
                  )
                  (binop_colon
                    (lc "value")
                    (lc "num")
                  )
                )
              )
              (binop_colon
                (lc "items")
                (list_literal
                  (lc "num")
                  (binop_star
                    (lc "num")
                    (num_literal_i32 2)
                  )
                  (binop_star
                    (lc "num")
                    (num_literal_i32 3)
                  )
                )
              )
            )
          )
          (binop_colon
            (lc "collection")
            (lc "empty_list")
          )
        )
      )
      (binop_colon
        (lc "results")
        (list_literal
          (record_literal
            (binop_colon
              (lc "data")
              (list_literal
                (num_literal_i32 1)
              )
            )
            (binop_colon
              (lc "tag")
              (str_literal_big "single")
            )
          )
          (record_literal
            (binop_colon
              (lc "data")
              (list_literal
                (num_literal_i32 1)
                (num_literal_i32 2)
              )
            )
            (binop_colon
              (lc "tag")
              (str_literal_small "ints")
            )
          )
          (record_literal
            (binop_colon
              (lc "data")
              (list_literal
                (num_literal_i32 1)
                (num_literal_i32 2)
                (num_literal_i32 3)
              )
            )
            (binop_colon
              (lc "tag")
              (str_literal_small "more")
            )
          )
        )
      )
    )
  )
  (binop_equals
    (lc "compute1")
    (binop_plus
      (lc "num")
      (num_literal_i32 10)
    )
  )
  (binop_equals
    (lc "compute2")
    (binop_star
      (lc "num")
      (num_literal_i32 2)
    )
  )
  (binop_equals
    (lc "compute3")
    (list_literal
      (lc "num")
      (lc "num")
    )
  )
  (binop_equals
    (lc "compute4")
    (record_literal
      (binop_colon
        (lc "base")
        (lc "num")
      )
      (binop_colon
        (lc "derived")
        (list_literal
          (lc "num")
          (binop_plus
            (lc "num")
            (num_literal_i32 1)
          )
          (binop_plus
            (lc "num")
            (num_literal_i32 2)
          )
        )
      )
    )
  )
  (binop_equals
    (lc "mixed")
    (record_literal
      (binop_colon
        (lc "numbers")
        (record_literal
          (binop_colon
            (lc "value")
            (lc "num")
          )
          (binop_colon
            (lc "list")
            (list_literal
              (lc "num")
              (lc "num")
            )
          )
          (binop_colon
            (lc "float")
            (lc "frac")
          )
        )
      )
      (binop_colon
        (lc "strings")
        (record_literal
          (binop_colon
            (lc "value")
            (lc "str")
          )
          (binop_colon
            (lc "list")
            (list_literal
              (lc "str")
              (lc "str")
            )
          )
        )
      )
      (binop_colon
        (lc "empty_lists")
        (record_literal
          (binop_colon
            (lc "raw")
            (lc "empty_list")
          )
          (binop_colon
            (lc "in_list")
            (list_literal
              (lc "empty_list")
            )
          )
          (binop_colon
            (lc "in_record")
            (block
              (binop_colon
                (lc "data")
                (lc "empty_list")
              )
            )
          )
        )
      )
      (binop_colon
        (lc "computations")
        (record_literal
          (binop_colon
            (lc "from_num")
            (binop_star
              (lc "num")
              (num_literal_i32 100)
            )
          )
          (binop_colon
            (lc "from_frac")
            (binop_star
              (lc "frac")
              (frac_literal_small 10)
            )
          )
          (binop_colon
            (lc "list_from_num")
            (list_literal
              (lc "num")
              (lc "num")
              (lc "num")
            )
          )
        )
      )
    )
  )
  (binop_equals
    (lc "main")
    (lambda
      (body
        (block
          (binop_plus
            (binop_pipe
              (lc "container1")
              (dot_lc "value")
            )
            (num_literal_i32 10)
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
app
{
	pf: "../basic-cli/platform.roc" platform [
		main,
	],
}

num = 42
frac = 4.2
str = "hello"
bool = True

# Polymorphic empty collections
empty_list = []
empty_record = {  }

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
	data: [
		1,
		2,
		3,
		4,
		5
	],
	metadata: {
		version: num,
		ratio: frac,
		description: str,
	},
	name: "integers",
}

config2 = {	# Test comment 1
	data: ["apple", "banana", "cherry"],	# Test comment 2
	metadata: {		# Test comment 3
		version: num,		# Test comment 4
		ratio: frac,		# Test comment 5
		description: str	# Test comment 6
	},	# Test comment 7
	name: "fruits"# Test comment 8
} # Test comment 9

# Polymorphic function-like structures
make_container = \val -> { value: val, wrapper: [val] }
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
		in_record: {
			data: empty_list
		},
	},
	computations: {
		from_num: num * 100,
		from_frac: frac * 10.0,
		list_from_num: [num, num, num],
	},
}

main = \_ -> {
	container1.value + 10
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 10:14 to 10:15

**Unsupported Node**
at 14:12 to 14:21

**Unsupported Node**
at 15:12 to 15:27

**Unsupported Node**
at 16:13 to 16:26

**Unsupported Node**
at 19:16 to 19:52

**Unsupported Node**
at 20:16 to 20:56

**Unsupported Node**
at 24:29 to 24:38

**Unsupported Node**
at 25:29 to 25:44

**Unsupported Node**
at 39:11 to 39:26

**Unsupported Node**
at 49:11 to 49:40

**Unsupported Node**
at 59:18 to 59:24

**Unsupported Node**
at 72:20 to 72:43

**Unsupported Node**
at 76:14 to 79:41

**Unsupported Node**
at 86:12 to 86:22

**Unsupported Node**
at 87:34 to 87:57

**Unsupported Node**
at 91:34 to 91:44

**Unsupported Node**
at 92:34 to 92:44

**Unsupported Node**
at 95:18 to 95:30

**Unsupported Node**
at 101:24 to 101:39

**Unsupported Node**
at 105:8 to 105:12

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
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
