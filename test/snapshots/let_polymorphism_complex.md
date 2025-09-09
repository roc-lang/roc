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
            (binop_dot
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
	(container1..value) + 10
}
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:4:1:4:4:**
```roc
num = 42
```
^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:5:1:5:5:**
```roc
frac = 4.2
```
^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:6:1:6:4:**
```roc
str = "hello"
```
^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:7:1:7:5:**
```roc
bool = True
```
^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:10:1:10:11:**
```roc
empty_list = []
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:11:1:11:13:**
```roc
empty_record = {}
```
^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:14:1:14:9:**
```roc
int_list = [1, 2, 3]
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:15:1:15:9:**
```roc
str_list = ["a", "b", "c"]
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:16:1:16:10:**
```roc
bool_list = [True, False]
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:19:1:19:13:**
```roc
nested_empty = [empty_list, empty_list, empty_list]
```
^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:20:1:20:13:**
```roc
mixed_nested = [empty_list, [1, 2], empty_list, [3, 4]]
```
^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:23:1:23:12:**
```roc
poly_record = { items: empty_list, count: 0 }
```
^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:24:1:24:17:**
```roc
use_poly_record1 = { items: [1, 2, 3], count: 0 }
```
^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:25:1:25:17:**
```roc
use_poly_record2 = { items: ["x", "y", "z"], count: 0 }
```
^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:28:1:28:12:**
```roc
base_config = {
```
^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:38:1:38:8:**
```roc
config1 = {
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:48:1:48:8:**
```roc
config2 = { # Test comment 1
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:59:1:59:15:**
```roc
make_container = |val| { value: val, wrapper: [val] }
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:60:1:60:11:**
```roc
container1 = make_container(num)
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:61:1:61:11:**
```roc
container2 = make_container(str)
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:62:1:62:11:**
```roc
container3 = make_container(frac)
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:65:1:65:5:**
```roc
deep = {
```
^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:84:1:84:9:**
```roc
compute1 = num + 10
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:85:1:85:9:**
```roc
compute2 = num * 2
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:86:1:86:9:**
```roc
compute3 = [num, num]
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:87:1:87:9:**
```roc
compute4 = { base: num, derived: [num, num + 1, num + 2] }
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_complex.md:90:1:90:6:**
```roc
mixed = {
```
^^^^^


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
    (Expr.tag_no_args)
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
      (Expr.record_field
        (Expr.malformed)
        (Expr.lookup "empty_list")
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.num_literal_i32 0)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "use_poly_record1"))
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.list_literal)
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.num_literal_i32 0)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "use_poly_record2"))
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.list_literal)
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.num_literal_i32 0)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "base_config"))
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.lookup "empty_list")
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.record_literal
          (Expr.record_field
            (Expr.malformed)
            (Expr.lookup "num")
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.lookup "frac")
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.lookup "str")
          )
        )
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "config1"))
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.list_literal)
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.record_literal
          (Expr.record_field
            (Expr.malformed)
            (Expr.lookup "num")
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.lookup "frac")
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.lookup "str")
          )
        )
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.str_literal_big)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "config2"))
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.list_literal)
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.record_literal
          (Expr.record_field
            (Expr.malformed)
            (Expr.lookup "num")
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.lookup "frac")
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.lookup "str")
          )
        )
      )
      (Expr.record_field
        (Expr.malformed)
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
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "container2"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "container3"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "deep"))
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.record_literal
          (Expr.record_field
            (Expr.malformed)
            (Expr.record_literal
              (Expr.record_field
                (Expr.malformed)
                (Expr.record_literal
                  (Expr.record_field
                    (Expr.malformed)
                    (Expr.lookup "empty_list")
                  )
                  (Expr.record_field
                    (Expr.malformed)
                    (Expr.lookup "num")
                  )
                )
              )
              (Expr.record_field
                (Expr.malformed)
                (Expr.list_literal)
              )
            )
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.lookup "empty_list")
          )
        )
      )
      (Expr.record_field
        (Expr.malformed)
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
      (Expr.record_field
        (Expr.malformed)
        (Expr.lookup "num")
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.list_literal)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "mixed"))
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.record_literal
          (Expr.record_field
            (Expr.malformed)
            (Expr.lookup "num")
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.list_literal)
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.lookup "frac")
          )
        )
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.record_literal
          (Expr.record_field
            (Expr.malformed)
            (Expr.lookup "str")
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.list_literal)
          )
        )
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.record_literal
          (Expr.record_field
            (Expr.malformed)
            (Expr.lookup "empty_list")
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.list_literal)
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.record_literal
              (Expr.record_field
                (Expr.malformed)
                (Expr.lookup "empty_list")
              )
            )
          )
        )
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.record_literal
          (Expr.record_field
            (Expr.malformed)
            (Expr.binop_star
              (Expr.lookup "num")
              (Expr.num_literal_i32 100)
            )
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.binop_star
              (Expr.lookup "frac")
              (Expr.frac_literal_small 10)
            )
          )
          (Expr.record_field
            (Expr.malformed)
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
; Total type variables: 434
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 -> #8)
(var #8 Num *)
(var #9 _)
(var #10 -> #11)
(var #11 F64)
(var #12 _)
(var #13 -> #14)
(var #14 Str)
(var #15 _)
(var #16 -> #17)
(var #17 _)
(var #18 _)
(var #19 -> #354)
(var #20 -> #354)
(var #21 _)
(var #22 -> #355)
(var #23 -> #355)
(var #24 _)
(var #25 -> #356)
(var #26 Num *)
(var #27 -> #26)
(var #28 -> #26)
(var #29 -> #356)
(var #30 _)
(var #31 -> #357)
(var #32 Str)
(var #33 -> #32)
(var #34 -> #32)
(var #35 -> #357)
(var #36 _)
(var #37 -> #358)
(var #38 _)
(var #39 -> #38)
(var #40 -> #358)
(var #41 _)
(var #42 -> #359)
(var #43 _)
(var #44 -> #43)
(var #45 -> #43)
(var #46 -> #359)
(var #47 _)
(var #48 -> #362)
(var #49 List #50)
(var #50 Num *)
(var #51 -> #50)
(var #52 -> #360)
(var #53 -> #49)
(var #54 -> #50)
(var #55 -> #54)
(var #56 -> #361)
(var #57 -> #362)
(var #58 _)
(var #59 -> #364)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 Num *)
(var #65 _)
(var #66 -> #364)
(var #67 _)
(var #68 -> #367)
(var #69 _)
(var #70 Num *)
(var #71 -> #70)
(var #72 -> #70)
(var #73 -> #365)
(var #74 _)
(var #75 _)
(var #76 Num *)
(var #77 _)
(var #78 -> #367)
(var #79 _)
(var #80 -> #370)
(var #81 _)
(var #82 Str)
(var #83 -> #82)
(var #84 -> #82)
(var #85 -> #368)
(var #86 _)
(var #87 _)
(var #88 Num *)
(var #89 _)
(var #90 -> #370)
(var #91 _)
(var #92 -> #374)
(var #93 _)
(var #94 _)
(var #95 _)
(var #96 _)
(var #97 _)
(var #98 _)
(var #99 _)
(var #100 _)
(var #101 _)
(var #102 _)
(var #103 _)
(var #104 _)
(var #105 _)
(var #106 -> #372)
(var #107 _)
(var #108 -> #374)
(var #109 _)
(var #110 -> #379)
(var #111 _)
(var #112 Num *)
(var #113 -> #112)
(var #114 -> #112)
(var #115 -> #112)
(var #116 -> #112)
(var #117 -> #375)
(var #118 _)
(var #119 _)
(var #120 _)
(var #121 _)
(var #122 _)
(var #123 _)
(var #124 _)
(var #125 _)
(var #126 _)
(var #127 _)
(var #128 _)
(var #129 -> #377)
(var #130 _)
(var #131 _)
(var #132 Str)
(var #133 _)
(var #134 -> #379)
(var #135 _)
(var #136 -> #384)
(var #137 _)
(var #138 Str)
(var #139 -> #138)
(var #140 -> #138)
(var #141 -> #380)
(var #142 _)
(var #143 _)
(var #144 _)
(var #145 _)
(var #146 _)
(var #147 _)
(var #148 _)
(var #149 _)
(var #150 _)
(var #151 _)
(var #152 _)
(var #153 -> #382)
(var #154 _)
(var #155 _)
(var #156 Str)
(var #157 _)
(var #158 -> #384)
(var #159 _)
(var #160 -> #389)
(var #161 _)
(var #162 _)
(var #163 _)
(var #164 _)
(var #165 _)
(var #166 _)
(var #167 -> #386)
(var #168 _)
(var #169 -> #388)
(var #170 -> #389)
(var #171 _)
(var #172 -> #175)
(var #173 -> #390)
(var #174 _)
(var #175 _)
(var #176 _)
(var #177 -> #180)
(var #178 -> #391)
(var #179 _)
(var #180 _)
(var #181 _)
(var #182 -> #185)
(var #183 -> #392)
(var #184 _)
(var #185 _)
(var #186 _)
(var #187 -> #411)
(var #188 _)
(var #189 _)
(var #190 _)
(var #191 _)
(var #192 _)
(var #193 _)
(var #194 _)
(var #195 _)
(var #196 _)
(var #197 -> #394)
(var #198 _)
(var #199 _)
(var #200 Num *)
(var #201 -> #202)
(var #202 -> #203)
(var #203 -> #200)
(var #204 -> #205)
(var #205 -> #206)
(var #206 -> #200)
(var #207 -> #395)
(var #208 _)
(var #209 -> #397)
(var #210 _)
(var #211 _)
(var #212 _)
(var #213 _)
(var #214 -> #399)
(var #215 _)
(var #216 _)
(var #217 _)
(var #218 Num *)
(var #219 -> #400)
(var #220 _)
(var #221 _)
(var #222 Str)
(var #223 _)
(var #224 -> #402)
(var #225 _)
(var #226 -> #218)
(var #227 -> #226)
(var #228 -> #403)
(var #229 _)
(var #230 _)
(var #231 -> #222)
(var #232 _)
(var #233 -> #405)
(var #234 _)
(var #235 -> #218)
(var #236 -> #235)
(var #237 -> #235)
(var #238 -> #406)
(var #239 _)
(var #240 _)
(var #241 -> #222)
(var #242 _)
(var #243 -> #408)
(var #244 -> #409)
(var #245 _)
(var #246 -> #411)
(var #247 _)
(var #248 -> #251)
(var #249 -> #250)
(var #250 -> #251)
(var #251 Num *)
(var #252 _)
(var #253 -> #256)
(var #254 -> #255)
(var #255 -> #256)
(var #256 Num *)
(var #257 _)
(var #258 -> #412)
(var #259 _)
(var #260 -> #259)
(var #261 -> #412)
(var #262 _)
(var #263 -> #415)
(var #264 _)
(var #265 _)
(var #266 _)
(var #267 _)
(var #268 Num *)
(var #269 -> #270)
(var #270 -> #271)
(var #271 -> #268)
(var #272 -> #273)
(var #273 -> #274)
(var #274 -> #268)
(var #275 -> #413)
(var #276 _)
(var #277 -> #415)
(var #278 _)
(var #279 -> #431)
(var #280 _)
(var #281 _)
(var #282 _)
(var #283 _)
(var #284 _)
(var #285 _)
(var #286 -> #285)
(var #287 -> #416)
(var #288 _)
(var #289 _)
(var #290 _)
(var #291 _)
(var #292 -> #418)
(var #293 _)
(var #294 _)
(var #295 _)
(var #296 _)
(var #297 _)
(var #298 _)
(var #299 _)
(var #300 -> #299)
(var #301 -> #419)
(var #302 _)
(var #303 -> #421)
(var #304 _)
(var #305 _)
(var #306 _)
(var #307 _)
(var #308 _)
(var #309 _)
(var #310 _)
(var #311 -> #422)
(var #312 _)
(var #313 _)
(var #314 _)
(var #315 _)
(var #316 _)
(var #317 -> #424)
(var #318 _)
(var #319 -> #426)
(var #320 _)
(var #321 _)
(var #322 _)
(var #323 -> #324)
(var #324 -> #325)
(var #325 Num *)
(var #326 _)
(var #327 _)
(var #328 -> #329)
(var #329 -> #330)
(var #330 F64)
(var #331 _)
(var #332 _)
(var #333 _)
(var #334 -> #333)
(var #335 -> #333)
(var #336 -> #427)
(var #337 _)
(var #338 -> #429)
(var #339 _)
(var #340 -> #431)
(var #341 _)
(var #342 -> #433)
(var #343 _)
(var #344 _)
(var #345 _)
(var #346 -> #347)
(var #347 -> #348)
(var #348 Num *)
(var #349 _)
(var #350 -> #433)
(var #351 _)
(var #352 _)
(var #353 _)
(var #354 List #353)
(var #355 {})
(var #356 List #26)
(var #357 List #32)
(var #358 List #38)
(var #359 List #43)
(var #360 -> #49)
(var #361 -> #49)
(var #362 List #49)
(var #363 {})
(var #364 record)
(var #365 List #70)
(var #366 {})
(var #367 record)
(var #368 List #82)
(var #369 {})
(var #370 record)
(var #371 {})
(var #372 record)
(var #373 {})
(var #374 record)
(var #375 List #112)
(var #376 {})
(var #377 record)
(var #378 {})
(var #379 record)
(var #380 List #138)
(var #381 {})
(var #382 record)
(var #383 {})
(var #384 record)
(var #385 _)
(var #386 List #166)
(var #387 {})
(var #388 record)
(var #389 fn_pure)
(var #390 fn_pure)
(var #391 fn_pure)
(var #392 fn_pure)
(var #393 {})
(var #394 record)
(var #395 List #200)
(var #396 {})
(var #397 record)
(var #398 {})
(var #399 record)
(var #400 List #218)
(var #401 {})
(var #402 record)
(var #403 -> #400)
(var #404 -> #401)
(var #405 -> #402)
(var #406 -> #400)
(var #407 -> #401)
(var #408 -> #402)
(var #409 List #224)
(var #410 {})
(var #411 record)
(var #412 List #259)
(var #413 List #268)
(var #414 {})
(var #415 record)
(var #416 List #285)
(var #417 {})
(var #418 record)
(var #419 List #299)
(var #420 {})
(var #421 record)
(var #422 List #310)
(var #423 {})
(var #424 record)
(var #425 {})
(var #426 record)
(var #427 List #333)
(var #428 {})
(var #429 record)
(var #430 {})
(var #431 record)
(var #432 _)
(var #433 fn_pure)
~~~
# TYPES
~~~roc
bool : _a
compute1 : Num(_size)
container3 : _a
config1 : { data: List(Num(_size)), metadata: { version: _field, ratio: _field2, description: _field3 }, name: Str }
num : Num(_size)
empty_record : {}
compute2 : Num(_size)
val : _a
container1 : _a
base_config : { data: _field, metadata: { version: _field2, ratio: _field3, description: _field4 } }
deep : { level1: { level2: { level3: { data: _field, value: _field2 }, items: List(Num(_size)) }, collection: _field3 }, results: List({ data: List(Num(_size2)), tag: Str }) }
int_list : List(Num(_size))
make_container : _arg -> { value: _field, wrapper: List(_elem) }
str : Str
bool_list : List(_elem)
use_poly_record1 : { items: List(Num(_size)), count: Num(_size2) }
compute4 : { base: _field, derived: List(Num(_size)) }
nested_empty : List(_elem)
empty_list : List(_elem)
str_list : List(Str)
mixed : { numbers: { value: _field, list: List(_elem), float: _field2 }, strings: { value: _field3, list: List(_elem2) }, empty_lists: { raw: _field4, in_list: List(_elem3), in_record: { data: _field5 } }, computations: { from_num: Num(_size), from_frac: F64, list_from_num: List(_elem4) } }
config2 : { data: List(Str), metadata: { version: _field, ratio: _field2, description: _field3 }, name: Str }
compute3 : List(_elem)
frac : F64
use_poly_record2 : { items: List(Str), count: Num(_size) }
mixed_nested : List(List(Num(_size)))
poly_record : { items: _field, count: Num(_size) }
container2 : _a
~~~
