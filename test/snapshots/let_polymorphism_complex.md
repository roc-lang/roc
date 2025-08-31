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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly BlankLine LineComment LowerIdent OpAssign Int LowerIdent OpAssign Float LowerIdent OpAssign String LowerIdent OpAssign UpperIdent BlankLine LineComment LowerIdent OpAssign OpenSquare CloseSquare LowerIdent OpAssign OpenCurly CloseCurly BlankLine LineComment LowerIdent OpAssign OpenSquare Int Comma Int Comma Int CloseSquare LowerIdent OpAssign OpenSquare String Comma String Comma String CloseSquare LowerIdent OpAssign OpenSquare UpperIdent Comma UpperIdent CloseSquare BlankLine LineComment LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign OpenSquare LowerIdent Comma OpenSquare Int Comma Int CloseSquare Comma LowerIdent Comma OpenSquare Int Comma Int CloseSquare CloseSquare BlankLine LineComment LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenSquare Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenSquare String Comma String Comma String CloseSquare Comma LowerIdent OpColon Int CloseCurly BlankLine LineComment LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma CloseCurly BlankLine LineComment LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenSquare Int Comma Int Comma Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma LowerIdent OpColon String Comma CloseCurly BlankLine LowerIdent OpAssign OpenCurly LineComment LowerIdent OpColon OpenSquare String Comma String Comma String CloseSquare Comma LineComment LowerIdent OpColon OpenCurly LineComment LowerIdent OpColon LowerIdent Comma LineComment LowerIdent OpColon LowerIdent Comma LineComment LowerIdent OpColon LowerIdent Comma LineComment CloseCurly Comma LineComment LowerIdent OpColon String Comma LineComment CloseCurly LineComment BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent CloseSquare CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound BlankLine LineComment LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent OpStar Int Comma LowerIdent OpStar Int CloseSquare Comma CloseCurly Comma LowerIdent OpColon LowerIdent Comma CloseCurly Comma LowerIdent OpColon OpenSquare OpenCurly LowerIdent OpColon OpenSquare Int CloseSquare Comma LowerIdent OpColon String CloseCurly Comma OpenCurly LowerIdent OpColon OpenSquare Int Comma Int CloseSquare Comma LowerIdent OpColon String CloseCurly Comma OpenCurly LowerIdent OpColon OpenSquare Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon String CloseCurly Comma CloseSquare Comma CloseCurly BlankLine LineComment LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent OpAssign LowerIdent OpStar Int LowerIdent OpAssign OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent OpPlus Int Comma LowerIdent OpPlus Int CloseSquare CloseCurly BlankLine LineComment LowerIdent OpAssign OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent CloseSquare Comma LowerIdent OpColon LowerIdent CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent CloseSquare CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon OpenSquare LowerIdent CloseSquare Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent CloseCurly Comma CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent OpStar Int Comma LowerIdent OpColon LowerIdent OpStar Float Comma LowerIdent OpColon OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare Comma CloseCurly Comma CloseCurly BlankLine LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent Dot LowerIdent OpPlus Int CloseCurly ~~~
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
poly_record = { items : empty_list, count : 0 }
use_poly_record1 = { items : [1, 2, 3], count : 0 }
use_poly_record2 = { items : ["x", "y", "z"], count : 0 }
# Complex nested structure with multiple polymorphic uses
base_config = {
	data : empty_list,
	metadata :
		{
			version : num,
			ratio : frac,
			description : str,
		},
}

# Different instantiations of base_config
config1 = { data : [1, 2, 3, 4, 5], metadata :
	{
		version : num,
		ratio : frac,
		description : str,
	}, name : "integers" }


config2 = { # Test comment 1
	data : ["apple", "banana", "cherry"], # Test comment 2
	metadata :
		{
			 # Test comment 3
version : num,
			 # Test comment 4
ratio : frac,
			 # Test comment 5
description : str,
		}, # Test comment 6
	# Test comment 7
	name : "fruits", # Test comment 8
}

# Test comment 9

# Polymorphic function-like structures
make_container = |val| { value : val, wrapper : [val] }
container1 = make_container(num)
container2 = make_container(str)
container3 = make_container(frac)
# Deeply nested polymorphism
deep = {
	level1 :
		{
			level2 : {
				level3 : {
					data : empty_list,
					value : num,
				},
				items : [num, num * 2, num * 3],
			},
			collection : empty_list,
		},
	results : [{ data : [1], tag : "single" }, { data : [1, 2], tag : "ints" }, { data : [1, 2, 3], tag : "more" }],
}

# Polymorphic values used in computations
compute1 = num + 10
compute2 = num * 2
compute3 = [num, num]
compute4 = { base : num, derived : [num, num + 1, num + 2] }
# Mixed polymorphic structures
mixed = { numbers : {value : num, list : [num, num], float : frac}, strings : {value : str, list : [str, str]}, empty_lists : {raw : empty_list, in_list : [empty_list], in_record : {
	data : empty_list
}}, computations : {from_num : num * 100, from_frac : frac * 10.0, list_from_num : [num, num, num]} }


main = |_| {
	# Just type-check everything
	container1.value + 10
}
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:23:17:23:34:**
```roc
poly_record = { items: empty_list, count: 0 }
```
                ^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:23:36:23:44:**
```roc
poly_record = { items: empty_list, count: 0 }
```
                                   ^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:24:22:24:38:**
```roc
use_poly_record1 = { items: [1, 2, 3], count: 0 }
```
                     ^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:24:40:24:48:**
```roc
use_poly_record1 = { items: [1, 2, 3], count: 0 }
```
                                       ^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:25:22:25:44:**
```roc
use_poly_record2 = { items: ["x", "y", "z"], count: 0 }
```
                     ^^^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:25:46:25:54:**
```roc
use_poly_record2 = { items: ["x", "y", "z"], count: 0 }
```
                                             ^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:29:5:29:21:**
```roc
    data: empty_list,
```
    ^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:30:5:34:6:**
```roc
    metadata: {
        version: num,
        ratio: frac,
        description: str,
    },
```


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:39:5:39:26:**
```roc
    data: [1, 2, 3, 4, 5],
```
    ^^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:40:5:44:6:**
```roc
    metadata: {
        version: num,
        ratio: frac,
        description: str,
    },
```


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:45:5:45:21:**
```roc
    name: "integers",
```
    ^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:49:5:49:40:**
```roc
    data: ["apple", "banana", "cherry"], # Test comment 2
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:50:5:54:6:**
```roc
    metadata: { # Test comment 3
        version: num, # Test comment 4
        ratio: frac, # Test comment 5
        description: str, # Test comment 6
    }, # Test comment 7
```


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:55:5:55:19:**
```roc
    name: "fruits", # Test comment 8
```
    ^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:59:26:59:36:**
```roc
make_container = |val| { value: val, wrapper: [val] }
```
                         ^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:59:38:59:52:**
```roc
make_container = |val| { value: val, wrapper: [val] }
```
                                     ^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **val** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_val` to suppress this warning.
The unused variable is declared here:

**let_polymorphism_complex.md:59:19:59:22:**
```roc
make_container = |val| { value: val, wrapper: [val] }
```
                  ^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:66:5:75:6:**
```roc
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
```


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:76:5:80:6:**
```roc
    results: [
        { data: [1], tag: "single" },
        { data: [1, 2], tag: "ints" },
        { data: [1, 2, 3], tag: "more" },
    ],
```


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:87:14:87:23:**
```roc
compute4 = { base: num, derived: [num, num + 1, num + 2] }
```
             ^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:87:25:87:57:**
```roc
compute4 = { base: num, derived: [num, num + 1, num + 2] }
```
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:91:5:91:59:**
```roc
    numbers: { value: num, list: [num, num], float: frac },
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:92:5:92:46:**
```roc
    strings: { value: str, list: [str, str] },
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:93:5:97:6:**
```roc
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list },
    },
```


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**let_polymorphism_complex.md:98:5:102:6:**
```roc
    computations: {
        from_num: num * 100,
        from_frac: frac * 10.0,
        list_from_num: [num, num, num],
    },
```


**UNUSED VARIABLE**
Variable **container1** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_container1` to suppress this warning.
The unused variable is declared here:

**let_polymorphism_complex.md:107:5:107:15:**
```roc
    container1.value + 10
```
    ^^^^^^^^^^


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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
