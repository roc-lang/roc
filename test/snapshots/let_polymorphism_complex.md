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
**UNDEFINED VARIABLE**
Nothing is named **items** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:23:17:23:22:**
```roc
poly_record = { items: empty_list, count: 0 }
```
                ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **count** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:23:36:23:41:**
```roc
poly_record = { items: empty_list, count: 0 }
```
                                   ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **items** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:24:22:24:27:**
```roc
use_poly_record1 = { items: [1, 2, 3], count: 0 }
```
                     ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **count** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:24:40:24:45:**
```roc
use_poly_record1 = { items: [1, 2, 3], count: 0 }
```
                                       ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **items** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:25:22:25:27:**
```roc
use_poly_record2 = { items: ["x", "y", "z"], count: 0 }
```
                     ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **count** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:25:46:25:51:**
```roc
use_poly_record2 = { items: ["x", "y", "z"], count: 0 }
```
                                             ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **data** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:29:5:29:9:**
```roc
    data: empty_list,
```
    ^^^^


**UNDEFINED VARIABLE**
Nothing is named **metadata** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:30:5:30:13:**
```roc
    metadata: {
```
    ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **version** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:31:9:31:16:**
```roc
        version: num,
```
        ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **ratio** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:32:9:32:14:**
```roc
        ratio: frac,
```
        ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **description** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:33:9:33:20:**
```roc
        description: str,
```
        ^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **data** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:39:5:39:9:**
```roc
    data: [1, 2, 3, 4, 5],
```
    ^^^^


**UNDEFINED VARIABLE**
Nothing is named **metadata** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:40:5:40:13:**
```roc
    metadata: {
```
    ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **version** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:41:9:41:16:**
```roc
        version: num,
```
        ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **ratio** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:42:9:42:14:**
```roc
        ratio: frac,
```
        ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **description** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:43:9:43:20:**
```roc
        description: str,
```
        ^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **name** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:45:5:45:9:**
```roc
    name: "integers",
```
    ^^^^


**UNDEFINED VARIABLE**
Nothing is named **data** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:49:5:49:9:**
```roc
    data: ["apple", "banana", "cherry"], # Test comment 2
```
    ^^^^


**UNDEFINED VARIABLE**
Nothing is named **metadata** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:50:5:50:13:**
```roc
    metadata: { # Test comment 3
```
    ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **version** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:51:9:51:16:**
```roc
        version: num, # Test comment 4
```
        ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **ratio** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:52:9:52:14:**
```roc
        ratio: frac, # Test comment 5
```
        ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **description** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:53:9:53:20:**
```roc
        description: str, # Test comment 6
```
        ^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **name** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:55:5:55:9:**
```roc
    name: "fruits", # Test comment 8
```
    ^^^^


**UNDEFINED VARIABLE**
Nothing is named **value** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:59:26:59:31:**
```roc
make_container = |val| { value: val, wrapper: [val] }
```
                         ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **wrapper** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:59:38:59:45:**
```roc
make_container = |val| { value: val, wrapper: [val] }
```
                                     ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **level1** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:66:5:66:11:**
```roc
    level1: {
```
    ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **level2** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:67:9:67:15:**
```roc
        level2: {
```
        ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **level3** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:68:13:68:19:**
```roc
            level3: {
```
            ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **data** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:69:17:69:21:**
```roc
                data: empty_list,
```
                ^^^^


**UNDEFINED VARIABLE**
Nothing is named **value** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:70:17:70:22:**
```roc
                value: num,
```
                ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **items** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:72:13:72:18:**
```roc
            items: [num, num * 2, num * 3],
```
            ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **collection** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:74:9:74:19:**
```roc
        collection: empty_list,
```
        ^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **results** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:76:5:76:12:**
```roc
    results: [
```
    ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **data** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:77:11:77:15:**
```roc
        { data: [1], tag: "single" },
```
          ^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:77:22:77:25:**
```roc
        { data: [1], tag: "single" },
```
                     ^^^


**UNDEFINED VARIABLE**
Nothing is named **data** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:78:11:78:15:**
```roc
        { data: [1, 2], tag: "ints" },
```
          ^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:78:25:78:28:**
```roc
        { data: [1, 2], tag: "ints" },
```
                        ^^^


**UNDEFINED VARIABLE**
Nothing is named **data** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:79:11:79:15:**
```roc
        { data: [1, 2, 3], tag: "more" },
```
          ^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:79:28:79:31:**
```roc
        { data: [1, 2, 3], tag: "more" },
```
                           ^^^


**UNDEFINED VARIABLE**
Nothing is named **base** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:87:14:87:18:**
```roc
compute4 = { base: num, derived: [num, num + 1, num + 2] }
```
             ^^^^


**UNDEFINED VARIABLE**
Nothing is named **derived** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:87:25:87:32:**
```roc
compute4 = { base: num, derived: [num, num + 1, num + 2] }
```
                        ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **numbers** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:91:5:91:12:**
```roc
    numbers: { value: num, list: [num, num], float: frac },
```
    ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **value** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:91:16:91:21:**
```roc
    numbers: { value: num, list: [num, num], float: frac },
```
               ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **list** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:91:28:91:32:**
```roc
    numbers: { value: num, list: [num, num], float: frac },
```
                           ^^^^


**UNDEFINED VARIABLE**
Nothing is named **float** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:91:46:91:51:**
```roc
    numbers: { value: num, list: [num, num], float: frac },
```
                                             ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **strings** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:92:5:92:12:**
```roc
    strings: { value: str, list: [str, str] },
```
    ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **value** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:92:16:92:21:**
```roc
    strings: { value: str, list: [str, str] },
```
               ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **list** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:92:28:92:32:**
```roc
    strings: { value: str, list: [str, str] },
```
                           ^^^^


**UNDEFINED VARIABLE**
Nothing is named **empty_lists** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:93:5:93:16:**
```roc
    empty_lists: {
```
    ^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **raw** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:94:9:94:12:**
```roc
        raw: empty_list,
```
        ^^^


**UNDEFINED VARIABLE**
Nothing is named **in_list** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:95:9:95:16:**
```roc
        in_list: [empty_list],
```
        ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **in_record** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:96:9:96:18:**
```roc
        in_record: { data: empty_list },
```
        ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **data** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:96:22:96:26:**
```roc
        in_record: { data: empty_list },
```
                     ^^^^


**UNDEFINED VARIABLE**
Nothing is named **computations** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:98:5:98:17:**
```roc
    computations: {
```
    ^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **from_num** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:99:9:99:17:**
```roc
        from_num: num * 100,
```
        ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **from_frac** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:100:9:100:18:**
```roc
        from_frac: frac * 10.0,
```
        ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **list_from_num** in this scope.
Is there an **import** or **exposing** missing up-top?

**let_polymorphism_complex.md:101:9:101:22:**
```roc
        list_from_num: [num, num, num],
```
        ^^^^^^^^^^^^^


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
    (Expr.apply_tag)
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
        (Expr.lookup "items")
        (Expr.lookup "empty_list")
      )
      (Expr.binop_colon
        (Expr.lookup "count")
        (Expr.num_literal_i32 0)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "use_poly_record1"))
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "items")
        (Expr.list_literal)
      )
      (Expr.binop_colon
        (Expr.lookup "count")
        (Expr.num_literal_i32 0)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "use_poly_record2"))
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "items")
        (Expr.list_literal)
      )
      (Expr.binop_colon
        (Expr.lookup "count")
        (Expr.num_literal_i32 0)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "base_config"))
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "data")
        (Expr.lookup "empty_list")
      )
      (Expr.binop_colon
        (Expr.lookup "metadata")
        (Expr.record_literal
          (Expr.binop_colon
            (Expr.lookup "version")
            (Expr.lookup "num")
          )
          (Expr.binop_colon
            (Expr.lookup "ratio")
            (Expr.lookup "frac")
          )
          (Expr.binop_colon
            (Expr.lookup "description")
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
        (Expr.lookup "data")
        (Expr.list_literal)
      )
      (Expr.binop_colon
        (Expr.lookup "metadata")
        (Expr.record_literal
          (Expr.binop_colon
            (Expr.lookup "version")
            (Expr.lookup "num")
          )
          (Expr.binop_colon
            (Expr.lookup "ratio")
            (Expr.lookup "frac")
          )
          (Expr.binop_colon
            (Expr.lookup "description")
            (Expr.lookup "str")
          )
        )
      )
      (Expr.binop_colon
        (Expr.lookup "name")
        (Expr.str_literal_big)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "config2"))
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "data")
        (Expr.list_literal)
      )
      (Expr.binop_colon
        (Expr.lookup "metadata")
        (Expr.record_literal
          (Expr.binop_colon
            (Expr.lookup "version")
            (Expr.lookup "num")
          )
          (Expr.binop_colon
            (Expr.lookup "ratio")
            (Expr.lookup "frac")
          )
          (Expr.binop_colon
            (Expr.lookup "description")
            (Expr.lookup "str")
          )
        )
      )
      (Expr.binop_colon
        (Expr.lookup "name")
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
        (Expr.lookup "level1")
        (Expr.record_literal
          (Expr.binop_colon
            (Expr.lookup "level2")
            (Expr.record_literal
              (Expr.binop_colon
                (Expr.lookup "level3")
                (Expr.record_literal
                  (Expr.binop_colon
                    (Expr.lookup "data")
                    (Expr.lookup "empty_list")
                  )
                  (Expr.binop_colon
                    (Expr.lookup "value")
                    (Expr.lookup "num")
                  )
                )
              )
              (Expr.binop_colon
                (Expr.lookup "items")
                (Expr.list_literal)
              )
            )
          )
          (Expr.binop_colon
            (Expr.lookup "collection")
            (Expr.lookup "empty_list")
          )
        )
      )
      (Expr.binop_colon
        (Expr.lookup "results")
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
        (Expr.lookup "base")
        (Expr.lookup "num")
      )
      (Expr.binop_colon
        (Expr.lookup "derived")
        (Expr.list_literal)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "mixed"))
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "numbers")
        (Expr.record_literal
          (Expr.binop_colon
            (Expr.lookup "value")
            (Expr.lookup "num")
          )
          (Expr.binop_colon
            (Expr.lookup "list")
            (Expr.list_literal)
          )
          (Expr.binop_colon
            (Expr.lookup "float")
            (Expr.lookup "frac")
          )
        )
      )
      (Expr.binop_colon
        (Expr.lookup "strings")
        (Expr.record_literal
          (Expr.binop_colon
            (Expr.lookup "value")
            (Expr.lookup "str")
          )
          (Expr.binop_colon
            (Expr.lookup "list")
            (Expr.list_literal)
          )
        )
      )
      (Expr.binop_colon
        (Expr.lookup "empty_lists")
        (Expr.record_literal
          (Expr.binop_colon
            (Expr.lookup "raw")
            (Expr.lookup "empty_list")
          )
          (Expr.binop_colon
            (Expr.lookup "in_list")
            (Expr.list_literal)
          )
          (Expr.binop_colon
            (Expr.lookup "in_record")
            (Expr.record_literal
              (Expr.binop_colon
                (Expr.lookup "data")
                (Expr.lookup "empty_list")
              )
            )
          )
        )
      )
      (Expr.binop_colon
        (Expr.lookup "computations")
        (Expr.record_literal
          (Expr.binop_colon
            (Expr.lookup "from_num")
            (Expr.binop_star
              (Expr.lookup "num")
              (Expr.num_literal_i32 100)
            )
          )
          (Expr.binop_colon
            (Expr.lookup "from_frac")
            (Expr.binop_star
              (Expr.lookup "frac")
              (Expr.frac_literal_small 10)
            )
          )
          (Expr.binop_colon
            (Expr.lookup "list_from_num")
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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
