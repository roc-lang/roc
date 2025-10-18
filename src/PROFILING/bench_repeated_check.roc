##
## !! Do not alter this file unless necessary !!
##
## Compiler phase benchmarks use this file, see `src/PROFILING/exec_bench.roc`.
## If the file changes, the benchmarks can't track performance over time.

x = 3.14
y = 1.23e45
z = 0.5

my_str : Str
my_str = "one"

binops = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one : U64 -> U64
add_one = |n| n + 1

map_add_one = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply = |arg_one, arg_two| arg_one * arg_two

num = 42
frac = 4.2
str = "hello"

# Polymorphic empty collections
empty_list = []

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

x_2 = 3.14
y_2 = 1.23e45
z_2 = 0.5

my_str_2 : Str
my_str_2 = "one"

binops_2 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_2 : U64 -> U64
add_one_2 = |n| n + 1

map_add_one_2 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_2 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_2 = |arg_one, arg_two| arg_one * arg_two

num_2 = 42
frac_2 = 4.2
str_2 = "hello"

# Polymorphic empty collections
empty_list_2 = []

# Mixed polymorphic structures
mixed_2 = {
    numbers: { value: num_2, list: [num_2, num_2], float: frac },
    strings: { value: str_2, list: [str_2, str_2] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_2 },
    },
    computations: {
        from_num: num_2 * 100,
        from_frac: frac_2 * 10.0,
        list_from_num: [num_2, num_2, num_2],
    },
}

x_3 = 3.14
y_3 = 1.23e45
z_3 = 0.5

my_str_3 : Str
my_str_3 = "one"

binops_3 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_3 : U64 -> U64
add_one_3 = |n| n + 1

map_add_one_3 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_3 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_3 = |arg_one, arg_two| arg_one * arg_two

num_3 = 42
frac_3 = 4.2
str_3 = "hello"

# Polymorphic empty collections
empty_list_3 = []

# Mixed polymorphic structures
mixed_3 = {
    numbers: { value: num_3, list: [num_3, num_3], float: frac },
    strings: { value: str_3, list: [str_3, str_3] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_3 },
    },
    computations: {
        from_num: num_3 * 100,
        from_frac: frac_3 * 10.0,
        list_from_num: [num_3, num_3, num_3],
    },
}

x_4 = 3.14
y_4 = 1.23e45
z_4 = 0.5

my_str_4 : Str
my_str_4 = "one"

binops_4 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_4 : U64 -> U64
add_one_4 = |n| n + 1

map_add_one_4 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_4 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_4 = |arg_one, arg_two| arg_one * arg_two

num_4 = 42
frac_4 = 4.2
str_4 = "hello"

# Polymorphic empty collections
empty_list_4 = []

# Mixed polymorphic structures
mixed_4 = {
    numbers: { value: num_4, list: [num_4, num_4], float: frac },
    strings: { value: str_4, list: [str_4, str_4] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_4 },
    },
    computations: {
        from_num: num_4 * 100,
        from_frac: frac_4 * 10.0,
        list_from_num: [num_4, num_4, num_4],
    },
}

x_5 = 3.14
y_5 = 1.23e45
z_5 = 0.5

my_str_5 : Str
my_str_5 = "one"

binops_5 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_5 : U64 -> U64
add_one_5 = |n| n + 1

map_add_one_5 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_5 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_5 = |arg_one, arg_two| arg_one * arg_two

num_5 = 42
frac_5 = 4.2
str_5 = "hello"

# Polymorphic empty collections
empty_list_5 = []

# Mixed polymorphic structures
mixed_5 = {
    numbers: { value: num_5, list: [num_5, num_5], float: frac },
    strings: { value: str_5, list: [str_5, str_5] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_5 },
    },
    computations: {
        from_num: num_5 * 100,
        from_frac: frac_5 * 10.0,
        list_from_num: [num_5, num_5, num_5],
    },
}

x_6 = 3.14
y_6 = 1.23e45
z_6 = 0.5

my_str_6 : Str
my_str_6 = "one"

binops_6 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_6 : U64 -> U64
add_one_6 = |n| n + 1

map_add_one_6 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_6 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_6 = |arg_one, arg_two| arg_one * arg_two

num_6 = 42
frac_6 = 4.2
str_6 = "hello"

# Polymorphic empty collections
empty_list_6 = []

# Mixed polymorphic structures
mixed_6 = {
    numbers: { value: num_6, list: [num_6, num_6], float: frac },
    strings: { value: str_6, list: [str_6, str_6] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_6 },
    },
    computations: {
        from_num: num_6 * 100,
        from_frac: frac_6 * 10.0,
        list_from_num: [num_6, num_6, num_6],
    },
}

x_7 = 3.14
y_7 = 1.23e45
z_7 = 0.5

my_str_7 : Str
my_str_7 = "one"

binops_7 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_7 : U64 -> U64
add_one_7 = |n| n + 1

map_add_one_7 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_7 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_7 = |arg_one, arg_two| arg_one * arg_two

num_7 = 42
frac_7 = 4.2
str_7 = "hello"

# Polymorphic empty collections
empty_list_7 = []

# Mixed polymorphic structures
mixed_7 = {
    numbers: { value: num_7, list: [num_7, num_7], float: frac },
    strings: { value: str_7, list: [str_7, str_7] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_7 },
    },
    computations: {
        from_num: num_7 * 100,
        from_frac: frac_7 * 10.0,
        list_from_num: [num_7, num_7, num_7],
    },
}

x_8 = 3.14
y_8 = 1.23e45
z_8 = 0.5

my_str_8 : Str
my_str_8 = "one"

binops_8 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_8 : U64 -> U64
add_one_8 = |n| n + 1

map_add_one_8 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_8 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_8 = |arg_one, arg_two| arg_one * arg_two

num_8 = 42
frac_8 = 4.2
str_8 = "hello"

# Polymorphic empty collections
empty_list_8 = []

# Mixed polymorphic structures
mixed_8 = {
    numbers: { value: num_8, list: [num_8, num_8], float: frac },
    strings: { value: str_8, list: [str_8, str_8] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_8 },
    },
    computations: {
        from_num: num_8 * 100,
        from_frac: frac_8 * 10.0,
        list_from_num: [num_8, num_8, num_8],
    },
}

x_9 = 3.14
y_9 = 1.23e45
z_9 = 0.5

my_str_9 : Str
my_str_9 = "one"

binops_9 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_9 : U64 -> U64
add_one_9 = |n| n + 1

map_add_one_9 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_9 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_9 = |arg_one, arg_two| arg_one * arg_two

num_9 = 42
frac_9 = 4.2
str_9 = "hello"

# Polymorphic empty collections
empty_list_9 = []

# Mixed polymorphic structures
mixed_9 = {
    numbers: { value: num_9, list: [num_9, num_9], float: frac },
    strings: { value: str_9, list: [str_9, str_9] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_9 },
    },
    computations: {
        from_num: num_9 * 100,
        from_frac: frac_9 * 10.0,
        list_from_num: [num_9, num_9, num_9],
    },
}

x_10 = 3.14
y_10 = 1.23e45
z_10 = 0.5

my_str_10 : Str
my_str_10 = "one"

binops_10 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_10 : U64 -> U64
add_one_10 = |n| n + 1

map_add_one_10 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_10 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_10 = |arg_one, arg_two| arg_one * arg_two

num_10 = 42
frac_10 = 4.2
str_10 = "hello"

# Polymorphic empty collections
empty_list_10 = []

# Mixed polymorphic structures
mixed_10 = {
    numbers: { value: num_10, list: [num_10, num_10], float: frac },
    strings: { value: str_10, list: [str_10, str_10] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_10 },
    },
    computations: {
        from_num: num_10 * 100,
        from_frac: frac_10 * 10.0,
        list_from_num: [num_10, num_10, num_10],
    },
}

x_11 = 3.14
y_11 = 1.23e45
z_11 = 0.5

my_str_11 : Str
my_str_11 = "one"

binops_11 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_11 : U64 -> U64
add_one_11 = |n| n + 1

map_add_one_11 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_11 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_11 = |arg_one, arg_two| arg_one * arg_two

num_11 = 42
frac_11 = 4.2
str_11 = "hello"

# Polymorphic empty collections
empty_list_11 = []

# Mixed polymorphic structures
mixed_11 = {
    numbers: { value: num_11, list: [num_11, num_11], float: frac },
    strings: { value: str_11, list: [str_11, str_11] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_11 },
    },
    computations: {
        from_num: num_11 * 100,
        from_frac: frac_11 * 10.0,
        list_from_num: [num_11, num_11, num_11],
    },
}

x_12 = 3.14
y_12 = 1.23e45
z_12 = 0.5

my_str_12 : Str
my_str_12 = "one"

binops_12 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_12 : U64 -> U64
add_one_12 = |n| n + 1

map_add_one_12 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_12 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_12 = |arg_one, arg_two| arg_one * arg_two

num_12 = 42
frac_12 = 4.2
str_12 = "hello"

# Polymorphic empty collections
empty_list_12 = []

# Mixed polymorphic structures
mixed_12 = {
    numbers: { value: num_12, list: [num_12, num_12], float: frac },
    strings: { value: str_12, list: [str_12, str_12] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_12 },
    },
    computations: {
        from_num: num_12 * 100,
        from_frac: frac_12 * 10.0,
        list_from_num: [num_12, num_12, num_12],
    },
}

x_13 = 3.14
y_13 = 1.23e45
z_13 = 0.5

my_str_13 : Str
my_str_13 = "one"

binops_13 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_13 : U64 -> U64
add_one_13 = |n| n + 1

map_add_one_13 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_13 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_13 = |arg_one, arg_two| arg_one * arg_two

num_13 = 42
frac_13 = 4.2
str_13 = "hello"

# Polymorphic empty collections
empty_list_13 = []

# Mixed polymorphic structures
mixed_13 = {
    numbers: { value: num_13, list: [num_13, num_13], float: frac },
    strings: { value: str_13, list: [str_13, str_13] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_13 },
    },
    computations: {
        from_num: num_13 * 100,
        from_frac: frac_13 * 10.0,
        list_from_num: [num_13, num_13, num_13],
    },
}

x_14 = 3.14
y_14 = 1.23e45
z_14 = 0.5

my_str_14 : Str
my_str_14 = "one"

binops_14 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_14 : U64 -> U64
add_one_14 = |n| n + 1

map_add_one_14 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_14 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_14 = |arg_one, arg_two| arg_one * arg_two

num_14 = 42
frac_14 = 4.2
str_14 = "hello"

# Polymorphic empty collections
empty_list_14 = []

# Mixed polymorphic structures
mixed_14 = {
    numbers: { value: num_14, list: [num_14, num_14], float: frac },
    strings: { value: str_14, list: [str_14, str_14] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_14 },
    },
    computations: {
        from_num: num_14 * 100,
        from_frac: frac_14 * 10.0,
        list_from_num: [num_14, num_14, num_14],
    },
}

x_15 = 3.14
y_15 = 1.23e45
z_15 = 0.5

my_str_15 : Str
my_str_15 = "one"

binops_15 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_15 : U64 -> U64
add_one_15 = |n| n + 1

map_add_one_15 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_15 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_15 = |arg_one, arg_two| arg_one * arg_two

num_15 = 42
frac_15 = 4.2
str_15 = "hello"

# Polymorphic empty collections
empty_list_15 = []

# Mixed polymorphic structures
mixed_15 = {
    numbers: { value: num_15, list: [num_15, num_15], float: frac },
    strings: { value: str_15, list: [str_15, str_15] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_15 },
    },
    computations: {
        from_num: num_15 * 100,
        from_frac: frac_15 * 10.0,
        list_from_num: [num_15, num_15, num_15],
    },
}

x_16 = 3.14
y_16 = 1.23e45
z_16 = 0.5

my_str_16 : Str
my_str_16 = "one"

binops_16 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_16 : U64 -> U64
add_one_16 = |n| n + 1

map_add_one_16 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_16 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_16 = |arg_one, arg_two| arg_one * arg_two

num_16 = 42
frac_16 = 4.2
str_16 = "hello"

# Polymorphic empty collections
empty_list_16 = []

# Mixed polymorphic structures
mixed_16 = {
    numbers: { value: num_16, list: [num_16, num_16], float: frac },
    strings: { value: str_16, list: [str_16, str_16] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_16 },
    },
    computations: {
        from_num: num_16 * 100,
        from_frac: frac_16 * 10.0,
        list_from_num: [num_16, num_16, num_16],
    },
}

x_17 = 3.14
y_17 = 1.23e45
z_17 = 0.5

my_str_17 : Str
my_str_17 = "one"

binops_17 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_17 : U64 -> U64
add_one_17 = |n| n + 1

map_add_one_17 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_17 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_17 = |arg_one, arg_two| arg_one * arg_two

num_17 = 42
frac_17 = 4.2
str_17 = "hello"

# Polymorphic empty collections
empty_list_17 = []

# Mixed polymorphic structures
mixed_17 = {
    numbers: { value: num_17, list: [num_17, num_17], float: frac },
    strings: { value: str_17, list: [str_17, str_17] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_17 },
    },
    computations: {
        from_num: num_17 * 100,
        from_frac: frac_17 * 10.0,
        list_from_num: [num_17, num_17, num_17],
    },
}

x_18 = 3.14
y_18 = 1.23e45
z_18 = 0.5

my_str_18 : Str
my_str_18 = "one"

binops_18 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_18 : U64 -> U64
add_one_18 = |n| n + 1

map_add_one_18 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_18 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_18 = |arg_one, arg_two| arg_one * arg_two

num_18 = 42
frac_18 = 4.2
str_18 = "hello"

# Polymorphic empty collections
empty_list_18 = []

# Mixed polymorphic structures
mixed_18 = {
    numbers: { value: num_18, list: [num_18, num_18], float: frac },
    strings: { value: str_18, list: [str_18, str_18] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_18 },
    },
    computations: {
        from_num: num_18 * 100,
        from_frac: frac_18 * 10.0,
        list_from_num: [num_18, num_18, num_18],
    },
}

x_19 = 3.14
y_19 = 1.23e45
z_19 = 0.5

my_str_19 : Str
my_str_19 = "one"

binops_19 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_19 : U64 -> U64
add_one_19 = |n| n + 1

map_add_one_19 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_19 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_19 = |arg_one, arg_two| arg_one * arg_two

num_19 = 42
frac_19 = 4.2
str_19 = "hello"

# Polymorphic empty collections
empty_list_19 = []

# Mixed polymorphic structures
mixed_19 = {
    numbers: { value: num_19, list: [num_19, num_19], float: frac },
    strings: { value: str_19, list: [str_19, str_19] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_19 },
    },
    computations: {
        from_num: num_19 * 100,
        from_frac: frac_19 * 10.0,
        list_from_num: [num_19, num_19, num_19],
    },
}

x_20 = 3.14
y_20 = 1.23e45
z_20 = 0.5

my_str_20 : Str
my_str_20 = "one"

binops_20 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_20 : U64 -> U64
add_one_20 = |n| n + 1

map_add_one_20 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_20 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_20 = |arg_one, arg_two| arg_one * arg_two

num_20 = 42
frac_20 = 4.2
str_20 = "hello"

# Polymorphic empty collections
empty_list_20 = []

# Mixed polymorphic structures
mixed_20 = {
    numbers: { value: num_20, list: [num_20, num_20], float: frac },
    strings: { value: str_20, list: [str_20, str_20] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_20 },
    },
    computations: {
        from_num: num_20 * 100,
        from_frac: frac_20 * 10.0,
        list_from_num: [num_20, num_20, num_20],
    },
}

x_21 = 3.14
y_21 = 1.23e45
z_21 = 0.5

my_str_21 : Str
my_str_21 = "one"

binops_21 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_21 : U64 -> U64
add_one_21 = |n| n + 1

map_add_one_21 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_21 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_21 = |arg_one, arg_two| arg_one * arg_two

num_21 = 42
frac_21 = 4.2
str_21 = "hello"

# Polymorphic empty collections
empty_list_21 = []

# Mixed polymorphic structures
mixed_21 = {
    numbers: { value: num_21, list: [num_21, num_21], float: frac },
    strings: { value: str_21, list: [str_21, str_21] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_21 },
    },
    computations: {
        from_num: num_21 * 100,
        from_frac: frac_21 * 10.0,
        list_from_num: [num_21, num_21, num_21],
    },
}

x_22 = 3.14
y_22 = 1.23e45
z_22 = 0.5

my_str_22 : Str
my_str_22 = "one"

binops_22 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_22 : U64 -> U64
add_one_22 = |n| n + 1

map_add_one_22 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_22 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_22 = |arg_one, arg_two| arg_one * arg_two

num_22 = 42
frac_22 = 4.2
str_22 = "hello"

# Polymorphic empty collections
empty_list_22 = []

# Mixed polymorphic structures
mixed_22 = {
    numbers: { value: num_22, list: [num_22, num_22], float: frac },
    strings: { value: str_22, list: [str_22, str_22] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_22 },
    },
    computations: {
        from_num: num_22 * 100,
        from_frac: frac_22 * 10.0,
        list_from_num: [num_22, num_22, num_22],
    },
}

x_23 = 3.14
y_23 = 1.23e45
z_23 = 0.5

my_str_23 : Str
my_str_23 = "one"

binops_23 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_23 : U64 -> U64
add_one_23 = |n| n + 1

map_add_one_23 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_23 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_23 = |arg_one, arg_two| arg_one * arg_two

num_23 = 42
frac_23 = 4.2
str_23 = "hello"

# Polymorphic empty collections
empty_list_23 = []

# Mixed polymorphic structures
mixed_23 = {
    numbers: { value: num_23, list: [num_23, num_23], float: frac },
    strings: { value: str_23, list: [str_23, str_23] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_23 },
    },
    computations: {
        from_num: num_23 * 100,
        from_frac: frac_23 * 10.0,
        list_from_num: [num_23, num_23, num_23],
    },
}

x_24 = 3.14
y_24 = 1.23e45
z_24 = 0.5

my_str_24 : Str
my_str_24 = "one"

binops_24 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_24 : U64 -> U64
add_one_24 = |n| n + 1

map_add_one_24 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_24 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_24 = |arg_one, arg_two| arg_one * arg_two

num_24 = 42
frac_24 = 4.2
str_24 = "hello"

# Polymorphic empty collections
empty_list_24 = []

# Mixed polymorphic structures
mixed_24 = {
    numbers: { value: num_24, list: [num_24, num_24], float: frac },
    strings: { value: str_24, list: [str_24, str_24] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_24 },
    },
    computations: {
        from_num: num_24 * 100,
        from_frac: frac_24 * 10.0,
        list_from_num: [num_24, num_24, num_24],
    },
}

x_25 = 3.14
y_25 = 1.23e45
z_25 = 0.5

my_str_25 : Str
my_str_25 = "one"

binops_25 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_25 : U64 -> U64
add_one_25 = |n| n + 1

map_add_one_25 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_25 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_25 = |arg_one, arg_two| arg_one * arg_two

num_25 = 42
frac_25 = 4.2
str_25 = "hello"

# Polymorphic empty collections
empty_list_25 = []

# Mixed polymorphic structures
mixed_25 = {
    numbers: { value: num_25, list: [num_25, num_25], float: frac },
    strings: { value: str_25, list: [str_25, str_25] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_25 },
    },
    computations: {
        from_num: num_25 * 100,
        from_frac: frac_25 * 10.0,
        list_from_num: [num_25, num_25, num_25],
    },
}

x_26 = 3.14
y_26 = 1.23e45
z_26 = 0.5

my_str_26 : Str
my_str_26 = "one"

binops_26 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_26 : U64 -> U64
add_one_26 = |n| n + 1

map_add_one_26 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_26 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_26 = |arg_one, arg_two| arg_one * arg_two

num_26 = 42
frac_26 = 4.2
str_26 = "hello"

# Polymorphic empty collections
empty_list_26 = []

# Mixed polymorphic structures
mixed_26 = {
    numbers: { value: num_26, list: [num_26, num_26], float: frac },
    strings: { value: str_26, list: [str_26, str_26] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_26 },
    },
    computations: {
        from_num: num_26 * 100,
        from_frac: frac_26 * 10.0,
        list_from_num: [num_26, num_26, num_26],
    },
}

x_27 = 3.14
y_27 = 1.23e45
z_27 = 0.5

my_str_27 : Str
my_str_27 = "one"

binops_27 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_27 : U64 -> U64
add_one_27 = |n| n + 1

map_add_one_27 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_27 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_27 = |arg_one, arg_two| arg_one * arg_two

num_27 = 42
frac_27 = 4.2
str_27 = "hello"

# Polymorphic empty collections
empty_list_27 = []

# Mixed polymorphic structures
mixed_27 = {
    numbers: { value: num_27, list: [num_27, num_27], float: frac },
    strings: { value: str_27, list: [str_27, str_27] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_27 },
    },
    computations: {
        from_num: num_27 * 100,
        from_frac: frac_27 * 10.0,
        list_from_num: [num_27, num_27, num_27],
    },
}

x_28 = 3.14
y_28 = 1.23e45
z_28 = 0.5

my_str_28 : Str
my_str_28 = "one"

binops_28 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_28 : U64 -> U64
add_one_28 = |n| n + 1

map_add_one_28 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_28 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_28 = |arg_one, arg_two| arg_one * arg_two

num_28 = 42
frac_28 = 4.2
str_28 = "hello"

# Polymorphic empty collections
empty_list_28 = []

# Mixed polymorphic structures
mixed_28 = {
    numbers: { value: num_28, list: [num_28, num_28], float: frac },
    strings: { value: str_28, list: [str_28, str_28] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_28 },
    },
    computations: {
        from_num: num_28 * 100,
        from_frac: frac_28 * 10.0,
        list_from_num: [num_28, num_28, num_28],
    },
}

x_29 = 3.14
y_29 = 1.23e45
z_29 = 0.5

my_str_29 : Str
my_str_29 = "one"

binops_29 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_29 : U64 -> U64
add_one_29 = |n| n + 1

map_add_one_29 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_29 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_29 = |arg_one, arg_two| arg_one * arg_two

num_29 = 42
frac_29 = 4.2
str_29 = "hello"

# Polymorphic empty collections
empty_list_29 = []

# Mixed polymorphic structures
mixed_29 = {
    numbers: { value: num_29, list: [num_29, num_29], float: frac },
    strings: { value: str_29, list: [str_29, str_29] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_29 },
    },
    computations: {
        from_num: num_29 * 100,
        from_frac: frac_29 * 10.0,
        list_from_num: [num_29, num_29, num_29],
    },
}

x_30 = 3.14
y_30 = 1.23e45
z_30 = 0.5

my_str_30 : Str
my_str_30 = "one"

binops_30 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_30 : U64 -> U64
add_one_30 = |n| n + 1

map_add_one_30 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_30 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_30 = |arg_one, arg_two| arg_one * arg_two

num_30 = 42
frac_30 = 4.2
str_30 = "hello"

# Polymorphic empty collections
empty_list_30 = []

# Mixed polymorphic structures
mixed_30 = {
    numbers: { value: num_30, list: [num_30, num_30], float: frac },
    strings: { value: str_30, list: [str_30, str_30] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_30 },
    },
    computations: {
        from_num: num_30 * 100,
        from_frac: frac_30 * 10.0,
        list_from_num: [num_30, num_30, num_30],
    },
}

x_31 = 3.14
y_31 = 1.23e45
z_31 = 0.5

my_str_31 : Str
my_str_31 = "one"

binops_31 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_31 : U64 -> U64
add_one_31 = |n| n + 1

map_add_one_31 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_31 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_31 = |arg_one, arg_two| arg_one * arg_two

num_31 = 42
frac_31 = 4.2
str_31 = "hello"

# Polymorphic empty collections
empty_list_31 = []

# Mixed polymorphic structures
mixed_31 = {
    numbers: { value: num_31, list: [num_31, num_31], float: frac },
    strings: { value: str_31, list: [str_31, str_31] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_31 },
    },
    computations: {
        from_num: num_31 * 100,
        from_frac: frac_31 * 10.0,
        list_from_num: [num_31, num_31, num_31],
    },
}

x_32 = 3.14
y_32 = 1.23e45
z_32 = 0.5

my_str_32 : Str
my_str_32 = "one"

binops_32 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_32 : U64 -> U64
add_one_32 = |n| n + 1

map_add_one_32 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_32 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_32 = |arg_one, arg_two| arg_one * arg_two

num_32 = 42
frac_32 = 4.2
str_32 = "hello"

# Polymorphic empty collections
empty_list_32 = []

# Mixed polymorphic structures
mixed_32 = {
    numbers: { value: num_32, list: [num_32, num_32], float: frac },
    strings: { value: str_32, list: [str_32, str_32] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_32 },
    },
    computations: {
        from_num: num_32 * 100,
        from_frac: frac_32 * 10.0,
        list_from_num: [num_32, num_32, num_32],
    },
}

x_33 = 3.14
y_33 = 1.23e45
z_33 = 0.5

my_str_33 : Str
my_str_33 = "one"

binops_33 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_33 : U64 -> U64
add_one_33 = |n| n + 1

map_add_one_33 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_33 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_33 = |arg_one, arg_two| arg_one * arg_two

num_33 = 42
frac_33 = 4.2
str_33 = "hello"

# Polymorphic empty collections
empty_list_33 = []

# Mixed polymorphic structures
mixed_33 = {
    numbers: { value: num_33, list: [num_33, num_33], float: frac },
    strings: { value: str_33, list: [str_33, str_33] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_33 },
    },
    computations: {
        from_num: num_33 * 100,
        from_frac: frac_33 * 10.0,
        list_from_num: [num_33, num_33, num_33],
    },
}

x_34 = 3.14
y_34 = 1.23e45
z_34 = 0.5

my_str_34 : Str
my_str_34 = "one"

binops_34 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_34 : U64 -> U64
add_one_34 = |n| n + 1

map_add_one_34 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_34 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_34 = |arg_one, arg_two| arg_one * arg_two

num_34 = 42
frac_34 = 4.2
str_34 = "hello"

# Polymorphic empty collections
empty_list_34 = []

# Mixed polymorphic structures
mixed_34 = {
    numbers: { value: num_34, list: [num_34, num_34], float: frac },
    strings: { value: str_34, list: [str_34, str_34] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_34 },
    },
    computations: {
        from_num: num_34 * 100,
        from_frac: frac_34 * 10.0,
        list_from_num: [num_34, num_34, num_34],
    },
}

x_35 = 3.14
y_35 = 1.23e45
z_35 = 0.5

my_str_35 : Str
my_str_35 = "one"

binops_35 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_35 : U64 -> U64
add_one_35 = |n| n + 1

map_add_one_35 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_35 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_35 = |arg_one, arg_two| arg_one * arg_two

num_35 = 42
frac_35 = 4.2
str_35 = "hello"

# Polymorphic empty collections
empty_list_35 = []

# Mixed polymorphic structures
mixed_35 = {
    numbers: { value: num_35, list: [num_35, num_35], float: frac },
    strings: { value: str_35, list: [str_35, str_35] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_35 },
    },
    computations: {
        from_num: num_35 * 100,
        from_frac: frac_35 * 10.0,
        list_from_num: [num_35, num_35, num_35],
    },
}

x_36 = 3.14
y_36 = 1.23e45
z_36 = 0.5

my_str_36 : Str
my_str_36 = "one"

binops_36 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_36 : U64 -> U64
add_one_36 = |n| n + 1

map_add_one_36 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_36 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_36 = |arg_one, arg_two| arg_one * arg_two

num_36 = 42
frac_36 = 4.2
str_36 = "hello"

# Polymorphic empty collections
empty_list_36 = []

# Mixed polymorphic structures
mixed_36 = {
    numbers: { value: num_36, list: [num_36, num_36], float: frac },
    strings: { value: str_36, list: [str_36, str_36] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_36 },
    },
    computations: {
        from_num: num_36 * 100,
        from_frac: frac_36 * 10.0,
        list_from_num: [num_36, num_36, num_36],
    },
}

x_37 = 3.14
y_37 = 1.23e45
z_37 = 0.5

my_str_37 : Str
my_str_37 = "one"

binops_37 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_37 : U64 -> U64
add_one_37 = |n| n + 1

map_add_one_37 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_37 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_37 = |arg_one, arg_two| arg_one * arg_two

num_37 = 42
frac_37 = 4.2
str_37 = "hello"

# Polymorphic empty collections
empty_list_37 = []

# Mixed polymorphic structures
mixed_37 = {
    numbers: { value: num_37, list: [num_37, num_37], float: frac },
    strings: { value: str_37, list: [str_37, str_37] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_37 },
    },
    computations: {
        from_num: num_37 * 100,
        from_frac: frac_37 * 10.0,
        list_from_num: [num_37, num_37, num_37],
    },
}

x_38 = 3.14
y_38 = 1.23e45
z_38 = 0.5

my_str_38 : Str
my_str_38 = "one"

binops_38 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_38 : U64 -> U64
add_one_38 = |n| n + 1

map_add_one_38 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_38 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_38 = |arg_one, arg_two| arg_one * arg_two

num_38 = 42
frac_38 = 4.2
str_38 = "hello"

# Polymorphic empty collections
empty_list_38 = []

# Mixed polymorphic structures
mixed_38 = {
    numbers: { value: num_38, list: [num_38, num_38], float: frac },
    strings: { value: str_38, list: [str_38, str_38] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_38 },
    },
    computations: {
        from_num: num_38 * 100,
        from_frac: frac_38 * 10.0,
        list_from_num: [num_38, num_38, num_38],
    },
}

x_39 = 3.14
y_39 = 1.23e45
z_39 = 0.5

my_str_39 : Str
my_str_39 = "one"

binops_39 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_39 : U64 -> U64
add_one_39 = |n| n + 1

map_add_one_39 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_39 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_39 = |arg_one, arg_two| arg_one * arg_two

num_39 = 42
frac_39 = 4.2
str_39 = "hello"

# Polymorphic empty collections
empty_list_39 = []

# Mixed polymorphic structures
mixed_39 = {
    numbers: { value: num_39, list: [num_39, num_39], float: frac },
    strings: { value: str_39, list: [str_39, str_39] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_39 },
    },
    computations: {
        from_num: num_39 * 100,
        from_frac: frac_39 * 10.0,
        list_from_num: [num_39, num_39, num_39],
    },
}

x_40 = 3.14
y_40 = 1.23e45
z_40 = 0.5

my_str_40 : Str
my_str_40 = "one"

binops_40 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_40 : U64 -> U64
add_one_40 = |n| n + 1

map_add_one_40 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_40 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_40 = |arg_one, arg_two| arg_one * arg_two

num_40 = 42
frac_40 = 4.2
str_40 = "hello"

# Polymorphic empty collections
empty_list_40 = []

# Mixed polymorphic structures
mixed_40 = {
    numbers: { value: num_40, list: [num_40, num_40], float: frac },
    strings: { value: str_40, list: [str_40, str_40] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_40 },
    },
    computations: {
        from_num: num_40 * 100,
        from_frac: frac_40 * 10.0,
        list_from_num: [num_40, num_40, num_40],
    },
}

x_41 = 3.14
y_41 = 1.23e45
z_41 = 0.5

my_str_41 : Str
my_str_41 = "one"

binops_41 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_41 : U64 -> U64
add_one_41 = |n| n + 1

map_add_one_41 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_41 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_41 = |arg_one, arg_two| arg_one * arg_two

num_41 = 42
frac_41 = 4.2
str_41 = "hello"

# Polymorphic empty collections
empty_list_41 = []

# Mixed polymorphic structures
mixed_41 = {
    numbers: { value: num_41, list: [num_41, num_41], float: frac },
    strings: { value: str_41, list: [str_41, str_41] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_41 },
    },
    computations: {
        from_num: num_41 * 100,
        from_frac: frac_41 * 10.0,
        list_from_num: [num_41, num_41, num_41],
    },
}

x_42 = 3.14
y_42 = 1.23e45
z_42 = 0.5

my_str_42 : Str
my_str_42 = "one"

binops_42 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_42 : U64 -> U64
add_one_42 = |n| n + 1

map_add_one_42 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_42 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_42 = |arg_one, arg_two| arg_one * arg_two

num_42 = 42
frac_42 = 4.2
str_42 = "hello"

# Polymorphic empty collections
empty_list_42 = []

# Mixed polymorphic structures
mixed_42 = {
    numbers: { value: num_42, list: [num_42, num_42], float: frac },
    strings: { value: str_42, list: [str_42, str_42] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_42 },
    },
    computations: {
        from_num: num_42 * 100,
        from_frac: frac_42 * 10.0,
        list_from_num: [num_42, num_42, num_42],
    },
}

x_43 = 3.14
y_43 = 1.23e45
z_43 = 0.5

my_str_43 : Str
my_str_43 = "one"

binops_43 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_43 : U64 -> U64
add_one_43 = |n| n + 1

map_add_one_43 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_43 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_43 = |arg_one, arg_two| arg_one * arg_two

num_43 = 42
frac_43 = 4.2
str_43 = "hello"

# Polymorphic empty collections
empty_list_43 = []

# Mixed polymorphic structures
mixed_43 = {
    numbers: { value: num_43, list: [num_43, num_43], float: frac },
    strings: { value: str_43, list: [str_43, str_43] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_43 },
    },
    computations: {
        from_num: num_43 * 100,
        from_frac: frac_43 * 10.0,
        list_from_num: [num_43, num_43, num_43],
    },
}

x_44 = 3.14
y_44 = 1.23e45
z_44 = 0.5

my_str_44 : Str
my_str_44 = "one"

binops_44 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_44 : U64 -> U64
add_one_44 = |n| n + 1

map_add_one_44 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_44 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_44 = |arg_one, arg_two| arg_one * arg_two

num_44 = 42
frac_44 = 4.2
str_44 = "hello"

# Polymorphic empty collections
empty_list_44 = []

# Mixed polymorphic structures
mixed_44 = {
    numbers: { value: num_44, list: [num_44, num_44], float: frac },
    strings: { value: str_44, list: [str_44, str_44] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_44 },
    },
    computations: {
        from_num: num_44 * 100,
        from_frac: frac_44 * 10.0,
        list_from_num: [num_44, num_44, num_44],
    },
}

x_45 = 3.14
y_45 = 1.23e45
z_45 = 0.5

my_str_45 : Str
my_str_45 = "one"

binops_45 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_45 : U64 -> U64
add_one_45 = |n| n + 1

map_add_one_45 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_45 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_45 = |arg_one, arg_two| arg_one * arg_two

num_45 = 42
frac_45 = 4.2
str_45 = "hello"

# Polymorphic empty collections
empty_list_45 = []

# Mixed polymorphic structures
mixed_45 = {
    numbers: { value: num_45, list: [num_45, num_45], float: frac },
    strings: { value: str_45, list: [str_45, str_45] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_45 },
    },
    computations: {
        from_num: num_45 * 100,
        from_frac: frac_45 * 10.0,
        list_from_num: [num_45, num_45, num_45],
    },
}

x_46 = 3.14
y_46 = 1.23e45
z_46 = 0.5

my_str_46 : Str
my_str_46 = "one"

binops_46 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_46 : U64 -> U64
add_one_46 = |n| n + 1

map_add_one_46 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_46 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_46 = |arg_one, arg_two| arg_one * arg_two

num_46 = 42
frac_46 = 4.2
str_46 = "hello"

# Polymorphic empty collections
empty_list_46 = []

# Mixed polymorphic structures
mixed_46 = {
    numbers: { value: num_46, list: [num_46, num_46], float: frac },
    strings: { value: str_46, list: [str_46, str_46] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_46 },
    },
    computations: {
        from_num: num_46 * 100,
        from_frac: frac_46 * 10.0,
        list_from_num: [num_46, num_46, num_46],
    },
}

x_47 = 3.14
y_47 = 1.23e45
z_47 = 0.5

my_str_47 : Str
my_str_47 = "one"

binops_47 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_47 : U64 -> U64
add_one_47 = |n| n + 1

map_add_one_47 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_47 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_47 = |arg_one, arg_two| arg_one * arg_two

num_47 = 42
frac_47 = 4.2
str_47 = "hello"

# Polymorphic empty collections
empty_list_47 = []

# Mixed polymorphic structures
mixed_47 = {
    numbers: { value: num_47, list: [num_47, num_47], float: frac },
    strings: { value: str_47, list: [str_47, str_47] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_47 },
    },
    computations: {
        from_num: num_47 * 100,
        from_frac: frac_47 * 10.0,
        list_from_num: [num_47, num_47, num_47],
    },
}

x_48 = 3.14
y_48 = 1.23e45
z_48 = 0.5

my_str_48 : Str
my_str_48 = "one"

binops_48 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_48 : U64 -> U64
add_one_48 = |n| n + 1

map_add_one_48 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_48 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_48 = |arg_one, arg_two| arg_one * arg_two

num_48 = 42
frac_48 = 4.2
str_48 = "hello"

# Polymorphic empty collections
empty_list_48 = []

# Mixed polymorphic structures
mixed_48 = {
    numbers: { value: num_48, list: [num_48, num_48], float: frac },
    strings: { value: str_48, list: [str_48, str_48] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_48 },
    },
    computations: {
        from_num: num_48 * 100,
        from_frac: frac_48 * 10.0,
        list_from_num: [num_48, num_48, num_48],
    },
}

x_49 = 3.14
y_49 = 1.23e45
z_49 = 0.5

my_str_49 : Str
my_str_49 = "one"

binops_49 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_49 : U64 -> U64
add_one_49 = |n| n + 1

map_add_one_49 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_49 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_49 = |arg_one, arg_two| arg_one * arg_two

num_49 = 42
frac_49 = 4.2
str_49 = "hello"

# Polymorphic empty collections
empty_list_49 = []

# Mixed polymorphic structures
mixed_49 = {
    numbers: { value: num_49, list: [num_49, num_49], float: frac },
    strings: { value: str_49, list: [str_49, str_49] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_49 },
    },
    computations: {
        from_num: num_49 * 100,
        from_frac: frac_49 * 10.0,
        list_from_num: [num_49, num_49, num_49],
    },
}

x_50 = 3.14
y_50 = 1.23e45
z_50 = 0.5

my_str_50 : Str
my_str_50 = "one"

binops_50 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_50 : U64 -> U64
add_one_50 = |n| n + 1

map_add_one_50 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_50 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_50 = |arg_one, arg_two| arg_one * arg_two

num_50 = 42
frac_50 = 4.2
str_50 = "hello"

# Polymorphic empty collections
empty_list_50 = []

# Mixed polymorphic structures
mixed_50 = {
    numbers: { value: num_50, list: [num_50, num_50], float: frac },
    strings: { value: str_50, list: [str_50, str_50] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_50 },
    },
    computations: {
        from_num: num_50 * 100,
        from_frac: frac_50 * 10.0,
        list_from_num: [num_50, num_50, num_50],
    },
}

x_51 = 3.14
y_51 = 1.23e45
z_51 = 0.5

my_str_51 : Str
my_str_51 = "one"

binops_51 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_51 : U64 -> U64
add_one_51 = |n| n + 1

map_add_one_51 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_51 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_51 = |arg_one, arg_two| arg_one * arg_two

num_51 = 42
frac_51 = 4.2
str_51 = "hello"

# Polymorphic empty collections
empty_list_51 = []

# Mixed polymorphic structures
mixed_51 = {
    numbers: { value: num_51, list: [num_51, num_51], float: frac },
    strings: { value: str_51, list: [str_51, str_51] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_51 },
    },
    computations: {
        from_num: num_51 * 100,
        from_frac: frac_51 * 10.0,
        list_from_num: [num_51, num_51, num_51],
    },
}

x_52 = 3.14
y_52 = 1.23e45
z_52 = 0.5

my_str_52 : Str
my_str_52 = "one"

binops_52 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_52 : U64 -> U64
add_one_52 = |n| n + 1

map_add_one_52 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_52 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_52 = |arg_one, arg_two| arg_one * arg_two

num_52 = 42
frac_52 = 4.2
str_52 = "hello"

# Polymorphic empty collections
empty_list_52 = []

# Mixed polymorphic structures
mixed_52 = {
    numbers: { value: num_52, list: [num_52, num_52], float: frac },
    strings: { value: str_52, list: [str_52, str_52] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_52 },
    },
    computations: {
        from_num: num_52 * 100,
        from_frac: frac_52 * 10.0,
        list_from_num: [num_52, num_52, num_52],
    },
}

x_53 = 3.14
y_53 = 1.23e45
z_53 = 0.5

my_str_53 : Str
my_str_53 = "one"

binops_53 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_53 : U64 -> U64
add_one_53 = |n| n + 1

map_add_one_53 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_53 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_53 = |arg_one, arg_two| arg_one * arg_two

num_53 = 42
frac_53 = 4.2
str_53 = "hello"

# Polymorphic empty collections
empty_list_53 = []

# Mixed polymorphic structures
mixed_53 = {
    numbers: { value: num_53, list: [num_53, num_53], float: frac },
    strings: { value: str_53, list: [str_53, str_53] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_53 },
    },
    computations: {
        from_num: num_53 * 100,
        from_frac: frac_53 * 10.0,
        list_from_num: [num_53, num_53, num_53],
    },
}

x_54 = 3.14
y_54 = 1.23e45
z_54 = 0.5

my_str_54 : Str
my_str_54 = "one"

binops_54 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_54 : U64 -> U64
add_one_54 = |n| n + 1

map_add_one_54 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_54 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_54 = |arg_one, arg_two| arg_one * arg_two

num_54 = 42
frac_54 = 4.2
str_54 = "hello"

# Polymorphic empty collections
empty_list_54 = []

# Mixed polymorphic structures
mixed_54 = {
    numbers: { value: num_54, list: [num_54, num_54], float: frac },
    strings: { value: str_54, list: [str_54, str_54] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_54 },
    },
    computations: {
        from_num: num_54 * 100,
        from_frac: frac_54 * 10.0,
        list_from_num: [num_54, num_54, num_54],
    },
}

x_55 = 3.14
y_55 = 1.23e45
z_55 = 0.5

my_str_55 : Str
my_str_55 = "one"

binops_55 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_55 : U64 -> U64
add_one_55 = |n| n + 1

map_add_one_55 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_55 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_55 = |arg_one, arg_two| arg_one * arg_two

num_55 = 42
frac_55 = 4.2
str_55 = "hello"

# Polymorphic empty collections
empty_list_55 = []

# Mixed polymorphic structures
mixed_55 = {
    numbers: { value: num_55, list: [num_55, num_55], float: frac },
    strings: { value: str_55, list: [str_55, str_55] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_55 },
    },
    computations: {
        from_num: num_55 * 100,
        from_frac: frac_55 * 10.0,
        list_from_num: [num_55, num_55, num_55],
    },
}

x_56 = 3.14
y_56 = 1.23e45
z_56 = 0.5

my_str_56 : Str
my_str_56 = "one"

binops_56 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_56 : U64 -> U64
add_one_56 = |n| n + 1

map_add_one_56 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_56 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_56 = |arg_one, arg_two| arg_one * arg_two

num_56 = 42
frac_56 = 4.2
str_56 = "hello"

# Polymorphic empty collections
empty_list_56 = []

# Mixed polymorphic structures
mixed_56 = {
    numbers: { value: num_56, list: [num_56, num_56], float: frac },
    strings: { value: str_56, list: [str_56, str_56] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_56 },
    },
    computations: {
        from_num: num_56 * 100,
        from_frac: frac_56 * 10.0,
        list_from_num: [num_56, num_56, num_56],
    },
}

x_57 = 3.14
y_57 = 1.23e45
z_57 = 0.5

my_str_57 : Str
my_str_57 = "one"

binops_57 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_57 : U64 -> U64
add_one_57 = |n| n + 1

map_add_one_57 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_57 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_57 = |arg_one, arg_two| arg_one * arg_two

num_57 = 42
frac_57 = 4.2
str_57 = "hello"

# Polymorphic empty collections
empty_list_57 = []

# Mixed polymorphic structures
mixed_57 = {
    numbers: { value: num_57, list: [num_57, num_57], float: frac },
    strings: { value: str_57, list: [str_57, str_57] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_57 },
    },
    computations: {
        from_num: num_57 * 100,
        from_frac: frac_57 * 10.0,
        list_from_num: [num_57, num_57, num_57],
    },
}

x_58 = 3.14
y_58 = 1.23e45
z_58 = 0.5

my_str_58 : Str
my_str_58 = "one"

binops_58 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_58 : U64 -> U64
add_one_58 = |n| n + 1

map_add_one_58 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_58 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_58 = |arg_one, arg_two| arg_one * arg_two

num_58 = 42
frac_58 = 4.2
str_58 = "hello"

# Polymorphic empty collections
empty_list_58 = []

# Mixed polymorphic structures
mixed_58 = {
    numbers: { value: num_58, list: [num_58, num_58], float: frac },
    strings: { value: str_58, list: [str_58, str_58] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_58 },
    },
    computations: {
        from_num: num_58 * 100,
        from_frac: frac_58 * 10.0,
        list_from_num: [num_58, num_58, num_58],
    },
}

x_59 = 3.14
y_59 = 1.23e45
z_59 = 0.5

my_str_59 : Str
my_str_59 = "one"

binops_59 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_59 : U64 -> U64
add_one_59 = |n| n + 1

map_add_one_59 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_59 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_59 = |arg_one, arg_two| arg_one * arg_two

num_59 = 42
frac_59 = 4.2
str_59 = "hello"

# Polymorphic empty collections
empty_list_59 = []

# Mixed polymorphic structures
mixed_59 = {
    numbers: { value: num_59, list: [num_59, num_59], float: frac },
    strings: { value: str_59, list: [str_59, str_59] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_59 },
    },
    computations: {
        from_num: num_59 * 100,
        from_frac: frac_59 * 10.0,
        list_from_num: [num_59, num_59, num_59],
    },
}

x_60 = 3.14
y_60 = 1.23e45
z_60 = 0.5

my_str_60 : Str
my_str_60 = "one"

binops_60 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_60 : U64 -> U64
add_one_60 = |n| n + 1

map_add_one_60 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_60 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_60 = |arg_one, arg_two| arg_one * arg_two

num_60 = 42
frac_60 = 4.2
str_60 = "hello"

# Polymorphic empty collections
empty_list_60 = []

# Mixed polymorphic structures
mixed_60 = {
    numbers: { value: num_60, list: [num_60, num_60], float: frac },
    strings: { value: str_60, list: [str_60, str_60] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_60 },
    },
    computations: {
        from_num: num_60 * 100,
        from_frac: frac_60 * 10.0,
        list_from_num: [num_60, num_60, num_60],
    },
}

x_61 = 3.14
y_61 = 1.23e45
z_61 = 0.5

my_str_61 : Str
my_str_61 = "one"

binops_61 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_61 : U64 -> U64
add_one_61 = |n| n + 1

map_add_one_61 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_61 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_61 = |arg_one, arg_two| arg_one * arg_two

num_61 = 42
frac_61 = 4.2
str_61 = "hello"

# Polymorphic empty collections
empty_list_61 = []

# Mixed polymorphic structures
mixed_61 = {
    numbers: { value: num_61, list: [num_61, num_61], float: frac },
    strings: { value: str_61, list: [str_61, str_61] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_61 },
    },
    computations: {
        from_num: num_61 * 100,
        from_frac: frac_61 * 10.0,
        list_from_num: [num_61, num_61, num_61],
    },
}

x_62 = 3.14
y_62 = 1.23e45
z_62 = 0.5

my_str_62 : Str
my_str_62 = "one"

binops_62 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_62 : U64 -> U64
add_one_62 = |n| n + 1

map_add_one_62 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_62 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_62 = |arg_one, arg_two| arg_one * arg_two

num_62 = 42
frac_62 = 4.2
str_62 = "hello"

# Polymorphic empty collections
empty_list_62 = []

# Mixed polymorphic structures
mixed_62 = {
    numbers: { value: num_62, list: [num_62, num_62], float: frac },
    strings: { value: str_62, list: [str_62, str_62] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_62 },
    },
    computations: {
        from_num: num_62 * 100,
        from_frac: frac_62 * 10.0,
        list_from_num: [num_62, num_62, num_62],
    },
}

x_63 = 3.14
y_63 = 1.23e45
z_63 = 0.5

my_str_63 : Str
my_str_63 = "one"

binops_63 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_63 : U64 -> U64
add_one_63 = |n| n + 1

map_add_one_63 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_63 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_63 = |arg_one, arg_two| arg_one * arg_two

num_63 = 42
frac_63 = 4.2
str_63 = "hello"

# Polymorphic empty collections
empty_list_63 = []

# Mixed polymorphic structures
mixed_63 = {
    numbers: { value: num_63, list: [num_63, num_63], float: frac },
    strings: { value: str_63, list: [str_63, str_63] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_63 },
    },
    computations: {
        from_num: num_63 * 100,
        from_frac: frac_63 * 10.0,
        list_from_num: [num_63, num_63, num_63],
    },
}

x_64 = 3.14
y_64 = 1.23e45
z_64 = 0.5

my_str_64 : Str
my_str_64 = "one"

binops_64 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_64 : U64 -> U64
add_one_64 = |n| n + 1

map_add_one_64 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_64 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_64 = |arg_one, arg_two| arg_one * arg_two

num_64 = 42
frac_64 = 4.2
str_64 = "hello"

# Polymorphic empty collections
empty_list_64 = []

# Mixed polymorphic structures
mixed_64 = {
    numbers: { value: num_64, list: [num_64, num_64], float: frac },
    strings: { value: str_64, list: [str_64, str_64] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_64 },
    },
    computations: {
        from_num: num_64 * 100,
        from_frac: frac_64 * 10.0,
        list_from_num: [num_64, num_64, num_64],
    },
}

x_65 = 3.14
y_65 = 1.23e45
z_65 = 0.5

my_str_65 : Str
my_str_65 = "one"

binops_65 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_65 : U64 -> U64
add_one_65 = |n| n + 1

map_add_one_65 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_65 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_65 = |arg_one, arg_two| arg_one * arg_two

num_65 = 42
frac_65 = 4.2
str_65 = "hello"

# Polymorphic empty collections
empty_list_65 = []

# Mixed polymorphic structures
mixed_65 = {
    numbers: { value: num_65, list: [num_65, num_65], float: frac },
    strings: { value: str_65, list: [str_65, str_65] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_65 },
    },
    computations: {
        from_num: num_65 * 100,
        from_frac: frac_65 * 10.0,
        list_from_num: [num_65, num_65, num_65],
    },
}

x_66 = 3.14
y_66 = 1.23e45
z_66 = 0.5

my_str_66 : Str
my_str_66 = "one"

binops_66 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_66 : U64 -> U64
add_one_66 = |n| n + 1

map_add_one_66 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_66 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_66 = |arg_one, arg_two| arg_one * arg_two

num_66 = 42
frac_66 = 4.2
str_66 = "hello"

# Polymorphic empty collections
empty_list_66 = []

# Mixed polymorphic structures
mixed_66 = {
    numbers: { value: num_66, list: [num_66, num_66], float: frac },
    strings: { value: str_66, list: [str_66, str_66] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_66 },
    },
    computations: {
        from_num: num_66 * 100,
        from_frac: frac_66 * 10.0,
        list_from_num: [num_66, num_66, num_66],
    },
}

x_67 = 3.14
y_67 = 1.23e45
z_67 = 0.5

my_str_67 : Str
my_str_67 = "one"

binops_67 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_67 : U64 -> U64
add_one_67 = |n| n + 1

map_add_one_67 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_67 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_67 = |arg_one, arg_two| arg_one * arg_two

num_67 = 42
frac_67 = 4.2
str_67 = "hello"

# Polymorphic empty collections
empty_list_67 = []

# Mixed polymorphic structures
mixed_67 = {
    numbers: { value: num_67, list: [num_67, num_67], float: frac },
    strings: { value: str_67, list: [str_67, str_67] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_67 },
    },
    computations: {
        from_num: num_67 * 100,
        from_frac: frac_67 * 10.0,
        list_from_num: [num_67, num_67, num_67],
    },
}

x_68 = 3.14
y_68 = 1.23e45
z_68 = 0.5

my_str_68 : Str
my_str_68 = "one"

binops_68 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_68 : U64 -> U64
add_one_68 = |n| n + 1

map_add_one_68 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_68 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_68 = |arg_one, arg_two| arg_one * arg_two

num_68 = 42
frac_68 = 4.2
str_68 = "hello"

# Polymorphic empty collections
empty_list_68 = []

# Mixed polymorphic structures
mixed_68 = {
    numbers: { value: num_68, list: [num_68, num_68], float: frac },
    strings: { value: str_68, list: [str_68, str_68] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_68 },
    },
    computations: {
        from_num: num_68 * 100,
        from_frac: frac_68 * 10.0,
        list_from_num: [num_68, num_68, num_68],
    },
}

x_69 = 3.14
y_69 = 1.23e45
z_69 = 0.5

my_str_69 : Str
my_str_69 = "one"

binops_69 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_69 : U64 -> U64
add_one_69 = |n| n + 1

map_add_one_69 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_69 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_69 = |arg_one, arg_two| arg_one * arg_two

num_69 = 42
frac_69 = 4.2
str_69 = "hello"

# Polymorphic empty collections
empty_list_69 = []

# Mixed polymorphic structures
mixed_69 = {
    numbers: { value: num_69, list: [num_69, num_69], float: frac },
    strings: { value: str_69, list: [str_69, str_69] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_69 },
    },
    computations: {
        from_num: num_69 * 100,
        from_frac: frac_69 * 10.0,
        list_from_num: [num_69, num_69, num_69],
    },
}

x_70 = 3.14
y_70 = 1.23e45
z_70 = 0.5

my_str_70 : Str
my_str_70 = "one"

binops_70 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_70 : U64 -> U64
add_one_70 = |n| n + 1

map_add_one_70 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_70 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_70 = |arg_one, arg_two| arg_one * arg_two

num_70 = 42
frac_70 = 4.2
str_70 = "hello"

# Polymorphic empty collections
empty_list_70 = []

# Mixed polymorphic structures
mixed_70 = {
    numbers: { value: num_70, list: [num_70, num_70], float: frac },
    strings: { value: str_70, list: [str_70, str_70] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_70 },
    },
    computations: {
        from_num: num_70 * 100,
        from_frac: frac_70 * 10.0,
        list_from_num: [num_70, num_70, num_70],
    },
}

x_71 = 3.14
y_71 = 1.23e45
z_71 = 0.5

my_str_71 : Str
my_str_71 = "one"

binops_71 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_71 : U64 -> U64
add_one_71 = |n| n + 1

map_add_one_71 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_71 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_71 = |arg_one, arg_two| arg_one * arg_two

num_71 = 42
frac_71 = 4.2
str_71 = "hello"

# Polymorphic empty collections
empty_list_71 = []

# Mixed polymorphic structures
mixed_71 = {
    numbers: { value: num_71, list: [num_71, num_71], float: frac },
    strings: { value: str_71, list: [str_71, str_71] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_71 },
    },
    computations: {
        from_num: num_71 * 100,
        from_frac: frac_71 * 10.0,
        list_from_num: [num_71, num_71, num_71],
    },
}

x_72 = 3.14
y_72 = 1.23e45
z_72 = 0.5

my_str_72 : Str
my_str_72 = "one"

binops_72 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_72 : U64 -> U64
add_one_72 = |n| n + 1

map_add_one_72 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_72 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_72 = |arg_one, arg_two| arg_one * arg_two

num_72 = 42
frac_72 = 4.2
str_72 = "hello"

# Polymorphic empty collections
empty_list_72 = []

# Mixed polymorphic structures
mixed_72 = {
    numbers: { value: num_72, list: [num_72, num_72], float: frac },
    strings: { value: str_72, list: [str_72, str_72] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_72 },
    },
    computations: {
        from_num: num_72 * 100,
        from_frac: frac_72 * 10.0,
        list_from_num: [num_72, num_72, num_72],
    },
}

x_73 = 3.14
y_73 = 1.23e45
z_73 = 0.5

my_str_73 : Str
my_str_73 = "one"

binops_73 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_73 : U64 -> U64
add_one_73 = |n| n + 1

map_add_one_73 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_73 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_73 = |arg_one, arg_two| arg_one * arg_two

num_73 = 42
frac_73 = 4.2
str_73 = "hello"

# Polymorphic empty collections
empty_list_73 = []

# Mixed polymorphic structures
mixed_73 = {
    numbers: { value: num_73, list: [num_73, num_73], float: frac },
    strings: { value: str_73, list: [str_73, str_73] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_73 },
    },
    computations: {
        from_num: num_73 * 100,
        from_frac: frac_73 * 10.0,
        list_from_num: [num_73, num_73, num_73],
    },
}

x_74 = 3.14
y_74 = 1.23e45
z_74 = 0.5

my_str_74 : Str
my_str_74 = "one"

binops_74 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_74 : U64 -> U64
add_one_74 = |n| n + 1

map_add_one_74 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_74 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_74 = |arg_one, arg_two| arg_one * arg_two

num_74 = 42
frac_74 = 4.2
str_74 = "hello"

# Polymorphic empty collections
empty_list_74 = []

# Mixed polymorphic structures
mixed_74 = {
    numbers: { value: num_74, list: [num_74, num_74], float: frac },
    strings: { value: str_74, list: [str_74, str_74] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_74 },
    },
    computations: {
        from_num: num_74 * 100,
        from_frac: frac_74 * 10.0,
        list_from_num: [num_74, num_74, num_74],
    },
}

x_75 = 3.14
y_75 = 1.23e45
z_75 = 0.5

my_str_75 : Str
my_str_75 = "one"

binops_75 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_75 : U64 -> U64
add_one_75 = |n| n + 1

map_add_one_75 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_75 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_75 = |arg_one, arg_two| arg_one * arg_two

num_75 = 42
frac_75 = 4.2
str_75 = "hello"

# Polymorphic empty collections
empty_list_75 = []

# Mixed polymorphic structures
mixed_75 = {
    numbers: { value: num_75, list: [num_75, num_75], float: frac },
    strings: { value: str_75, list: [str_75, str_75] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_75 },
    },
    computations: {
        from_num: num_75 * 100,
        from_frac: frac_75 * 10.0,
        list_from_num: [num_75, num_75, num_75],
    },
}

x_76 = 3.14
y_76 = 1.23e45
z_76 = 0.5

my_str_76 : Str
my_str_76 = "one"

binops_76 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_76 : U64 -> U64
add_one_76 = |n| n + 1

map_add_one_76 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_76 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_76 = |arg_one, arg_two| arg_one * arg_two

num_76 = 42
frac_76 = 4.2
str_76 = "hello"

# Polymorphic empty collections
empty_list_76 = []

# Mixed polymorphic structures
mixed_76 = {
    numbers: { value: num_76, list: [num_76, num_76], float: frac },
    strings: { value: str_76, list: [str_76, str_76] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_76 },
    },
    computations: {
        from_num: num_76 * 100,
        from_frac: frac_76 * 10.0,
        list_from_num: [num_76, num_76, num_76],
    },
}

x_77 = 3.14
y_77 = 1.23e45
z_77 = 0.5

my_str_77 : Str
my_str_77 = "one"

binops_77 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_77 : U64 -> U64
add_one_77 = |n| n + 1

map_add_one_77 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_77 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_77 = |arg_one, arg_two| arg_one * arg_two

num_77 = 42
frac_77 = 4.2
str_77 = "hello"

# Polymorphic empty collections
empty_list_77 = []

# Mixed polymorphic structures
mixed_77 = {
    numbers: { value: num_77, list: [num_77, num_77], float: frac },
    strings: { value: str_77, list: [str_77, str_77] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_77 },
    },
    computations: {
        from_num: num_77 * 100,
        from_frac: frac_77 * 10.0,
        list_from_num: [num_77, num_77, num_77],
    },
}

x_78 = 3.14
y_78 = 1.23e45
z_78 = 0.5

my_str_78 : Str
my_str_78 = "one"

binops_78 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_78 : U64 -> U64
add_one_78 = |n| n + 1

map_add_one_78 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_78 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_78 = |arg_one, arg_two| arg_one * arg_two

num_78 = 42
frac_78 = 4.2
str_78 = "hello"

# Polymorphic empty collections
empty_list_78 = []

# Mixed polymorphic structures
mixed_78 = {
    numbers: { value: num_78, list: [num_78, num_78], float: frac },
    strings: { value: str_78, list: [str_78, str_78] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_78 },
    },
    computations: {
        from_num: num_78 * 100,
        from_frac: frac_78 * 10.0,
        list_from_num: [num_78, num_78, num_78],
    },
}

x_79 = 3.14
y_79 = 1.23e45
z_79 = 0.5

my_str_79 : Str
my_str_79 = "one"

binops_79 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_79 : U64 -> U64
add_one_79 = |n| n + 1

map_add_one_79 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_79 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_79 = |arg_one, arg_two| arg_one * arg_two

num_79 = 42
frac_79 = 4.2
str_79 = "hello"

# Polymorphic empty collections
empty_list_79 = []

# Mixed polymorphic structures
mixed_79 = {
    numbers: { value: num_79, list: [num_79, num_79], float: frac },
    strings: { value: str_79, list: [str_79, str_79] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_79 },
    },
    computations: {
        from_num: num_79 * 100,
        from_frac: frac_79 * 10.0,
        list_from_num: [num_79, num_79, num_79],
    },
}

x_80 = 3.14
y_80 = 1.23e45
z_80 = 0.5

my_str_80 : Str
my_str_80 = "one"

binops_80 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_80 : U64 -> U64
add_one_80 = |n| n + 1

map_add_one_80 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_80 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_80 = |arg_one, arg_two| arg_one * arg_two

num_80 = 42
frac_80 = 4.2
str_80 = "hello"

# Polymorphic empty collections
empty_list_80 = []

# Mixed polymorphic structures
mixed_80 = {
    numbers: { value: num_80, list: [num_80, num_80], float: frac },
    strings: { value: str_80, list: [str_80, str_80] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_80 },
    },
    computations: {
        from_num: num_80 * 100,
        from_frac: frac_80 * 10.0,
        list_from_num: [num_80, num_80, num_80],
    },
}

x_81 = 3.14
y_81 = 1.23e45
z_81 = 0.5

my_str_81 : Str
my_str_81 = "one"

binops_81 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_81 : U64 -> U64
add_one_81 = |n| n + 1

map_add_one_81 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_81 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_81 = |arg_one, arg_two| arg_one * arg_two

num_81 = 42
frac_81 = 4.2
str_81 = "hello"

# Polymorphic empty collections
empty_list_81 = []

# Mixed polymorphic structures
mixed_81 = {
    numbers: { value: num_81, list: [num_81, num_81], float: frac },
    strings: { value: str_81, list: [str_81, str_81] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_81 },
    },
    computations: {
        from_num: num_81 * 100,
        from_frac: frac_81 * 10.0,
        list_from_num: [num_81, num_81, num_81],
    },
}

x_82 = 3.14
y_82 = 1.23e45
z_82 = 0.5

my_str_82 : Str
my_str_82 = "one"

binops_82 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_82 : U64 -> U64
add_one_82 = |n| n + 1

map_add_one_82 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_82 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_82 = |arg_one, arg_two| arg_one * arg_two

num_82 = 42
frac_82 = 4.2
str_82 = "hello"

# Polymorphic empty collections
empty_list_82 = []

# Mixed polymorphic structures
mixed_82 = {
    numbers: { value: num_82, list: [num_82, num_82], float: frac },
    strings: { value: str_82, list: [str_82, str_82] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_82 },
    },
    computations: {
        from_num: num_82 * 100,
        from_frac: frac_82 * 10.0,
        list_from_num: [num_82, num_82, num_82],
    },
}

x_83 = 3.14
y_83 = 1.23e45
z_83 = 0.5

my_str_83 : Str
my_str_83 = "one"

binops_83 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_83 : U64 -> U64
add_one_83 = |n| n + 1

map_add_one_83 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_83 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_83 = |arg_one, arg_two| arg_one * arg_two

num_83 = 42
frac_83 = 4.2
str_83 = "hello"

# Polymorphic empty collections
empty_list_83 = []

# Mixed polymorphic structures
mixed_83 = {
    numbers: { value: num_83, list: [num_83, num_83], float: frac },
    strings: { value: str_83, list: [str_83, str_83] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_83 },
    },
    computations: {
        from_num: num_83 * 100,
        from_frac: frac_83 * 10.0,
        list_from_num: [num_83, num_83, num_83],
    },
}

x_84 = 3.14
y_84 = 1.23e45
z_84 = 0.5

my_str_84 : Str
my_str_84 = "one"

binops_84 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_84 : U64 -> U64
add_one_84 = |n| n + 1

map_add_one_84 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_84 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_84 = |arg_one, arg_two| arg_one * arg_two

num_84 = 42
frac_84 = 4.2
str_84 = "hello"

# Polymorphic empty collections
empty_list_84 = []

# Mixed polymorphic structures
mixed_84 = {
    numbers: { value: num_84, list: [num_84, num_84], float: frac },
    strings: { value: str_84, list: [str_84, str_84] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_84 },
    },
    computations: {
        from_num: num_84 * 100,
        from_frac: frac_84 * 10.0,
        list_from_num: [num_84, num_84, num_84],
    },
}

x_85 = 3.14
y_85 = 1.23e45
z_85 = 0.5

my_str_85 : Str
my_str_85 = "one"

binops_85 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_85 : U64 -> U64
add_one_85 = |n| n + 1

map_add_one_85 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_85 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_85 = |arg_one, arg_two| arg_one * arg_two

num_85 = 42
frac_85 = 4.2
str_85 = "hello"

# Polymorphic empty collections
empty_list_85 = []

# Mixed polymorphic structures
mixed_85 = {
    numbers: { value: num_85, list: [num_85, num_85], float: frac },
    strings: { value: str_85, list: [str_85, str_85] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_85 },
    },
    computations: {
        from_num: num_85 * 100,
        from_frac: frac_85 * 10.0,
        list_from_num: [num_85, num_85, num_85],
    },
}

x_86 = 3.14
y_86 = 1.23e45
z_86 = 0.5

my_str_86 : Str
my_str_86 = "one"

binops_86 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_86 : U64 -> U64
add_one_86 = |n| n + 1

map_add_one_86 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_86 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_86 = |arg_one, arg_two| arg_one * arg_two

num_86 = 42
frac_86 = 4.2
str_86 = "hello"

# Polymorphic empty collections
empty_list_86 = []

# Mixed polymorphic structures
mixed_86 = {
    numbers: { value: num_86, list: [num_86, num_86], float: frac },
    strings: { value: str_86, list: [str_86, str_86] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_86 },
    },
    computations: {
        from_num: num_86 * 100,
        from_frac: frac_86 * 10.0,
        list_from_num: [num_86, num_86, num_86],
    },
}

x_87 = 3.14
y_87 = 1.23e45
z_87 = 0.5

my_str_87 : Str
my_str_87 = "one"

binops_87 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_87 : U64 -> U64
add_one_87 = |n| n + 1

map_add_one_87 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_87 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_87 = |arg_one, arg_two| arg_one * arg_two

num_87 = 42
frac_87 = 4.2
str_87 = "hello"

# Polymorphic empty collections
empty_list_87 = []

# Mixed polymorphic structures
mixed_87 = {
    numbers: { value: num_87, list: [num_87, num_87], float: frac },
    strings: { value: str_87, list: [str_87, str_87] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_87 },
    },
    computations: {
        from_num: num_87 * 100,
        from_frac: frac_87 * 10.0,
        list_from_num: [num_87, num_87, num_87],
    },
}

x_88 = 3.14
y_88 = 1.23e45
z_88 = 0.5

my_str_88 : Str
my_str_88 = "one"

binops_88 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_88 : U64 -> U64
add_one_88 = |n| n + 1

map_add_one_88 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_88 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_88 = |arg_one, arg_two| arg_one * arg_two

num_88 = 42
frac_88 = 4.2
str_88 = "hello"

# Polymorphic empty collections
empty_list_88 = []

# Mixed polymorphic structures
mixed_88 = {
    numbers: { value: num_88, list: [num_88, num_88], float: frac },
    strings: { value: str_88, list: [str_88, str_88] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_88 },
    },
    computations: {
        from_num: num_88 * 100,
        from_frac: frac_88 * 10.0,
        list_from_num: [num_88, num_88, num_88],
    },
}

x_89 = 3.14
y_89 = 1.23e45
z_89 = 0.5

my_str_89 : Str
my_str_89 = "one"

binops_89 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_89 : U64 -> U64
add_one_89 = |n| n + 1

map_add_one_89 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_89 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_89 = |arg_one, arg_two| arg_one * arg_two

num_89 = 42
frac_89 = 4.2
str_89 = "hello"

# Polymorphic empty collections
empty_list_89 = []

# Mixed polymorphic structures
mixed_89 = {
    numbers: { value: num_89, list: [num_89, num_89], float: frac },
    strings: { value: str_89, list: [str_89, str_89] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_89 },
    },
    computations: {
        from_num: num_89 * 100,
        from_frac: frac_89 * 10.0,
        list_from_num: [num_89, num_89, num_89],
    },
}

x_90 = 3.14
y_90 = 1.23e45
z_90 = 0.5

my_str_90 : Str
my_str_90 = "one"

binops_90 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_90 : U64 -> U64
add_one_90 = |n| n + 1

map_add_one_90 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_90 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_90 = |arg_one, arg_two| arg_one * arg_two

num_90 = 42
frac_90 = 4.2
str_90 = "hello"

# Polymorphic empty collections
empty_list_90 = []

# Mixed polymorphic structures
mixed_90 = {
    numbers: { value: num_90, list: [num_90, num_90], float: frac },
    strings: { value: str_90, list: [str_90, str_90] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_90 },
    },
    computations: {
        from_num: num_90 * 100,
        from_frac: frac_90 * 10.0,
        list_from_num: [num_90, num_90, num_90],
    },
}

x_91 = 3.14
y_91 = 1.23e45
z_91 = 0.5

my_str_91 : Str
my_str_91 = "one"

binops_91 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_91 : U64 -> U64
add_one_91 = |n| n + 1

map_add_one_91 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_91 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_91 = |arg_one, arg_two| arg_one * arg_two

num_91 = 42
frac_91 = 4.2
str_91 = "hello"

# Polymorphic empty collections
empty_list_91 = []

# Mixed polymorphic structures
mixed_91 = {
    numbers: { value: num_91, list: [num_91, num_91], float: frac },
    strings: { value: str_91, list: [str_91, str_91] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_91 },
    },
    computations: {
        from_num: num_91 * 100,
        from_frac: frac_91 * 10.0,
        list_from_num: [num_91, num_91, num_91],
    },
}

x_92 = 3.14
y_92 = 1.23e45
z_92 = 0.5

my_str_92 : Str
my_str_92 = "one"

binops_92 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_92 : U64 -> U64
add_one_92 = |n| n + 1

map_add_one_92 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_92 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_92 = |arg_one, arg_two| arg_one * arg_two

num_92 = 42
frac_92 = 4.2
str_92 = "hello"

# Polymorphic empty collections
empty_list_92 = []

# Mixed polymorphic structures
mixed_92 = {
    numbers: { value: num_92, list: [num_92, num_92], float: frac },
    strings: { value: str_92, list: [str_92, str_92] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_92 },
    },
    computations: {
        from_num: num_92 * 100,
        from_frac: frac_92 * 10.0,
        list_from_num: [num_92, num_92, num_92],
    },
}

x_93 = 3.14
y_93 = 1.23e45
z_93 = 0.5

my_str_93 : Str
my_str_93 = "one"

binops_93 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_93 : U64 -> U64
add_one_93 = |n| n + 1

map_add_one_93 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_93 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_93 = |arg_one, arg_two| arg_one * arg_two

num_93 = 42
frac_93 = 4.2
str_93 = "hello"

# Polymorphic empty collections
empty_list_93 = []

# Mixed polymorphic structures
mixed_93 = {
    numbers: { value: num_93, list: [num_93, num_93], float: frac },
    strings: { value: str_93, list: [str_93, str_93] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_93 },
    },
    computations: {
        from_num: num_93 * 100,
        from_frac: frac_93 * 10.0,
        list_from_num: [num_93, num_93, num_93],
    },
}

x_94 = 3.14
y_94 = 1.23e45
z_94 = 0.5

my_str_94 : Str
my_str_94 = "one"

binops_94 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_94 : U64 -> U64
add_one_94 = |n| n + 1

map_add_one_94 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_94 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_94 = |arg_one, arg_two| arg_one * arg_two

num_94 = 42
frac_94 = 4.2
str_94 = "hello"

# Polymorphic empty collections
empty_list_94 = []

# Mixed polymorphic structures
mixed_94 = {
    numbers: { value: num_94, list: [num_94, num_94], float: frac },
    strings: { value: str_94, list: [str_94, str_94] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_94 },
    },
    computations: {
        from_num: num_94 * 100,
        from_frac: frac_94 * 10.0,
        list_from_num: [num_94, num_94, num_94],
    },
}

x_95 = 3.14
y_95 = 1.23e45
z_95 = 0.5

my_str_95 : Str
my_str_95 = "one"

binops_95 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_95 : U64 -> U64
add_one_95 = |n| n + 1

map_add_one_95 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_95 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_95 = |arg_one, arg_two| arg_one * arg_two

num_95 = 42
frac_95 = 4.2
str_95 = "hello"

# Polymorphic empty collections
empty_list_95 = []

# Mixed polymorphic structures
mixed_95 = {
    numbers: { value: num_95, list: [num_95, num_95], float: frac },
    strings: { value: str_95, list: [str_95, str_95] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_95 },
    },
    computations: {
        from_num: num_95 * 100,
        from_frac: frac_95 * 10.0,
        list_from_num: [num_95, num_95, num_95],
    },
}

x_96 = 3.14
y_96 = 1.23e45
z_96 = 0.5

my_str_96 : Str
my_str_96 = "one"

binops_96 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_96 : U64 -> U64
add_one_96 = |n| n + 1

map_add_one_96 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_96 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_96 = |arg_one, arg_two| arg_one * arg_two

num_96 = 42
frac_96 = 4.2
str_96 = "hello"

# Polymorphic empty collections
empty_list_96 = []

# Mixed polymorphic structures
mixed_96 = {
    numbers: { value: num_96, list: [num_96, num_96], float: frac },
    strings: { value: str_96, list: [str_96, str_96] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_96 },
    },
    computations: {
        from_num: num_96 * 100,
        from_frac: frac_96 * 10.0,
        list_from_num: [num_96, num_96, num_96],
    },
}

x_97 = 3.14
y_97 = 1.23e45
z_97 = 0.5

my_str_97 : Str
my_str_97 = "one"

binops_97 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_97 : U64 -> U64
add_one_97 = |n| n + 1

map_add_one_97 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_97 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_97 = |arg_one, arg_two| arg_one * arg_two

num_97 = 42
frac_97 = 4.2
str_97 = "hello"

# Polymorphic empty collections
empty_list_97 = []

# Mixed polymorphic structures
mixed_97 = {
    numbers: { value: num_97, list: [num_97, num_97], float: frac },
    strings: { value: str_97, list: [str_97, str_97] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_97 },
    },
    computations: {
        from_num: num_97 * 100,
        from_frac: frac_97 * 10.0,
        list_from_num: [num_97, num_97, num_97],
    },
}

x_98 = 3.14
y_98 = 1.23e45
z_98 = 0.5

my_str_98 : Str
my_str_98 = "one"

binops_98 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_98 : U64 -> U64
add_one_98 = |n| n + 1

map_add_one_98 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_98 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_98 = |arg_one, arg_two| arg_one * arg_two

num_98 = 42
frac_98 = 4.2
str_98 = "hello"

# Polymorphic empty collections
empty_list_98 = []

# Mixed polymorphic structures
mixed_98 = {
    numbers: { value: num_98, list: [num_98, num_98], float: frac },
    strings: { value: str_98, list: [str_98, str_98] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_98 },
    },
    computations: {
        from_num: num_98 * 100,
        from_frac: frac_98 * 10.0,
        list_from_num: [num_98, num_98, num_98],
    },
}

x_99 = 3.14
y_99 = 1.23e45
z_99 = 0.5

my_str_99 : Str
my_str_99 = "one"

binops_99 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_99 : U64 -> U64
add_one_99 = |n| n + 1

map_add_one_99 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_99 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_99 = |arg_one, arg_two| arg_one * arg_two

num_99 = 42
frac_99 = 4.2
str_99 = "hello"

# Polymorphic empty collections
empty_list_99 = []

# Mixed polymorphic structures
mixed_99 = {
    numbers: { value: num_99, list: [num_99, num_99], float: frac },
    strings: { value: str_99, list: [str_99, str_99] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_99 },
    },
    computations: {
        from_num: num_99 * 100,
        from_frac: frac_99 * 10.0,
        list_from_num: [num_99, num_99, num_99],
    },
}

x_100 = 3.14
y_100 = 1.23e45
z_100 = 0.5

my_str_100 : Str
my_str_100 = "one"

binops_100 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_100 : U64 -> U64
add_one_100 = |n| n + 1

map_add_one_100 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_100 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_100 = |arg_one, arg_two| arg_one * arg_two

num_100 = 42
frac_100 = 4.2
str_100 = "hello"

# Polymorphic empty collections
empty_list_100 = []

# Mixed polymorphic structures
mixed_100 = {
    numbers: { value: num_100, list: [num_100, num_100], float: frac },
    strings: { value: str_100, list: [str_100, str_100] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_100 },
    },
    computations: {
        from_num: num_100 * 100,
        from_frac: frac_100 * 10.0,
        list_from_num: [num_100, num_100, num_100],
    },
}

x_101 = 3.14
y_101 = 1.23e45
z_101 = 0.5

my_str_101 : Str
my_str_101 = "one"

binops_101 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_101 : U64 -> U64
add_one_101 = |n| n + 1

map_add_one_101 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_101 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_101 = |arg_one, arg_two| arg_one * arg_two

num_101 = 42
frac_101 = 4.2
str_101 = "hello"

# Polymorphic empty collections
empty_list_101 = []

# Mixed polymorphic structures
mixed_101 = {
    numbers: { value: num_101, list: [num_101, num_101], float: frac },
    strings: { value: str_101, list: [str_101, str_101] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_101 },
    },
    computations: {
        from_num: num_101 * 100,
        from_frac: frac_101 * 10.0,
        list_from_num: [num_101, num_101, num_101],
    },
}

x_102 = 3.14
y_102 = 1.23e45
z_102 = 0.5

my_str_102 : Str
my_str_102 = "one"

binops_102 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_102 : U64 -> U64
add_one_102 = |n| n + 1

map_add_one_102 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_102 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_102 = |arg_one, arg_two| arg_one * arg_two

num_102 = 42
frac_102 = 4.2
str_102 = "hello"

# Polymorphic empty collections
empty_list_102 = []

# Mixed polymorphic structures
mixed_102 = {
    numbers: { value: num_102, list: [num_102, num_102], float: frac },
    strings: { value: str_102, list: [str_102, str_102] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_102 },
    },
    computations: {
        from_num: num_102 * 100,
        from_frac: frac_102 * 10.0,
        list_from_num: [num_102, num_102, num_102],
    },
}

x_103 = 3.14
y_103 = 1.23e45
z_103 = 0.5

my_str_103 : Str
my_str_103 = "one"

binops_103 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_103 : U64 -> U64
add_one_103 = |n| n + 1

map_add_one_103 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_103 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_103 = |arg_one, arg_two| arg_one * arg_two

num_103 = 42
frac_103 = 4.2
str_103 = "hello"

# Polymorphic empty collections
empty_list_103 = []

# Mixed polymorphic structures
mixed_103 = {
    numbers: { value: num_103, list: [num_103, num_103], float: frac },
    strings: { value: str_103, list: [str_103, str_103] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_103 },
    },
    computations: {
        from_num: num_103 * 100,
        from_frac: frac_103 * 10.0,
        list_from_num: [num_103, num_103, num_103],
    },
}

x_104 = 3.14
y_104 = 1.23e45
z_104 = 0.5

my_str_104 : Str
my_str_104 = "one"

binops_104 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_104 : U64 -> U64
add_one_104 = |n| n + 1

map_add_one_104 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_104 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_104 = |arg_one, arg_two| arg_one * arg_two

num_104 = 42
frac_104 = 4.2
str_104 = "hello"

# Polymorphic empty collections
empty_list_104 = []

# Mixed polymorphic structures
mixed_104 = {
    numbers: { value: num_104, list: [num_104, num_104], float: frac },
    strings: { value: str_104, list: [str_104, str_104] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_104 },
    },
    computations: {
        from_num: num_104 * 100,
        from_frac: frac_104 * 10.0,
        list_from_num: [num_104, num_104, num_104],
    },
}

x_105 = 3.14
y_105 = 1.23e45
z_105 = 0.5

my_str_105 : Str
my_str_105 = "one"

binops_105 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_105 : U64 -> U64
add_one_105 = |n| n + 1

map_add_one_105 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_105 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_105 = |arg_one, arg_two| arg_one * arg_two

num_105 = 42
frac_105 = 4.2
str_105 = "hello"

# Polymorphic empty collections
empty_list_105 = []

# Mixed polymorphic structures
mixed_105 = {
    numbers: { value: num_105, list: [num_105, num_105], float: frac },
    strings: { value: str_105, list: [str_105, str_105] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_105 },
    },
    computations: {
        from_num: num_105 * 100,
        from_frac: frac_105 * 10.0,
        list_from_num: [num_105, num_105, num_105],
    },
}

x_106 = 3.14
y_106 = 1.23e45
z_106 = 0.5

my_str_106 : Str
my_str_106 = "one"

binops_106 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_106 : U64 -> U64
add_one_106 = |n| n + 1

map_add_one_106 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_106 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_106 = |arg_one, arg_two| arg_one * arg_two

num_106 = 42
frac_106 = 4.2
str_106 = "hello"

# Polymorphic empty collections
empty_list_106 = []

# Mixed polymorphic structures
mixed_106 = {
    numbers: { value: num_106, list: [num_106, num_106], float: frac },
    strings: { value: str_106, list: [str_106, str_106] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_106 },
    },
    computations: {
        from_num: num_106 * 100,
        from_frac: frac_106 * 10.0,
        list_from_num: [num_106, num_106, num_106],
    },
}

x_107 = 3.14
y_107 = 1.23e45
z_107 = 0.5

my_str_107 : Str
my_str_107 = "one"

binops_107 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_107 : U64 -> U64
add_one_107 = |n| n + 1

map_add_one_107 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_107 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_107 = |arg_one, arg_two| arg_one * arg_two

num_107 = 42
frac_107 = 4.2
str_107 = "hello"

# Polymorphic empty collections
empty_list_107 = []

# Mixed polymorphic structures
mixed_107 = {
    numbers: { value: num_107, list: [num_107, num_107], float: frac },
    strings: { value: str_107, list: [str_107, str_107] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_107 },
    },
    computations: {
        from_num: num_107 * 100,
        from_frac: frac_107 * 10.0,
        list_from_num: [num_107, num_107, num_107],
    },
}

x_108 = 3.14
y_108 = 1.23e45
z_108 = 0.5

my_str_108 : Str
my_str_108 = "one"

binops_108 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_108 : U64 -> U64
add_one_108 = |n| n + 1

map_add_one_108 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_108 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_108 = |arg_one, arg_two| arg_one * arg_two

num_108 = 42
frac_108 = 4.2
str_108 = "hello"

# Polymorphic empty collections
empty_list_108 = []

# Mixed polymorphic structures
mixed_108 = {
    numbers: { value: num_108, list: [num_108, num_108], float: frac },
    strings: { value: str_108, list: [str_108, str_108] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_108 },
    },
    computations: {
        from_num: num_108 * 100,
        from_frac: frac_108 * 10.0,
        list_from_num: [num_108, num_108, num_108],
    },
}

x_109 = 3.14
y_109 = 1.23e45
z_109 = 0.5

my_str_109 : Str
my_str_109 = "one"

binops_109 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_109 : U64 -> U64
add_one_109 = |n| n + 1

map_add_one_109 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_109 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_109 = |arg_one, arg_two| arg_one * arg_two

num_109 = 42
frac_109 = 4.2
str_109 = "hello"

# Polymorphic empty collections
empty_list_109 = []

# Mixed polymorphic structures
mixed_109 = {
    numbers: { value: num_109, list: [num_109, num_109], float: frac },
    strings: { value: str_109, list: [str_109, str_109] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_109 },
    },
    computations: {
        from_num: num_109 * 100,
        from_frac: frac_109 * 10.0,
        list_from_num: [num_109, num_109, num_109],
    },
}

x_110 = 3.14
y_110 = 1.23e45
z_110 = 0.5

my_str_110 : Str
my_str_110 = "one"

binops_110 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_110 : U64 -> U64
add_one_110 = |n| n + 1

map_add_one_110 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_110 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_110 = |arg_one, arg_two| arg_one * arg_two

num_110 = 42
frac_110 = 4.2
str_110 = "hello"

# Polymorphic empty collections
empty_list_110 = []

# Mixed polymorphic structures
mixed_110 = {
    numbers: { value: num_110, list: [num_110, num_110], float: frac },
    strings: { value: str_110, list: [str_110, str_110] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_110 },
    },
    computations: {
        from_num: num_110 * 100,
        from_frac: frac_110 * 10.0,
        list_from_num: [num_110, num_110, num_110],
    },
}

x_111 = 3.14
y_111 = 1.23e45
z_111 = 0.5

my_str_111 : Str
my_str_111 = "one"

binops_111 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_111 : U64 -> U64
add_one_111 = |n| n + 1

map_add_one_111 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_111 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_111 = |arg_one, arg_two| arg_one * arg_two

num_111 = 42
frac_111 = 4.2
str_111 = "hello"

# Polymorphic empty collections
empty_list_111 = []

# Mixed polymorphic structures
mixed_111 = {
    numbers: { value: num_111, list: [num_111, num_111], float: frac },
    strings: { value: str_111, list: [str_111, str_111] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_111 },
    },
    computations: {
        from_num: num_111 * 100,
        from_frac: frac_111 * 10.0,
        list_from_num: [num_111, num_111, num_111],
    },
}

x_112 = 3.14
y_112 = 1.23e45
z_112 = 0.5

my_str_112 : Str
my_str_112 = "one"

binops_112 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_112 : U64 -> U64
add_one_112 = |n| n + 1

map_add_one_112 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_112 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_112 = |arg_one, arg_two| arg_one * arg_two

num_112 = 42
frac_112 = 4.2
str_112 = "hello"

# Polymorphic empty collections
empty_list_112 = []

# Mixed polymorphic structures
mixed_112 = {
    numbers: { value: num_112, list: [num_112, num_112], float: frac },
    strings: { value: str_112, list: [str_112, str_112] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_112 },
    },
    computations: {
        from_num: num_112 * 100,
        from_frac: frac_112 * 10.0,
        list_from_num: [num_112, num_112, num_112],
    },
}

x_113 = 3.14
y_113 = 1.23e45
z_113 = 0.5

my_str_113 : Str
my_str_113 = "one"

binops_113 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_113 : U64 -> U64
add_one_113 = |n| n + 1

map_add_one_113 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_113 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_113 = |arg_one, arg_two| arg_one * arg_two

num_113 = 42
frac_113 = 4.2
str_113 = "hello"

# Polymorphic empty collections
empty_list_113 = []

# Mixed polymorphic structures
mixed_113 = {
    numbers: { value: num_113, list: [num_113, num_113], float: frac },
    strings: { value: str_113, list: [str_113, str_113] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_113 },
    },
    computations: {
        from_num: num_113 * 100,
        from_frac: frac_113 * 10.0,
        list_from_num: [num_113, num_113, num_113],
    },
}

x_114 = 3.14
y_114 = 1.23e45
z_114 = 0.5

my_str_114 : Str
my_str_114 = "one"

binops_114 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_114 : U64 -> U64
add_one_114 = |n| n + 1

map_add_one_114 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_114 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_114 = |arg_one, arg_two| arg_one * arg_two

num_114 = 42
frac_114 = 4.2
str_114 = "hello"

# Polymorphic empty collections
empty_list_114 = []

# Mixed polymorphic structures
mixed_114 = {
    numbers: { value: num_114, list: [num_114, num_114], float: frac },
    strings: { value: str_114, list: [str_114, str_114] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_114 },
    },
    computations: {
        from_num: num_114 * 100,
        from_frac: frac_114 * 10.0,
        list_from_num: [num_114, num_114, num_114],
    },
}

x_115 = 3.14
y_115 = 1.23e45
z_115 = 0.5

my_str_115 : Str
my_str_115 = "one"

binops_115 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_115 : U64 -> U64
add_one_115 = |n| n + 1

map_add_one_115 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_115 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_115 = |arg_one, arg_two| arg_one * arg_two

num_115 = 42
frac_115 = 4.2
str_115 = "hello"

# Polymorphic empty collections
empty_list_115 = []

# Mixed polymorphic structures
mixed_115 = {
    numbers: { value: num_115, list: [num_115, num_115], float: frac },
    strings: { value: str_115, list: [str_115, str_115] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_115 },
    },
    computations: {
        from_num: num_115 * 100,
        from_frac: frac_115 * 10.0,
        list_from_num: [num_115, num_115, num_115],
    },
}

x_116 = 3.14
y_116 = 1.23e45
z_116 = 0.5

my_str_116 : Str
my_str_116 = "one"

binops_116 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_116 : U64 -> U64
add_one_116 = |n| n + 1

map_add_one_116 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_116 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_116 = |arg_one, arg_two| arg_one * arg_two

num_116 = 42
frac_116 = 4.2
str_116 = "hello"

# Polymorphic empty collections
empty_list_116 = []

# Mixed polymorphic structures
mixed_116 = {
    numbers: { value: num_116, list: [num_116, num_116], float: frac },
    strings: { value: str_116, list: [str_116, str_116] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_116 },
    },
    computations: {
        from_num: num_116 * 100,
        from_frac: frac_116 * 10.0,
        list_from_num: [num_116, num_116, num_116],
    },
}

x_117 = 3.14
y_117 = 1.23e45
z_117 = 0.5

my_str_117 : Str
my_str_117 = "one"

binops_117 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_117 : U64 -> U64
add_one_117 = |n| n + 1

map_add_one_117 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_117 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_117 = |arg_one, arg_two| arg_one * arg_two

num_117 = 42
frac_117 = 4.2
str_117 = "hello"

# Polymorphic empty collections
empty_list_117 = []

# Mixed polymorphic structures
mixed_117 = {
    numbers: { value: num_117, list: [num_117, num_117], float: frac },
    strings: { value: str_117, list: [str_117, str_117] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_117 },
    },
    computations: {
        from_num: num_117 * 100,
        from_frac: frac_117 * 10.0,
        list_from_num: [num_117, num_117, num_117],
    },
}

x_118 = 3.14
y_118 = 1.23e45
z_118 = 0.5

my_str_118 : Str
my_str_118 = "one"

binops_118 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_118 : U64 -> U64
add_one_118 = |n| n + 1

map_add_one_118 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_118 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_118 = |arg_one, arg_two| arg_one * arg_two

num_118 = 42
frac_118 = 4.2
str_118 = "hello"

# Polymorphic empty collections
empty_list_118 = []

# Mixed polymorphic structures
mixed_118 = {
    numbers: { value: num_118, list: [num_118, num_118], float: frac },
    strings: { value: str_118, list: [str_118, str_118] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_118 },
    },
    computations: {
        from_num: num_118 * 100,
        from_frac: frac_118 * 10.0,
        list_from_num: [num_118, num_118, num_118],
    },
}

x_119 = 3.14
y_119 = 1.23e45
z_119 = 0.5

my_str_119 : Str
my_str_119 = "one"

binops_119 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_119 : U64 -> U64
add_one_119 = |n| n + 1

map_add_one_119 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_119 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_119 = |arg_one, arg_two| arg_one * arg_two

num_119 = 42
frac_119 = 4.2
str_119 = "hello"

# Polymorphic empty collections
empty_list_119 = []

# Mixed polymorphic structures
mixed_119 = {
    numbers: { value: num_119, list: [num_119, num_119], float: frac },
    strings: { value: str_119, list: [str_119, str_119] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_119 },
    },
    computations: {
        from_num: num_119 * 100,
        from_frac: frac_119 * 10.0,
        list_from_num: [num_119, num_119, num_119],
    },
}

x_120 = 3.14
y_120 = 1.23e45
z_120 = 0.5

my_str_120 : Str
my_str_120 = "one"

binops_120 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_120 : U64 -> U64
add_one_120 = |n| n + 1

map_add_one_120 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_120 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_120 = |arg_one, arg_two| arg_one * arg_two

num_120 = 42
frac_120 = 4.2
str_120 = "hello"

# Polymorphic empty collections
empty_list_120 = []

# Mixed polymorphic structures
mixed_120 = {
    numbers: { value: num_120, list: [num_120, num_120], float: frac },
    strings: { value: str_120, list: [str_120, str_120] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_120 },
    },
    computations: {
        from_num: num_120 * 100,
        from_frac: frac_120 * 10.0,
        list_from_num: [num_120, num_120, num_120],
    },
}

x_121 = 3.14
y_121 = 1.23e45
z_121 = 0.5

my_str_121 : Str
my_str_121 = "one"

binops_121 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_121 : U64 -> U64
add_one_121 = |n| n + 1

map_add_one_121 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_121 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_121 = |arg_one, arg_two| arg_one * arg_two

num_121 = 42
frac_121 = 4.2
str_121 = "hello"

# Polymorphic empty collections
empty_list_121 = []

# Mixed polymorphic structures
mixed_121 = {
    numbers: { value: num_121, list: [num_121, num_121], float: frac },
    strings: { value: str_121, list: [str_121, str_121] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_121 },
    },
    computations: {
        from_num: num_121 * 100,
        from_frac: frac_121 * 10.0,
        list_from_num: [num_121, num_121, num_121],
    },
}

x_122 = 3.14
y_122 = 1.23e45
z_122 = 0.5

my_str_122 : Str
my_str_122 = "one"

binops_122 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_122 : U64 -> U64
add_one_122 = |n| n + 1

map_add_one_122 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_122 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_122 = |arg_one, arg_two| arg_one * arg_two

num_122 = 42
frac_122 = 4.2
str_122 = "hello"

# Polymorphic empty collections
empty_list_122 = []

# Mixed polymorphic structures
mixed_122 = {
    numbers: { value: num_122, list: [num_122, num_122], float: frac },
    strings: { value: str_122, list: [str_122, str_122] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_122 },
    },
    computations: {
        from_num: num_122 * 100,
        from_frac: frac_122 * 10.0,
        list_from_num: [num_122, num_122, num_122],
    },
}

x_123 = 3.14
y_123 = 1.23e45
z_123 = 0.5

my_str_123 : Str
my_str_123 = "one"

binops_123 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_123 : U64 -> U64
add_one_123 = |n| n + 1

map_add_one_123 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_123 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_123 = |arg_one, arg_two| arg_one * arg_two

num_123 = 42
frac_123 = 4.2
str_123 = "hello"

# Polymorphic empty collections
empty_list_123 = []

# Mixed polymorphic structures
mixed_123 = {
    numbers: { value: num_123, list: [num_123, num_123], float: frac },
    strings: { value: str_123, list: [str_123, str_123] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_123 },
    },
    computations: {
        from_num: num_123 * 100,
        from_frac: frac_123 * 10.0,
        list_from_num: [num_123, num_123, num_123],
    },
}

x_124 = 3.14
y_124 = 1.23e45
z_124 = 0.5

my_str_124 : Str
my_str_124 = "one"

binops_124 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_124 : U64 -> U64
add_one_124 = |n| n + 1

map_add_one_124 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_124 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_124 = |arg_one, arg_two| arg_one * arg_two

num_124 = 42
frac_124 = 4.2
str_124 = "hello"

# Polymorphic empty collections
empty_list_124 = []

# Mixed polymorphic structures
mixed_124 = {
    numbers: { value: num_124, list: [num_124, num_124], float: frac },
    strings: { value: str_124, list: [str_124, str_124] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_124 },
    },
    computations: {
        from_num: num_124 * 100,
        from_frac: frac_124 * 10.0,
        list_from_num: [num_124, num_124, num_124],
    },
}

x_125 = 3.14
y_125 = 1.23e45
z_125 = 0.5

my_str_125 : Str
my_str_125 = "one"

binops_125 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_125 : U64 -> U64
add_one_125 = |n| n + 1

map_add_one_125 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_125 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_125 = |arg_one, arg_two| arg_one * arg_two

num_125 = 42
frac_125 = 4.2
str_125 = "hello"

# Polymorphic empty collections
empty_list_125 = []

# Mixed polymorphic structures
mixed_125 = {
    numbers: { value: num_125, list: [num_125, num_125], float: frac },
    strings: { value: str_125, list: [str_125, str_125] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_125 },
    },
    computations: {
        from_num: num_125 * 100,
        from_frac: frac_125 * 10.0,
        list_from_num: [num_125, num_125, num_125],
    },
}

x_126 = 3.14
y_126 = 1.23e45
z_126 = 0.5

my_str_126 : Str
my_str_126 = "one"

binops_126 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_126 : U64 -> U64
add_one_126 = |n| n + 1

map_add_one_126 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_126 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_126 = |arg_one, arg_two| arg_one * arg_two

num_126 = 42
frac_126 = 4.2
str_126 = "hello"

# Polymorphic empty collections
empty_list_126 = []

# Mixed polymorphic structures
mixed_126 = {
    numbers: { value: num_126, list: [num_126, num_126], float: frac },
    strings: { value: str_126, list: [str_126, str_126] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_126 },
    },
    computations: {
        from_num: num_126 * 100,
        from_frac: frac_126 * 10.0,
        list_from_num: [num_126, num_126, num_126],
    },
}

x_127 = 3.14
y_127 = 1.23e45
z_127 = 0.5

my_str_127 : Str
my_str_127 = "one"

binops_127 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_127 : U64 -> U64
add_one_127 = |n| n + 1

map_add_one_127 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_127 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_127 = |arg_one, arg_two| arg_one * arg_two

num_127 = 42
frac_127 = 4.2
str_127 = "hello"

# Polymorphic empty collections
empty_list_127 = []

# Mixed polymorphic structures
mixed_127 = {
    numbers: { value: num_127, list: [num_127, num_127], float: frac },
    strings: { value: str_127, list: [str_127, str_127] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_127 },
    },
    computations: {
        from_num: num_127 * 100,
        from_frac: frac_127 * 10.0,
        list_from_num: [num_127, num_127, num_127],
    },
}

x_128 = 3.14
y_128 = 1.23e45
z_128 = 0.5

my_str_128 : Str
my_str_128 = "one"

binops_128 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_128 : U64 -> U64
add_one_128 = |n| n + 1

map_add_one_128 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_128 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_128 = |arg_one, arg_two| arg_one * arg_two

num_128 = 42
frac_128 = 4.2
str_128 = "hello"

# Polymorphic empty collections
empty_list_128 = []

# Mixed polymorphic structures
mixed_128 = {
    numbers: { value: num_128, list: [num_128, num_128], float: frac },
    strings: { value: str_128, list: [str_128, str_128] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_128 },
    },
    computations: {
        from_num: num_128 * 100,
        from_frac: frac_128 * 10.0,
        list_from_num: [num_128, num_128, num_128],
    },
}

x_129 = 3.14
y_129 = 1.23e45
z_129 = 0.5

my_str_129 : Str
my_str_129 = "one"

binops_129 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_129 : U64 -> U64
add_one_129 = |n| n + 1

map_add_one_129 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_129 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_129 = |arg_one, arg_two| arg_one * arg_two

num_129 = 42
frac_129 = 4.2
str_129 = "hello"

# Polymorphic empty collections
empty_list_129 = []

# Mixed polymorphic structures
mixed_129 = {
    numbers: { value: num_129, list: [num_129, num_129], float: frac },
    strings: { value: str_129, list: [str_129, str_129] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_129 },
    },
    computations: {
        from_num: num_129 * 100,
        from_frac: frac_129 * 10.0,
        list_from_num: [num_129, num_129, num_129],
    },
}

x_130 = 3.14
y_130 = 1.23e45
z_130 = 0.5

my_str_130 : Str
my_str_130 = "one"

binops_130 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_130 : U64 -> U64
add_one_130 = |n| n + 1

map_add_one_130 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_130 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_130 = |arg_one, arg_two| arg_one * arg_two

num_130 = 42
frac_130 = 4.2
str_130 = "hello"

# Polymorphic empty collections
empty_list_130 = []

# Mixed polymorphic structures
mixed_130 = {
    numbers: { value: num_130, list: [num_130, num_130], float: frac },
    strings: { value: str_130, list: [str_130, str_130] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_130 },
    },
    computations: {
        from_num: num_130 * 100,
        from_frac: frac_130 * 10.0,
        list_from_num: [num_130, num_130, num_130],
    },
}

x_131 = 3.14
y_131 = 1.23e45
z_131 = 0.5

my_str_131 : Str
my_str_131 = "one"

binops_131 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_131 : U64 -> U64
add_one_131 = |n| n + 1

map_add_one_131 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_131 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_131 = |arg_one, arg_two| arg_one * arg_two

num_131 = 42
frac_131 = 4.2
str_131 = "hello"

# Polymorphic empty collections
empty_list_131 = []

# Mixed polymorphic structures
mixed_131 = {
    numbers: { value: num_131, list: [num_131, num_131], float: frac },
    strings: { value: str_131, list: [str_131, str_131] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_131 },
    },
    computations: {
        from_num: num_131 * 100,
        from_frac: frac_131 * 10.0,
        list_from_num: [num_131, num_131, num_131],
    },
}

x_132 = 3.14
y_132 = 1.23e45
z_132 = 0.5

my_str_132 : Str
my_str_132 = "one"

binops_132 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_132 : U64 -> U64
add_one_132 = |n| n + 1

map_add_one_132 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_132 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_132 = |arg_one, arg_two| arg_one * arg_two

num_132 = 42
frac_132 = 4.2
str_132 = "hello"

# Polymorphic empty collections
empty_list_132 = []

# Mixed polymorphic structures
mixed_132 = {
    numbers: { value: num_132, list: [num_132, num_132], float: frac },
    strings: { value: str_132, list: [str_132, str_132] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_132 },
    },
    computations: {
        from_num: num_132 * 100,
        from_frac: frac_132 * 10.0,
        list_from_num: [num_132, num_132, num_132],
    },
}

x_133 = 3.14
y_133 = 1.23e45
z_133 = 0.5

my_str_133 : Str
my_str_133 = "one"

binops_133 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_133 : U64 -> U64
add_one_133 = |n| n + 1

map_add_one_133 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_133 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_133 = |arg_one, arg_two| arg_one * arg_two

num_133 = 42
frac_133 = 4.2
str_133 = "hello"

# Polymorphic empty collections
empty_list_133 = []

# Mixed polymorphic structures
mixed_133 = {
    numbers: { value: num_133, list: [num_133, num_133], float: frac },
    strings: { value: str_133, list: [str_133, str_133] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_133 },
    },
    computations: {
        from_num: num_133 * 100,
        from_frac: frac_133 * 10.0,
        list_from_num: [num_133, num_133, num_133],
    },
}

x_134 = 3.14
y_134 = 1.23e45
z_134 = 0.5

my_str_134 : Str
my_str_134 = "one"

binops_134 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_134 : U64 -> U64
add_one_134 = |n| n + 1

map_add_one_134 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_134 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_134 = |arg_one, arg_two| arg_one * arg_two

num_134 = 42
frac_134 = 4.2
str_134 = "hello"

# Polymorphic empty collections
empty_list_134 = []

# Mixed polymorphic structures
mixed_134 = {
    numbers: { value: num_134, list: [num_134, num_134], float: frac },
    strings: { value: str_134, list: [str_134, str_134] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_134 },
    },
    computations: {
        from_num: num_134 * 100,
        from_frac: frac_134 * 10.0,
        list_from_num: [num_134, num_134, num_134],
    },
}

x_135 = 3.14
y_135 = 1.23e45
z_135 = 0.5

my_str_135 : Str
my_str_135 = "one"

binops_135 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_135 : U64 -> U64
add_one_135 = |n| n + 1

map_add_one_135 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_135 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_135 = |arg_one, arg_two| arg_one * arg_two

num_135 = 42
frac_135 = 4.2
str_135 = "hello"

# Polymorphic empty collections
empty_list_135 = []

# Mixed polymorphic structures
mixed_135 = {
    numbers: { value: num_135, list: [num_135, num_135], float: frac },
    strings: { value: str_135, list: [str_135, str_135] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_135 },
    },
    computations: {
        from_num: num_135 * 100,
        from_frac: frac_135 * 10.0,
        list_from_num: [num_135, num_135, num_135],
    },
}

x_136 = 3.14
y_136 = 1.23e45
z_136 = 0.5

my_str_136 : Str
my_str_136 = "one"

binops_136 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_136 : U64 -> U64
add_one_136 = |n| n + 1

map_add_one_136 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_136 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_136 = |arg_one, arg_two| arg_one * arg_two

num_136 = 42
frac_136 = 4.2
str_136 = "hello"

# Polymorphic empty collections
empty_list_136 = []

# Mixed polymorphic structures
mixed_136 = {
    numbers: { value: num_136, list: [num_136, num_136], float: frac },
    strings: { value: str_136, list: [str_136, str_136] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_136 },
    },
    computations: {
        from_num: num_136 * 100,
        from_frac: frac_136 * 10.0,
        list_from_num: [num_136, num_136, num_136],
    },
}

x_137 = 3.14
y_137 = 1.23e45
z_137 = 0.5

my_str_137 : Str
my_str_137 = "one"

binops_137 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_137 : U64 -> U64
add_one_137 = |n| n + 1

map_add_one_137 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_137 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_137 = |arg_one, arg_two| arg_one * arg_two

num_137 = 42
frac_137 = 4.2
str_137 = "hello"

# Polymorphic empty collections
empty_list_137 = []

# Mixed polymorphic structures
mixed_137 = {
    numbers: { value: num_137, list: [num_137, num_137], float: frac },
    strings: { value: str_137, list: [str_137, str_137] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_137 },
    },
    computations: {
        from_num: num_137 * 100,
        from_frac: frac_137 * 10.0,
        list_from_num: [num_137, num_137, num_137],
    },
}

x_138 = 3.14
y_138 = 1.23e45
z_138 = 0.5

my_str_138 : Str
my_str_138 = "one"

binops_138 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_138 : U64 -> U64
add_one_138 = |n| n + 1

map_add_one_138 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_138 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_138 = |arg_one, arg_two| arg_one * arg_two

num_138 = 42
frac_138 = 4.2
str_138 = "hello"

# Polymorphic empty collections
empty_list_138 = []

# Mixed polymorphic structures
mixed_138 = {
    numbers: { value: num_138, list: [num_138, num_138], float: frac },
    strings: { value: str_138, list: [str_138, str_138] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_138 },
    },
    computations: {
        from_num: num_138 * 100,
        from_frac: frac_138 * 10.0,
        list_from_num: [num_138, num_138, num_138],
    },
}

x_139 = 3.14
y_139 = 1.23e45
z_139 = 0.5

my_str_139 : Str
my_str_139 = "one"

binops_139 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_139 : U64 -> U64
add_one_139 = |n| n + 1

map_add_one_139 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_139 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_139 = |arg_one, arg_two| arg_one * arg_two

num_139 = 42
frac_139 = 4.2
str_139 = "hello"

# Polymorphic empty collections
empty_list_139 = []

# Mixed polymorphic structures
mixed_139 = {
    numbers: { value: num_139, list: [num_139, num_139], float: frac },
    strings: { value: str_139, list: [str_139, str_139] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_139 },
    },
    computations: {
        from_num: num_139 * 100,
        from_frac: frac_139 * 10.0,
        list_from_num: [num_139, num_139, num_139],
    },
}

x_140 = 3.14
y_140 = 1.23e45
z_140 = 0.5

my_str_140 : Str
my_str_140 = "one"

binops_140 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_140 : U64 -> U64
add_one_140 = |n| n + 1

map_add_one_140 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_140 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_140 = |arg_one, arg_two| arg_one * arg_two

num_140 = 42
frac_140 = 4.2
str_140 = "hello"

# Polymorphic empty collections
empty_list_140 = []

# Mixed polymorphic structures
mixed_140 = {
    numbers: { value: num_140, list: [num_140, num_140], float: frac },
    strings: { value: str_140, list: [str_140, str_140] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_140 },
    },
    computations: {
        from_num: num_140 * 100,
        from_frac: frac_140 * 10.0,
        list_from_num: [num_140, num_140, num_140],
    },
}

x_141 = 3.14
y_141 = 1.23e45
z_141 = 0.5

my_str_141 : Str
my_str_141 = "one"

binops_141 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_141 : U64 -> U64
add_one_141 = |n| n + 1

map_add_one_141 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_141 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_141 = |arg_one, arg_two| arg_one * arg_two

num_141 = 42
frac_141 = 4.2
str_141 = "hello"

# Polymorphic empty collections
empty_list_141 = []

# Mixed polymorphic structures
mixed_141 = {
    numbers: { value: num_141, list: [num_141, num_141], float: frac },
    strings: { value: str_141, list: [str_141, str_141] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_141 },
    },
    computations: {
        from_num: num_141 * 100,
        from_frac: frac_141 * 10.0,
        list_from_num: [num_141, num_141, num_141],
    },
}

x_142 = 3.14
y_142 = 1.23e45
z_142 = 0.5

my_str_142 : Str
my_str_142 = "one"

binops_142 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_142 : U64 -> U64
add_one_142 = |n| n + 1

map_add_one_142 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_142 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_142 = |arg_one, arg_two| arg_one * arg_two

num_142 = 42
frac_142 = 4.2
str_142 = "hello"

# Polymorphic empty collections
empty_list_142 = []

# Mixed polymorphic structures
mixed_142 = {
    numbers: { value: num_142, list: [num_142, num_142], float: frac },
    strings: { value: str_142, list: [str_142, str_142] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_142 },
    },
    computations: {
        from_num: num_142 * 100,
        from_frac: frac_142 * 10.0,
        list_from_num: [num_142, num_142, num_142],
    },
}

x_143 = 3.14
y_143 = 1.23e45
z_143 = 0.5

my_str_143 : Str
my_str_143 = "one"

binops_143 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_143 : U64 -> U64
add_one_143 = |n| n + 1

map_add_one_143 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_143 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_143 = |arg_one, arg_two| arg_one * arg_two

num_143 = 42
frac_143 = 4.2
str_143 = "hello"

# Polymorphic empty collections
empty_list_143 = []

# Mixed polymorphic structures
mixed_143 = {
    numbers: { value: num_143, list: [num_143, num_143], float: frac },
    strings: { value: str_143, list: [str_143, str_143] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_143 },
    },
    computations: {
        from_num: num_143 * 100,
        from_frac: frac_143 * 10.0,
        list_from_num: [num_143, num_143, num_143],
    },
}

x_144 = 3.14
y_144 = 1.23e45
z_144 = 0.5

my_str_144 : Str
my_str_144 = "one"

binops_144 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_144 : U64 -> U64
add_one_144 = |n| n + 1

map_add_one_144 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_144 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_144 = |arg_one, arg_two| arg_one * arg_two

num_144 = 42
frac_144 = 4.2
str_144 = "hello"

# Polymorphic empty collections
empty_list_144 = []

# Mixed polymorphic structures
mixed_144 = {
    numbers: { value: num_144, list: [num_144, num_144], float: frac },
    strings: { value: str_144, list: [str_144, str_144] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_144 },
    },
    computations: {
        from_num: num_144 * 100,
        from_frac: frac_144 * 10.0,
        list_from_num: [num_144, num_144, num_144],
    },
}

x_145 = 3.14
y_145 = 1.23e45
z_145 = 0.5

my_str_145 : Str
my_str_145 = "one"

binops_145 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_145 : U64 -> U64
add_one_145 = |n| n + 1

map_add_one_145 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_145 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_145 = |arg_one, arg_two| arg_one * arg_two

num_145 = 42
frac_145 = 4.2
str_145 = "hello"

# Polymorphic empty collections
empty_list_145 = []

# Mixed polymorphic structures
mixed_145 = {
    numbers: { value: num_145, list: [num_145, num_145], float: frac },
    strings: { value: str_145, list: [str_145, str_145] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_145 },
    },
    computations: {
        from_num: num_145 * 100,
        from_frac: frac_145 * 10.0,
        list_from_num: [num_145, num_145, num_145],
    },
}

x_146 = 3.14
y_146 = 1.23e45
z_146 = 0.5

my_str_146 : Str
my_str_146 = "one"

binops_146 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_146 : U64 -> U64
add_one_146 = |n| n + 1

map_add_one_146 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_146 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_146 = |arg_one, arg_two| arg_one * arg_two

num_146 = 42
frac_146 = 4.2
str_146 = "hello"

# Polymorphic empty collections
empty_list_146 = []

# Mixed polymorphic structures
mixed_146 = {
    numbers: { value: num_146, list: [num_146, num_146], float: frac },
    strings: { value: str_146, list: [str_146, str_146] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_146 },
    },
    computations: {
        from_num: num_146 * 100,
        from_frac: frac_146 * 10.0,
        list_from_num: [num_146, num_146, num_146],
    },
}

x_147 = 3.14
y_147 = 1.23e45
z_147 = 0.5

my_str_147 : Str
my_str_147 = "one"

binops_147 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_147 : U64 -> U64
add_one_147 = |n| n + 1

map_add_one_147 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_147 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_147 = |arg_one, arg_two| arg_one * arg_two

num_147 = 42
frac_147 = 4.2
str_147 = "hello"

# Polymorphic empty collections
empty_list_147 = []

# Mixed polymorphic structures
mixed_147 = {
    numbers: { value: num_147, list: [num_147, num_147], float: frac },
    strings: { value: str_147, list: [str_147, str_147] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_147 },
    },
    computations: {
        from_num: num_147 * 100,
        from_frac: frac_147 * 10.0,
        list_from_num: [num_147, num_147, num_147],
    },
}

x_148 = 3.14
y_148 = 1.23e45
z_148 = 0.5

my_str_148 : Str
my_str_148 = "one"

binops_148 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_148 : U64 -> U64
add_one_148 = |n| n + 1

map_add_one_148 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_148 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_148 = |arg_one, arg_two| arg_one * arg_two

num_148 = 42
frac_148 = 4.2
str_148 = "hello"

# Polymorphic empty collections
empty_list_148 = []

# Mixed polymorphic structures
mixed_148 = {
    numbers: { value: num_148, list: [num_148, num_148], float: frac },
    strings: { value: str_148, list: [str_148, str_148] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_148 },
    },
    computations: {
        from_num: num_148 * 100,
        from_frac: frac_148 * 10.0,
        list_from_num: [num_148, num_148, num_148],
    },
}

x_149 = 3.14
y_149 = 1.23e45
z_149 = 0.5

my_str_149 : Str
my_str_149 = "one"

binops_149 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_149 : U64 -> U64
add_one_149 = |n| n + 1

map_add_one_149 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_149 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_149 = |arg_one, arg_two| arg_one * arg_two

num_149 = 42
frac_149 = 4.2
str_149 = "hello"

# Polymorphic empty collections
empty_list_149 = []

# Mixed polymorphic structures
mixed_149 = {
    numbers: { value: num_149, list: [num_149, num_149], float: frac },
    strings: { value: str_149, list: [str_149, str_149] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_149 },
    },
    computations: {
        from_num: num_149 * 100,
        from_frac: frac_149 * 10.0,
        list_from_num: [num_149, num_149, num_149],
    },
}

x_150 = 3.14
y_150 = 1.23e45
z_150 = 0.5

my_str_150 : Str
my_str_150 = "one"

binops_150 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_150 : U64 -> U64
add_one_150 = |n| n + 1

map_add_one_150 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_150 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_150 = |arg_one, arg_two| arg_one * arg_two

num_150 = 42
frac_150 = 4.2
str_150 = "hello"

# Polymorphic empty collections
empty_list_150 = []

# Mixed polymorphic structures
mixed_150 = {
    numbers: { value: num_150, list: [num_150, num_150], float: frac },
    strings: { value: str_150, list: [str_150, str_150] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_150 },
    },
    computations: {
        from_num: num_150 * 100,
        from_frac: frac_150 * 10.0,
        list_from_num: [num_150, num_150, num_150],
    },
}

x_151 = 3.14
y_151 = 1.23e45
z_151 = 0.5

my_str_151 : Str
my_str_151 = "one"

binops_151 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_151 : U64 -> U64
add_one_151 = |n| n + 1

map_add_one_151 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_151 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_151 = |arg_one, arg_two| arg_one * arg_two

num_151 = 42
frac_151 = 4.2
str_151 = "hello"

# Polymorphic empty collections
empty_list_151 = []

# Mixed polymorphic structures
mixed_151 = {
    numbers: { value: num_151, list: [num_151, num_151], float: frac },
    strings: { value: str_151, list: [str_151, str_151] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_151 },
    },
    computations: {
        from_num: num_151 * 100,
        from_frac: frac_151 * 10.0,
        list_from_num: [num_151, num_151, num_151],
    },
}

x_152 = 3.14
y_152 = 1.23e45
z_152 = 0.5

my_str_152 : Str
my_str_152 = "one"

binops_152 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_152 : U64 -> U64
add_one_152 = |n| n + 1

map_add_one_152 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_152 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_152 = |arg_one, arg_two| arg_one * arg_two

num_152 = 42
frac_152 = 4.2
str_152 = "hello"

# Polymorphic empty collections
empty_list_152 = []

# Mixed polymorphic structures
mixed_152 = {
    numbers: { value: num_152, list: [num_152, num_152], float: frac },
    strings: { value: str_152, list: [str_152, str_152] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_152 },
    },
    computations: {
        from_num: num_152 * 100,
        from_frac: frac_152 * 10.0,
        list_from_num: [num_152, num_152, num_152],
    },
}

x_153 = 3.14
y_153 = 1.23e45
z_153 = 0.5

my_str_153 : Str
my_str_153 = "one"

binops_153 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_153 : U64 -> U64
add_one_153 = |n| n + 1

map_add_one_153 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_153 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_153 = |arg_one, arg_two| arg_one * arg_two

num_153 = 42
frac_153 = 4.2
str_153 = "hello"

# Polymorphic empty collections
empty_list_153 = []

# Mixed polymorphic structures
mixed_153 = {
    numbers: { value: num_153, list: [num_153, num_153], float: frac },
    strings: { value: str_153, list: [str_153, str_153] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_153 },
    },
    computations: {
        from_num: num_153 * 100,
        from_frac: frac_153 * 10.0,
        list_from_num: [num_153, num_153, num_153],
    },
}

x_154 = 3.14
y_154 = 1.23e45
z_154 = 0.5

my_str_154 : Str
my_str_154 = "one"

binops_154 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_154 : U64 -> U64
add_one_154 = |n| n + 1

map_add_one_154 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_154 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_154 = |arg_one, arg_two| arg_one * arg_two

num_154 = 42
frac_154 = 4.2
str_154 = "hello"

# Polymorphic empty collections
empty_list_154 = []

# Mixed polymorphic structures
mixed_154 = {
    numbers: { value: num_154, list: [num_154, num_154], float: frac },
    strings: { value: str_154, list: [str_154, str_154] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_154 },
    },
    computations: {
        from_num: num_154 * 100,
        from_frac: frac_154 * 10.0,
        list_from_num: [num_154, num_154, num_154],
    },
}

x_155 = 3.14
y_155 = 1.23e45
z_155 = 0.5

my_str_155 : Str
my_str_155 = "one"

binops_155 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_155 : U64 -> U64
add_one_155 = |n| n + 1

map_add_one_155 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_155 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_155 = |arg_one, arg_two| arg_one * arg_two

num_155 = 42
frac_155 = 4.2
str_155 = "hello"

# Polymorphic empty collections
empty_list_155 = []

# Mixed polymorphic structures
mixed_155 = {
    numbers: { value: num_155, list: [num_155, num_155], float: frac },
    strings: { value: str_155, list: [str_155, str_155] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_155 },
    },
    computations: {
        from_num: num_155 * 100,
        from_frac: frac_155 * 10.0,
        list_from_num: [num_155, num_155, num_155],
    },
}

x_156 = 3.14
y_156 = 1.23e45
z_156 = 0.5

my_str_156 : Str
my_str_156 = "one"

binops_156 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_156 : U64 -> U64
add_one_156 = |n| n + 1

map_add_one_156 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_156 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_156 = |arg_one, arg_two| arg_one * arg_two

num_156 = 42
frac_156 = 4.2
str_156 = "hello"

# Polymorphic empty collections
empty_list_156 = []

# Mixed polymorphic structures
mixed_156 = {
    numbers: { value: num_156, list: [num_156, num_156], float: frac },
    strings: { value: str_156, list: [str_156, str_156] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_156 },
    },
    computations: {
        from_num: num_156 * 100,
        from_frac: frac_156 * 10.0,
        list_from_num: [num_156, num_156, num_156],
    },
}

x_157 = 3.14
y_157 = 1.23e45
z_157 = 0.5

my_str_157 : Str
my_str_157 = "one"

binops_157 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_157 : U64 -> U64
add_one_157 = |n| n + 1

map_add_one_157 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_157 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_157 = |arg_one, arg_two| arg_one * arg_two

num_157 = 42
frac_157 = 4.2
str_157 = "hello"

# Polymorphic empty collections
empty_list_157 = []

# Mixed polymorphic structures
mixed_157 = {
    numbers: { value: num_157, list: [num_157, num_157], float: frac },
    strings: { value: str_157, list: [str_157, str_157] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_157 },
    },
    computations: {
        from_num: num_157 * 100,
        from_frac: frac_157 * 10.0,
        list_from_num: [num_157, num_157, num_157],
    },
}

x_158 = 3.14
y_158 = 1.23e45
z_158 = 0.5

my_str_158 : Str
my_str_158 = "one"

binops_158 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_158 : U64 -> U64
add_one_158 = |n| n + 1

map_add_one_158 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_158 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_158 = |arg_one, arg_two| arg_one * arg_two

num_158 = 42
frac_158 = 4.2
str_158 = "hello"

# Polymorphic empty collections
empty_list_158 = []

# Mixed polymorphic structures
mixed_158 = {
    numbers: { value: num_158, list: [num_158, num_158], float: frac },
    strings: { value: str_158, list: [str_158, str_158] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_158 },
    },
    computations: {
        from_num: num_158 * 100,
        from_frac: frac_158 * 10.0,
        list_from_num: [num_158, num_158, num_158],
    },
}

x_159 = 3.14
y_159 = 1.23e45
z_159 = 0.5

my_str_159 : Str
my_str_159 = "one"

binops_159 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_159 : U64 -> U64
add_one_159 = |n| n + 1

map_add_one_159 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_159 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_159 = |arg_one, arg_two| arg_one * arg_two

num_159 = 42
frac_159 = 4.2
str_159 = "hello"

# Polymorphic empty collections
empty_list_159 = []

# Mixed polymorphic structures
mixed_159 = {
    numbers: { value: num_159, list: [num_159, num_159], float: frac },
    strings: { value: str_159, list: [str_159, str_159] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_159 },
    },
    computations: {
        from_num: num_159 * 100,
        from_frac: frac_159 * 10.0,
        list_from_num: [num_159, num_159, num_159],
    },
}

x_160 = 3.14
y_160 = 1.23e45
z_160 = 0.5

my_str_160 : Str
my_str_160 = "one"

binops_160 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_160 : U64 -> U64
add_one_160 = |n| n + 1

map_add_one_160 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_160 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_160 = |arg_one, arg_two| arg_one * arg_two

num_160 = 42
frac_160 = 4.2
str_160 = "hello"

# Polymorphic empty collections
empty_list_160 = []

# Mixed polymorphic structures
mixed_160 = {
    numbers: { value: num_160, list: [num_160, num_160], float: frac },
    strings: { value: str_160, list: [str_160, str_160] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_160 },
    },
    computations: {
        from_num: num_160 * 100,
        from_frac: frac_160 * 10.0,
        list_from_num: [num_160, num_160, num_160],
    },
}

x_161 = 3.14
y_161 = 1.23e45
z_161 = 0.5

my_str_161 : Str
my_str_161 = "one"

binops_161 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_161 : U64 -> U64
add_one_161 = |n| n + 1

map_add_one_161 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_161 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_161 = |arg_one, arg_two| arg_one * arg_two

num_161 = 42
frac_161 = 4.2
str_161 = "hello"

# Polymorphic empty collections
empty_list_161 = []

# Mixed polymorphic structures
mixed_161 = {
    numbers: { value: num_161, list: [num_161, num_161], float: frac },
    strings: { value: str_161, list: [str_161, str_161] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_161 },
    },
    computations: {
        from_num: num_161 * 100,
        from_frac: frac_161 * 10.0,
        list_from_num: [num_161, num_161, num_161],
    },
}

x_162 = 3.14
y_162 = 1.23e45
z_162 = 0.5

my_str_162 : Str
my_str_162 = "one"

binops_162 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_162 : U64 -> U64
add_one_162 = |n| n + 1

map_add_one_162 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_162 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_162 = |arg_one, arg_two| arg_one * arg_two

num_162 = 42
frac_162 = 4.2
str_162 = "hello"

# Polymorphic empty collections
empty_list_162 = []

# Mixed polymorphic structures
mixed_162 = {
    numbers: { value: num_162, list: [num_162, num_162], float: frac },
    strings: { value: str_162, list: [str_162, str_162] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_162 },
    },
    computations: {
        from_num: num_162 * 100,
        from_frac: frac_162 * 10.0,
        list_from_num: [num_162, num_162, num_162],
    },
}

x_163 = 3.14
y_163 = 1.23e45
z_163 = 0.5

my_str_163 : Str
my_str_163 = "one"

binops_163 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_163 : U64 -> U64
add_one_163 = |n| n + 1

map_add_one_163 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_163 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_163 = |arg_one, arg_two| arg_one * arg_two

num_163 = 42
frac_163 = 4.2
str_163 = "hello"

# Polymorphic empty collections
empty_list_163 = []

# Mixed polymorphic structures
mixed_163 = {
    numbers: { value: num_163, list: [num_163, num_163], float: frac },
    strings: { value: str_163, list: [str_163, str_163] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_163 },
    },
    computations: {
        from_num: num_163 * 100,
        from_frac: frac_163 * 10.0,
        list_from_num: [num_163, num_163, num_163],
    },
}

x_164 = 3.14
y_164 = 1.23e45
z_164 = 0.5

my_str_164 : Str
my_str_164 = "one"

binops_164 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_164 : U64 -> U64
add_one_164 = |n| n + 1

map_add_one_164 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_164 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_164 = |arg_one, arg_two| arg_one * arg_two

num_164 = 42
frac_164 = 4.2
str_164 = "hello"

# Polymorphic empty collections
empty_list_164 = []

# Mixed polymorphic structures
mixed_164 = {
    numbers: { value: num_164, list: [num_164, num_164], float: frac },
    strings: { value: str_164, list: [str_164, str_164] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_164 },
    },
    computations: {
        from_num: num_164 * 100,
        from_frac: frac_164 * 10.0,
        list_from_num: [num_164, num_164, num_164],
    },
}

x_165 = 3.14
y_165 = 1.23e45
z_165 = 0.5

my_str_165 : Str
my_str_165 = "one"

binops_165 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_165 : U64 -> U64
add_one_165 = |n| n + 1

map_add_one_165 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_165 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_165 = |arg_one, arg_two| arg_one * arg_two

num_165 = 42
frac_165 = 4.2
str_165 = "hello"

# Polymorphic empty collections
empty_list_165 = []

# Mixed polymorphic structures
mixed_165 = {
    numbers: { value: num_165, list: [num_165, num_165], float: frac },
    strings: { value: str_165, list: [str_165, str_165] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_165 },
    },
    computations: {
        from_num: num_165 * 100,
        from_frac: frac_165 * 10.0,
        list_from_num: [num_165, num_165, num_165],
    },
}

x_166 = 3.14
y_166 = 1.23e45
z_166 = 0.5

my_str_166 : Str
my_str_166 = "one"

binops_166 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_166 : U64 -> U64
add_one_166 = |n| n + 1

map_add_one_166 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_166 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_166 = |arg_one, arg_two| arg_one * arg_two

num_166 = 42
frac_166 = 4.2
str_166 = "hello"

# Polymorphic empty collections
empty_list_166 = []

# Mixed polymorphic structures
mixed_166 = {
    numbers: { value: num_166, list: [num_166, num_166], float: frac },
    strings: { value: str_166, list: [str_166, str_166] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_166 },
    },
    computations: {
        from_num: num_166 * 100,
        from_frac: frac_166 * 10.0,
        list_from_num: [num_166, num_166, num_166],
    },
}

x_167 = 3.14
y_167 = 1.23e45
z_167 = 0.5

my_str_167 : Str
my_str_167 = "one"

binops_167 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_167 : U64 -> U64
add_one_167 = |n| n + 1

map_add_one_167 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_167 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_167 = |arg_one, arg_two| arg_one * arg_two

num_167 = 42
frac_167 = 4.2
str_167 = "hello"

# Polymorphic empty collections
empty_list_167 = []

# Mixed polymorphic structures
mixed_167 = {
    numbers: { value: num_167, list: [num_167, num_167], float: frac },
    strings: { value: str_167, list: [str_167, str_167] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_167 },
    },
    computations: {
        from_num: num_167 * 100,
        from_frac: frac_167 * 10.0,
        list_from_num: [num_167, num_167, num_167],
    },
}

x_168 = 3.14
y_168 = 1.23e45
z_168 = 0.5

my_str_168 : Str
my_str_168 = "one"

binops_168 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_168 : U64 -> U64
add_one_168 = |n| n + 1

map_add_one_168 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_168 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_168 = |arg_one, arg_two| arg_one * arg_two

num_168 = 42
frac_168 = 4.2
str_168 = "hello"

# Polymorphic empty collections
empty_list_168 = []

# Mixed polymorphic structures
mixed_168 = {
    numbers: { value: num_168, list: [num_168, num_168], float: frac },
    strings: { value: str_168, list: [str_168, str_168] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_168 },
    },
    computations: {
        from_num: num_168 * 100,
        from_frac: frac_168 * 10.0,
        list_from_num: [num_168, num_168, num_168],
    },
}

x_169 = 3.14
y_169 = 1.23e45
z_169 = 0.5

my_str_169 : Str
my_str_169 = "one"

binops_169 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_169 : U64 -> U64
add_one_169 = |n| n + 1

map_add_one_169 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_169 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_169 = |arg_one, arg_two| arg_one * arg_two

num_169 = 42
frac_169 = 4.2
str_169 = "hello"

# Polymorphic empty collections
empty_list_169 = []

# Mixed polymorphic structures
mixed_169 = {
    numbers: { value: num_169, list: [num_169, num_169], float: frac },
    strings: { value: str_169, list: [str_169, str_169] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_169 },
    },
    computations: {
        from_num: num_169 * 100,
        from_frac: frac_169 * 10.0,
        list_from_num: [num_169, num_169, num_169],
    },
}

x_170 = 3.14
y_170 = 1.23e45
z_170 = 0.5

my_str_170 : Str
my_str_170 = "one"

binops_170 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_170 : U64 -> U64
add_one_170 = |n| n + 1

map_add_one_170 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_170 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_170 = |arg_one, arg_two| arg_one * arg_two

num_170 = 42
frac_170 = 4.2
str_170 = "hello"

# Polymorphic empty collections
empty_list_170 = []

# Mixed polymorphic structures
mixed_170 = {
    numbers: { value: num_170, list: [num_170, num_170], float: frac },
    strings: { value: str_170, list: [str_170, str_170] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_170 },
    },
    computations: {
        from_num: num_170 * 100,
        from_frac: frac_170 * 10.0,
        list_from_num: [num_170, num_170, num_170],
    },
}

x_171 = 3.14
y_171 = 1.23e45
z_171 = 0.5

my_str_171 : Str
my_str_171 = "one"

binops_171 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_171 : U64 -> U64
add_one_171 = |n| n + 1

map_add_one_171 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_171 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_171 = |arg_one, arg_two| arg_one * arg_two

num_171 = 42
frac_171 = 4.2
str_171 = "hello"

# Polymorphic empty collections
empty_list_171 = []

# Mixed polymorphic structures
mixed_171 = {
    numbers: { value: num_171, list: [num_171, num_171], float: frac },
    strings: { value: str_171, list: [str_171, str_171] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_171 },
    },
    computations: {
        from_num: num_171 * 100,
        from_frac: frac_171 * 10.0,
        list_from_num: [num_171, num_171, num_171],
    },
}

x_172 = 3.14
y_172 = 1.23e45
z_172 = 0.5

my_str_172 : Str
my_str_172 = "one"

binops_172 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_172 : U64 -> U64
add_one_172 = |n| n + 1

map_add_one_172 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_172 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_172 = |arg_one, arg_two| arg_one * arg_two

num_172 = 42
frac_172 = 4.2
str_172 = "hello"

# Polymorphic empty collections
empty_list_172 = []

# Mixed polymorphic structures
mixed_172 = {
    numbers: { value: num_172, list: [num_172, num_172], float: frac },
    strings: { value: str_172, list: [str_172, str_172] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_172 },
    },
    computations: {
        from_num: num_172 * 100,
        from_frac: frac_172 * 10.0,
        list_from_num: [num_172, num_172, num_172],
    },
}

x_173 = 3.14
y_173 = 1.23e45
z_173 = 0.5

my_str_173 : Str
my_str_173 = "one"

binops_173 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_173 : U64 -> U64
add_one_173 = |n| n + 1

map_add_one_173 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_173 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_173 = |arg_one, arg_two| arg_one * arg_two

num_173 = 42
frac_173 = 4.2
str_173 = "hello"

# Polymorphic empty collections
empty_list_173 = []

# Mixed polymorphic structures
mixed_173 = {
    numbers: { value: num_173, list: [num_173, num_173], float: frac },
    strings: { value: str_173, list: [str_173, str_173] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_173 },
    },
    computations: {
        from_num: num_173 * 100,
        from_frac: frac_173 * 10.0,
        list_from_num: [num_173, num_173, num_173],
    },
}

x_174 = 3.14
y_174 = 1.23e45
z_174 = 0.5

my_str_174 : Str
my_str_174 = "one"

binops_174 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_174 : U64 -> U64
add_one_174 = |n| n + 1

map_add_one_174 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_174 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_174 = |arg_one, arg_two| arg_one * arg_two

num_174 = 42
frac_174 = 4.2
str_174 = "hello"

# Polymorphic empty collections
empty_list_174 = []

# Mixed polymorphic structures
mixed_174 = {
    numbers: { value: num_174, list: [num_174, num_174], float: frac },
    strings: { value: str_174, list: [str_174, str_174] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_174 },
    },
    computations: {
        from_num: num_174 * 100,
        from_frac: frac_174 * 10.0,
        list_from_num: [num_174, num_174, num_174],
    },
}

x_175 = 3.14
y_175 = 1.23e45
z_175 = 0.5

my_str_175 : Str
my_str_175 = "one"

binops_175 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_175 : U64 -> U64
add_one_175 = |n| n + 1

map_add_one_175 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_175 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_175 = |arg_one, arg_two| arg_one * arg_two

num_175 = 42
frac_175 = 4.2
str_175 = "hello"

# Polymorphic empty collections
empty_list_175 = []

# Mixed polymorphic structures
mixed_175 = {
    numbers: { value: num_175, list: [num_175, num_175], float: frac },
    strings: { value: str_175, list: [str_175, str_175] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_175 },
    },
    computations: {
        from_num: num_175 * 100,
        from_frac: frac_175 * 10.0,
        list_from_num: [num_175, num_175, num_175],
    },
}

x_176 = 3.14
y_176 = 1.23e45
z_176 = 0.5

my_str_176 : Str
my_str_176 = "one"

binops_176 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_176 : U64 -> U64
add_one_176 = |n| n + 1

map_add_one_176 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_176 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_176 = |arg_one, arg_two| arg_one * arg_two

num_176 = 42
frac_176 = 4.2
str_176 = "hello"

# Polymorphic empty collections
empty_list_176 = []

# Mixed polymorphic structures
mixed_176 = {
    numbers: { value: num_176, list: [num_176, num_176], float: frac },
    strings: { value: str_176, list: [str_176, str_176] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_176 },
    },
    computations: {
        from_num: num_176 * 100,
        from_frac: frac_176 * 10.0,
        list_from_num: [num_176, num_176, num_176],
    },
}

x_177 = 3.14
y_177 = 1.23e45
z_177 = 0.5

my_str_177 : Str
my_str_177 = "one"

binops_177 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_177 : U64 -> U64
add_one_177 = |n| n + 1

map_add_one_177 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_177 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_177 = |arg_one, arg_two| arg_one * arg_two

num_177 = 42
frac_177 = 4.2
str_177 = "hello"

# Polymorphic empty collections
empty_list_177 = []

# Mixed polymorphic structures
mixed_177 = {
    numbers: { value: num_177, list: [num_177, num_177], float: frac },
    strings: { value: str_177, list: [str_177, str_177] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_177 },
    },
    computations: {
        from_num: num_177 * 100,
        from_frac: frac_177 * 10.0,
        list_from_num: [num_177, num_177, num_177],
    },
}

x_178 = 3.14
y_178 = 1.23e45
z_178 = 0.5

my_str_178 : Str
my_str_178 = "one"

binops_178 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_178 : U64 -> U64
add_one_178 = |n| n + 1

map_add_one_178 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_178 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_178 = |arg_one, arg_two| arg_one * arg_two

num_178 = 42
frac_178 = 4.2
str_178 = "hello"

# Polymorphic empty collections
empty_list_178 = []

# Mixed polymorphic structures
mixed_178 = {
    numbers: { value: num_178, list: [num_178, num_178], float: frac },
    strings: { value: str_178, list: [str_178, str_178] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_178 },
    },
    computations: {
        from_num: num_178 * 100,
        from_frac: frac_178 * 10.0,
        list_from_num: [num_178, num_178, num_178],
    },
}

x_179 = 3.14
y_179 = 1.23e45
z_179 = 0.5

my_str_179 : Str
my_str_179 = "one"

binops_179 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_179 : U64 -> U64
add_one_179 = |n| n + 1

map_add_one_179 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_179 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_179 = |arg_one, arg_two| arg_one * arg_two

num_179 = 42
frac_179 = 4.2
str_179 = "hello"

# Polymorphic empty collections
empty_list_179 = []

# Mixed polymorphic structures
mixed_179 = {
    numbers: { value: num_179, list: [num_179, num_179], float: frac },
    strings: { value: str_179, list: [str_179, str_179] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_179 },
    },
    computations: {
        from_num: num_179 * 100,
        from_frac: frac_179 * 10.0,
        list_from_num: [num_179, num_179, num_179],
    },
}

x_180 = 3.14
y_180 = 1.23e45
z_180 = 0.5

my_str_180 : Str
my_str_180 = "one"

binops_180 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_180 : U64 -> U64
add_one_180 = |n| n + 1

map_add_one_180 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_180 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_180 = |arg_one, arg_two| arg_one * arg_two

num_180 = 42
frac_180 = 4.2
str_180 = "hello"

# Polymorphic empty collections
empty_list_180 = []

# Mixed polymorphic structures
mixed_180 = {
    numbers: { value: num_180, list: [num_180, num_180], float: frac },
    strings: { value: str_180, list: [str_180, str_180] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_180 },
    },
    computations: {
        from_num: num_180 * 100,
        from_frac: frac_180 * 10.0,
        list_from_num: [num_180, num_180, num_180],
    },
}

x_181 = 3.14
y_181 = 1.23e45
z_181 = 0.5

my_str_181 : Str
my_str_181 = "one"

binops_181 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_181 : U64 -> U64
add_one_181 = |n| n + 1

map_add_one_181 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_181 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_181 = |arg_one, arg_two| arg_one * arg_two

num_181 = 42
frac_181 = 4.2
str_181 = "hello"

# Polymorphic empty collections
empty_list_181 = []

# Mixed polymorphic structures
mixed_181 = {
    numbers: { value: num_181, list: [num_181, num_181], float: frac },
    strings: { value: str_181, list: [str_181, str_181] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_181 },
    },
    computations: {
        from_num: num_181 * 100,
        from_frac: frac_181 * 10.0,
        list_from_num: [num_181, num_181, num_181],
    },
}

x_182 = 3.14
y_182 = 1.23e45
z_182 = 0.5

my_str_182 : Str
my_str_182 = "one"

binops_182 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_182 : U64 -> U64
add_one_182 = |n| n + 1

map_add_one_182 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_182 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_182 = |arg_one, arg_two| arg_one * arg_two

num_182 = 42
frac_182 = 4.2
str_182 = "hello"

# Polymorphic empty collections
empty_list_182 = []

# Mixed polymorphic structures
mixed_182 = {
    numbers: { value: num_182, list: [num_182, num_182], float: frac },
    strings: { value: str_182, list: [str_182, str_182] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_182 },
    },
    computations: {
        from_num: num_182 * 100,
        from_frac: frac_182 * 10.0,
        list_from_num: [num_182, num_182, num_182],
    },
}

x_183 = 3.14
y_183 = 1.23e45
z_183 = 0.5

my_str_183 : Str
my_str_183 = "one"

binops_183 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_183 : U64 -> U64
add_one_183 = |n| n + 1

map_add_one_183 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_183 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_183 = |arg_one, arg_two| arg_one * arg_two

num_183 = 42
frac_183 = 4.2
str_183 = "hello"

# Polymorphic empty collections
empty_list_183 = []

# Mixed polymorphic structures
mixed_183 = {
    numbers: { value: num_183, list: [num_183, num_183], float: frac },
    strings: { value: str_183, list: [str_183, str_183] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_183 },
    },
    computations: {
        from_num: num_183 * 100,
        from_frac: frac_183 * 10.0,
        list_from_num: [num_183, num_183, num_183],
    },
}

x_184 = 3.14
y_184 = 1.23e45
z_184 = 0.5

my_str_184 : Str
my_str_184 = "one"

binops_184 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_184 : U64 -> U64
add_one_184 = |n| n + 1

map_add_one_184 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_184 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_184 = |arg_one, arg_two| arg_one * arg_two

num_184 = 42
frac_184 = 4.2
str_184 = "hello"

# Polymorphic empty collections
empty_list_184 = []

# Mixed polymorphic structures
mixed_184 = {
    numbers: { value: num_184, list: [num_184, num_184], float: frac },
    strings: { value: str_184, list: [str_184, str_184] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_184 },
    },
    computations: {
        from_num: num_184 * 100,
        from_frac: frac_184 * 10.0,
        list_from_num: [num_184, num_184, num_184],
    },
}

x_185 = 3.14
y_185 = 1.23e45
z_185 = 0.5

my_str_185 : Str
my_str_185 = "one"

binops_185 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_185 : U64 -> U64
add_one_185 = |n| n + 1

map_add_one_185 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_185 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_185 = |arg_one, arg_two| arg_one * arg_two

num_185 = 42
frac_185 = 4.2
str_185 = "hello"

# Polymorphic empty collections
empty_list_185 = []

# Mixed polymorphic structures
mixed_185 = {
    numbers: { value: num_185, list: [num_185, num_185], float: frac },
    strings: { value: str_185, list: [str_185, str_185] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_185 },
    },
    computations: {
        from_num: num_185 * 100,
        from_frac: frac_185 * 10.0,
        list_from_num: [num_185, num_185, num_185],
    },
}

x_186 = 3.14
y_186 = 1.23e45
z_186 = 0.5

my_str_186 : Str
my_str_186 = "one"

binops_186 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_186 : U64 -> U64
add_one_186 = |n| n + 1

map_add_one_186 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_186 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_186 = |arg_one, arg_two| arg_one * arg_two

num_186 = 42
frac_186 = 4.2
str_186 = "hello"

# Polymorphic empty collections
empty_list_186 = []

# Mixed polymorphic structures
mixed_186 = {
    numbers: { value: num_186, list: [num_186, num_186], float: frac },
    strings: { value: str_186, list: [str_186, str_186] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_186 },
    },
    computations: {
        from_num: num_186 * 100,
        from_frac: frac_186 * 10.0,
        list_from_num: [num_186, num_186, num_186],
    },
}

x_187 = 3.14
y_187 = 1.23e45
z_187 = 0.5

my_str_187 : Str
my_str_187 = "one"

binops_187 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_187 : U64 -> U64
add_one_187 = |n| n + 1

map_add_one_187 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_187 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_187 = |arg_one, arg_two| arg_one * arg_two

num_187 = 42
frac_187 = 4.2
str_187 = "hello"

# Polymorphic empty collections
empty_list_187 = []

# Mixed polymorphic structures
mixed_187 = {
    numbers: { value: num_187, list: [num_187, num_187], float: frac },
    strings: { value: str_187, list: [str_187, str_187] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_187 },
    },
    computations: {
        from_num: num_187 * 100,
        from_frac: frac_187 * 10.0,
        list_from_num: [num_187, num_187, num_187],
    },
}

x_188 = 3.14
y_188 = 1.23e45
z_188 = 0.5

my_str_188 : Str
my_str_188 = "one"

binops_188 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_188 : U64 -> U64
add_one_188 = |n| n + 1

map_add_one_188 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_188 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_188 = |arg_one, arg_two| arg_one * arg_two

num_188 = 42
frac_188 = 4.2
str_188 = "hello"

# Polymorphic empty collections
empty_list_188 = []

# Mixed polymorphic structures
mixed_188 = {
    numbers: { value: num_188, list: [num_188, num_188], float: frac },
    strings: { value: str_188, list: [str_188, str_188] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_188 },
    },
    computations: {
        from_num: num_188 * 100,
        from_frac: frac_188 * 10.0,
        list_from_num: [num_188, num_188, num_188],
    },
}

x_189 = 3.14
y_189 = 1.23e45
z_189 = 0.5

my_str_189 : Str
my_str_189 = "one"

binops_189 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_189 : U64 -> U64
add_one_189 = |n| n + 1

map_add_one_189 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_189 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_189 = |arg_one, arg_two| arg_one * arg_two

num_189 = 42
frac_189 = 4.2
str_189 = "hello"

# Polymorphic empty collections
empty_list_189 = []

# Mixed polymorphic structures
mixed_189 = {
    numbers: { value: num_189, list: [num_189, num_189], float: frac },
    strings: { value: str_189, list: [str_189, str_189] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_189 },
    },
    computations: {
        from_num: num_189 * 100,
        from_frac: frac_189 * 10.0,
        list_from_num: [num_189, num_189, num_189],
    },
}

x_190 = 3.14
y_190 = 1.23e45
z_190 = 0.5

my_str_190 : Str
my_str_190 = "one"

binops_190 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_190 : U64 -> U64
add_one_190 = |n| n + 1

map_add_one_190 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_190 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_190 = |arg_one, arg_two| arg_one * arg_two

num_190 = 42
frac_190 = 4.2
str_190 = "hello"

# Polymorphic empty collections
empty_list_190 = []

# Mixed polymorphic structures
mixed_190 = {
    numbers: { value: num_190, list: [num_190, num_190], float: frac },
    strings: { value: str_190, list: [str_190, str_190] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_190 },
    },
    computations: {
        from_num: num_190 * 100,
        from_frac: frac_190 * 10.0,
        list_from_num: [num_190, num_190, num_190],
    },
}

x_191 = 3.14
y_191 = 1.23e45
z_191 = 0.5

my_str_191 : Str
my_str_191 = "one"

binops_191 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_191 : U64 -> U64
add_one_191 = |n| n + 1

map_add_one_191 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_191 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_191 = |arg_one, arg_two| arg_one * arg_two

num_191 = 42
frac_191 = 4.2
str_191 = "hello"

# Polymorphic empty collections
empty_list_191 = []

# Mixed polymorphic structures
mixed_191 = {
    numbers: { value: num_191, list: [num_191, num_191], float: frac },
    strings: { value: str_191, list: [str_191, str_191] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_191 },
    },
    computations: {
        from_num: num_191 * 100,
        from_frac: frac_191 * 10.0,
        list_from_num: [num_191, num_191, num_191],
    },
}

x_192 = 3.14
y_192 = 1.23e45
z_192 = 0.5

my_str_192 : Str
my_str_192 = "one"

binops_192 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_192 : U64 -> U64
add_one_192 = |n| n + 1

map_add_one_192 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_192 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_192 = |arg_one, arg_two| arg_one * arg_two

num_192 = 42
frac_192 = 4.2
str_192 = "hello"

# Polymorphic empty collections
empty_list_192 = []

# Mixed polymorphic structures
mixed_192 = {
    numbers: { value: num_192, list: [num_192, num_192], float: frac },
    strings: { value: str_192, list: [str_192, str_192] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_192 },
    },
    computations: {
        from_num: num_192 * 100,
        from_frac: frac_192 * 10.0,
        list_from_num: [num_192, num_192, num_192],
    },
}

x_193 = 3.14
y_193 = 1.23e45
z_193 = 0.5

my_str_193 : Str
my_str_193 = "one"

binops_193 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_193 : U64 -> U64
add_one_193 = |n| n + 1

map_add_one_193 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_193 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_193 = |arg_one, arg_two| arg_one * arg_two

num_193 = 42
frac_193 = 4.2
str_193 = "hello"

# Polymorphic empty collections
empty_list_193 = []

# Mixed polymorphic structures
mixed_193 = {
    numbers: { value: num_193, list: [num_193, num_193], float: frac },
    strings: { value: str_193, list: [str_193, str_193] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_193 },
    },
    computations: {
        from_num: num_193 * 100,
        from_frac: frac_193 * 10.0,
        list_from_num: [num_193, num_193, num_193],
    },
}

x_194 = 3.14
y_194 = 1.23e45
z_194 = 0.5

my_str_194 : Str
my_str_194 = "one"

binops_194 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_194 : U64 -> U64
add_one_194 = |n| n + 1

map_add_one_194 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_194 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_194 = |arg_one, arg_two| arg_one * arg_two

num_194 = 42
frac_194 = 4.2
str_194 = "hello"

# Polymorphic empty collections
empty_list_194 = []

# Mixed polymorphic structures
mixed_194 = {
    numbers: { value: num_194, list: [num_194, num_194], float: frac },
    strings: { value: str_194, list: [str_194, str_194] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_194 },
    },
    computations: {
        from_num: num_194 * 100,
        from_frac: frac_194 * 10.0,
        list_from_num: [num_194, num_194, num_194],
    },
}

x_195 = 3.14
y_195 = 1.23e45
z_195 = 0.5

my_str_195 : Str
my_str_195 = "one"

binops_195 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_195 : U64 -> U64
add_one_195 = |n| n + 1

map_add_one_195 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_195 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_195 = |arg_one, arg_two| arg_one * arg_two

num_195 = 42
frac_195 = 4.2
str_195 = "hello"

# Polymorphic empty collections
empty_list_195 = []

# Mixed polymorphic structures
mixed_195 = {
    numbers: { value: num_195, list: [num_195, num_195], float: frac },
    strings: { value: str_195, list: [str_195, str_195] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_195 },
    },
    computations: {
        from_num: num_195 * 100,
        from_frac: frac_195 * 10.0,
        list_from_num: [num_195, num_195, num_195],
    },
}

x_196 = 3.14
y_196 = 1.23e45
z_196 = 0.5

my_str_196 : Str
my_str_196 = "one"

binops_196 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_196 : U64 -> U64
add_one_196 = |n| n + 1

map_add_one_196 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_196 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_196 = |arg_one, arg_two| arg_one * arg_two

num_196 = 42
frac_196 = 4.2
str_196 = "hello"

# Polymorphic empty collections
empty_list_196 = []

# Mixed polymorphic structures
mixed_196 = {
    numbers: { value: num_196, list: [num_196, num_196], float: frac },
    strings: { value: str_196, list: [str_196, str_196] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_196 },
    },
    computations: {
        from_num: num_196 * 100,
        from_frac: frac_196 * 10.0,
        list_from_num: [num_196, num_196, num_196],
    },
}

x_197 = 3.14
y_197 = 1.23e45
z_197 = 0.5

my_str_197 : Str
my_str_197 = "one"

binops_197 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_197 : U64 -> U64
add_one_197 = |n| n + 1

map_add_one_197 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_197 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_197 = |arg_one, arg_two| arg_one * arg_two

num_197 = 42
frac_197 = 4.2
str_197 = "hello"

# Polymorphic empty collections
empty_list_197 = []

# Mixed polymorphic structures
mixed_197 = {
    numbers: { value: num_197, list: [num_197, num_197], float: frac },
    strings: { value: str_197, list: [str_197, str_197] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_197 },
    },
    computations: {
        from_num: num_197 * 100,
        from_frac: frac_197 * 10.0,
        list_from_num: [num_197, num_197, num_197],
    },
}

x_198 = 3.14
y_198 = 1.23e45
z_198 = 0.5

my_str_198 : Str
my_str_198 = "one"

binops_198 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_198 : U64 -> U64
add_one_198 = |n| n + 1

map_add_one_198 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_198 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_198 = |arg_one, arg_two| arg_one * arg_two

num_198 = 42
frac_198 = 4.2
str_198 = "hello"

# Polymorphic empty collections
empty_list_198 = []

# Mixed polymorphic structures
mixed_198 = {
    numbers: { value: num_198, list: [num_198, num_198], float: frac },
    strings: { value: str_198, list: [str_198, str_198] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_198 },
    },
    computations: {
        from_num: num_198 * 100,
        from_frac: frac_198 * 10.0,
        list_from_num: [num_198, num_198, num_198],
    },
}

x_199 = 3.14
y_199 = 1.23e45
z_199 = 0.5

my_str_199 : Str
my_str_199 = "one"

binops_199 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_199 : U64 -> U64
add_one_199 = |n| n + 1

map_add_one_199 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_199 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_199 = |arg_one, arg_two| arg_one * arg_two

num_199 = 42
frac_199 = 4.2
str_199 = "hello"

# Polymorphic empty collections
empty_list_199 = []

# Mixed polymorphic structures
mixed_199 = {
    numbers: { value: num_199, list: [num_199, num_199], float: frac },
    strings: { value: str_199, list: [str_199, str_199] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_199 },
    },
    computations: {
        from_num: num_199 * 100,
        from_frac: frac_199 * 10.0,
        list_from_num: [num_199, num_199, num_199],
    },
}

x_200 = 3.14
y_200 = 1.23e45
z_200 = 0.5

my_str_200 : Str
my_str_200 = "one"

binops_200 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_200 : U64 -> U64
add_one_200 = |n| n + 1

map_add_one_200 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_200 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_200 = |arg_one, arg_two| arg_one * arg_two

num_200 = 42
frac_200 = 4.2
str_200 = "hello"

# Polymorphic empty collections
empty_list_200 = []

# Mixed polymorphic structures
mixed_200 = {
    numbers: { value: num_200, list: [num_200, num_200], float: frac },
    strings: { value: str_200, list: [str_200, str_200] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_200 },
    },
    computations: {
        from_num: num_200 * 100,
        from_frac: frac_200 * 10.0,
        list_from_num: [num_200, num_200, num_200],
    },
}

x_201 = 3.14
y_201 = 1.23e45
z_201 = 0.5

my_str_201 : Str
my_str_201 = "one"

binops_201 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_201 : U64 -> U64
add_one_201 = |n| n + 1

map_add_one_201 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_201 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_201 = |arg_one, arg_two| arg_one * arg_two

num_201 = 42
frac_201 = 4.2
str_201 = "hello"

# Polymorphic empty collections
empty_list_201 = []

# Mixed polymorphic structures
mixed_201 = {
    numbers: { value: num_201, list: [num_201, num_201], float: frac },
    strings: { value: str_201, list: [str_201, str_201] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_201 },
    },
    computations: {
        from_num: num_201 * 100,
        from_frac: frac_201 * 10.0,
        list_from_num: [num_201, num_201, num_201],
    },
}

x_202 = 3.14
y_202 = 1.23e45
z_202 = 0.5

my_str_202 : Str
my_str_202 = "one"

binops_202 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_202 : U64 -> U64
add_one_202 = |n| n + 1

map_add_one_202 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_202 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_202 = |arg_one, arg_two| arg_one * arg_two

num_202 = 42
frac_202 = 4.2
str_202 = "hello"

# Polymorphic empty collections
empty_list_202 = []

# Mixed polymorphic structures
mixed_202 = {
    numbers: { value: num_202, list: [num_202, num_202], float: frac },
    strings: { value: str_202, list: [str_202, str_202] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_202 },
    },
    computations: {
        from_num: num_202 * 100,
        from_frac: frac_202 * 10.0,
        list_from_num: [num_202, num_202, num_202],
    },
}

x_203 = 3.14
y_203 = 1.23e45
z_203 = 0.5

my_str_203 : Str
my_str_203 = "one"

binops_203 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_203 : U64 -> U64
add_one_203 = |n| n + 1

map_add_one_203 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_203 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_203 = |arg_one, arg_two| arg_one * arg_two

num_203 = 42
frac_203 = 4.2
str_203 = "hello"

# Polymorphic empty collections
empty_list_203 = []

# Mixed polymorphic structures
mixed_203 = {
    numbers: { value: num_203, list: [num_203, num_203], float: frac },
    strings: { value: str_203, list: [str_203, str_203] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_203 },
    },
    computations: {
        from_num: num_203 * 100,
        from_frac: frac_203 * 10.0,
        list_from_num: [num_203, num_203, num_203],
    },
}

x_204 = 3.14
y_204 = 1.23e45
z_204 = 0.5

my_str_204 : Str
my_str_204 = "one"

binops_204 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_204 : U64 -> U64
add_one_204 = |n| n + 1

map_add_one_204 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_204 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_204 = |arg_one, arg_two| arg_one * arg_two

num_204 = 42
frac_204 = 4.2
str_204 = "hello"

# Polymorphic empty collections
empty_list_204 = []

# Mixed polymorphic structures
mixed_204 = {
    numbers: { value: num_204, list: [num_204, num_204], float: frac },
    strings: { value: str_204, list: [str_204, str_204] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_204 },
    },
    computations: {
        from_num: num_204 * 100,
        from_frac: frac_204 * 10.0,
        list_from_num: [num_204, num_204, num_204],
    },
}

x_205 = 3.14
y_205 = 1.23e45
z_205 = 0.5

my_str_205 : Str
my_str_205 = "one"

binops_205 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_205 : U64 -> U64
add_one_205 = |n| n + 1

map_add_one_205 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_205 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_205 = |arg_one, arg_two| arg_one * arg_two

num_205 = 42
frac_205 = 4.2
str_205 = "hello"

# Polymorphic empty collections
empty_list_205 = []

# Mixed polymorphic structures
mixed_205 = {
    numbers: { value: num_205, list: [num_205, num_205], float: frac },
    strings: { value: str_205, list: [str_205, str_205] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_205 },
    },
    computations: {
        from_num: num_205 * 100,
        from_frac: frac_205 * 10.0,
        list_from_num: [num_205, num_205, num_205],
    },
}

x_206 = 3.14
y_206 = 1.23e45
z_206 = 0.5

my_str_206 : Str
my_str_206 = "one"

binops_206 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_206 : U64 -> U64
add_one_206 = |n| n + 1

map_add_one_206 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_206 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_206 = |arg_one, arg_two| arg_one * arg_two

num_206 = 42
frac_206 = 4.2
str_206 = "hello"

# Polymorphic empty collections
empty_list_206 = []

# Mixed polymorphic structures
mixed_206 = {
    numbers: { value: num_206, list: [num_206, num_206], float: frac },
    strings: { value: str_206, list: [str_206, str_206] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_206 },
    },
    computations: {
        from_num: num_206 * 100,
        from_frac: frac_206 * 10.0,
        list_from_num: [num_206, num_206, num_206],
    },
}

x_207 = 3.14
y_207 = 1.23e45
z_207 = 0.5

my_str_207 : Str
my_str_207 = "one"

binops_207 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_207 : U64 -> U64
add_one_207 = |n| n + 1

map_add_one_207 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_207 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_207 = |arg_one, arg_two| arg_one * arg_two

num_207 = 42
frac_207 = 4.2
str_207 = "hello"

# Polymorphic empty collections
empty_list_207 = []

# Mixed polymorphic structures
mixed_207 = {
    numbers: { value: num_207, list: [num_207, num_207], float: frac },
    strings: { value: str_207, list: [str_207, str_207] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_207 },
    },
    computations: {
        from_num: num_207 * 100,
        from_frac: frac_207 * 10.0,
        list_from_num: [num_207, num_207, num_207],
    },
}

x_208 = 3.14
y_208 = 1.23e45
z_208 = 0.5

my_str_208 : Str
my_str_208 = "one"

binops_208 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_208 : U64 -> U64
add_one_208 = |n| n + 1

map_add_one_208 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_208 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_208 = |arg_one, arg_two| arg_one * arg_two

num_208 = 42
frac_208 = 4.2
str_208 = "hello"

# Polymorphic empty collections
empty_list_208 = []

# Mixed polymorphic structures
mixed_208 = {
    numbers: { value: num_208, list: [num_208, num_208], float: frac },
    strings: { value: str_208, list: [str_208, str_208] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_208 },
    },
    computations: {
        from_num: num_208 * 100,
        from_frac: frac_208 * 10.0,
        list_from_num: [num_208, num_208, num_208],
    },
}

x_209 = 3.14
y_209 = 1.23e45
z_209 = 0.5

my_str_209 : Str
my_str_209 = "one"

binops_209 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_209 : U64 -> U64
add_one_209 = |n| n + 1

map_add_one_209 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_209 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_209 = |arg_one, arg_two| arg_one * arg_two

num_209 = 42
frac_209 = 4.2
str_209 = "hello"

# Polymorphic empty collections
empty_list_209 = []

# Mixed polymorphic structures
mixed_209 = {
    numbers: { value: num_209, list: [num_209, num_209], float: frac },
    strings: { value: str_209, list: [str_209, str_209] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_209 },
    },
    computations: {
        from_num: num_209 * 100,
        from_frac: frac_209 * 10.0,
        list_from_num: [num_209, num_209, num_209],
    },
}

x_210 = 3.14
y_210 = 1.23e45
z_210 = 0.5

my_str_210 : Str
my_str_210 = "one"

binops_210 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_210 : U64 -> U64
add_one_210 = |n| n + 1

map_add_one_210 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_210 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_210 = |arg_one, arg_two| arg_one * arg_two

num_210 = 42
frac_210 = 4.2
str_210 = "hello"

# Polymorphic empty collections
empty_list_210 = []

# Mixed polymorphic structures
mixed_210 = {
    numbers: { value: num_210, list: [num_210, num_210], float: frac },
    strings: { value: str_210, list: [str_210, str_210] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_210 },
    },
    computations: {
        from_num: num_210 * 100,
        from_frac: frac_210 * 10.0,
        list_from_num: [num_210, num_210, num_210],
    },
}

x_211 = 3.14
y_211 = 1.23e45
z_211 = 0.5

my_str_211 : Str
my_str_211 = "one"

binops_211 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_211 : U64 -> U64
add_one_211 = |n| n + 1

map_add_one_211 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_211 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_211 = |arg_one, arg_two| arg_one * arg_two

num_211 = 42
frac_211 = 4.2
str_211 = "hello"

# Polymorphic empty collections
empty_list_211 = []

# Mixed polymorphic structures
mixed_211 = {
    numbers: { value: num_211, list: [num_211, num_211], float: frac },
    strings: { value: str_211, list: [str_211, str_211] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_211 },
    },
    computations: {
        from_num: num_211 * 100,
        from_frac: frac_211 * 10.0,
        list_from_num: [num_211, num_211, num_211],
    },
}

x_212 = 3.14
y_212 = 1.23e45
z_212 = 0.5

my_str_212 : Str
my_str_212 = "one"

binops_212 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_212 : U64 -> U64
add_one_212 = |n| n + 1

map_add_one_212 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_212 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_212 = |arg_one, arg_two| arg_one * arg_two

num_212 = 42
frac_212 = 4.2
str_212 = "hello"

# Polymorphic empty collections
empty_list_212 = []

# Mixed polymorphic structures
mixed_212 = {
    numbers: { value: num_212, list: [num_212, num_212], float: frac },
    strings: { value: str_212, list: [str_212, str_212] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_212 },
    },
    computations: {
        from_num: num_212 * 100,
        from_frac: frac_212 * 10.0,
        list_from_num: [num_212, num_212, num_212],
    },
}

x_213 = 3.14
y_213 = 1.23e45
z_213 = 0.5

my_str_213 : Str
my_str_213 = "one"

binops_213 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_213 : U64 -> U64
add_one_213 = |n| n + 1

map_add_one_213 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_213 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_213 = |arg_one, arg_two| arg_one * arg_two

num_213 = 42
frac_213 = 4.2
str_213 = "hello"

# Polymorphic empty collections
empty_list_213 = []

# Mixed polymorphic structures
mixed_213 = {
    numbers: { value: num_213, list: [num_213, num_213], float: frac },
    strings: { value: str_213, list: [str_213, str_213] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_213 },
    },
    computations: {
        from_num: num_213 * 100,
        from_frac: frac_213 * 10.0,
        list_from_num: [num_213, num_213, num_213],
    },
}

x_214 = 3.14
y_214 = 1.23e45
z_214 = 0.5

my_str_214 : Str
my_str_214 = "one"

binops_214 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_214 : U64 -> U64
add_one_214 = |n| n + 1

map_add_one_214 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_214 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_214 = |arg_one, arg_two| arg_one * arg_two

num_214 = 42
frac_214 = 4.2
str_214 = "hello"

# Polymorphic empty collections
empty_list_214 = []

# Mixed polymorphic structures
mixed_214 = {
    numbers: { value: num_214, list: [num_214, num_214], float: frac },
    strings: { value: str_214, list: [str_214, str_214] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_214 },
    },
    computations: {
        from_num: num_214 * 100,
        from_frac: frac_214 * 10.0,
        list_from_num: [num_214, num_214, num_214],
    },
}

x_215 = 3.14
y_215 = 1.23e45
z_215 = 0.5

my_str_215 : Str
my_str_215 = "one"

binops_215 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_215 : U64 -> U64
add_one_215 = |n| n + 1

map_add_one_215 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_215 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_215 = |arg_one, arg_two| arg_one * arg_two

num_215 = 42
frac_215 = 4.2
str_215 = "hello"

# Polymorphic empty collections
empty_list_215 = []

# Mixed polymorphic structures
mixed_215 = {
    numbers: { value: num_215, list: [num_215, num_215], float: frac },
    strings: { value: str_215, list: [str_215, str_215] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_215 },
    },
    computations: {
        from_num: num_215 * 100,
        from_frac: frac_215 * 10.0,
        list_from_num: [num_215, num_215, num_215],
    },
}

x_216 = 3.14
y_216 = 1.23e45
z_216 = 0.5

my_str_216 : Str
my_str_216 = "one"

binops_216 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_216 : U64 -> U64
add_one_216 = |n| n + 1

map_add_one_216 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_216 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_216 = |arg_one, arg_two| arg_one * arg_two

num_216 = 42
frac_216 = 4.2
str_216 = "hello"

# Polymorphic empty collections
empty_list_216 = []

# Mixed polymorphic structures
mixed_216 = {
    numbers: { value: num_216, list: [num_216, num_216], float: frac },
    strings: { value: str_216, list: [str_216, str_216] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_216 },
    },
    computations: {
        from_num: num_216 * 100,
        from_frac: frac_216 * 10.0,
        list_from_num: [num_216, num_216, num_216],
    },
}

x_217 = 3.14
y_217 = 1.23e45
z_217 = 0.5

my_str_217 : Str
my_str_217 = "one"

binops_217 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_217 : U64 -> U64
add_one_217 = |n| n + 1

map_add_one_217 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_217 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_217 = |arg_one, arg_two| arg_one * arg_two

num_217 = 42
frac_217 = 4.2
str_217 = "hello"

# Polymorphic empty collections
empty_list_217 = []

# Mixed polymorphic structures
mixed_217 = {
    numbers: { value: num_217, list: [num_217, num_217], float: frac },
    strings: { value: str_217, list: [str_217, str_217] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_217 },
    },
    computations: {
        from_num: num_217 * 100,
        from_frac: frac_217 * 10.0,
        list_from_num: [num_217, num_217, num_217],
    },
}

x_218 = 3.14
y_218 = 1.23e45
z_218 = 0.5

my_str_218 : Str
my_str_218 = "one"

binops_218 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_218 : U64 -> U64
add_one_218 = |n| n + 1

map_add_one_218 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_218 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_218 = |arg_one, arg_two| arg_one * arg_two

num_218 = 42
frac_218 = 4.2
str_218 = "hello"

# Polymorphic empty collections
empty_list_218 = []

# Mixed polymorphic structures
mixed_218 = {
    numbers: { value: num_218, list: [num_218, num_218], float: frac },
    strings: { value: str_218, list: [str_218, str_218] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_218 },
    },
    computations: {
        from_num: num_218 * 100,
        from_frac: frac_218 * 10.0,
        list_from_num: [num_218, num_218, num_218],
    },
}

x_219 = 3.14
y_219 = 1.23e45
z_219 = 0.5

my_str_219 : Str
my_str_219 = "one"

binops_219 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_219 : U64 -> U64
add_one_219 = |n| n + 1

map_add_one_219 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_219 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_219 = |arg_one, arg_two| arg_one * arg_two

num_219 = 42
frac_219 = 4.2
str_219 = "hello"

# Polymorphic empty collections
empty_list_219 = []

# Mixed polymorphic structures
mixed_219 = {
    numbers: { value: num_219, list: [num_219, num_219], float: frac },
    strings: { value: str_219, list: [str_219, str_219] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_219 },
    },
    computations: {
        from_num: num_219 * 100,
        from_frac: frac_219 * 10.0,
        list_from_num: [num_219, num_219, num_219],
    },
}

x_220 = 3.14
y_220 = 1.23e45
z_220 = 0.5

my_str_220 : Str
my_str_220 = "one"

binops_220 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_220 : U64 -> U64
add_one_220 = |n| n + 1

map_add_one_220 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_220 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_220 = |arg_one, arg_two| arg_one * arg_two

num_220 = 42
frac_220 = 4.2
str_220 = "hello"

# Polymorphic empty collections
empty_list_220 = []

# Mixed polymorphic structures
mixed_220 = {
    numbers: { value: num_220, list: [num_220, num_220], float: frac },
    strings: { value: str_220, list: [str_220, str_220] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_220 },
    },
    computations: {
        from_num: num_220 * 100,
        from_frac: frac_220 * 10.0,
        list_from_num: [num_220, num_220, num_220],
    },
}

x_221 = 3.14
y_221 = 1.23e45
z_221 = 0.5

my_str_221 : Str
my_str_221 = "one"

binops_221 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_221 : U64 -> U64
add_one_221 = |n| n + 1

map_add_one_221 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_221 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_221 = |arg_one, arg_two| arg_one * arg_two

num_221 = 42
frac_221 = 4.2
str_221 = "hello"

# Polymorphic empty collections
empty_list_221 = []

# Mixed polymorphic structures
mixed_221 = {
    numbers: { value: num_221, list: [num_221, num_221], float: frac },
    strings: { value: str_221, list: [str_221, str_221] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_221 },
    },
    computations: {
        from_num: num_221 * 100,
        from_frac: frac_221 * 10.0,
        list_from_num: [num_221, num_221, num_221],
    },
}

x_222 = 3.14
y_222 = 1.23e45
z_222 = 0.5

my_str_222 : Str
my_str_222 = "one"

binops_222 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_222 : U64 -> U64
add_one_222 = |n| n + 1

map_add_one_222 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_222 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_222 = |arg_one, arg_two| arg_one * arg_two

num_222 = 42
frac_222 = 4.2
str_222 = "hello"

# Polymorphic empty collections
empty_list_222 = []

# Mixed polymorphic structures
mixed_222 = {
    numbers: { value: num_222, list: [num_222, num_222], float: frac },
    strings: { value: str_222, list: [str_222, str_222] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_222 },
    },
    computations: {
        from_num: num_222 * 100,
        from_frac: frac_222 * 10.0,
        list_from_num: [num_222, num_222, num_222],
    },
}

x_223 = 3.14
y_223 = 1.23e45
z_223 = 0.5

my_str_223 : Str
my_str_223 = "one"

binops_223 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_223 : U64 -> U64
add_one_223 = |n| n + 1

map_add_one_223 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_223 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_223 = |arg_one, arg_two| arg_one * arg_two

num_223 = 42
frac_223 = 4.2
str_223 = "hello"

# Polymorphic empty collections
empty_list_223 = []

# Mixed polymorphic structures
mixed_223 = {
    numbers: { value: num_223, list: [num_223, num_223], float: frac },
    strings: { value: str_223, list: [str_223, str_223] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_223 },
    },
    computations: {
        from_num: num_223 * 100,
        from_frac: frac_223 * 10.0,
        list_from_num: [num_223, num_223, num_223],
    },
}

x_224 = 3.14
y_224 = 1.23e45
z_224 = 0.5

my_str_224 : Str
my_str_224 = "one"

binops_224 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_224 : U64 -> U64
add_one_224 = |n| n + 1

map_add_one_224 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_224 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_224 = |arg_one, arg_two| arg_one * arg_two

num_224 = 42
frac_224 = 4.2
str_224 = "hello"

# Polymorphic empty collections
empty_list_224 = []

# Mixed polymorphic structures
mixed_224 = {
    numbers: { value: num_224, list: [num_224, num_224], float: frac },
    strings: { value: str_224, list: [str_224, str_224] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_224 },
    },
    computations: {
        from_num: num_224 * 100,
        from_frac: frac_224 * 10.0,
        list_from_num: [num_224, num_224, num_224],
    },
}

x_225 = 3.14
y_225 = 1.23e45
z_225 = 0.5

my_str_225 : Str
my_str_225 = "one"

binops_225 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_225 : U64 -> U64
add_one_225 = |n| n + 1

map_add_one_225 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_225 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_225 = |arg_one, arg_two| arg_one * arg_two

num_225 = 42
frac_225 = 4.2
str_225 = "hello"

# Polymorphic empty collections
empty_list_225 = []

# Mixed polymorphic structures
mixed_225 = {
    numbers: { value: num_225, list: [num_225, num_225], float: frac },
    strings: { value: str_225, list: [str_225, str_225] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_225 },
    },
    computations: {
        from_num: num_225 * 100,
        from_frac: frac_225 * 10.0,
        list_from_num: [num_225, num_225, num_225],
    },
}

x_226 = 3.14
y_226 = 1.23e45
z_226 = 0.5

my_str_226 : Str
my_str_226 = "one"

binops_226 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_226 : U64 -> U64
add_one_226 = |n| n + 1

map_add_one_226 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_226 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_226 = |arg_one, arg_two| arg_one * arg_two

num_226 = 42
frac_226 = 4.2
str_226 = "hello"

# Polymorphic empty collections
empty_list_226 = []

# Mixed polymorphic structures
mixed_226 = {
    numbers: { value: num_226, list: [num_226, num_226], float: frac },
    strings: { value: str_226, list: [str_226, str_226] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_226 },
    },
    computations: {
        from_num: num_226 * 100,
        from_frac: frac_226 * 10.0,
        list_from_num: [num_226, num_226, num_226],
    },
}

x_227 = 3.14
y_227 = 1.23e45
z_227 = 0.5

my_str_227 : Str
my_str_227 = "one"

binops_227 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_227 : U64 -> U64
add_one_227 = |n| n + 1

map_add_one_227 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_227 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_227 = |arg_one, arg_two| arg_one * arg_two

num_227 = 42
frac_227 = 4.2
str_227 = "hello"

# Polymorphic empty collections
empty_list_227 = []

# Mixed polymorphic structures
mixed_227 = {
    numbers: { value: num_227, list: [num_227, num_227], float: frac },
    strings: { value: str_227, list: [str_227, str_227] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_227 },
    },
    computations: {
        from_num: num_227 * 100,
        from_frac: frac_227 * 10.0,
        list_from_num: [num_227, num_227, num_227],
    },
}

x_228 = 3.14
y_228 = 1.23e45
z_228 = 0.5

my_str_228 : Str
my_str_228 = "one"

binops_228 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_228 : U64 -> U64
add_one_228 = |n| n + 1

map_add_one_228 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_228 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_228 = |arg_one, arg_two| arg_one * arg_two

num_228 = 42
frac_228 = 4.2
str_228 = "hello"

# Polymorphic empty collections
empty_list_228 = []

# Mixed polymorphic structures
mixed_228 = {
    numbers: { value: num_228, list: [num_228, num_228], float: frac },
    strings: { value: str_228, list: [str_228, str_228] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_228 },
    },
    computations: {
        from_num: num_228 * 100,
        from_frac: frac_228 * 10.0,
        list_from_num: [num_228, num_228, num_228],
    },
}

x_229 = 3.14
y_229 = 1.23e45
z_229 = 0.5

my_str_229 : Str
my_str_229 = "one"

binops_229 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_229 : U64 -> U64
add_one_229 = |n| n + 1

map_add_one_229 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_229 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_229 = |arg_one, arg_two| arg_one * arg_two

num_229 = 42
frac_229 = 4.2
str_229 = "hello"

# Polymorphic empty collections
empty_list_229 = []

# Mixed polymorphic structures
mixed_229 = {
    numbers: { value: num_229, list: [num_229, num_229], float: frac },
    strings: { value: str_229, list: [str_229, str_229] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_229 },
    },
    computations: {
        from_num: num_229 * 100,
        from_frac: frac_229 * 10.0,
        list_from_num: [num_229, num_229, num_229],
    },
}

x_230 = 3.14
y_230 = 1.23e45
z_230 = 0.5

my_str_230 : Str
my_str_230 = "one"

binops_230 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_230 : U64 -> U64
add_one_230 = |n| n + 1

map_add_one_230 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_230 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_230 = |arg_one, arg_two| arg_one * arg_two

num_230 = 42
frac_230 = 4.2
str_230 = "hello"

# Polymorphic empty collections
empty_list_230 = []

# Mixed polymorphic structures
mixed_230 = {
    numbers: { value: num_230, list: [num_230, num_230], float: frac },
    strings: { value: str_230, list: [str_230, str_230] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_230 },
    },
    computations: {
        from_num: num_230 * 100,
        from_frac: frac_230 * 10.0,
        list_from_num: [num_230, num_230, num_230],
    },
}

x_231 = 3.14
y_231 = 1.23e45
z_231 = 0.5

my_str_231 : Str
my_str_231 = "one"

binops_231 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_231 : U64 -> U64
add_one_231 = |n| n + 1

map_add_one_231 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_231 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_231 = |arg_one, arg_two| arg_one * arg_two

num_231 = 42
frac_231 = 4.2
str_231 = "hello"

# Polymorphic empty collections
empty_list_231 = []

# Mixed polymorphic structures
mixed_231 = {
    numbers: { value: num_231, list: [num_231, num_231], float: frac },
    strings: { value: str_231, list: [str_231, str_231] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_231 },
    },
    computations: {
        from_num: num_231 * 100,
        from_frac: frac_231 * 10.0,
        list_from_num: [num_231, num_231, num_231],
    },
}

x_232 = 3.14
y_232 = 1.23e45
z_232 = 0.5

my_str_232 : Str
my_str_232 = "one"

binops_232 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_232 : U64 -> U64
add_one_232 = |n| n + 1

map_add_one_232 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_232 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_232 = |arg_one, arg_two| arg_one * arg_two

num_232 = 42
frac_232 = 4.2
str_232 = "hello"

# Polymorphic empty collections
empty_list_232 = []

# Mixed polymorphic structures
mixed_232 = {
    numbers: { value: num_232, list: [num_232, num_232], float: frac },
    strings: { value: str_232, list: [str_232, str_232] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_232 },
    },
    computations: {
        from_num: num_232 * 100,
        from_frac: frac_232 * 10.0,
        list_from_num: [num_232, num_232, num_232],
    },
}

x_233 = 3.14
y_233 = 1.23e45
z_233 = 0.5

my_str_233 : Str
my_str_233 = "one"

binops_233 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_233 : U64 -> U64
add_one_233 = |n| n + 1

map_add_one_233 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_233 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_233 = |arg_one, arg_two| arg_one * arg_two

num_233 = 42
frac_233 = 4.2
str_233 = "hello"

# Polymorphic empty collections
empty_list_233 = []

# Mixed polymorphic structures
mixed_233 = {
    numbers: { value: num_233, list: [num_233, num_233], float: frac },
    strings: { value: str_233, list: [str_233, str_233] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_233 },
    },
    computations: {
        from_num: num_233 * 100,
        from_frac: frac_233 * 10.0,
        list_from_num: [num_233, num_233, num_233],
    },
}

x_234 = 3.14
y_234 = 1.23e45
z_234 = 0.5

my_str_234 : Str
my_str_234 = "one"

binops_234 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_234 : U64 -> U64
add_one_234 = |n| n + 1

map_add_one_234 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_234 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_234 = |arg_one, arg_two| arg_one * arg_two

num_234 = 42
frac_234 = 4.2
str_234 = "hello"

# Polymorphic empty collections
empty_list_234 = []

# Mixed polymorphic structures
mixed_234 = {
    numbers: { value: num_234, list: [num_234, num_234], float: frac },
    strings: { value: str_234, list: [str_234, str_234] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_234 },
    },
    computations: {
        from_num: num_234 * 100,
        from_frac: frac_234 * 10.0,
        list_from_num: [num_234, num_234, num_234],
    },
}

x_235 = 3.14
y_235 = 1.23e45
z_235 = 0.5

my_str_235 : Str
my_str_235 = "one"

binops_235 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_235 : U64 -> U64
add_one_235 = |n| n + 1

map_add_one_235 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_235 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_235 = |arg_one, arg_two| arg_one * arg_two

num_235 = 42
frac_235 = 4.2
str_235 = "hello"

# Polymorphic empty collections
empty_list_235 = []

# Mixed polymorphic structures
mixed_235 = {
    numbers: { value: num_235, list: [num_235, num_235], float: frac },
    strings: { value: str_235, list: [str_235, str_235] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_235 },
    },
    computations: {
        from_num: num_235 * 100,
        from_frac: frac_235 * 10.0,
        list_from_num: [num_235, num_235, num_235],
    },
}

x_236 = 3.14
y_236 = 1.23e45
z_236 = 0.5

my_str_236 : Str
my_str_236 = "one"

binops_236 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_236 : U64 -> U64
add_one_236 = |n| n + 1

map_add_one_236 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_236 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_236 = |arg_one, arg_two| arg_one * arg_two

num_236 = 42
frac_236 = 4.2
str_236 = "hello"

# Polymorphic empty collections
empty_list_236 = []

# Mixed polymorphic structures
mixed_236 = {
    numbers: { value: num_236, list: [num_236, num_236], float: frac },
    strings: { value: str_236, list: [str_236, str_236] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_236 },
    },
    computations: {
        from_num: num_236 * 100,
        from_frac: frac_236 * 10.0,
        list_from_num: [num_236, num_236, num_236],
    },
}

x_237 = 3.14
y_237 = 1.23e45
z_237 = 0.5

my_str_237 : Str
my_str_237 = "one"

binops_237 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_237 : U64 -> U64
add_one_237 = |n| n + 1

map_add_one_237 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_237 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_237 = |arg_one, arg_two| arg_one * arg_two

num_237 = 42
frac_237 = 4.2
str_237 = "hello"

# Polymorphic empty collections
empty_list_237 = []

# Mixed polymorphic structures
mixed_237 = {
    numbers: { value: num_237, list: [num_237, num_237], float: frac },
    strings: { value: str_237, list: [str_237, str_237] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_237 },
    },
    computations: {
        from_num: num_237 * 100,
        from_frac: frac_237 * 10.0,
        list_from_num: [num_237, num_237, num_237],
    },
}

x_238 = 3.14
y_238 = 1.23e45
z_238 = 0.5

my_str_238 : Str
my_str_238 = "one"

binops_238 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_238 : U64 -> U64
add_one_238 = |n| n + 1

map_add_one_238 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_238 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_238 = |arg_one, arg_two| arg_one * arg_two

num_238 = 42
frac_238 = 4.2
str_238 = "hello"

# Polymorphic empty collections
empty_list_238 = []

# Mixed polymorphic structures
mixed_238 = {
    numbers: { value: num_238, list: [num_238, num_238], float: frac },
    strings: { value: str_238, list: [str_238, str_238] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_238 },
    },
    computations: {
        from_num: num_238 * 100,
        from_frac: frac_238 * 10.0,
        list_from_num: [num_238, num_238, num_238],
    },
}

x_239 = 3.14
y_239 = 1.23e45
z_239 = 0.5

my_str_239 : Str
my_str_239 = "one"

binops_239 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_239 : U64 -> U64
add_one_239 = |n| n + 1

map_add_one_239 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_239 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_239 = |arg_one, arg_two| arg_one * arg_two

num_239 = 42
frac_239 = 4.2
str_239 = "hello"

# Polymorphic empty collections
empty_list_239 = []

# Mixed polymorphic structures
mixed_239 = {
    numbers: { value: num_239, list: [num_239, num_239], float: frac },
    strings: { value: str_239, list: [str_239, str_239] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_239 },
    },
    computations: {
        from_num: num_239 * 100,
        from_frac: frac_239 * 10.0,
        list_from_num: [num_239, num_239, num_239],
    },
}

x_240 = 3.14
y_240 = 1.23e45
z_240 = 0.5

my_str_240 : Str
my_str_240 = "one"

binops_240 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_240 : U64 -> U64
add_one_240 = |n| n + 1

map_add_one_240 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_240 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_240 = |arg_one, arg_two| arg_one * arg_two

num_240 = 42
frac_240 = 4.2
str_240 = "hello"

# Polymorphic empty collections
empty_list_240 = []

# Mixed polymorphic structures
mixed_240 = {
    numbers: { value: num_240, list: [num_240, num_240], float: frac },
    strings: { value: str_240, list: [str_240, str_240] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_240 },
    },
    computations: {
        from_num: num_240 * 100,
        from_frac: frac_240 * 10.0,
        list_from_num: [num_240, num_240, num_240],
    },
}

x_241 = 3.14
y_241 = 1.23e45
z_241 = 0.5

my_str_241 : Str
my_str_241 = "one"

binops_241 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_241 : U64 -> U64
add_one_241 = |n| n + 1

map_add_one_241 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_241 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_241 = |arg_one, arg_two| arg_one * arg_two

num_241 = 42
frac_241 = 4.2
str_241 = "hello"

# Polymorphic empty collections
empty_list_241 = []

# Mixed polymorphic structures
mixed_241 = {
    numbers: { value: num_241, list: [num_241, num_241], float: frac },
    strings: { value: str_241, list: [str_241, str_241] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_241 },
    },
    computations: {
        from_num: num_241 * 100,
        from_frac: frac_241 * 10.0,
        list_from_num: [num_241, num_241, num_241],
    },
}

x_242 = 3.14
y_242 = 1.23e45
z_242 = 0.5

my_str_242 : Str
my_str_242 = "one"

binops_242 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_242 : U64 -> U64
add_one_242 = |n| n + 1

map_add_one_242 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_242 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_242 = |arg_one, arg_two| arg_one * arg_two

num_242 = 42
frac_242 = 4.2
str_242 = "hello"

# Polymorphic empty collections
empty_list_242 = []

# Mixed polymorphic structures
mixed_242 = {
    numbers: { value: num_242, list: [num_242, num_242], float: frac },
    strings: { value: str_242, list: [str_242, str_242] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_242 },
    },
    computations: {
        from_num: num_242 * 100,
        from_frac: frac_242 * 10.0,
        list_from_num: [num_242, num_242, num_242],
    },
}

x_243 = 3.14
y_243 = 1.23e45
z_243 = 0.5

my_str_243 : Str
my_str_243 = "one"

binops_243 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_243 : U64 -> U64
add_one_243 = |n| n + 1

map_add_one_243 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_243 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_243 = |arg_one, arg_two| arg_one * arg_two

num_243 = 42
frac_243 = 4.2
str_243 = "hello"

# Polymorphic empty collections
empty_list_243 = []

# Mixed polymorphic structures
mixed_243 = {
    numbers: { value: num_243, list: [num_243, num_243], float: frac },
    strings: { value: str_243, list: [str_243, str_243] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_243 },
    },
    computations: {
        from_num: num_243 * 100,
        from_frac: frac_243 * 10.0,
        list_from_num: [num_243, num_243, num_243],
    },
}

x_244 = 3.14
y_244 = 1.23e45
z_244 = 0.5

my_str_244 : Str
my_str_244 = "one"

binops_244 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_244 : U64 -> U64
add_one_244 = |n| n + 1

map_add_one_244 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_244 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_244 = |arg_one, arg_two| arg_one * arg_two

num_244 = 42
frac_244 = 4.2
str_244 = "hello"

# Polymorphic empty collections
empty_list_244 = []

# Mixed polymorphic structures
mixed_244 = {
    numbers: { value: num_244, list: [num_244, num_244], float: frac },
    strings: { value: str_244, list: [str_244, str_244] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_244 },
    },
    computations: {
        from_num: num_244 * 100,
        from_frac: frac_244 * 10.0,
        list_from_num: [num_244, num_244, num_244],
    },
}

x_245 = 3.14
y_245 = 1.23e45
z_245 = 0.5

my_str_245 : Str
my_str_245 = "one"

binops_245 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_245 : U64 -> U64
add_one_245 = |n| n + 1

map_add_one_245 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_245 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_245 = |arg_one, arg_two| arg_one * arg_two

num_245 = 42
frac_245 = 4.2
str_245 = "hello"

# Polymorphic empty collections
empty_list_245 = []

# Mixed polymorphic structures
mixed_245 = {
    numbers: { value: num_245, list: [num_245, num_245], float: frac },
    strings: { value: str_245, list: [str_245, str_245] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_245 },
    },
    computations: {
        from_num: num_245 * 100,
        from_frac: frac_245 * 10.0,
        list_from_num: [num_245, num_245, num_245],
    },
}

x_246 = 3.14
y_246 = 1.23e45
z_246 = 0.5

my_str_246 : Str
my_str_246 = "one"

binops_246 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_246 : U64 -> U64
add_one_246 = |n| n + 1

map_add_one_246 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_246 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_246 = |arg_one, arg_two| arg_one * arg_two

num_246 = 42
frac_246 = 4.2
str_246 = "hello"

# Polymorphic empty collections
empty_list_246 = []

# Mixed polymorphic structures
mixed_246 = {
    numbers: { value: num_246, list: [num_246, num_246], float: frac },
    strings: { value: str_246, list: [str_246, str_246] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_246 },
    },
    computations: {
        from_num: num_246 * 100,
        from_frac: frac_246 * 10.0,
        list_from_num: [num_246, num_246, num_246],
    },
}

x_247 = 3.14
y_247 = 1.23e45
z_247 = 0.5

my_str_247 : Str
my_str_247 = "one"

binops_247 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_247 : U64 -> U64
add_one_247 = |n| n + 1

map_add_one_247 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_247 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_247 = |arg_one, arg_two| arg_one * arg_two

num_247 = 42
frac_247 = 4.2
str_247 = "hello"

# Polymorphic empty collections
empty_list_247 = []

# Mixed polymorphic structures
mixed_247 = {
    numbers: { value: num_247, list: [num_247, num_247], float: frac },
    strings: { value: str_247, list: [str_247, str_247] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_247 },
    },
    computations: {
        from_num: num_247 * 100,
        from_frac: frac_247 * 10.0,
        list_from_num: [num_247, num_247, num_247],
    },
}

x_248 = 3.14
y_248 = 1.23e45
z_248 = 0.5

my_str_248 : Str
my_str_248 = "one"

binops_248 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_248 : U64 -> U64
add_one_248 = |n| n + 1

map_add_one_248 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_248 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_248 = |arg_one, arg_two| arg_one * arg_two

num_248 = 42
frac_248 = 4.2
str_248 = "hello"

# Polymorphic empty collections
empty_list_248 = []

# Mixed polymorphic structures
mixed_248 = {
    numbers: { value: num_248, list: [num_248, num_248], float: frac },
    strings: { value: str_248, list: [str_248, str_248] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_248 },
    },
    computations: {
        from_num: num_248 * 100,
        from_frac: frac_248 * 10.0,
        list_from_num: [num_248, num_248, num_248],
    },
}

x_249 = 3.14
y_249 = 1.23e45
z_249 = 0.5

my_str_249 : Str
my_str_249 = "one"

binops_249 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_249 : U64 -> U64
add_one_249 = |n| n + 1

map_add_one_249 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_249 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_249 = |arg_one, arg_two| arg_one * arg_two

num_249 = 42
frac_249 = 4.2
str_249 = "hello"

# Polymorphic empty collections
empty_list_249 = []

# Mixed polymorphic structures
mixed_249 = {
    numbers: { value: num_249, list: [num_249, num_249], float: frac },
    strings: { value: str_249, list: [str_249, str_249] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_249 },
    },
    computations: {
        from_num: num_249 * 100,
        from_frac: frac_249 * 10.0,
        list_from_num: [num_249, num_249, num_249],
    },
}

x_250 = 3.14
y_250 = 1.23e45
z_250 = 0.5

my_str_250 : Str
my_str_250 = "one"

binops_250 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_250 : U64 -> U64
add_one_250 = |n| n + 1

map_add_one_250 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_250 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_250 = |arg_one, arg_two| arg_one * arg_two

num_250 = 42
frac_250 = 4.2
str_250 = "hello"

# Polymorphic empty collections
empty_list_250 = []

# Mixed polymorphic structures
mixed_250 = {
    numbers: { value: num_250, list: [num_250, num_250], float: frac },
    strings: { value: str_250, list: [str_250, str_250] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_250 },
    },
    computations: {
        from_num: num_250 * 100,
        from_frac: frac_250 * 10.0,
        list_from_num: [num_250, num_250, num_250],
    },
}

x_251 = 3.14
y_251 = 1.23e45
z_251 = 0.5

my_str_251 : Str
my_str_251 = "one"

binops_251 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_251 : U64 -> U64
add_one_251 = |n| n + 1

map_add_one_251 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_251 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_251 = |arg_one, arg_two| arg_one * arg_two

num_251 = 42
frac_251 = 4.2
str_251 = "hello"

# Polymorphic empty collections
empty_list_251 = []

# Mixed polymorphic structures
mixed_251 = {
    numbers: { value: num_251, list: [num_251, num_251], float: frac },
    strings: { value: str_251, list: [str_251, str_251] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_251 },
    },
    computations: {
        from_num: num_251 * 100,
        from_frac: frac_251 * 10.0,
        list_from_num: [num_251, num_251, num_251],
    },
}

x_252 = 3.14
y_252 = 1.23e45
z_252 = 0.5

my_str_252 : Str
my_str_252 = "one"

binops_252 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_252 : U64 -> U64
add_one_252 = |n| n + 1

map_add_one_252 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_252 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_252 = |arg_one, arg_two| arg_one * arg_two

num_252 = 42
frac_252 = 4.2
str_252 = "hello"

# Polymorphic empty collections
empty_list_252 = []

# Mixed polymorphic structures
mixed_252 = {
    numbers: { value: num_252, list: [num_252, num_252], float: frac },
    strings: { value: str_252, list: [str_252, str_252] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_252 },
    },
    computations: {
        from_num: num_252 * 100,
        from_frac: frac_252 * 10.0,
        list_from_num: [num_252, num_252, num_252],
    },
}

x_253 = 3.14
y_253 = 1.23e45
z_253 = 0.5

my_str_253 : Str
my_str_253 = "one"

binops_253 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_253 : U64 -> U64
add_one_253 = |n| n + 1

map_add_one_253 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_253 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_253 = |arg_one, arg_two| arg_one * arg_two

num_253 = 42
frac_253 = 4.2
str_253 = "hello"

# Polymorphic empty collections
empty_list_253 = []

# Mixed polymorphic structures
mixed_253 = {
    numbers: { value: num_253, list: [num_253, num_253], float: frac },
    strings: { value: str_253, list: [str_253, str_253] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_253 },
    },
    computations: {
        from_num: num_253 * 100,
        from_frac: frac_253 * 10.0,
        list_from_num: [num_253, num_253, num_253],
    },
}

x_254 = 3.14
y_254 = 1.23e45
z_254 = 0.5

my_str_254 : Str
my_str_254 = "one"

binops_254 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_254 : U64 -> U64
add_one_254 = |n| n + 1

map_add_one_254 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_254 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_254 = |arg_one, arg_two| arg_one * arg_two

num_254 = 42
frac_254 = 4.2
str_254 = "hello"

# Polymorphic empty collections
empty_list_254 = []

# Mixed polymorphic structures
mixed_254 = {
    numbers: { value: num_254, list: [num_254, num_254], float: frac },
    strings: { value: str_254, list: [str_254, str_254] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_254 },
    },
    computations: {
        from_num: num_254 * 100,
        from_frac: frac_254 * 10.0,
        list_from_num: [num_254, num_254, num_254],
    },
}

x_255 = 3.14
y_255 = 1.23e45
z_255 = 0.5

my_str_255 : Str
my_str_255 = "one"

binops_255 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_255 : U64 -> U64
add_one_255 = |n| n + 1

map_add_one_255 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_255 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_255 = |arg_one, arg_two| arg_one * arg_two

num_255 = 42
frac_255 = 4.2
str_255 = "hello"

# Polymorphic empty collections
empty_list_255 = []

# Mixed polymorphic structures
mixed_255 = {
    numbers: { value: num_255, list: [num_255, num_255], float: frac },
    strings: { value: str_255, list: [str_255, str_255] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_255 },
    },
    computations: {
        from_num: num_255 * 100,
        from_frac: frac_255 * 10.0,
        list_from_num: [num_255, num_255, num_255],
    },
}

x_256 = 3.14
y_256 = 1.23e45
z_256 = 0.5

my_str_256 : Str
my_str_256 = "one"

binops_256 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_256 : U64 -> U64
add_one_256 = |n| n + 1

map_add_one_256 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_256 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_256 = |arg_one, arg_two| arg_one * arg_two

num_256 = 42
frac_256 = 4.2
str_256 = "hello"

# Polymorphic empty collections
empty_list_256 = []

# Mixed polymorphic structures
mixed_256 = {
    numbers: { value: num_256, list: [num_256, num_256], float: frac },
    strings: { value: str_256, list: [str_256, str_256] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_256 },
    },
    computations: {
        from_num: num_256 * 100,
        from_frac: frac_256 * 10.0,
        list_from_num: [num_256, num_256, num_256],
    },
}

x_257 = 3.14
y_257 = 1.23e45
z_257 = 0.5

my_str_257 : Str
my_str_257 = "one"

binops_257 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_257 : U64 -> U64
add_one_257 = |n| n + 1

map_add_one_257 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_257 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_257 = |arg_one, arg_two| arg_one * arg_two

num_257 = 42
frac_257 = 4.2
str_257 = "hello"

# Polymorphic empty collections
empty_list_257 = []

# Mixed polymorphic structures
mixed_257 = {
    numbers: { value: num_257, list: [num_257, num_257], float: frac },
    strings: { value: str_257, list: [str_257, str_257] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_257 },
    },
    computations: {
        from_num: num_257 * 100,
        from_frac: frac_257 * 10.0,
        list_from_num: [num_257, num_257, num_257],
    },
}

x_258 = 3.14
y_258 = 1.23e45
z_258 = 0.5

my_str_258 : Str
my_str_258 = "one"

binops_258 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_258 : U64 -> U64
add_one_258 = |n| n + 1

map_add_one_258 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_258 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_258 = |arg_one, arg_two| arg_one * arg_two

num_258 = 42
frac_258 = 4.2
str_258 = "hello"

# Polymorphic empty collections
empty_list_258 = []

# Mixed polymorphic structures
mixed_258 = {
    numbers: { value: num_258, list: [num_258, num_258], float: frac },
    strings: { value: str_258, list: [str_258, str_258] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_258 },
    },
    computations: {
        from_num: num_258 * 100,
        from_frac: frac_258 * 10.0,
        list_from_num: [num_258, num_258, num_258],
    },
}

x_259 = 3.14
y_259 = 1.23e45
z_259 = 0.5

my_str_259 : Str
my_str_259 = "one"

binops_259 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_259 : U64 -> U64
add_one_259 = |n| n + 1

map_add_one_259 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_259 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_259 = |arg_one, arg_two| arg_one * arg_two

num_259 = 42
frac_259 = 4.2
str_259 = "hello"

# Polymorphic empty collections
empty_list_259 = []

# Mixed polymorphic structures
mixed_259 = {
    numbers: { value: num_259, list: [num_259, num_259], float: frac },
    strings: { value: str_259, list: [str_259, str_259] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_259 },
    },
    computations: {
        from_num: num_259 * 100,
        from_frac: frac_259 * 10.0,
        list_from_num: [num_259, num_259, num_259],
    },
}

x_260 = 3.14
y_260 = 1.23e45
z_260 = 0.5

my_str_260 : Str
my_str_260 = "one"

binops_260 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_260 : U64 -> U64
add_one_260 = |n| n + 1

map_add_one_260 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_260 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_260 = |arg_one, arg_two| arg_one * arg_two

num_260 = 42
frac_260 = 4.2
str_260 = "hello"

# Polymorphic empty collections
empty_list_260 = []

# Mixed polymorphic structures
mixed_260 = {
    numbers: { value: num_260, list: [num_260, num_260], float: frac },
    strings: { value: str_260, list: [str_260, str_260] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_260 },
    },
    computations: {
        from_num: num_260 * 100,
        from_frac: frac_260 * 10.0,
        list_from_num: [num_260, num_260, num_260],
    },
}

x_261 = 3.14
y_261 = 1.23e45
z_261 = 0.5

my_str_261 : Str
my_str_261 = "one"

binops_261 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_261 : U64 -> U64
add_one_261 = |n| n + 1

map_add_one_261 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_261 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_261 = |arg_one, arg_two| arg_one * arg_two

num_261 = 42
frac_261 = 4.2
str_261 = "hello"

# Polymorphic empty collections
empty_list_261 = []

# Mixed polymorphic structures
mixed_261 = {
    numbers: { value: num_261, list: [num_261, num_261], float: frac },
    strings: { value: str_261, list: [str_261, str_261] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_261 },
    },
    computations: {
        from_num: num_261 * 100,
        from_frac: frac_261 * 10.0,
        list_from_num: [num_261, num_261, num_261],
    },
}

x_262 = 3.14
y_262 = 1.23e45
z_262 = 0.5

my_str_262 : Str
my_str_262 = "one"

binops_262 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_262 : U64 -> U64
add_one_262 = |n| n + 1

map_add_one_262 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_262 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_262 = |arg_one, arg_two| arg_one * arg_two

num_262 = 42
frac_262 = 4.2
str_262 = "hello"

# Polymorphic empty collections
empty_list_262 = []

# Mixed polymorphic structures
mixed_262 = {
    numbers: { value: num_262, list: [num_262, num_262], float: frac },
    strings: { value: str_262, list: [str_262, str_262] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_262 },
    },
    computations: {
        from_num: num_262 * 100,
        from_frac: frac_262 * 10.0,
        list_from_num: [num_262, num_262, num_262],
    },
}

x_263 = 3.14
y_263 = 1.23e45
z_263 = 0.5

my_str_263 : Str
my_str_263 = "one"

binops_263 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_263 : U64 -> U64
add_one_263 = |n| n + 1

map_add_one_263 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_263 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_263 = |arg_one, arg_two| arg_one * arg_two

num_263 = 42
frac_263 = 4.2
str_263 = "hello"

# Polymorphic empty collections
empty_list_263 = []

# Mixed polymorphic structures
mixed_263 = {
    numbers: { value: num_263, list: [num_263, num_263], float: frac },
    strings: { value: str_263, list: [str_263, str_263] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_263 },
    },
    computations: {
        from_num: num_263 * 100,
        from_frac: frac_263 * 10.0,
        list_from_num: [num_263, num_263, num_263],
    },
}

x_264 = 3.14
y_264 = 1.23e45
z_264 = 0.5

my_str_264 : Str
my_str_264 = "one"

binops_264 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_264 : U64 -> U64
add_one_264 = |n| n + 1

map_add_one_264 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_264 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_264 = |arg_one, arg_two| arg_one * arg_two

num_264 = 42
frac_264 = 4.2
str_264 = "hello"

# Polymorphic empty collections
empty_list_264 = []

# Mixed polymorphic structures
mixed_264 = {
    numbers: { value: num_264, list: [num_264, num_264], float: frac },
    strings: { value: str_264, list: [str_264, str_264] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_264 },
    },
    computations: {
        from_num: num_264 * 100,
        from_frac: frac_264 * 10.0,
        list_from_num: [num_264, num_264, num_264],
    },
}

x_265 = 3.14
y_265 = 1.23e45
z_265 = 0.5

my_str_265 : Str
my_str_265 = "one"

binops_265 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_265 : U64 -> U64
add_one_265 = |n| n + 1

map_add_one_265 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_265 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_265 = |arg_one, arg_two| arg_one * arg_two

num_265 = 42
frac_265 = 4.2
str_265 = "hello"

# Polymorphic empty collections
empty_list_265 = []

# Mixed polymorphic structures
mixed_265 = {
    numbers: { value: num_265, list: [num_265, num_265], float: frac },
    strings: { value: str_265, list: [str_265, str_265] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_265 },
    },
    computations: {
        from_num: num_265 * 100,
        from_frac: frac_265 * 10.0,
        list_from_num: [num_265, num_265, num_265],
    },
}

x_266 = 3.14
y_266 = 1.23e45
z_266 = 0.5

my_str_266 : Str
my_str_266 = "one"

binops_266 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_266 : U64 -> U64
add_one_266 = |n| n + 1

map_add_one_266 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_266 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_266 = |arg_one, arg_two| arg_one * arg_two

num_266 = 42
frac_266 = 4.2
str_266 = "hello"

# Polymorphic empty collections
empty_list_266 = []

# Mixed polymorphic structures
mixed_266 = {
    numbers: { value: num_266, list: [num_266, num_266], float: frac },
    strings: { value: str_266, list: [str_266, str_266] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_266 },
    },
    computations: {
        from_num: num_266 * 100,
        from_frac: frac_266 * 10.0,
        list_from_num: [num_266, num_266, num_266],
    },
}

x_267 = 3.14
y_267 = 1.23e45
z_267 = 0.5

my_str_267 : Str
my_str_267 = "one"

binops_267 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_267 : U64 -> U64
add_one_267 = |n| n + 1

map_add_one_267 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_267 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_267 = |arg_one, arg_two| arg_one * arg_two

num_267 = 42
frac_267 = 4.2
str_267 = "hello"

# Polymorphic empty collections
empty_list_267 = []

# Mixed polymorphic structures
mixed_267 = {
    numbers: { value: num_267, list: [num_267, num_267], float: frac },
    strings: { value: str_267, list: [str_267, str_267] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_267 },
    },
    computations: {
        from_num: num_267 * 100,
        from_frac: frac_267 * 10.0,
        list_from_num: [num_267, num_267, num_267],
    },
}

x_268 = 3.14
y_268 = 1.23e45
z_268 = 0.5

my_str_268 : Str
my_str_268 = "one"

binops_268 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_268 : U64 -> U64
add_one_268 = |n| n + 1

map_add_one_268 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_268 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_268 = |arg_one, arg_two| arg_one * arg_two

num_268 = 42
frac_268 = 4.2
str_268 = "hello"

# Polymorphic empty collections
empty_list_268 = []

# Mixed polymorphic structures
mixed_268 = {
    numbers: { value: num_268, list: [num_268, num_268], float: frac },
    strings: { value: str_268, list: [str_268, str_268] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_268 },
    },
    computations: {
        from_num: num_268 * 100,
        from_frac: frac_268 * 10.0,
        list_from_num: [num_268, num_268, num_268],
    },
}

x_269 = 3.14
y_269 = 1.23e45
z_269 = 0.5

my_str_269 : Str
my_str_269 = "one"

binops_269 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_269 : U64 -> U64
add_one_269 = |n| n + 1

map_add_one_269 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_269 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_269 = |arg_one, arg_two| arg_one * arg_two

num_269 = 42
frac_269 = 4.2
str_269 = "hello"

# Polymorphic empty collections
empty_list_269 = []

# Mixed polymorphic structures
mixed_269 = {
    numbers: { value: num_269, list: [num_269, num_269], float: frac },
    strings: { value: str_269, list: [str_269, str_269] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_269 },
    },
    computations: {
        from_num: num_269 * 100,
        from_frac: frac_269 * 10.0,
        list_from_num: [num_269, num_269, num_269],
    },
}

x_270 = 3.14
y_270 = 1.23e45
z_270 = 0.5

my_str_270 : Str
my_str_270 = "one"

binops_270 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_270 : U64 -> U64
add_one_270 = |n| n + 1

map_add_one_270 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_270 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_270 = |arg_one, arg_two| arg_one * arg_two

num_270 = 42
frac_270 = 4.2
str_270 = "hello"

# Polymorphic empty collections
empty_list_270 = []

# Mixed polymorphic structures
mixed_270 = {
    numbers: { value: num_270, list: [num_270, num_270], float: frac },
    strings: { value: str_270, list: [str_270, str_270] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_270 },
    },
    computations: {
        from_num: num_270 * 100,
        from_frac: frac_270 * 10.0,
        list_from_num: [num_270, num_270, num_270],
    },
}

x_271 = 3.14
y_271 = 1.23e45
z_271 = 0.5

my_str_271 : Str
my_str_271 = "one"

binops_271 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_271 : U64 -> U64
add_one_271 = |n| n + 1

map_add_one_271 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_271 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_271 = |arg_one, arg_two| arg_one * arg_two

num_271 = 42
frac_271 = 4.2
str_271 = "hello"

# Polymorphic empty collections
empty_list_271 = []

# Mixed polymorphic structures
mixed_271 = {
    numbers: { value: num_271, list: [num_271, num_271], float: frac },
    strings: { value: str_271, list: [str_271, str_271] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_271 },
    },
    computations: {
        from_num: num_271 * 100,
        from_frac: frac_271 * 10.0,
        list_from_num: [num_271, num_271, num_271],
    },
}

x_272 = 3.14
y_272 = 1.23e45
z_272 = 0.5

my_str_272 : Str
my_str_272 = "one"

binops_272 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_272 : U64 -> U64
add_one_272 = |n| n + 1

map_add_one_272 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_272 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_272 = |arg_one, arg_two| arg_one * arg_two

num_272 = 42
frac_272 = 4.2
str_272 = "hello"

# Polymorphic empty collections
empty_list_272 = []

# Mixed polymorphic structures
mixed_272 = {
    numbers: { value: num_272, list: [num_272, num_272], float: frac },
    strings: { value: str_272, list: [str_272, str_272] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_272 },
    },
    computations: {
        from_num: num_272 * 100,
        from_frac: frac_272 * 10.0,
        list_from_num: [num_272, num_272, num_272],
    },
}

x_273 = 3.14
y_273 = 1.23e45
z_273 = 0.5

my_str_273 : Str
my_str_273 = "one"

binops_273 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_273 : U64 -> U64
add_one_273 = |n| n + 1

map_add_one_273 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_273 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_273 = |arg_one, arg_two| arg_one * arg_two

num_273 = 42
frac_273 = 4.2
str_273 = "hello"

# Polymorphic empty collections
empty_list_273 = []

# Mixed polymorphic structures
mixed_273 = {
    numbers: { value: num_273, list: [num_273, num_273], float: frac },
    strings: { value: str_273, list: [str_273, str_273] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_273 },
    },
    computations: {
        from_num: num_273 * 100,
        from_frac: frac_273 * 10.0,
        list_from_num: [num_273, num_273, num_273],
    },
}

x_274 = 3.14
y_274 = 1.23e45
z_274 = 0.5

my_str_274 : Str
my_str_274 = "one"

binops_274 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_274 : U64 -> U64
add_one_274 = |n| n + 1

map_add_one_274 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_274 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_274 = |arg_one, arg_two| arg_one * arg_two

num_274 = 42
frac_274 = 4.2
str_274 = "hello"

# Polymorphic empty collections
empty_list_274 = []

# Mixed polymorphic structures
mixed_274 = {
    numbers: { value: num_274, list: [num_274, num_274], float: frac },
    strings: { value: str_274, list: [str_274, str_274] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_274 },
    },
    computations: {
        from_num: num_274 * 100,
        from_frac: frac_274 * 10.0,
        list_from_num: [num_274, num_274, num_274],
    },
}

x_275 = 3.14
y_275 = 1.23e45
z_275 = 0.5

my_str_275 : Str
my_str_275 = "one"

binops_275 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_275 : U64 -> U64
add_one_275 = |n| n + 1

map_add_one_275 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_275 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_275 = |arg_one, arg_two| arg_one * arg_two

num_275 = 42
frac_275 = 4.2
str_275 = "hello"

# Polymorphic empty collections
empty_list_275 = []

# Mixed polymorphic structures
mixed_275 = {
    numbers: { value: num_275, list: [num_275, num_275], float: frac },
    strings: { value: str_275, list: [str_275, str_275] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_275 },
    },
    computations: {
        from_num: num_275 * 100,
        from_frac: frac_275 * 10.0,
        list_from_num: [num_275, num_275, num_275],
    },
}

x_276 = 3.14
y_276 = 1.23e45
z_276 = 0.5

my_str_276 : Str
my_str_276 = "one"

binops_276 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_276 : U64 -> U64
add_one_276 = |n| n + 1

map_add_one_276 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_276 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_276 = |arg_one, arg_two| arg_one * arg_two

num_276 = 42
frac_276 = 4.2
str_276 = "hello"

# Polymorphic empty collections
empty_list_276 = []

# Mixed polymorphic structures
mixed_276 = {
    numbers: { value: num_276, list: [num_276, num_276], float: frac },
    strings: { value: str_276, list: [str_276, str_276] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_276 },
    },
    computations: {
        from_num: num_276 * 100,
        from_frac: frac_276 * 10.0,
        list_from_num: [num_276, num_276, num_276],
    },
}

x_277 = 3.14
y_277 = 1.23e45
z_277 = 0.5

my_str_277 : Str
my_str_277 = "one"

binops_277 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_277 : U64 -> U64
add_one_277 = |n| n + 1

map_add_one_277 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_277 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_277 = |arg_one, arg_two| arg_one * arg_two

num_277 = 42
frac_277 = 4.2
str_277 = "hello"

# Polymorphic empty collections
empty_list_277 = []

# Mixed polymorphic structures
mixed_277 = {
    numbers: { value: num_277, list: [num_277, num_277], float: frac },
    strings: { value: str_277, list: [str_277, str_277] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_277 },
    },
    computations: {
        from_num: num_277 * 100,
        from_frac: frac_277 * 10.0,
        list_from_num: [num_277, num_277, num_277],
    },
}

x_278 = 3.14
y_278 = 1.23e45
z_278 = 0.5

my_str_278 : Str
my_str_278 = "one"

binops_278 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_278 : U64 -> U64
add_one_278 = |n| n + 1

map_add_one_278 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_278 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_278 = |arg_one, arg_two| arg_one * arg_two

num_278 = 42
frac_278 = 4.2
str_278 = "hello"

# Polymorphic empty collections
empty_list_278 = []

# Mixed polymorphic structures
mixed_278 = {
    numbers: { value: num_278, list: [num_278, num_278], float: frac },
    strings: { value: str_278, list: [str_278, str_278] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_278 },
    },
    computations: {
        from_num: num_278 * 100,
        from_frac: frac_278 * 10.0,
        list_from_num: [num_278, num_278, num_278],
    },
}

x_279 = 3.14
y_279 = 1.23e45
z_279 = 0.5

my_str_279 : Str
my_str_279 = "one"

binops_279 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_279 : U64 -> U64
add_one_279 = |n| n + 1

map_add_one_279 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_279 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_279 = |arg_one, arg_two| arg_one * arg_two

num_279 = 42
frac_279 = 4.2
str_279 = "hello"

# Polymorphic empty collections
empty_list_279 = []

# Mixed polymorphic structures
mixed_279 = {
    numbers: { value: num_279, list: [num_279, num_279], float: frac },
    strings: { value: str_279, list: [str_279, str_279] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_279 },
    },
    computations: {
        from_num: num_279 * 100,
        from_frac: frac_279 * 10.0,
        list_from_num: [num_279, num_279, num_279],
    },
}

x_280 = 3.14
y_280 = 1.23e45
z_280 = 0.5

my_str_280 : Str
my_str_280 = "one"

binops_280 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_280 : U64 -> U64
add_one_280 = |n| n + 1

map_add_one_280 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_280 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_280 = |arg_one, arg_two| arg_one * arg_two

num_280 = 42
frac_280 = 4.2
str_280 = "hello"

# Polymorphic empty collections
empty_list_280 = []

# Mixed polymorphic structures
mixed_280 = {
    numbers: { value: num_280, list: [num_280, num_280], float: frac },
    strings: { value: str_280, list: [str_280, str_280] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_280 },
    },
    computations: {
        from_num: num_280 * 100,
        from_frac: frac_280 * 10.0,
        list_from_num: [num_280, num_280, num_280],
    },
}

x_281 = 3.14
y_281 = 1.23e45
z_281 = 0.5

my_str_281 : Str
my_str_281 = "one"

binops_281 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_281 : U64 -> U64
add_one_281 = |n| n + 1

map_add_one_281 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_281 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_281 = |arg_one, arg_two| arg_one * arg_two

num_281 = 42
frac_281 = 4.2
str_281 = "hello"

# Polymorphic empty collections
empty_list_281 = []

# Mixed polymorphic structures
mixed_281 = {
    numbers: { value: num_281, list: [num_281, num_281], float: frac },
    strings: { value: str_281, list: [str_281, str_281] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_281 },
    },
    computations: {
        from_num: num_281 * 100,
        from_frac: frac_281 * 10.0,
        list_from_num: [num_281, num_281, num_281],
    },
}

x_282 = 3.14
y_282 = 1.23e45
z_282 = 0.5

my_str_282 : Str
my_str_282 = "one"

binops_282 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_282 : U64 -> U64
add_one_282 = |n| n + 1

map_add_one_282 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_282 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_282 = |arg_one, arg_two| arg_one * arg_two

num_282 = 42
frac_282 = 4.2
str_282 = "hello"

# Polymorphic empty collections
empty_list_282 = []

# Mixed polymorphic structures
mixed_282 = {
    numbers: { value: num_282, list: [num_282, num_282], float: frac },
    strings: { value: str_282, list: [str_282, str_282] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_282 },
    },
    computations: {
        from_num: num_282 * 100,
        from_frac: frac_282 * 10.0,
        list_from_num: [num_282, num_282, num_282],
    },
}

x_283 = 3.14
y_283 = 1.23e45
z_283 = 0.5

my_str_283 : Str
my_str_283 = "one"

binops_283 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_283 : U64 -> U64
add_one_283 = |n| n + 1

map_add_one_283 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_283 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_283 = |arg_one, arg_two| arg_one * arg_two

num_283 = 42
frac_283 = 4.2
str_283 = "hello"

# Polymorphic empty collections
empty_list_283 = []

# Mixed polymorphic structures
mixed_283 = {
    numbers: { value: num_283, list: [num_283, num_283], float: frac },
    strings: { value: str_283, list: [str_283, str_283] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_283 },
    },
    computations: {
        from_num: num_283 * 100,
        from_frac: frac_283 * 10.0,
        list_from_num: [num_283, num_283, num_283],
    },
}

x_284 = 3.14
y_284 = 1.23e45
z_284 = 0.5

my_str_284 : Str
my_str_284 = "one"

binops_284 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_284 : U64 -> U64
add_one_284 = |n| n + 1

map_add_one_284 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_284 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_284 = |arg_one, arg_two| arg_one * arg_two

num_284 = 42
frac_284 = 4.2
str_284 = "hello"

# Polymorphic empty collections
empty_list_284 = []

# Mixed polymorphic structures
mixed_284 = {
    numbers: { value: num_284, list: [num_284, num_284], float: frac },
    strings: { value: str_284, list: [str_284, str_284] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_284 },
    },
    computations: {
        from_num: num_284 * 100,
        from_frac: frac_284 * 10.0,
        list_from_num: [num_284, num_284, num_284],
    },
}

x_285 = 3.14
y_285 = 1.23e45
z_285 = 0.5

my_str_285 : Str
my_str_285 = "one"

binops_285 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_285 : U64 -> U64
add_one_285 = |n| n + 1

map_add_one_285 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_285 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_285 = |arg_one, arg_two| arg_one * arg_two

num_285 = 42
frac_285 = 4.2
str_285 = "hello"

# Polymorphic empty collections
empty_list_285 = []

# Mixed polymorphic structures
mixed_285 = {
    numbers: { value: num_285, list: [num_285, num_285], float: frac },
    strings: { value: str_285, list: [str_285, str_285] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_285 },
    },
    computations: {
        from_num: num_285 * 100,
        from_frac: frac_285 * 10.0,
        list_from_num: [num_285, num_285, num_285],
    },
}

x_286 = 3.14
y_286 = 1.23e45
z_286 = 0.5

my_str_286 : Str
my_str_286 = "one"

binops_286 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_286 : U64 -> U64
add_one_286 = |n| n + 1

map_add_one_286 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_286 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_286 = |arg_one, arg_two| arg_one * arg_two

num_286 = 42
frac_286 = 4.2
str_286 = "hello"

# Polymorphic empty collections
empty_list_286 = []

# Mixed polymorphic structures
mixed_286 = {
    numbers: { value: num_286, list: [num_286, num_286], float: frac },
    strings: { value: str_286, list: [str_286, str_286] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_286 },
    },
    computations: {
        from_num: num_286 * 100,
        from_frac: frac_286 * 10.0,
        list_from_num: [num_286, num_286, num_286],
    },
}

x_287 = 3.14
y_287 = 1.23e45
z_287 = 0.5

my_str_287 : Str
my_str_287 = "one"

binops_287 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_287 : U64 -> U64
add_one_287 = |n| n + 1

map_add_one_287 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_287 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_287 = |arg_one, arg_two| arg_one * arg_two

num_287 = 42
frac_287 = 4.2
str_287 = "hello"

# Polymorphic empty collections
empty_list_287 = []

# Mixed polymorphic structures
mixed_287 = {
    numbers: { value: num_287, list: [num_287, num_287], float: frac },
    strings: { value: str_287, list: [str_287, str_287] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_287 },
    },
    computations: {
        from_num: num_287 * 100,
        from_frac: frac_287 * 10.0,
        list_from_num: [num_287, num_287, num_287],
    },
}

x_288 = 3.14
y_288 = 1.23e45
z_288 = 0.5

my_str_288 : Str
my_str_288 = "one"

binops_288 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_288 : U64 -> U64
add_one_288 = |n| n + 1

map_add_one_288 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_288 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_288 = |arg_one, arg_two| arg_one * arg_two

num_288 = 42
frac_288 = 4.2
str_288 = "hello"

# Polymorphic empty collections
empty_list_288 = []

# Mixed polymorphic structures
mixed_288 = {
    numbers: { value: num_288, list: [num_288, num_288], float: frac },
    strings: { value: str_288, list: [str_288, str_288] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_288 },
    },
    computations: {
        from_num: num_288 * 100,
        from_frac: frac_288 * 10.0,
        list_from_num: [num_288, num_288, num_288],
    },
}

x_289 = 3.14
y_289 = 1.23e45
z_289 = 0.5

my_str_289 : Str
my_str_289 = "one"

binops_289 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_289 : U64 -> U64
add_one_289 = |n| n + 1

map_add_one_289 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_289 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_289 = |arg_one, arg_two| arg_one * arg_two

num_289 = 42
frac_289 = 4.2
str_289 = "hello"

# Polymorphic empty collections
empty_list_289 = []

# Mixed polymorphic structures
mixed_289 = {
    numbers: { value: num_289, list: [num_289, num_289], float: frac },
    strings: { value: str_289, list: [str_289, str_289] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_289 },
    },
    computations: {
        from_num: num_289 * 100,
        from_frac: frac_289 * 10.0,
        list_from_num: [num_289, num_289, num_289],
    },
}

x_290 = 3.14
y_290 = 1.23e45
z_290 = 0.5

my_str_290 : Str
my_str_290 = "one"

binops_290 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_290 : U64 -> U64
add_one_290 = |n| n + 1

map_add_one_290 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_290 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_290 = |arg_one, arg_two| arg_one * arg_two

num_290 = 42
frac_290 = 4.2
str_290 = "hello"

# Polymorphic empty collections
empty_list_290 = []

# Mixed polymorphic structures
mixed_290 = {
    numbers: { value: num_290, list: [num_290, num_290], float: frac },
    strings: { value: str_290, list: [str_290, str_290] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_290 },
    },
    computations: {
        from_num: num_290 * 100,
        from_frac: frac_290 * 10.0,
        list_from_num: [num_290, num_290, num_290],
    },
}

x_291 = 3.14
y_291 = 1.23e45
z_291 = 0.5

my_str_291 : Str
my_str_291 = "one"

binops_291 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_291 : U64 -> U64
add_one_291 = |n| n + 1

map_add_one_291 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_291 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_291 = |arg_one, arg_two| arg_one * arg_two

num_291 = 42
frac_291 = 4.2
str_291 = "hello"

# Polymorphic empty collections
empty_list_291 = []

# Mixed polymorphic structures
mixed_291 = {
    numbers: { value: num_291, list: [num_291, num_291], float: frac },
    strings: { value: str_291, list: [str_291, str_291] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_291 },
    },
    computations: {
        from_num: num_291 * 100,
        from_frac: frac_291 * 10.0,
        list_from_num: [num_291, num_291, num_291],
    },
}

x_292 = 3.14
y_292 = 1.23e45
z_292 = 0.5

my_str_292 : Str
my_str_292 = "one"

binops_292 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_292 : U64 -> U64
add_one_292 = |n| n + 1

map_add_one_292 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_292 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_292 = |arg_one, arg_two| arg_one * arg_two

num_292 = 42
frac_292 = 4.2
str_292 = "hello"

# Polymorphic empty collections
empty_list_292 = []

# Mixed polymorphic structures
mixed_292 = {
    numbers: { value: num_292, list: [num_292, num_292], float: frac },
    strings: { value: str_292, list: [str_292, str_292] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_292 },
    },
    computations: {
        from_num: num_292 * 100,
        from_frac: frac_292 * 10.0,
        list_from_num: [num_292, num_292, num_292],
    },
}

x_293 = 3.14
y_293 = 1.23e45
z_293 = 0.5

my_str_293 : Str
my_str_293 = "one"

binops_293 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_293 : U64 -> U64
add_one_293 = |n| n + 1

map_add_one_293 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_293 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_293 = |arg_one, arg_two| arg_one * arg_two

num_293 = 42
frac_293 = 4.2
str_293 = "hello"

# Polymorphic empty collections
empty_list_293 = []

# Mixed polymorphic structures
mixed_293 = {
    numbers: { value: num_293, list: [num_293, num_293], float: frac },
    strings: { value: str_293, list: [str_293, str_293] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_293 },
    },
    computations: {
        from_num: num_293 * 100,
        from_frac: frac_293 * 10.0,
        list_from_num: [num_293, num_293, num_293],
    },
}

x_294 = 3.14
y_294 = 1.23e45
z_294 = 0.5

my_str_294 : Str
my_str_294 = "one"

binops_294 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_294 : U64 -> U64
add_one_294 = |n| n + 1

map_add_one_294 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_294 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_294 = |arg_one, arg_two| arg_one * arg_two

num_294 = 42
frac_294 = 4.2
str_294 = "hello"

# Polymorphic empty collections
empty_list_294 = []

# Mixed polymorphic structures
mixed_294 = {
    numbers: { value: num_294, list: [num_294, num_294], float: frac },
    strings: { value: str_294, list: [str_294, str_294] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_294 },
    },
    computations: {
        from_num: num_294 * 100,
        from_frac: frac_294 * 10.0,
        list_from_num: [num_294, num_294, num_294],
    },
}

x_295 = 3.14
y_295 = 1.23e45
z_295 = 0.5

my_str_295 : Str
my_str_295 = "one"

binops_295 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_295 : U64 -> U64
add_one_295 = |n| n + 1

map_add_one_295 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_295 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_295 = |arg_one, arg_two| arg_one * arg_two

num_295 = 42
frac_295 = 4.2
str_295 = "hello"

# Polymorphic empty collections
empty_list_295 = []

# Mixed polymorphic structures
mixed_295 = {
    numbers: { value: num_295, list: [num_295, num_295], float: frac },
    strings: { value: str_295, list: [str_295, str_295] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_295 },
    },
    computations: {
        from_num: num_295 * 100,
        from_frac: frac_295 * 10.0,
        list_from_num: [num_295, num_295, num_295],
    },
}

x_296 = 3.14
y_296 = 1.23e45
z_296 = 0.5

my_str_296 : Str
my_str_296 = "one"

binops_296 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_296 : U64 -> U64
add_one_296 = |n| n + 1

map_add_one_296 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_296 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_296 = |arg_one, arg_two| arg_one * arg_two

num_296 = 42
frac_296 = 4.2
str_296 = "hello"

# Polymorphic empty collections
empty_list_296 = []

# Mixed polymorphic structures
mixed_296 = {
    numbers: { value: num_296, list: [num_296, num_296], float: frac },
    strings: { value: str_296, list: [str_296, str_296] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_296 },
    },
    computations: {
        from_num: num_296 * 100,
        from_frac: frac_296 * 10.0,
        list_from_num: [num_296, num_296, num_296],
    },
}

x_297 = 3.14
y_297 = 1.23e45
z_297 = 0.5

my_str_297 : Str
my_str_297 = "one"

binops_297 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_297 : U64 -> U64
add_one_297 = |n| n + 1

map_add_one_297 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_297 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_297 = |arg_one, arg_two| arg_one * arg_two

num_297 = 42
frac_297 = 4.2
str_297 = "hello"

# Polymorphic empty collections
empty_list_297 = []

# Mixed polymorphic structures
mixed_297 = {
    numbers: { value: num_297, list: [num_297, num_297], float: frac },
    strings: { value: str_297, list: [str_297, str_297] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_297 },
    },
    computations: {
        from_num: num_297 * 100,
        from_frac: frac_297 * 10.0,
        list_from_num: [num_297, num_297, num_297],
    },
}

x_298 = 3.14
y_298 = 1.23e45
z_298 = 0.5

my_str_298 : Str
my_str_298 = "one"

binops_298 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_298 : U64 -> U64
add_one_298 = |n| n + 1

map_add_one_298 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_298 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_298 = |arg_one, arg_two| arg_one * arg_two

num_298 = 42
frac_298 = 4.2
str_298 = "hello"

# Polymorphic empty collections
empty_list_298 = []

# Mixed polymorphic structures
mixed_298 = {
    numbers: { value: num_298, list: [num_298, num_298], float: frac },
    strings: { value: str_298, list: [str_298, str_298] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_298 },
    },
    computations: {
        from_num: num_298 * 100,
        from_frac: frac_298 * 10.0,
        list_from_num: [num_298, num_298, num_298],
    },
}

x_299 = 3.14
y_299 = 1.23e45
z_299 = 0.5

my_str_299 : Str
my_str_299 = "one"

binops_299 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_299 : U64 -> U64
add_one_299 = |n| n + 1

map_add_one_299 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_299 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_299 = |arg_one, arg_two| arg_one * arg_two

num_299 = 42
frac_299 = 4.2
str_299 = "hello"

# Polymorphic empty collections
empty_list_299 = []

# Mixed polymorphic structures
mixed_299 = {
    numbers: { value: num_299, list: [num_299, num_299], float: frac },
    strings: { value: str_299, list: [str_299, str_299] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_299 },
    },
    computations: {
        from_num: num_299 * 100,
        from_frac: frac_299 * 10.0,
        list_from_num: [num_299, num_299, num_299],
    },
}

x_300 = 3.14
y_300 = 1.23e45
z_300 = 0.5

my_str_300 : Str
my_str_300 = "one"

binops_300 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_300 : U64 -> U64
add_one_300 = |n| n + 1

map_add_one_300 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_300 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_300 = |arg_one, arg_two| arg_one * arg_two

num_300 = 42
frac_300 = 4.2
str_300 = "hello"

# Polymorphic empty collections
empty_list_300 = []

# Mixed polymorphic structures
mixed_300 = {
    numbers: { value: num_300, list: [num_300, num_300], float: frac },
    strings: { value: str_300, list: [str_300, str_300] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_300 },
    },
    computations: {
        from_num: num_300 * 100,
        from_frac: frac_300 * 10.0,
        list_from_num: [num_300, num_300, num_300],
    },
}

x_301 = 3.14
y_301 = 1.23e45
z_301 = 0.5

my_str_301 : Str
my_str_301 = "one"

binops_301 = (
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
)

add_one_301 : U64 -> U64
add_one_301 = |n| n + 1

map_add_one_301 = |list| {
    fn = |numy| numy + 1
    list.map(fn)
}

# Function showing var vs regular identifier independence
test_func_301 = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}

multiply_301 = |arg_one, arg_two| arg_one * arg_two

num_301 = 42
frac_301 = 4.2
str_301 = "hello"

# Polymorphic empty collections
empty_list_301 = []

# Mixed polymorphic structures
mixed_301 = {
    numbers: { value: num_301, list: [num_301, num_301], float: frac },
    strings: { value: str_301, list: [str_301, str_301] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list_301 },
    },
    computations: {
        from_num: num_301 * 100,
        from_frac: frac_301 * 10.0,
        list_from_num: [num_301, num_301, num_301],
    },
}
