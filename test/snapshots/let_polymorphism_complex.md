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
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Float,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,OpAssign,OpenSquare,CloseSquare,
LowerIdent,OpAssign,OpenCurly,CloseCurly,
LowerIdent,OpAssign,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,
LowerIdent,OpAssign,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,
LowerIdent,OpAssign,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpenSquare,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpenSquare,LowerIdent,Comma,OpenSquare,Int,Comma,Int,CloseSquare,Comma,LowerIdent,Comma,OpenSquare,Int,Comma,Int,CloseSquare,CloseSquare,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
CloseCurly,Comma,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpColon,OpenSquare,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,CloseSquare,Comma,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
CloseCurly,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpColon,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,Comma,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
CloseCurly,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,OpenSquare,LowerIdent,CloseSquare,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
CloseCurly,Comma,
LowerIdent,OpColon,OpenSquare,LowerIdent,Comma,LowerIdent,OpStar,Int,Comma,LowerIdent,OpStar,Int,CloseSquare,Comma,
CloseCurly,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
CloseCurly,Comma,
LowerIdent,OpColon,OpenSquare,
OpenCurly,LowerIdent,OpColon,OpenSquare,Int,CloseSquare,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,Comma,
OpenCurly,LowerIdent,OpColon,OpenSquare,Int,Comma,Int,CloseSquare,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,Comma,
OpenCurly,LowerIdent,OpColon,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,Comma,
CloseSquare,Comma,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
LowerIdent,OpAssign,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,OpenSquare,LowerIdent,Comma,LowerIdent,OpPlus,Int,Comma,LowerIdent,OpPlus,Int,CloseSquare,CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,Comma,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,CloseCurly,Comma,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,OpenSquare,LowerIdent,CloseSquare,Comma,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,Comma,
CloseCurly,Comma,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,LowerIdent,OpStar,Int,Comma,
LowerIdent,OpColon,LowerIdent,OpStar,Float,Comma,
LowerIdent,OpColon,OpenSquare,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseSquare,Comma,
CloseCurly,Comma,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,NoSpaceDotLowerIdent,OpPlus,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-decl
			(p-ident (raw "num"))
			(e-int (raw "42")))
		(s-decl
			(p-ident (raw "frac"))
			(e-frac (raw "4.2")))
		(s-decl
			(p-ident (raw "str"))
			(e-string
				(e-string-part (raw "hello"))))
		(s-decl
			(p-ident (raw "bool"))
			(e-tag (raw "True")))
		(s-decl
			(p-ident (raw "empty_list"))
			(e-list))
		(s-decl
			(p-ident (raw "empty_record"))
			(e-record))
		(s-decl
			(p-ident (raw "int_list"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))
				(e-int (raw "3"))))
		(s-decl
			(p-ident (raw "str_list"))
			(e-list
				(e-string
					(e-string-part (raw "a")))
				(e-string
					(e-string-part (raw "b")))
				(e-string
					(e-string-part (raw "c")))))
		(s-decl
			(p-ident (raw "bool_list"))
			(e-list
				(e-tag (raw "True"))
				(e-tag (raw "False"))))
		(s-decl
			(p-ident (raw "nested_empty"))
			(e-list
				(e-ident (raw "empty_list"))
				(e-ident (raw "empty_list"))
				(e-ident (raw "empty_list"))))
		(s-decl
			(p-ident (raw "mixed_nested"))
			(e-list
				(e-ident (raw "empty_list"))
				(e-list
					(e-int (raw "1"))
					(e-int (raw "2")))
				(e-ident (raw "empty_list"))
				(e-list
					(e-int (raw "3"))
					(e-int (raw "4")))))
		(s-decl
			(p-ident (raw "poly_record"))
			(e-record
				(field (field "items")
					(e-ident (raw "empty_list")))
				(field (field "count")
					(e-int (raw "0")))))
		(s-decl
			(p-ident (raw "use_poly_record1"))
			(e-record
				(field (field "items")
					(e-list
						(e-int (raw "1"))
						(e-int (raw "2"))
						(e-int (raw "3"))))
				(field (field "count")
					(e-int (raw "0")))))
		(s-decl
			(p-ident (raw "use_poly_record2"))
			(e-record
				(field (field "items")
					(e-list
						(e-string
							(e-string-part (raw "x")))
						(e-string
							(e-string-part (raw "y")))
						(e-string
							(e-string-part (raw "z")))))
				(field (field "count")
					(e-int (raw "0")))))
		(s-decl
			(p-ident (raw "base_config"))
			(e-record
				(field (field "data")
					(e-ident (raw "empty_list")))
				(field (field "metadata")
					(e-record
						(field (field "version")
							(e-ident (raw "num")))
						(field (field "ratio")
							(e-ident (raw "frac")))
						(field (field "description")
							(e-ident (raw "str")))))))
		(s-decl
			(p-ident (raw "config1"))
			(e-record
				(field (field "data")
					(e-list
						(e-int (raw "1"))
						(e-int (raw "2"))
						(e-int (raw "3"))
						(e-int (raw "4"))
						(e-int (raw "5"))))
				(field (field "metadata")
					(e-record
						(field (field "version")
							(e-ident (raw "num")))
						(field (field "ratio")
							(e-ident (raw "frac")))
						(field (field "description")
							(e-ident (raw "str")))))
				(field (field "name")
					(e-string
						(e-string-part (raw "integers"))))))
		(s-decl
			(p-ident (raw "config2"))
			(e-record
				(field (field "data")
					(e-list
						(e-string
							(e-string-part (raw "apple")))
						(e-string
							(e-string-part (raw "banana")))
						(e-string
							(e-string-part (raw "cherry")))))
				(field (field "metadata")
					(e-record
						(field (field "version")
							(e-ident (raw "num")))
						(field (field "ratio")
							(e-ident (raw "frac")))
						(field (field "description")
							(e-ident (raw "str")))))
				(field (field "name")
					(e-string
						(e-string-part (raw "fruits"))))))
		(s-decl
			(p-ident (raw "make_container"))
			(e-lambda
				(args
					(p-ident (raw "val")))
				(e-record
					(field (field "value")
						(e-ident (raw "val")))
					(field (field "wrapper")
						(e-list
							(e-ident (raw "val")))))))
		(s-decl
			(p-ident (raw "container1"))
			(e-apply
				(e-ident (raw "make_container"))
				(e-ident (raw "num"))))
		(s-decl
			(p-ident (raw "container2"))
			(e-apply
				(e-ident (raw "make_container"))
				(e-ident (raw "str"))))
		(s-decl
			(p-ident (raw "container3"))
			(e-apply
				(e-ident (raw "make_container"))
				(e-ident (raw "frac"))))
		(s-decl
			(p-ident (raw "deep"))
			(e-record
				(field (field "level1")
					(e-record
						(field (field "level2")
							(e-record
								(field (field "level3")
									(e-record
										(field (field "data")
											(e-ident (raw "empty_list")))
										(field (field "value")
											(e-ident (raw "num")))))
								(field (field "items")
									(e-list
										(e-ident (raw "num"))
										(e-binop (op "*")
											(e-ident (raw "num"))
											(e-int (raw "2")))
										(e-binop (op "*")
											(e-ident (raw "num"))
											(e-int (raw "3")))))))
						(field (field "collection")
							(e-ident (raw "empty_list")))))
				(field (field "results")
					(e-list
						(e-record
							(field (field "data")
								(e-list
									(e-int (raw "1"))))
							(field (field "tag")
								(e-string
									(e-string-part (raw "single")))))
						(e-record
							(field (field "data")
								(e-list
									(e-int (raw "1"))
									(e-int (raw "2"))))
							(field (field "tag")
								(e-string
									(e-string-part (raw "ints")))))
						(e-record
							(field (field "data")
								(e-list
									(e-int (raw "1"))
									(e-int (raw "2"))
									(e-int (raw "3"))))
							(field (field "tag")
								(e-string
									(e-string-part (raw "more")))))))))
		(s-decl
			(p-ident (raw "compute1"))
			(e-binop (op "+")
				(e-ident (raw "num"))
				(e-int (raw "10"))))
		(s-decl
			(p-ident (raw "compute2"))
			(e-binop (op "*")
				(e-ident (raw "num"))
				(e-int (raw "2"))))
		(s-decl
			(p-ident (raw "compute3"))
			(e-list
				(e-ident (raw "num"))
				(e-ident (raw "num"))))
		(s-decl
			(p-ident (raw "compute4"))
			(e-record
				(field (field "base")
					(e-ident (raw "num")))
				(field (field "derived")
					(e-list
						(e-ident (raw "num"))
						(e-binop (op "+")
							(e-ident (raw "num"))
							(e-int (raw "1")))
						(e-binop (op "+")
							(e-ident (raw "num"))
							(e-int (raw "2")))))))
		(s-decl
			(p-ident (raw "mixed"))
			(e-record
				(field (field "numbers")
					(e-record
						(field (field "value")
							(e-ident (raw "num")))
						(field (field "list")
							(e-list
								(e-ident (raw "num"))
								(e-ident (raw "num"))))
						(field (field "float")
							(e-ident (raw "frac")))))
				(field (field "strings")
					(e-record
						(field (field "value")
							(e-ident (raw "str")))
						(field (field "list")
							(e-list
								(e-ident (raw "str"))
								(e-ident (raw "str"))))))
				(field (field "empty_lists")
					(e-record
						(field (field "raw")
							(e-ident (raw "empty_list")))
						(field (field "in_list")
							(e-list
								(e-ident (raw "empty_list"))))
						(field (field "in_record")
							(e-record
								(field (field "data")
									(e-ident (raw "empty_list")))))))
				(field (field "computations")
					(e-record
						(field (field "from_num")
							(e-binop (op "*")
								(e-ident (raw "num"))
								(e-int (raw "100"))))
						(field (field "from_frac")
							(e-binop (op "*")
								(e-ident (raw "frac"))
								(e-frac (raw "10.0"))))
						(field (field "list_from_num")
							(e-list
								(e-ident (raw "num"))
								(e-ident (raw "num"))
								(e-ident (raw "num"))))))))
		(s-decl
			(p-ident (raw "main"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(e-binop (op "+")
							(e-field-access
								(e-ident (raw "container1"))
								(e-ident (raw "value")))
							(e-int (raw "10")))))))))
~~~
# FORMATTED
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
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "num"))
		(e-num (value "42")))
	(d-let
		(p-assign (ident "frac"))
		(e-dec-small (numerator "42") (denominator-power-of-ten "1") (value "4.2")))
	(d-let
		(p-assign (ident "str"))
		(e-string
			(e-literal (string "hello"))))
	(d-let
		(p-assign (ident "bool"))
		(e-tag (name "True")))
	(d-let
		(p-assign (ident "empty_list"))
		(e-empty_list))
	(d-let
		(p-assign (ident "empty_record"))
		(e-empty_record))
	(d-let
		(p-assign (ident "int_list"))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2"))
				(e-num (value "3")))))
	(d-let
		(p-assign (ident "str_list"))
		(e-list
			(elems
				(e-string
					(e-literal (string "a")))
				(e-string
					(e-literal (string "b")))
				(e-string
					(e-literal (string "c"))))))
	(d-let
		(p-assign (ident "bool_list"))
		(e-list
			(elems
				(e-tag (name "True"))
				(e-tag (name "False")))))
	(d-let
		(p-assign (ident "nested_empty"))
		(e-list
			(elems
				(e-lookup-local
					(p-assign (ident "empty_list")))
				(e-lookup-local
					(p-assign (ident "empty_list")))
				(e-lookup-local
					(p-assign (ident "empty_list"))))))
	(d-let
		(p-assign (ident "mixed_nested"))
		(e-list
			(elems
				(e-lookup-local
					(p-assign (ident "empty_list")))
				(e-list
					(elems
						(e-num (value "1"))
						(e-num (value "2"))))
				(e-lookup-local
					(p-assign (ident "empty_list")))
				(e-list
					(elems
						(e-num (value "3"))
						(e-num (value "4")))))))
	(d-let
		(p-assign (ident "poly_record"))
		(e-record
			(fields
				(field (name "items")
					(e-lookup-local
						(p-assign (ident "empty_list"))))
				(field (name "count")
					(e-num (value "0"))))))
	(d-let
		(p-assign (ident "use_poly_record1"))
		(e-record
			(fields
				(field (name "items")
					(e-list
						(elems
							(e-num (value "1"))
							(e-num (value "2"))
							(e-num (value "3")))))
				(field (name "count")
					(e-num (value "0"))))))
	(d-let
		(p-assign (ident "use_poly_record2"))
		(e-record
			(fields
				(field (name "items")
					(e-list
						(elems
							(e-string
								(e-literal (string "x")))
							(e-string
								(e-literal (string "y")))
							(e-string
								(e-literal (string "z"))))))
				(field (name "count")
					(e-num (value "0"))))))
	(d-let
		(p-assign (ident "base_config"))
		(e-record
			(fields
				(field (name "data")
					(e-lookup-local
						(p-assign (ident "empty_list"))))
				(field (name "metadata")
					(e-record
						(fields
							(field (name "version")
								(e-lookup-local
									(p-assign (ident "num"))))
							(field (name "ratio")
								(e-lookup-local
									(p-assign (ident "frac"))))
							(field (name "description")
								(e-lookup-local
									(p-assign (ident "str"))))))))))
	(d-let
		(p-assign (ident "config1"))
		(e-record
			(fields
				(field (name "data")
					(e-list
						(elems
							(e-num (value "1"))
							(e-num (value "2"))
							(e-num (value "3"))
							(e-num (value "4"))
							(e-num (value "5")))))
				(field (name "metadata")
					(e-record
						(fields
							(field (name "version")
								(e-lookup-local
									(p-assign (ident "num"))))
							(field (name "ratio")
								(e-lookup-local
									(p-assign (ident "frac"))))
							(field (name "description")
								(e-lookup-local
									(p-assign (ident "str")))))))
				(field (name "name")
					(e-string
						(e-literal (string "integers")))))))
	(d-let
		(p-assign (ident "config2"))
		(e-record
			(fields
				(field (name "data")
					(e-list
						(elems
							(e-string
								(e-literal (string "apple")))
							(e-string
								(e-literal (string "banana")))
							(e-string
								(e-literal (string "cherry"))))))
				(field (name "metadata")
					(e-record
						(fields
							(field (name "version")
								(e-lookup-local
									(p-assign (ident "num"))))
							(field (name "ratio")
								(e-lookup-local
									(p-assign (ident "frac"))))
							(field (name "description")
								(e-lookup-local
									(p-assign (ident "str")))))))
				(field (name "name")
					(e-string
						(e-literal (string "fruits")))))))
	(d-let
		(p-assign (ident "make_container"))
		(e-lambda
			(args
				(p-assign (ident "val")))
			(e-record
				(fields
					(field (name "value")
						(e-lookup-local
							(p-assign (ident "val"))))
					(field (name "wrapper")
						(e-list
							(elems
								(e-lookup-local
									(p-assign (ident "val"))))))))))
	(d-let
		(p-assign (ident "container1"))
		(e-call
			(e-lookup-local
				(p-assign (ident "make_container")))
			(e-lookup-local
				(p-assign (ident "num")))))
	(d-let
		(p-assign (ident "container2"))
		(e-call
			(e-lookup-local
				(p-assign (ident "make_container")))
			(e-lookup-local
				(p-assign (ident "str")))))
	(d-let
		(p-assign (ident "container3"))
		(e-call
			(e-lookup-local
				(p-assign (ident "make_container")))
			(e-lookup-local
				(p-assign (ident "frac")))))
	(d-let
		(p-assign (ident "deep"))
		(e-record
			(fields
				(field (name "level1")
					(e-record
						(fields
							(field (name "level2")
								(e-record
									(fields
										(field (name "level3")
											(e-record
												(fields
													(field (name "data")
														(e-lookup-local
															(p-assign (ident "empty_list"))))
													(field (name "value")
														(e-lookup-local
															(p-assign (ident "num")))))))
										(field (name "items")
											(e-list
												(elems
													(e-lookup-local
														(p-assign (ident "num")))
													(e-binop (op "mul")
														(e-lookup-local
															(p-assign (ident "num")))
														(e-num (value "2")))
													(e-binop (op "mul")
														(e-lookup-local
															(p-assign (ident "num")))
														(e-num (value "3")))))))))
							(field (name "collection")
								(e-lookup-local
									(p-assign (ident "empty_list")))))))
				(field (name "results")
					(e-list
						(elems
							(e-record
								(fields
									(field (name "data")
										(e-list
											(elems
												(e-num (value "1")))))
									(field (name "tag")
										(e-string
											(e-literal (string "single"))))))
							(e-record
								(fields
									(field (name "data")
										(e-list
											(elems
												(e-num (value "1"))
												(e-num (value "2")))))
									(field (name "tag")
										(e-string
											(e-literal (string "ints"))))))
							(e-record
								(fields
									(field (name "data")
										(e-list
											(elems
												(e-num (value "1"))
												(e-num (value "2"))
												(e-num (value "3")))))
									(field (name "tag")
										(e-string
											(e-literal (string "more"))))))))))))
	(d-let
		(p-assign (ident "compute1"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "num")))
			(e-num (value "10"))))
	(d-let
		(p-assign (ident "compute2"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "num")))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "compute3"))
		(e-list
			(elems
				(e-lookup-local
					(p-assign (ident "num")))
				(e-lookup-local
					(p-assign (ident "num"))))))
	(d-let
		(p-assign (ident "compute4"))
		(e-record
			(fields
				(field (name "base")
					(e-lookup-local
						(p-assign (ident "num"))))
				(field (name "derived")
					(e-list
						(elems
							(e-lookup-local
								(p-assign (ident "num")))
							(e-binop (op "add")
								(e-lookup-local
									(p-assign (ident "num")))
								(e-num (value "1")))
							(e-binop (op "add")
								(e-lookup-local
									(p-assign (ident "num")))
								(e-num (value "2")))))))))
	(d-let
		(p-assign (ident "mixed"))
		(e-record
			(fields
				(field (name "numbers")
					(e-record
						(fields
							(field (name "value")
								(e-lookup-local
									(p-assign (ident "num"))))
							(field (name "list")
								(e-list
									(elems
										(e-lookup-local
											(p-assign (ident "num")))
										(e-lookup-local
											(p-assign (ident "num"))))))
							(field (name "float")
								(e-lookup-local
									(p-assign (ident "frac")))))))
				(field (name "strings")
					(e-record
						(fields
							(field (name "value")
								(e-lookup-local
									(p-assign (ident "str"))))
							(field (name "list")
								(e-list
									(elems
										(e-lookup-local
											(p-assign (ident "str")))
										(e-lookup-local
											(p-assign (ident "str")))))))))
				(field (name "empty_lists")
					(e-record
						(fields
							(field (name "raw")
								(e-lookup-local
									(p-assign (ident "empty_list"))))
							(field (name "in_list")
								(e-list
									(elems
										(e-lookup-local
											(p-assign (ident "empty_list"))))))
							(field (name "in_record")
								(e-record
									(fields
										(field (name "data")
											(e-lookup-local
												(p-assign (ident "empty_list"))))))))))
				(field (name "computations")
					(e-record
						(fields
							(field (name "from_num")
								(e-binop (op "mul")
									(e-lookup-local
										(p-assign (ident "num")))
									(e-num (value "100"))))
							(field (name "from_frac")
								(e-binop (op "mul")
									(e-lookup-local
										(p-assign (ident "frac")))
									(e-dec-small (numerator "100") (denominator-power-of-ten "1") (value "10"))))
							(field (name "list_from_num")
								(e-list
									(elems
										(e-lookup-local
											(p-assign (ident "num")))
										(e-lookup-local
											(p-assign (ident "num")))
										(e-lookup-local
											(p-assign (ident "num"))))))))))))
	(d-let
		(p-assign (ident "main"))
		(e-closure
			(captures
				(capture (ident "container1")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(e-binop (op "add")
						(e-dot-access (field "value")
							(receiver
								(e-lookup-local
									(p-assign (ident "container1")))))
						(e-num (value "10"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size)"))
		(patt (type "Num(Frac(_size))"))
		(patt (type "Error"))
		(patt (type "[True]_others"))
		(patt (type "List(Num(_size))"))
		(patt (type "{}"))
		(patt (type "List(Num(_size))"))
		(patt (type "List(Error)"))
		(patt (type "List([True, False]_others)"))
		(patt (type "List(List(Num(_size)))"))
		(patt (type "List(List(Num(_size)))"))
		(patt (type "{ count: Num(_size), items: List(Num(_size2)) }"))
		(patt (type "{ count: Num(_size), items: List(Num(_size2)) }"))
		(patt (type "{ count: Num(_size), items: List(Error) }"))
		(patt (type "{ data: List(Num(_size)), metadata: { description: Error, ratio: Num(Frac(_size2)), version: Num(_size3) } }"))
		(patt (type "{ data: List(Num(_size)), metadata: { description: Error, ratio: Num(Frac(_size2)), version: Num(_size3) }, name: Error }"))
		(patt (type "{ data: List(Error), metadata: { description: Error, ratio: Num(Frac(_size)), version: Num(_size2) }, name: Error }"))
		(patt (type "a -> { value: a, wrapper: List(a) }"))
		(patt (type "{ value: Num(_size), wrapper: List(Num(_size2)) }"))
		(patt (type "Error"))
		(patt (type "{ value: Num(Frac(_size)), wrapper: List(Num(Frac(_size2))) }"))
		(patt (type "{ level1: { collection: List(Num(_size)), level2: { items: List(Num(_size2)), level3: { data: List(Num(_size3)), value: Num(_size4) } } }, results: List({ data: List(Num(_size5)), tag: Error }) }"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "List(Num(_size))"))
		(patt (type "{ base: Num(_size), derived: List(Num(_size2)) }"))
		(patt (type "{ computations: { from_frac: Num(Frac(_size)), from_num: Num(_size2), list_from_num: List(Num(_size3)) }, empty_lists: { in_list: List(List(Num(_size4))), in_record: { data: List(Num(_size5)) }, raw: List(Num(_size6)) }, numbers: { float: Num(Frac(_size7)), list: List(Num(_size8)), value: Num(_size9) }, strings: { list: List(Error), value: Error } }"))
		(patt (type "_arg -> Num(_size)")))
	(expressions
		(expr (type "Num(_size)"))
		(expr (type "Num(Frac(_size))"))
		(expr (type "Error"))
		(expr (type "[True]_others"))
		(expr (type "List(Num(_size))"))
		(expr (type "{}"))
		(expr (type "List(Num(_size))"))
		(expr (type "List(Error)"))
		(expr (type "List([True, False]_others)"))
		(expr (type "List(List(Num(_size)))"))
		(expr (type "List(List(Num(_size)))"))
		(expr (type "{ count: Num(_size), items: List(Num(_size2)) }"))
		(expr (type "{ count: Num(_size), items: List(Num(_size2)) }"))
		(expr (type "{ count: Num(_size), items: List(Error) }"))
		(expr (type "{ data: List(Num(_size)), metadata: { description: Error, ratio: Num(Frac(_size2)), version: Num(_size3) } }"))
		(expr (type "{ data: List(Num(_size)), metadata: { description: Error, ratio: Num(Frac(_size2)), version: Num(_size3) }, name: Error }"))
		(expr (type "{ data: List(Error), metadata: { description: Error, ratio: Num(Frac(_size)), version: Num(_size2) }, name: Error }"))
		(expr (type "a -> { value: a, wrapper: List(a) }"))
		(expr (type "{ value: Num(_size), wrapper: List(Num(_size2)) }"))
		(expr (type "Error"))
		(expr (type "{ value: Num(Frac(_size)), wrapper: List(Num(Frac(_size2))) }"))
		(expr (type "{ level1: { collection: List(Num(_size)), level2: { items: List(Num(_size2)), level3: { data: List(Num(_size3)), value: Num(_size4) } } }, results: List({ data: List(Num(_size5)), tag: Error }) }"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "List(Num(_size))"))
		(expr (type "{ base: Num(_size), derived: List(Num(_size2)) }"))
		(expr (type "{ computations: { from_frac: Num(Frac(_size)), from_num: Num(_size2), list_from_num: List(Num(_size3)) }, empty_lists: { in_list: List(List(Num(_size4))), in_record: { data: List(Num(_size5)) }, raw: List(Num(_size6)) }, numbers: { float: Num(Frac(_size7)), list: List(Num(_size8)), value: Num(_size9) }, strings: { list: List(Error), value: Error } }"))
		(expr (type "_arg -> Num(_size)"))))
~~~
