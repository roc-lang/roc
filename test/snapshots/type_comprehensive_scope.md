# META
~~~ini
description=Comprehensive type scope validation - built-ins, user types, redeclaration, forward refs
type=file
~~~
# SOURCE
~~~roc
# Built-in types should work
MyU64 : U64
MyString : Str
MyBool : Bool

# Simple user-defined type
Person : { name: Str, age: U64 }

# Type with parameters
Result(ok, err) : [Ok(ok), Err(err)]

# Forward reference - Tree references Node before Node is defined
Tree(a) : [Branch(Node(a)), Leaf(a)]

# Node definition comes after Tree
Node(a) : { value: a, children: List(Tree(a)) }

# Using a previously defined type
MyResult : Result(Str, U64)

# Type redeclaration (should error)
Person : U64

# Using an undeclared type (should error)
BadType : SomeUndeclaredType

# Using built-in types with parameters
MyList : List(Str)
MyDict : Dict(Str, U64)

# Complex nested type using multiple declared types
Complex : {
    person: Person,
    result: Result(Bool, Str),
    tree: Tree(U64)
}
~~~
# EXPECTED
TYPE REDECLARED - type_comprehensive_scope.md:10:1:10:37
UNDECLARED TYPE - type_comprehensive_scope.md:13:19:13:23
TYPE REDECLARED - type_comprehensive_scope.md:22:1:22:13
UNDECLARED TYPE - type_comprehensive_scope.md:25:11:25:29
TYPE MODULE MISSING MATCHING TYPE - type_comprehensive_scope.md:2:1:36:2
# PROBLEMS
**TYPE REDECLARED**
The type _Result_ is being redeclared.

The redeclaration is here:
**type_comprehensive_scope.md:10:1:10:37:**
```roc
Result(ok, err) : [Ok(ok), Err(err)]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

But _Result_ was already declared here:
**type_comprehensive_scope.md:1:1:1:1:**
```roc
# Built-in types should work
```
^


**UNDECLARED TYPE**
The type _Node_ is not declared in this scope.

This type is referenced here:
**type_comprehensive_scope.md:13:19:13:23:**
```roc
Tree(a) : [Branch(Node(a)), Leaf(a)]
```
                  ^^^^


**TYPE REDECLARED**
The type _Person_ is being redeclared.

The redeclaration is here:
**type_comprehensive_scope.md:22:1:22:13:**
```roc
Person : U64
```
^^^^^^^^^^^^

But _Person_ was already declared here:
**type_comprehensive_scope.md:7:1:7:33:**
```roc
Person : { name: Str, age: U64 }
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _SomeUndeclaredType_ is not declared in this scope.

This type is referenced here:
**type_comprehensive_scope.md:25:11:25:29:**
```roc
BadType : SomeUndeclaredType
```
          ^^^^^^^^^^^^^^^^^^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `type_comprehensive_scope.roc`, but no top-level type declaration named `type_comprehensive_scope` was found.

Add either:
`type_comprehensive_scope := ...` (nominal type)
or:
`type_comprehensive_scope : ...` (type alias)
**type_comprehensive_scope.md:2:1:36:2:**
```roc
MyU64 : U64
MyString : Str
MyBool : Bool

# Simple user-defined type
Person : { name: Str, age: U64 }

# Type with parameters
Result(ok, err) : [Ok(ok), Err(err)]

# Forward reference - Tree references Node before Node is defined
Tree(a) : [Branch(Node(a)), Leaf(a)]

# Node definition comes after Tree
Node(a) : { value: a, children: List(Tree(a)) }

# Using a previously defined type
MyResult : Result(Str, U64)

# Type redeclaration (should error)
Person : U64

# Using an undeclared type (should error)
BadType : SomeUndeclaredType

# Using built-in types with parameters
MyList : List(Str)
MyDict : Dict(Str, U64)

# Complex nested type using multiple declared types
Complex : {
    person: Person,
    result: Result(Bool, Str),
    tree: Tree(U64)
}
```


# TOKENS
~~~zig
UpperIdent(2:1-2:6),OpColon(2:7-2:8),UpperIdent(2:9-2:12),
UpperIdent(3:1-3:9),OpColon(3:10-3:11),UpperIdent(3:12-3:15),
UpperIdent(4:1-4:7),OpColon(4:8-4:9),UpperIdent(4:10-4:14),
UpperIdent(7:1-7:7),OpColon(7:8-7:9),OpenCurly(7:10-7:11),LowerIdent(7:12-7:16),OpColon(7:16-7:17),UpperIdent(7:18-7:21),Comma(7:21-7:22),LowerIdent(7:23-7:26),OpColon(7:26-7:27),UpperIdent(7:28-7:31),CloseCurly(7:32-7:33),
UpperIdent(10:1-10:7),NoSpaceOpenRound(10:7-10:8),LowerIdent(10:8-10:10),Comma(10:10-10:11),LowerIdent(10:12-10:15),CloseRound(10:15-10:16),OpColon(10:17-10:18),OpenSquare(10:19-10:20),UpperIdent(10:20-10:22),NoSpaceOpenRound(10:22-10:23),LowerIdent(10:23-10:25),CloseRound(10:25-10:26),Comma(10:26-10:27),UpperIdent(10:28-10:31),NoSpaceOpenRound(10:31-10:32),LowerIdent(10:32-10:35),CloseRound(10:35-10:36),CloseSquare(10:36-10:37),
UpperIdent(13:1-13:5),NoSpaceOpenRound(13:5-13:6),LowerIdent(13:6-13:7),CloseRound(13:7-13:8),OpColon(13:9-13:10),OpenSquare(13:11-13:12),UpperIdent(13:12-13:18),NoSpaceOpenRound(13:18-13:19),UpperIdent(13:19-13:23),NoSpaceOpenRound(13:23-13:24),LowerIdent(13:24-13:25),CloseRound(13:25-13:26),CloseRound(13:26-13:27),Comma(13:27-13:28),UpperIdent(13:29-13:33),NoSpaceOpenRound(13:33-13:34),LowerIdent(13:34-13:35),CloseRound(13:35-13:36),CloseSquare(13:36-13:37),
UpperIdent(16:1-16:5),NoSpaceOpenRound(16:5-16:6),LowerIdent(16:6-16:7),CloseRound(16:7-16:8),OpColon(16:9-16:10),OpenCurly(16:11-16:12),LowerIdent(16:13-16:18),OpColon(16:18-16:19),LowerIdent(16:20-16:21),Comma(16:21-16:22),LowerIdent(16:23-16:31),OpColon(16:31-16:32),UpperIdent(16:33-16:37),NoSpaceOpenRound(16:37-16:38),UpperIdent(16:38-16:42),NoSpaceOpenRound(16:42-16:43),LowerIdent(16:43-16:44),CloseRound(16:44-16:45),CloseRound(16:45-16:46),CloseCurly(16:47-16:48),
UpperIdent(19:1-19:9),OpColon(19:10-19:11),UpperIdent(19:12-19:18),NoSpaceOpenRound(19:18-19:19),UpperIdent(19:19-19:22),Comma(19:22-19:23),UpperIdent(19:24-19:27),CloseRound(19:27-19:28),
UpperIdent(22:1-22:7),OpColon(22:8-22:9),UpperIdent(22:10-22:13),
UpperIdent(25:1-25:8),OpColon(25:9-25:10),UpperIdent(25:11-25:29),
UpperIdent(28:1-28:7),OpColon(28:8-28:9),UpperIdent(28:10-28:14),NoSpaceOpenRound(28:14-28:15),UpperIdent(28:15-28:18),CloseRound(28:18-28:19),
UpperIdent(29:1-29:7),OpColon(29:8-29:9),UpperIdent(29:10-29:14),NoSpaceOpenRound(29:14-29:15),UpperIdent(29:15-29:18),Comma(29:18-29:19),UpperIdent(29:20-29:23),CloseRound(29:23-29:24),
UpperIdent(32:1-32:8),OpColon(32:9-32:10),OpenCurly(32:11-32:12),
LowerIdent(33:5-33:11),OpColon(33:11-33:12),UpperIdent(33:13-33:19),Comma(33:19-33:20),
LowerIdent(34:5-34:11),OpColon(34:11-34:12),UpperIdent(34:13-34:19),NoSpaceOpenRound(34:19-34:20),UpperIdent(34:20-34:24),Comma(34:24-34:25),UpperIdent(34:26-34:29),CloseRound(34:29-34:30),Comma(34:30-34:31),
LowerIdent(35:5-35:9),OpColon(35:9-35:10),UpperIdent(35:11-35:15),NoSpaceOpenRound(35:15-35:16),UpperIdent(35:16-35:19),CloseRound(35:19-35:20),
CloseCurly(36:1-36:2),
EndOfFile(37:1-37:1),
~~~
# PARSE
~~~clojure
(file @2.1-36.2
	(type-module @2.1-2.6)
	(statements
		(s-type-decl @2.1-2.12
			(header @2.1-2.6 (name "MyU64")
				(args))
			(ty @2.9-2.12 (name "U64")))
		(s-type-decl @3.1-3.15
			(header @3.1-3.9 (name "MyString")
				(args))
			(ty @3.12-3.15 (name "Str")))
		(s-type-decl @4.1-4.14
			(header @4.1-4.7 (name "MyBool")
				(args))
			(ty @4.10-4.14 (name "Bool")))
		(s-type-decl @7.1-7.33
			(header @7.1-7.7 (name "Person")
				(args))
			(ty-record @7.10-7.33
				(anno-record-field @7.12-7.21 (name "name")
					(ty @7.18-7.21 (name "Str")))
				(anno-record-field @7.23-7.31 (name "age")
					(ty @7.28-7.31 (name "U64")))))
		(s-type-decl @10.1-10.37
			(header @10.1-10.16 (name "Result")
				(args
					(ty-var @10.8-10.10 (raw "ok"))
					(ty-var @10.12-10.15 (raw "err"))))
			(ty-tag-union @10.19-10.37
				(tags
					(ty-apply @10.20-10.26
						(ty @10.20-10.22 (name "Ok"))
						(ty-var @10.23-10.25 (raw "ok")))
					(ty-apply @10.28-10.36
						(ty @10.28-10.31 (name "Err"))
						(ty-var @10.32-10.35 (raw "err"))))))
		(s-type-decl @13.1-13.37
			(header @13.1-13.8 (name "Tree")
				(args
					(ty-var @13.6-13.7 (raw "a"))))
			(ty-tag-union @13.11-13.37
				(tags
					(ty-apply @13.12-13.27
						(ty @13.12-13.18 (name "Branch"))
						(ty-apply @13.19-13.26
							(ty @13.19-13.23 (name "Node"))
							(ty-var @13.24-13.25 (raw "a"))))
					(ty-apply @13.29-13.36
						(ty @13.29-13.33 (name "Leaf"))
						(ty-var @13.34-13.35 (raw "a"))))))
		(s-type-decl @16.1-16.48
			(header @16.1-16.8 (name "Node")
				(args
					(ty-var @16.6-16.7 (raw "a"))))
			(ty-record @16.11-16.48
				(anno-record-field @16.13-16.21 (name "value")
					(ty-var @16.20-16.21 (raw "a")))
				(anno-record-field @16.23-16.46 (name "children")
					(ty-apply @16.33-16.46
						(ty @16.33-16.37 (name "List"))
						(ty-apply @16.38-16.45
							(ty @16.38-16.42 (name "Tree"))
							(ty-var @16.43-16.44 (raw "a")))))))
		(s-type-decl @19.1-19.28
			(header @19.1-19.9 (name "MyResult")
				(args))
			(ty-apply @19.12-19.28
				(ty @19.12-19.18 (name "Result"))
				(ty @19.19-19.22 (name "Str"))
				(ty @19.24-19.27 (name "U64"))))
		(s-type-decl @22.1-22.13
			(header @22.1-22.7 (name "Person")
				(args))
			(ty @22.10-22.13 (name "U64")))
		(s-type-decl @25.1-25.29
			(header @25.1-25.8 (name "BadType")
				(args))
			(ty @25.11-25.29 (name "SomeUndeclaredType")))
		(s-type-decl @28.1-28.19
			(header @28.1-28.7 (name "MyList")
				(args))
			(ty-apply @28.10-28.19
				(ty @28.10-28.14 (name "List"))
				(ty @28.15-28.18 (name "Str"))))
		(s-type-decl @29.1-29.24
			(header @29.1-29.7 (name "MyDict")
				(args))
			(ty-apply @29.10-29.24
				(ty @29.10-29.14 (name "Dict"))
				(ty @29.15-29.18 (name "Str"))
				(ty @29.20-29.23 (name "U64"))))
		(s-type-decl @32.1-36.2
			(header @32.1-32.8 (name "Complex")
				(args))
			(ty-record @32.11-36.2
				(anno-record-field @33.5-33.19 (name "person")
					(ty @33.13-33.19 (name "Person")))
				(anno-record-field @34.5-34.30 (name "result")
					(ty-apply @34.13-34.30
						(ty @34.13-34.19 (name "Result"))
						(ty @34.20-34.24 (name "Bool"))
						(ty @34.26-34.29 (name "Str"))))
				(anno-record-field @35.5-35.20 (name "tree")
					(ty-apply @35.11-35.20
						(ty @35.11-35.15 (name "Tree"))
						(ty @35.16-35.19 (name "U64"))))))))
~~~
# FORMATTED
~~~roc
# Built-in types should work
# Built-in types should work
MyU64 : U64
MyString : Str
MyBool : Bool

# Simple user-defined type
Person : { name : Str, age : U64 }

# Type with parameters
Result(ok, err) : [Ok(ok), Err(err)]

# Forward reference - Tree references Node before Node is defined
Tree(a) : [Branch(Node(a)), Leaf(a)]

# Node definition comes after Tree
Node(a) : { value : a, children : List(Tree(a)) }

# Using a previously defined type
MyResult : Result(Str, U64)

# Type redeclaration (should error)
Person : U64

# Using an undeclared type (should error)
BadType : SomeUndeclaredType

# Using built-in types with parameters
MyList : List(Str)
MyDict : Dict(Str, U64)

# Complex nested type using multiple declared types
Complex : {
	person : Person,
	result : Result(Bool, Str),
	tree : Tree(U64),
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @2.1-2.12
		(ty-header @2.1-2.6 (name "MyU64"))
		(ty @2.9-2.12 (name "U64")))
	(s-alias-decl @3.1-3.15
		(ty-header @3.1-3.9 (name "MyString"))
		(ty @3.12-3.15 (name "Str")))
	(s-alias-decl @4.1-4.14
		(ty-header @4.1-4.7 (name "MyBool"))
		(ty @4.10-4.14 (name "Bool")))
	(s-alias-decl @7.1-7.33
		(ty-header @7.1-7.7 (name "Person"))
		(ty-record @7.10-7.33
			(field (field "name")
				(ty @7.18-7.21 (name "Str")))
			(field (field "age")
				(ty @7.28-7.31 (name "U64")))))
	(s-alias-decl @10.1-10.37
		(ty-header @10.1-10.16 (name "Result")
			(ty-args
				(ty-var @10.8-10.10 (name "ok"))
				(ty-var @10.12-10.15 (name "err"))))
		(ty-tag-union @10.19-10.37
			(ty-apply @10.20-10.26 (symbol "Ok")
				(ty-var @10.23-10.25 (name "ok")))
			(ty-apply @10.28-10.36 (symbol "Err")
				(ty-var @10.32-10.35 (name "err")))))
	(s-alias-decl @13.1-13.37
		(ty-header @13.1-13.8 (name "Tree")
			(ty-args
				(ty-var @13.6-13.7 (name "a"))))
		(ty-tag-union @13.11-13.37
			(ty-apply @13.12-13.27 (symbol "Branch")
				(ty-apply @13.19-13.26 (symbol "Node")
					(ty-var @13.24-13.25 (name "a"))))
			(ty-apply @13.29-13.36 (symbol "Leaf")
				(ty-var @13.34-13.35 (name "a")))))
	(s-alias-decl @16.1-16.48
		(ty-header @16.1-16.8 (name "Node")
			(ty-args
				(ty-var @16.6-16.7 (name "a"))))
		(ty-record @16.11-16.48
			(field (field "value")
				(ty-var @16.20-16.21 (name "a")))
			(field (field "children")
				(ty-apply @16.33-16.46 (symbol "List")
					(ty-apply @16.38-16.45 (symbol "Tree")
						(ty-var @16.43-16.44 (name "a")))))))
	(s-alias-decl @19.1-19.28
		(ty-header @19.1-19.9 (name "MyResult"))
		(ty-apply @19.12-19.28 (symbol "Result")
			(ty @19.19-19.22 (name "Str"))
			(ty @19.24-19.27 (name "U64"))))
	(s-alias-decl @22.1-22.13
		(ty-header @22.1-22.7 (name "Person"))
		(ty @22.10-22.13 (name "U64")))
	(s-alias-decl @25.1-25.29
		(ty-header @25.1-25.8 (name "BadType"))
		(ty @25.11-25.29 (name "SomeUndeclaredType")))
	(s-alias-decl @28.1-28.19
		(ty-header @28.1-28.7 (name "MyList"))
		(ty-apply @28.10-28.19 (symbol "List")
			(ty @28.15-28.18 (name "Str"))))
	(s-alias-decl @29.1-29.24
		(ty-header @29.1-29.7 (name "MyDict"))
		(ty-apply @29.10-29.24 (symbol "Dict")
			(ty @29.15-29.18 (name "Str"))
			(ty @29.20-29.23 (name "U64"))))
	(s-alias-decl @32.1-36.2
		(ty-header @32.1-32.8 (name "Complex"))
		(ty-record @32.11-36.2
			(field (field "person")
				(ty @33.13-33.19 (name "Person")))
			(field (field "result")
				(ty-apply @34.13-34.30 (symbol "Result")
					(ty @34.20-34.24 (name "Bool"))
					(ty @34.26-34.29 (name "Str"))))
			(field (field "tree")
				(ty-apply @35.11-35.20 (symbol "Tree")
					(ty @35.16-35.19 (name "U64")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @2.1-2.12 (type "MyU64")
			(ty-header @2.1-2.6 (name "MyU64")))
		(alias @3.1-3.15 (type "MyString")
			(ty-header @3.1-3.9 (name "MyString")))
		(alias @4.1-4.14 (type "MyBool")
			(ty-header @4.1-4.7 (name "MyBool")))
		(alias @7.1-7.33 (type "Person")
			(ty-header @7.1-7.7 (name "Person")))
		(alias @10.1-10.37 (type "Result(ok, err)")
			(ty-header @10.1-10.16 (name "Result")
				(ty-args
					(ty-var @10.8-10.10 (name "ok"))
					(ty-var @10.12-10.15 (name "err")))))
		(alias @13.1-13.37 (type "Tree(a)")
			(ty-header @13.1-13.8 (name "Tree")
				(ty-args
					(ty-var @13.6-13.7 (name "a")))))
		(alias @16.1-16.48 (type "Node(a)")
			(ty-header @16.1-16.8 (name "Node")
				(ty-args
					(ty-var @16.6-16.7 (name "a")))))
		(alias @19.1-19.28 (type "MyResult")
			(ty-header @19.1-19.9 (name "MyResult")))
		(alias @22.1-22.13 (type "Person")
			(ty-header @22.1-22.7 (name "Person")))
		(alias @25.1-25.29 (type "Error")
			(ty-header @25.1-25.8 (name "BadType")))
		(alias @28.1-28.19 (type "MyList")
			(ty-header @28.1-28.7 (name "MyList")))
		(alias @29.1-29.24 (type "MyDict")
			(ty-header @29.1-29.7 (name "MyDict")))
		(alias @32.1-36.2 (type "Complex")
			(ty-header @32.1-32.8 (name "Complex"))))
	(expressions))
~~~
