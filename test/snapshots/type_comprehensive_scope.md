# META
~~~ini
description=Comprehensive type scope validation - built-ins, user types, redeclaration, forward refs
type=snippet
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
Try(ok, err) : [Ok(ok), Err(err)]

# Forward reference - Tree references Node before Node is defined
Tree(a) : [Branch(Node(a)), Leaf(a)]

# Node definition comes after Tree
Node(a) : { value: a, children: List(Tree(a)) }

# Using a previously defined type
MyTry : Try(Str, U64)

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
    result: Try(Bool, Str),
    tree: Tree(U64)
}
~~~
# EXPECTED
BUILTIN TYPE SHADOWED - type_comprehensive_scope.md:10:1:10:34
MUTUALLY RECURSIVE TYPE ALIASES - type_comprehensive_scope.md:13:1:13:37
MUTUALLY RECURSIVE TYPE ALIASES - type_comprehensive_scope.md:16:1:16:48
TYPE REDECLARED - type_comprehensive_scope.md:22:1:22:13
UNDECLARED TYPE - type_comprehensive_scope.md:25:11:25:29
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Builtin Type Shadowed")
		(region (start 10 1) (end 10 34))
		(headline
			(text "The type ")
			(annotated symbol-unqualified "Try")
			(text " shadows a builtin type."))
		(document
			(reflow "This may make the builtin type inaccessible in this scope.")
			(line-break)
			(source-region (file "type_comprehensive_scope.md") (start 10 1) (end 10 34) (annotation warning) (line-text "Try(ok, err) : [Ok(ok), Err(err)]"))))
	(report
		(severity runtime_error)
		(title "Mutually Recursive Type Aliases")
		(region (start 13 1) (end 13 37))
		(headline
			(reflow "The type alias ")
			(annotated code "Tree")
			(reflow " and ")
			(annotated code "Node")
			(reflow " form a recursive cycle."))
		(document
			(reflow "Type aliases are transparent synonyms and cannot be mutually recursive. ")
			(reflow "If you need recursive types, use nominal types (")
			(annotated code ":=")
			(reflow ") instead.")
			(line-break)
			(line-break)
			(source-region (file "type_comprehensive_scope.md") (start 13 1) (end 13 37) (annotation error) (line-text "Tree(a) : [Branch(Node(a)), Leaf(a)]"))
			(line-break)
			(reflow "And it references ")
			(annotated type "Node")
			(reflow " declared in ")
			(source-location
				(file "type_comprehensive_scope.md")
				(line 16)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "type_comprehensive_scope.md") (start 16 1) (end 16 48) (annotation dim) (line-text "Node(a) : { value: a, children: List(Tree(a)) }"))))
	(report
		(severity runtime_error)
		(title "Mutually Recursive Type Aliases")
		(region (start 16 1) (end 16 48))
		(headline
			(reflow "The type alias ")
			(annotated code "Node")
			(reflow " and ")
			(annotated code "Tree")
			(reflow " form a recursive cycle."))
		(document
			(reflow "Type aliases are transparent synonyms and cannot be mutually recursive. ")
			(reflow "If you need recursive types, use nominal types (")
			(annotated code ":=")
			(reflow ") instead.")
			(line-break)
			(line-break)
			(source-region (file "type_comprehensive_scope.md") (start 16 1) (end 16 48) (annotation error) (line-text "Node(a) : { value: a, children: List(Tree(a)) }"))
			(line-break)
			(reflow "And it references ")
			(annotated type "Tree")
			(reflow " declared in ")
			(source-location
				(file "type_comprehensive_scope.md")
				(line 13)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "type_comprehensive_scope.md") (start 13 1) (end 13 37) (annotation dim) (line-text "Tree(a) : [Branch(Node(a)), Leaf(a)]"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 22 1) (end 22 13))
		(headline
			(reflow "The type ")
			(annotated code "Person")
			(reflow " is being redeclared."))
		(document
			(source-region (file "type_comprehensive_scope.md") (start 22 1) (end 22 13) (annotation error) (line-text "Person : U64"))
			(line-break)
			(reflow "But ")
			(annotated type "Person")
			(reflow " was already declared in ")
			(source-location
				(file "type_comprehensive_scope.md")
				(line 7)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "type_comprehensive_scope.md") (start 7 1) (end 7 33) (annotation dim) (line-text "Person : { name: Str, age: U64 }"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 25 11) (end 25 29))
		(headline
			(reflow "The type ")
			(annotated code "SomeUndeclaredType")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "type_comprehensive_scope.md") (start 25 11) (end 25 29) (annotation error) (line-text "BadType : SomeUndeclaredType")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,CloseCurly,
UpperIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
UpperIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
UpperIdent,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "MyU64")
				(args))
			(ty (name "U64")))
		(s-type-decl
			(header (name "MyString")
				(args))
			(ty (name "Str")))
		(s-type-decl
			(header (name "MyBool")
				(args))
			(ty (name "Bool")))
		(s-type-decl
			(header (name "Person")
				(args))
			(ty-record
				(anno-record-field (name "name")
					(ty (name "Str")))
				(anno-record-field (name "age")
					(ty (name "U64")))))
		(s-type-decl
			(header (name "Try")
				(args
					(ty-var (raw "ok"))
					(ty-var (raw "err"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Ok"))
						(ty-var (raw "ok")))
					(ty-apply
						(ty (name "Err"))
						(ty-var (raw "err"))))))
		(s-type-decl
			(header (name "Tree")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Branch"))
						(ty-apply
							(ty (name "Node"))
							(ty-var (raw "a"))))
					(ty-apply
						(ty (name "Leaf"))
						(ty-var (raw "a"))))))
		(s-type-decl
			(header (name "Node")
				(args
					(ty-var (raw "a"))))
			(ty-record
				(anno-record-field (name "value")
					(ty-var (raw "a")))
				(anno-record-field (name "children")
					(ty-apply
						(ty (name "List"))
						(ty-apply
							(ty (name "Tree"))
							(ty-var (raw "a")))))))
		(s-type-decl
			(header (name "MyTry")
				(args))
			(ty-apply
				(ty (name "Try"))
				(ty (name "Str"))
				(ty (name "U64"))))
		(s-type-decl
			(header (name "Person")
				(args))
			(ty (name "U64")))
		(s-type-decl
			(header (name "BadType")
				(args))
			(ty (name "SomeUndeclaredType")))
		(s-type-decl
			(header (name "MyList")
				(args))
			(ty-apply
				(ty (name "List"))
				(ty (name "Str"))))
		(s-type-decl
			(header (name "MyDict")
				(args))
			(ty-apply
				(ty (name "Dict"))
				(ty (name "Str"))
				(ty (name "U64"))))
		(s-type-decl
			(header (name "Complex")
				(args))
			(ty-record
				(anno-record-field (name "person")
					(ty (name "Person")))
				(anno-record-field (name "result")
					(ty-apply
						(ty (name "Try"))
						(ty (name "Bool"))
						(ty (name "Str"))))
				(anno-record-field (name "tree")
					(ty-apply
						(ty (name "Tree"))
						(ty (name "U64"))))))))
~~~
# FORMATTED
~~~roc
# Built-in types should work
MyU64 : U64

MyString : Str

MyBool : Bool

# Simple user-defined type
Person : { name : Str, age : U64 }

# Type with parameters
Try(ok, err) : [Ok(ok), Err(err)]

# Forward reference - Tree references Node before Node is defined
Tree(a) : [Branch(Node(a)), Leaf(a)]

# Node definition comes after Tree
Node(a) : { value : a, children : List(Tree(a)) }

# Using a previously defined type
MyTry : Try(Str, U64)

# Type redeclaration (should error)
Person : U64

# Using an undeclared type (should error)
BadType : SomeUndeclaredType

# Using built-in types with parameters
MyList : List(Str)

MyDict : Dict(Str, U64)

# Complex nested type using multiple declared types
Complex : { person : Person, result : Try(Bool, Str), tree : Tree(U64) }
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "MyU64"))
		(ty-lookup (name "U64") (builtin)))
	(s-alias-decl
		(ty-header (name "MyString"))
		(ty-lookup (name "Str") (builtin)))
	(s-alias-decl
		(ty-header (name "MyBool"))
		(ty-lookup (name "Bool") (builtin)))
	(s-alias-decl
		(ty-header (name "Person"))
		(ty-record
			(field (field "name")
				(ty-lookup (name "Str") (builtin)))
			(field (field "age")
				(ty-lookup (name "U64") (builtin)))))
	(s-alias-decl
		(ty-header (name "Try")
			(ty-args
				(ty-rigid-var (name "ok"))
				(ty-rigid-var (name "err"))))
		(ty-tag-union
			(ty-tag-name (name "Ok")
				(ty-rigid-var-lookup (ty-rigid-var (name "ok"))))
			(ty-tag-name (name "Err")
				(ty-rigid-var-lookup (ty-rigid-var (name "err"))))))
	(s-alias-decl
		(ty-header (name "Tree")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-malformed))
	(s-alias-decl
		(ty-header (name "Node")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-malformed))
	(s-alias-decl
		(ty-header (name "MyTry"))
		(ty-apply (name "Try") (local)
			(ty-lookup (name "Str") (builtin))
			(ty-lookup (name "U64") (builtin))))
	(s-alias-decl
		(ty-header (name "Person"))
		(ty-lookup (name "U64") (builtin)))
	(s-alias-decl
		(ty-header (name "BadType"))
		(ty-malformed))
	(s-alias-decl
		(ty-header (name "MyList"))
		(ty-apply (name "List") (builtin)
			(ty-lookup (name "Str") (builtin))))
	(s-alias-decl
		(ty-header (name "MyDict"))
		(ty-apply (name "Dict") (builtin)
			(ty-lookup (name "Str") (builtin))
			(ty-lookup (name "U64") (builtin))))
	(s-alias-decl
		(ty-header (name "Complex"))
		(ty-record
			(field (field "person")
				(ty-lookup (name "Person") (local)))
			(field (field "result")
				(ty-apply (name "Try") (local)
					(ty-lookup (name "Bool") (builtin))
					(ty-lookup (name "Str") (builtin))))
			(field (field "tree")
				(ty-apply (name "Tree") (local)
					(ty-lookup (name "U64") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "MyU64")
			(ty-header (name "MyU64")))
		(alias (type "MyString")
			(ty-header (name "MyString")))
		(alias (type "MyBool")
			(ty-header (name "MyBool")))
		(alias (type "Person")
			(ty-header (name "Person")))
		(alias (type "Try(ok, err)")
			(ty-header (name "Try")
				(ty-args
					(ty-rigid-var (name "ok"))
					(ty-rigid-var (name "err")))))
		(alias (type "Error")
			(ty-header (name "Tree")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Error")
			(ty-header (name "Node")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "MyTry")
			(ty-header (name "MyTry")))
		(alias (type "Person")
			(ty-header (name "Person")))
		(alias (type "Error")
			(ty-header (name "BadType")))
		(alias (type "MyList")
			(ty-header (name "MyList")))
		(alias (type "MyDict")
			(ty-header (name "MyDict")))
		(alias (type "Error")
			(ty-header (name "Complex"))))
	(expressions))
~~~
