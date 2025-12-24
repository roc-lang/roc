# META
~~~ini
description=Local type declarations in block contexts
type=snippet
~~~
# SOURCE
~~~roc
# Function with local type alias
processData = |_| {
    # Local type alias for U64
    MyNum : U64

    # Local type alias for a record
    MyRecord : { value: U64 }

    42
}

# Nested blocks with local types
nestedExample = |_x| {
    OuterType : Str

    innerBlock = {
        InnerType : U64
        123
    }

    innerBlock
}

# Local nominal type
withNominal = |n| {
    Counter := U64

    n
}

# Local opaque type
withOpaque = |s| {
    Secret :: Str

    s
}

# Multiple local types in same block
multipleTypes = |_| {
    First : U64
    Second : Str
    Third : { a: U64, b: Str }

    "result"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,
Int,
CloseCurly,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
UpperIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
UpperIdent,OpColon,UpperIdent,
Int,
CloseCurly,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
UpperIdent,OpColonEqual,UpperIdent,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
UpperIdent,OpDoubleColon,UpperIdent,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "processData"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-type-decl
							(header (name "MyNum")
								(args))
							(ty (name "U64")))
						(s-type-decl
							(header (name "MyRecord")
								(args))
							(ty-record
								(anno-record-field (name "value")
									(ty (name "U64")))))
						(e-int (raw "42"))))))
		(s-decl
			(p-ident (raw "nestedExample"))
			(e-lambda
				(args
					(p-ident (raw "_x")))
				(e-block
					(statements
						(s-type-decl
							(header (name "OuterType")
								(args))
							(ty (name "Str")))
						(s-decl
							(p-ident (raw "innerBlock"))
							(e-block
								(statements
									(s-type-decl
										(header (name "InnerType")
											(args))
										(ty (name "U64")))
									(e-int (raw "123")))))
						(e-ident (raw "innerBlock"))))))
		(s-decl
			(p-ident (raw "withNominal"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-block
					(statements
						(s-type-decl
							(header (name "Counter")
								(args))
							(ty (name "U64")))
						(e-ident (raw "n"))))))
		(s-decl
			(p-ident (raw "withOpaque"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-type-decl
							(header (name "Secret")
								(args))
							(ty (name "Str")))
						(e-ident (raw "s"))))))
		(s-decl
			(p-ident (raw "multipleTypes"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-type-decl
							(header (name "First")
								(args))
							(ty (name "U64")))
						(s-type-decl
							(header (name "Second")
								(args))
							(ty (name "Str")))
						(s-type-decl
							(header (name "Third")
								(args))
							(ty-record
								(anno-record-field (name "a")
									(ty (name "U64")))
								(anno-record-field (name "b")
									(ty (name "Str")))))
						(e-string
							(e-string-part (raw "result")))))))))
~~~
# FORMATTED
~~~roc
# Function with local type alias
processData = |_| {
	# Local type alias for U64
	MyNum : U64

	# Local type alias for a record
	MyRecord : { value : U64 }

	42
}

# Nested blocks with local types
nestedExample = |_x| {
	OuterType : Str

	innerBlock = {
		InnerType : U64
		123
	}

	innerBlock
}

# Local nominal type
withNominal = |n| {
	Counter := U64

	n
}

# Local opaque type
withOpaque = |s| {
	Secret :: Str

	s
}

# Multiple local types in same block
multipleTypes = |_| {
	First : U64
	Second : Str
	Third : { a : U64, b : Str }

	"result"
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "processData"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-alias-decl
					(ty-header (name "MyNum"))
					(ty-lookup (name "U64") (builtin)))
				(s-alias-decl
					(ty-header (name "MyRecord"))
					(ty-record
						(field (field "value")
							(ty-lookup (name "U64") (builtin)))))
				(e-num (value "42")))))
	(d-let
		(p-assign (ident "nestedExample"))
		(e-lambda
			(args
				(p-assign (ident "_x")))
			(e-block
				(s-alias-decl
					(ty-header (name "OuterType"))
					(ty-lookup (name "Str") (builtin)))
				(s-let
					(p-assign (ident "innerBlock"))
					(e-block
						(s-alias-decl
							(ty-header (name "InnerType"))
							(ty-lookup (name "U64") (builtin)))
						(e-num (value "123"))))
				(e-lookup-local
					(p-assign (ident "innerBlock"))))))
	(d-let
		(p-assign (ident "withNominal"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-block
				(s-nominal-decl
					(ty-header (name "Counter"))
					(ty-lookup (name "U64") (builtin)))
				(e-lookup-local
					(p-assign (ident "n"))))))
	(d-let
		(p-assign (ident "withOpaque"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-nominal-decl
					(ty-header (name "Secret"))
					(ty-lookup (name "Str") (builtin)))
				(e-lookup-local
					(p-assign (ident "s"))))))
	(d-let
		(p-assign (ident "multipleTypes"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-alias-decl
					(ty-header (name "First"))
					(ty-lookup (name "U64") (builtin)))
				(s-alias-decl
					(ty-header (name "Second"))
					(ty-lookup (name "Str") (builtin)))
				(s-alias-decl
					(ty-header (name "Third"))
					(ty-record
						(field (field "a")
							(ty-lookup (name "U64") (builtin)))
						(field (field "b")
							(ty-lookup (name "Str") (builtin)))))
				(e-string
					(e-literal (string "result")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "_arg -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c -> c"))
		(patt (type "c -> c"))
		(patt (type "_arg -> Str")))
	(expressions
		(expr (type "_arg -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "_arg -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "c -> c"))
		(expr (type "c -> c"))
		(expr (type "_arg -> Str"))))
~~~
