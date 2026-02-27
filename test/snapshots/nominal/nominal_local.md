# META
~~~ini
description=Local type declarations in block contexts
type=snippet
~~~
# SOURCE
~~~roc
main! = |_| {}

test = |{}| {
    Utf8Format := {}.{
        encode_str : Utf8Format, Str -> List(U8)
        encode_str = |_fmt, s| Str.to_utf8(s)
    }
		fmt = Utf8Format
    Str.encode("hi", fmt)
}
~~~
# EXPECTED
TYPE MISMATCH - nominal_local.md:9:22:9:25
# PROBLEMS
**TYPE MISMATCH**
The `encode_str` method on `Utf8Format` has an incompatible type:
**nominal_local.md:9:22:9:25:**
```roc
    Str.encode("hi", fmt)
```
                     ^^^

The method `encode_str` has the type:

    Utf8Format, Str -> List(U8)

But I need it to have the type:

    Utf8Format, Str -> Try(encoded, err)

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,CloseCurly,
LowerIdent,OpAssign,OpBar,OpenCurly,CloseCurly,OpBar,OpenCurly,
UpperIdent,OpColonEqual,OpenCurly,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,NamedUnderscore,Comma,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-record)))
		(s-decl
			(p-ident (raw "test"))
			(e-lambda
				(args
					(p-record))
				(e-block
					(statements
						(s-type-decl
							(header (name "Utf8Format")
								(args))
							(ty-record)
							(associated
								(s-type-anno (name "encode_str")
									(ty-fn
										(ty (name "Utf8Format"))
										(ty (name "Str"))
										(ty-apply
											(ty (name "List"))
											(ty (name "U8")))))
								(s-decl
									(p-ident (raw "encode_str"))
									(e-lambda
										(args
											(p-ident (raw "_fmt"))
											(p-ident (raw "s")))
										(e-apply
											(e-ident (raw "Str.to_utf8"))
											(e-ident (raw "s")))))))
						(s-decl
							(p-ident (raw "fmt"))
							(e-tag (raw "Utf8Format")))
						(e-apply
							(e-ident (raw "Str.encode"))
							(e-string
								(e-string-part (raw "hi")))
							(e-ident (raw "fmt")))))))))
~~~
# FORMATTED
~~~roc
main! = |_| {}

test = |{}| {
	Utf8Format := {}.{
		encode_str : Utf8Format, Str -> List(U8)
		encode_str = |_fmt, s| Str.to_utf8(s)
	}
	fmt = Utf8Format
	Str.encode("hi", fmt)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "echo!"))
		(e-hosted-lambda (symbol "echo!")
			(args
				(p-underscore)))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-tag-union
						(ty-underscore))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record)))
	(d-let
		(p-assign (ident "Utf8Format.encode_str"))
		(e-lambda
			(args
				(p-assign (ident "_fmt"))
				(p-assign (ident "s")))
			(e-call
				(e-lookup-external
					(builtin))
				(e-lookup-local
					(p-assign (ident "s")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Utf8Format") (local))
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "U8") (builtin))))))
	(d-let
		(p-assign (ident "test"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs)))
			(e-block
				(s-nominal-decl
					(ty-header (name "Utf8Format"))
					(ty-record))
				(s-let
					(p-assign (ident "fmt"))
					(e-nominal (nominal "Utf8Format")
						(e-empty_record)))
				(e-call
					(e-lookup-external
						(builtin))
					(e-string
						(e-literal (string "hi")))
					(e-lookup-local
						(p-assign (ident "fmt")))))))
	(s-nominal-decl
		(ty-header (name "Utf8Format"))
		(ty-record)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => Try({  }, [..])"))
		(patt (type "_arg -> {}"))
		(patt (type "Error"))
		(patt (type "{ .. } -> Error")))
	(type_decls
		(nominal (type "Error")
			(ty-header (name "Utf8Format"))))
	(expressions
		(expr (type "Str => Try({  }, [..])"))
		(expr (type "_arg -> {}"))
		(expr (type "Error, Error -> List(U8)"))
		(expr (type "{ .. } -> Error"))))
~~~
