# META
~~~ini
description=Calling a method on a structural type should error
type=file
~~~
# SOURCE
~~~roc
# Define a nominal type with empty record backing and a method
Person := {}.{
  greet : Person -> Str
  greet = |_| "Hello"
}

# This should error: calling a method on an anonymous record,
# even though Person has compatible backing type
main = {
    x = {}
    x.greet()
}
~~~
# EXPECTED
MISSING METHOD - StructuralMethodError.md:11:5:11:14
# PROBLEMS
**MISSING METHOD**
This **greet** method is being called on a value whose type doesn't have that method:
**StructuralMethodError.md:11:5:11:14:**
```roc
    x.greet()
```
    ^^^^^^^^^

The value's type, which does not have a method named **greet**, is:

    {}

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,OpenCurly,CloseCurly,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Person")
				(args))
			(ty-record)
			(associated
				(s-type-anno (name "greet")
					(ty-fn
						(ty (name "Person"))
						(ty (name "Str"))))
				(s-decl
					(p-ident (raw "greet"))
					(e-lambda
						(args
							(p-underscore))
						(e-string
							(e-string-part (raw "Hello")))))))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "x"))
						(e-record))
					(e-method-call (method ".greet")
						(receiver
							(e-ident (raw "x")))
						(args)))))))
~~~
# FORMATTED
~~~roc
# Define a nominal type with empty record backing and a method
Person := {}.{
	greet : Person -> Str
	greet = |_| "Hello"
}

# This should error: calling a method on an anonymous record,
# even though Person has compatible backing type
main = {
	x = {}
	x.greet()
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "StructuralMethodError.Person.greet"))
		(e-lambda
			(args
				(p-underscore))
			(e-string
				(e-literal (string "Hello"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Person") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "main"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-nominal-decl
		(ty-header (name "Person"))
		(ty-record)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Person -> Str"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "Person")
			(ty-header (name "Person"))))
	(expressions
		(expr (type "Person -> Str"))
		(expr (type "Error"))))
~~~
