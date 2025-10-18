# META
~~~ini
description=Record construction with complex field types including lists and tag unions
type=expr
~~~
# SOURCE
~~~roc
{
    name: "Alice",
    scores: [95, 87, 92, 78],
    status: Active({ since: "2023-01-15" }),
    preferences: { theme: Dark, notifications: Email("alice@example.com") },
    metadata: Ok({
        tags: ["developer", "senior", "fullstack"],
        permissions: [Read, Write, Admin],
    }),
    callback: |x| x + 1,
    nested: {
        items: [Some("first"), None, Some("third")],
        result: Success({ data: [1, 2, 3], timestamp: "2024-01-01" }),
    },
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,OpenSquare,Int,Comma,Int,Comma,Int,Comma,Int,CloseSquare,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,CloseRound,Comma,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseCurly,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,
LowerIdent,OpColon,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,Comma,
LowerIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,Comma,
CloseCurly,CloseRound,Comma,
LowerIdent,OpColon,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,Comma,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,Comma,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseSquare,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,CloseRound,Comma,
CloseCurly,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "name")
		(e-string
			(e-string-part (raw "Alice"))))
	(field (field "scores")
		(e-list
			(e-int (raw "95"))
			(e-int (raw "87"))
			(e-int (raw "92"))
			(e-int (raw "78"))))
	(field (field "status")
		(e-apply
			(e-tag (raw "Active"))
			(e-record
				(field (field "since")
					(e-string
						(e-string-part (raw "2023-01-15")))))))
	(field (field "preferences")
		(e-record
			(field (field "theme")
				(e-tag (raw "Dark")))
			(field (field "notifications")
				(e-apply
					(e-tag (raw "Email"))
					(e-string
						(e-string-part (raw "alice@example.com")))))))
	(field (field "metadata")
		(e-apply
			(e-tag (raw "Ok"))
			(e-record
				(field (field "tags")
					(e-list
						(e-string
							(e-string-part (raw "developer")))
						(e-string
							(e-string-part (raw "senior")))
						(e-string
							(e-string-part (raw "fullstack")))))
				(field (field "permissions")
					(e-list
						(e-tag (raw "Read"))
						(e-tag (raw "Write"))
						(e-tag (raw "Admin")))))))
	(field (field "callback")
		(e-lambda
			(args
				(p-ident (raw "x")))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-int (raw "1")))))
	(field (field "nested")
		(e-record
			(field (field "items")
				(e-list
					(e-apply
						(e-tag (raw "Some"))
						(e-string
							(e-string-part (raw "first"))))
					(e-tag (raw "None"))
					(e-apply
						(e-tag (raw "Some"))
						(e-string
							(e-string-part (raw "third"))))))
			(field (field "result")
				(e-apply
					(e-tag (raw "Success"))
					(e-record
						(field (field "data")
							(e-list
								(e-int (raw "1"))
								(e-int (raw "2"))
								(e-int (raw "3"))))
						(field (field "timestamp")
							(e-string
								(e-string-part (raw "2024-01-01"))))))))))
~~~
# FORMATTED
~~~roc
{
	name: "Alice",
	scores: [95, 87, 92, 78],
	status: Active({ since: "2023-01-15" }),
	preferences: { theme: Dark, notifications: Email("alice@example.com") },
	metadata: Ok(
		{
			tags: ["developer", "senior", "fullstack"],
			permissions: [Read, Write, Admin],
		},
	),
	callback: |x| x + 1,
	nested: {
		items: [Some("first"), None, Some("third")],
		result: Success({ data: [1, 2, 3], timestamp: "2024-01-01" }),
	},
}
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "name")
			(e-string
				(e-literal (string "Alice"))))
		(field (name "scores")
			(e-list
				(elems
					(e-num (value "95"))
					(e-num (value "87"))
					(e-num (value "92"))
					(e-num (value "78")))))
		(field (name "status")
			(e-tag (name "Active")
				(args
					(e-record
						(fields
							(field (name "since")
								(e-string
									(e-literal (string "2023-01-15")))))))))
		(field (name "preferences")
			(e-record
				(fields
					(field (name "theme")
						(e-tag (name "Dark")))
					(field (name "notifications")
						(e-tag (name "Email")
							(args
								(e-string
									(e-literal (string "alice@example.com")))))))))
		(field (name "metadata")
			(e-nominal (nominal "Result")
				(e-tag (name "Ok")
					(args
						(e-record
							(fields
								(field (name "tags")
									(e-list
										(elems
											(e-string
												(e-literal (string "developer")))
											(e-string
												(e-literal (string "senior")))
											(e-string
												(e-literal (string "fullstack"))))))
								(field (name "permissions")
									(e-list
										(elems
											(e-tag (name "Read"))
											(e-tag (name "Write"))
											(e-tag (name "Admin")))))))))))
		(field (name "callback")
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "x")))
					(e-num (value "1")))))
		(field (name "nested")
			(e-record
				(fields
					(field (name "items")
						(e-list
							(elems
								(e-tag (name "Some")
									(args
										(e-string
											(e-literal (string "first")))))
								(e-tag (name "None"))
								(e-tag (name "Some")
									(args
										(e-string
											(e-literal (string "third"))))))))
					(field (name "result")
						(e-tag (name "Success")
							(args
								(e-record
									(fields
										(field (name "data")
											(e-list
												(elems
													(e-num (value "1"))
													(e-num (value "2"))
													(e-num (value "3")))))
										(field (name "timestamp")
											(e-string
												(e-literal (string "2024-01-01"))))))))))))))
~~~
# TYPES
~~~clojure
(expr (type "{ callback: Num(_size) -> Num(_size2), metadata: Result({ permissions: List([Read, Write, Admin]_others), tags: List(Str) }, err), name: Str, nested: { items: List([Some(Str)][None]_others2), result: [Success({ data: List(Num(_size3)), timestamp: Str })]_others3 }, preferences: { notifications: [Email(Str)]_others4, theme: [Dark]_others5 }, scores: List(Num(_size4)), status: [Active({ since: Str })]_others6 }"))
~~~
