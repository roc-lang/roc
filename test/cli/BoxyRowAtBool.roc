## A local function building a record generalizes the row of a tag it
## constructs, and a use reads that row at `Bool`. The descriptors the function
## receives then describe the constructed tag's row and the field's `Bool`, so
## the tag is re-tagged into Bool's own byte.
BoxyRowAtBool := [].{}

Check : { key : Str, pass : Bool, reason : Str }

check : Str, Str -> Check
check = |key, value| {
	failed = |reason| { key, pass: False, reason }
	passed = |reason| { key, pass: True, reason }
	if value == "ok" {
		passed("fine")
	} else {
		failed("${key} differs")
	}
}

## The same re-tagging below a list and a tag payload.
checks : List(Str) -> List(Try(Check, Str))
checks = |values| values.map(|value| Ok(check("item", value)))

expect check("a", "ok") == { key: "a", pass: True, reason: "fine" }
expect check("a", "no") == { key: "a", pass: False, reason: "a differs" }
expect !check("a", "no").pass
expect check("a", "ok").pass
expect checks(["ok", "no"]) == [Ok({ key: "item", pass: True, reason: "fine" }), Ok({ key: "item", pass: False, reason: "item differs" })]
