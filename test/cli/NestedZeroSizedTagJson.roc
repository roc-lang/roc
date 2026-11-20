NestedZeroSizedTagJson := [].{}

# https://github.com/roc-lang/roc/issues/13235
# A tag whose payloads are all zero-sized lives in a union with no storage, so
# encoding reads each payload as its zero-sized value.

expect Json.to_str(Ok(C)) == "{\"Ok\":\"C\"}"

expect Json.to_str(Ok(Ok(C))) == "{\"Ok\":{\"Ok\":\"C\"}}"

expect Json.to_str(Pair(C, D)) == "{\"Pair\":[\"C\",\"D\"]}"
