## A record-destructure field whose binder is rejected after the destructure
## statement itself was checked (a missing nested field, or a binder whose
## uses demand another type) rejects the whole statement. Only the expects
## that reach a rejected statement are compiler errors; the other one runs.
missing_nested = |_| {
    { a: { b } } = { a: {} }
    b
}

first_tag : { name : Str, tags : List(Str) } -> Str
first_tag = |item| {
    { tags, .. } = item
    tags
}

name_of : { name : Str, tags : List(Str) } -> Str
name_of = |item| {
    { name, .. } = item
    name
}

expect missing_nested({}) == 1
expect first_tag({ name: "backup", tags: ["nightly"] }) == "nightly"
expect name_of({ name: "backup", tags: [] }) == "backup"
