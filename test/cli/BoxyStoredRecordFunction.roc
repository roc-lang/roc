BoxyStoredRecordFunction := {}

# repro for https://github.com/roc-lang/roc/issues/11874: Boxy restores a
# function stored inside a compile-time record constant.
c : { f : () -> {} }
c = { f: || {} }

expect (|_| Bool.True)(c)

shout : { exclaim : Str -> Str, name : Str }
shout = { exclaim: |s| Str.concat(s, "!"), name: "shout" }

expect (shout.exclaim)(shout.name) == "shout!"
