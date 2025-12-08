app [main!] { pf: platform "./platform/main.roc" }

# Regression test: Calling .sublist() method on a List(U8) from "".to_utf8()
# causes a segfault when the variable doesn't have an explicit type annotation.
# Error was: "Roc crashed: Error evaluating from shared memory: InvalidMethodReceiver"
# The bug was that translateTypeVar was using the wrong module (closure's source module)
# instead of the caller's module when translating the return type.
main! = || {
    # Test case 1: Method call without type annotation (original bug)
    s = "".to_utf8()
    _slice = s.sublist({ start: 0, len: 0 })

    # Test case 2: Comparing empty list with method result
    _ignore = "".to_utf8() == []
}
