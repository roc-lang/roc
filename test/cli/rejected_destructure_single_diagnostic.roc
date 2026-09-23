# A rejected top-level destructure is reported once by the checker; its
# `pattern_error` roots are excluded from compile-time evaluation, so no
# secondary compile-time crash is reported for the bound names.
(3, bad) = ("wrong", 1)

main! = |_args| Ok({})
