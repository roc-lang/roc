platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

# This case is important to test because the U128
# gives the whole struct an alignment of 16, but the
# Str is the largest variant, so the whole union has
# a size of 32 (due to alignment, rounded up from Str's 24),
# and the discriminant is stored in the 8+ bytes of padding
# that all variants have.
NonRecursive : [Foo Str, Bar U128, Blah I32, Baz]

main_for_host : {} -> NonRecursive
main_for_host = \{} -> main
