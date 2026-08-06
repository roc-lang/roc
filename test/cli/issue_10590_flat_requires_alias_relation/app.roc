app [Foo, bar, baz] { pf: platform "platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/10590
#
# A flat multi-entry platform requires for-clause may bind the same identity
# variable in each entry; this app should check cleanly.

Foo : Str

bar : {} -> Foo
bar = |{}| ""

baz : Foo -> {}
baz = |_| {}
