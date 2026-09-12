app [Msg, program] { pf: platform "./platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/11288
#
# The platform spawns a task from a zero-argument closure that captures `io`,
# selects `io.files()`, waits on a hosted read, and wraps the result in one of
# the app's message constructors. That closure lives in a provided export whose
# type is generic over the app's required `Msg`, so the root lowers the template
# at its own scheme. Building the app must lower that nested closure instead of
# crashing in post-check Monotype lowering.

Msg : [SmallRead(Str), BigRead(Str)]

program = { decode, update }

decode : Str -> Msg
decode = |contents| SmallRead(contents)

update : List(Msg) -> {}
update = |_messages| {}
