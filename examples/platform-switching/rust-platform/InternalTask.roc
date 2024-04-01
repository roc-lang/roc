interface InternalTask
    exposes [Task, stdoutLine] # , andThen, fail, succeed]
    imports []

Op : [
    StdoutLine Str ({} -> Op),
    StdinLine (Str -> Op),
    None,
]

Task ok err : [Task ((Result ok err -> Op) -> Op)]

# andThen : Task a err, (a -> Task b err) -> Task b err
# andThen = \@Task fromResult, fromOk ->
#     cont = \continue ->
#         fromResult \result ->
#             (@Task inner) =
#                 when result is
#                     Ok ok -> fromOk ok
#                     Err error -> fail error

#             inner continue

#     @Task cont

# succeed : ok -> Task ok *
# succeed = \ok -> @Task (\continue -> continue (Ok ok))

# fail : err -> Task * err
# fail = \err -> @Task (\continue -> continue (Err err))

stdoutLine : Str -> Task {} *
stdoutLine = \line ->
    Task \fromResult ->
        StdoutLine line \{} -> fromResult (Ok {})

# stdoutLine : Str -> Task {} *
# stdoutLine = \line ->
#     cont : (Result {} err -> Op) -> Op
#     cont = \fromResult ->
#         fromResult2 : Result {} err -> Op
#         fromResult2 = fromResult

#         unboxed : {} -> Op
#         unboxed = \{} -> fromResult2 (Ok {})

#         # boxed : Box ({} -> Op)
#         # boxed = Box.box unboxed

#         answer : Op
#         answer = StdoutLine line unboxed

#         crash "blah"

#     @Task (Box.box cont)

# stdinLine : Task Str *
# stdinLine =
#     @Task \toNext ->
#         StdinLine \line -> toNext (Ok line)

# Op : [
#     StdoutLine Str (Box ({} -> Op)),
#     StdinLine (Box (Str -> Op)),
#     None,
# ]

# Task ok err := Box ((Result ok err -> Op) -> Op)

# andThen : Task a err, (a -> Task b err) -> Task b err
# andThen = \@Task fromResult, fromOk ->
#     cont = \continue ->
#         (Box.unbox fromResult) \result ->
#             (@Task inner) =
#                 when result is
#                     Ok ok -> fromOk ok
#                     Err error -> fail error

#             (Box.unbox inner) continue

#     @Task (Box.box cont)

# succeed : ok -> Task ok *
# succeed = \ok -> @Task (Box.box \continue -> continue (Ok ok))

# fail : err -> Task * err
# fail = \err -> @Task (Box.box \continue -> continue (Err err))

# stdoutLine : Str -> Task {} *
# stdoutLine = \line ->
#     cont : (Result {} err -> Op) -> Op
#     cont = \fromResult ->
#         fromResult2 : Result {} err -> Op
#         fromResult2 = fromResult

#         unboxed : {} -> Op
#         unboxed = \{} -> fromResult2 (Ok {})

#         boxed : Box ({} -> Op)
#         boxed = Box.box unboxed

#         answer : Op
#         answer = StdoutLine line boxed

#         crash "blah"

#     @Task (Box.box cont)

# # stdinLine : Task Str *
# # stdinLine =
# #     @Task \toNext ->
# #         StdinLine \line -> toNext (Ok line)
