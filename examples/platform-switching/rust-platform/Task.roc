interface Task
    exposes [Task, stdoutLine, andThen, fail, succeed, taskToOp, Op]
    imports []

Op : [
    StdoutLine Str (Box ({} -> Op)),
    StdinLine (Box (Str -> Op)),
    None,
]

Task ok err : [Task ((Result ok err -> Op) -> Op)]

# Internal-only helper for getting an Op to the host,
# so the host doesn't need to deal with Task
taskToOp : Task {} [] -> Op
taskToOp = \Task fromResult -> fromResult \Ok {} -> None

andThen : Task a err, (a -> Task b err) -> Task b err
andThen = \Task fromResult, fromOk ->
    Task \continue ->
        fromResult \result ->
            (Task inner) =
                when result is
                    Ok ok -> fromOk ok
                    Err error -> fail error

            inner continue

succeed : ok -> Task ok *
succeed = \ok -> Task \continue -> continue (Ok ok)

fail : err -> Task * err
fail = \err -> Task \continue -> continue (Err err)

stdoutLine : Str -> Task {} *
stdoutLine = \line ->
    Task \fromResult ->
        StdoutLine line (Box.box \{} -> fromResult (Ok {}))
