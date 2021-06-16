interface Process.Child
    exposes
        [
            Child,
        ]
    imports [ fx.Effect, Pid.Internal ]

Child :
    @Child
        {
            sdtin : Cursor,
            stdout : Cursor,
            stderr : Cursor,
            pid : Pid,
        }

stdin : Child -> Cursor
stdin = \@Child child -> child.stdin

stdout : Child -> Cursor
stdout = \@Child child -> child.stdout

stderr : Child -> Cursor
stderr = \@Child child -> child.stderr

process : Child -> Cursor
process = \@Child child -> child.pid
