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

stdin : Child -> Cursor { read : Yes, write : Yes }
stdin = \@Child child -> child.stdin

stdout : Child -> Cursor { read : Yes, write : Yes }
stdout = \@Child child -> child.stdout

stderr : Child -> Cursor { read : Yes, write : Yes }
stderr = \@Child child -> child.stderr

process : Child -> Cursor { read : Yes, write : Yes }
process = \@Child child -> child.pid
