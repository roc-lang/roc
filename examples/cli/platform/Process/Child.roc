interface Process.Child
    exposes [
        Child,
    ]
    imports [ fx.Effect, Pid.Internal ]

Child :
    @Child
        {
            sdtin : Stream,
            stdout : Stream,
            stderr : Stream,
            pid : Pid,
        }

stdin : Child -> Stream
stdin = \@Child child -> child.stdin

stdout : Child -> Stream
stdout = \@Child child -> child.stdout

stderr : Child -> Stream
stderr = \@Child child -> child.stderr

process : Child -> Pid
process = \@Child child -> child.pid
