BoxyDeadGeneralizedLambdaLiteral :: [].{}

# Generalized local functions with a numeric literal that nothing calls. No
# use fixes the literal's type, so nothing converts it, whether the function
# is never read, read once and discarded, or stored and never called.

unread = |s| {
    _local = |_| {
        _ = [s]
        1
    }
    2
}

read_once = |s| {
    local = |_| {
        _ = [s]
        1
    }
    _ = local
    2
}

stored = |s| {
    local = |_| {
        _ = [s]
        1
    }
    record = { f: local }
    _ = record
    2
}

expect unread("x") == 2
expect read_once("x") == 2
expect stored("x") == 2
