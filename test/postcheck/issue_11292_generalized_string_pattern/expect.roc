# Run with: roc test expect.roc --no-cache --specialize=no
pick = |value| match value {
    0 => 1
    _ => 2
}

rank = |value| match value {
    "low" => 1
    "high" => 2
    _ => 3
}

expect pick(0) == 1
expect pick(7.U64) == 2
expect rank("low".Str) == 1
expect rank("high".Str) == 2
expect rank("other".Str) == 3
