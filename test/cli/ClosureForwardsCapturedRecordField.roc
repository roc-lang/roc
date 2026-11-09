ClosureForwardsCapturedRecordField := {}

# A closure captures a record and forwards it through another function into
# code that takes the record's list field and folds or maps over it. The taken
# field is aliased before it moves into a call, and the field must be released
# exactly once.

first_found = |config| {
    found = [1]
    config.names.append("b").fold(Err(ListWasEmpty), |_, _| found.first())
}

forward = |config| first_found(config)

expect {
    config = { names: ["c"] }
    check = |_| forward(config).is_ok()
    check({})
}

all_found = |config| {
    found = ["a"]
    config.names.concat(["b"]).map(|_| found.first())
}

forward_all = |config| all_found(config)

expect {
    config = { names: ["c"] }
    check = |_| forward_all(config).len() == 2
    check({})
}
