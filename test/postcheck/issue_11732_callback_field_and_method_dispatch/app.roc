app [main!] { pf: platform "./platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/11732
#
# `args.db` is both passed to a callback field (`args.get_token`) and used for
# method dispatch (`args.db.query(...)`). Post-check Monotype lowering must
# agree on the target contract's evidence kind for the nested callable site;
# building the app must lower to LIR instead of panicking with
# "checked target contract differed from substitution-derived evidence kind".

Row :: { n : I32 }.{
    i32 : Row -> I32
    i32 = |row| row.n
}

Db :: {}.{
    query : Db, (Row -> a) -> a
    query = |db, decode_row| {
        _ = db
        decode_row({ n: 1 })
    }
}

process = |args| {
    get_token = args.get_token
    token = get_token(args.db)
    rows = args.db.query(|row| row.i32())
    _ = token
    _ = rows
    {}
}

run = |args| {
    process({ db: args.db, get_token: |_db| 1 })
    Ok({})
}

main! = |_args| {
    db = Db.{}
    _ = run({ db, })
    Ok({})
}
