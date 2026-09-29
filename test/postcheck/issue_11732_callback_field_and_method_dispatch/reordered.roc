app [main!] { pf: platform "./platform/main.roc" }

Row :: { n : I32 }.{
    i32 : Row -> I32
    i32 = |row| row.n
}

OtherRow :: { value : I32 }.{
    i32 : OtherRow -> I32
    i32 = |row| row.value + 10
}

Db :: {}.{
    query : Db, (Row -> a) -> a
    query = |_db, decode_row| decode_row({ n: 1 })
}

OtherDb :: {}.{
    query : OtherDb, (OtherRow -> a) -> a
    query = |_db, decode_row| decode_row({ value: 2 })
}

process = |args| {
    rows = args.db.query(|row| row.i32())
    get_token = args.get_token
    token = get_token(args.db)
    { rows, token }
}

run = |args| process({ db: args.db, get_token: |_db| 1.I32 })

main! = |_args| {
    first = run({ db: Db.{} })
    other = run({ db: OtherDb.{} })
    repeated = run({ db: Db.{} })
    if first.rows == 1 and other.rows == 12 and repeated.rows == 1 and first.token == 1 and other.token == 1 and repeated.token == 1 {
        Ok({})
    } else {
        Err(WrongResult)
    }
}
