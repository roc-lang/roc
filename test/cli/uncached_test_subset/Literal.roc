import Query

expect {
    dbg "literal test outcome"
    query : Query.Sql(I32)
    query = Query.get(1)
    query.text == "select 1"
}
