app "echo"
    packages { pf: "platform" }
    imports [ pf.Stdout, pf.Task.{await}, pf.Db ]
    provides [ main ] to pf

query1 = "CREATE TABLE IF NOT EXISTS foo (bar INTEGER);"
query2 = "INSERT INTO foo (bar) VALUES 1;"
query3 = "SELECT * FROM foo;"

main : Task.Task {} []
main =
    _ <- await (Db.raw (query1))
    _ <- await (Db.raw (query2))
    answer <- await (Db.raw (query3))
    Stdout.line "The answer is \(answer)!"

