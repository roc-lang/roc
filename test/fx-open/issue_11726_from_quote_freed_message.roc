app [main!] { pf: platform "./platform/main.roc" }

Sql(a) := { text : Str }.{
      from_quote : Str -> Try(Sql(a), [BadQuotedBytes(Str)])
      from_quote = |raw|
              if raw == "bad" {
                      Err(BadQuotedBytes("this message is long enough to live on the heap"))
              } else {
                      Ok(Sql.{ text: raw.repeat(10) })
              }
}

use : Sql(a) -> {}
use = |_| {}

main! = |_args| {
      use("bad")
      use("good")
      Ok({})
}
