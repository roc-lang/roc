main! : List(Str) => Try(Str, _)
main! = |args| Json.parse(Str.join_with(args, ""))
