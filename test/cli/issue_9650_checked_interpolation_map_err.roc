package [f] {}

f : Str -> Try(U64, Str)
f = |s| Try.map_err(Err("${(s)}"), |_| "")
