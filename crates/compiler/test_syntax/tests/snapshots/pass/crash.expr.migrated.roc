_ = crash("")
_ = crash("", "")
_ = crash(15, 123)
_ = try(foo, (|_| crash("")))
_ =
{
    _ = crash("")
    crash
}

{f: crash("")}
