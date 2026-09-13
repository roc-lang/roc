import Host

Stdout := [].{
    report! : Try({}, [StdoutErr(Str)]) => {}
    report! = |result| Host.report!(result)
}
