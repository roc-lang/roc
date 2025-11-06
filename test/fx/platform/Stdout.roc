import pf.Host

Stdout := [].{
    line! : Str => {}
    line! = |text| Host.put_stdout!(text)
}
