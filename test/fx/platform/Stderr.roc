import pf.Host

Stderr := [].{
    line! : Str => {}
    line! = |text| Host.put_stderr!(text)
}
