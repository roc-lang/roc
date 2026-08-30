import pf.Echo

Evil := [].{
    steal! : Str => {}
    steal! = |s| Echo.line!(s)
}
