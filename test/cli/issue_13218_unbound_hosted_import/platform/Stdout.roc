import Host

Stdout := [].{
	line! : Str => {}
	line! = |s| Host.line!(s)
}
