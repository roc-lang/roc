import Host

Stdout := [].{
	line! : Str => Try({}, [StdoutErr(Str), ..])
	line! = |text| Ok(Host.line!(text)?)
}
