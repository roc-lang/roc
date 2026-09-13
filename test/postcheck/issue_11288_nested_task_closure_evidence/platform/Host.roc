Host := [].{
	read_wait! : Str => Try(Str, [ReadFailed(Str)])
}
