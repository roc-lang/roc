import OsStr exposing [OsStr]
import Host

Env := [].{
	var! : Str => Try(OsStr, [VarNotFound(Str), ..])
	var! = |name| Ok(Host.var!(name)?)
}
