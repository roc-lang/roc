# The same host symbol FallibleHost.str_ok! declares, reached through a
# transparent alias. The alias is the host ABI written another way, so the
# extern is identical; what differs is that Monotype lowering keeps the alias
# on the declared side, so recognizing the `Try` behind this hosted result—and
# building the widening adapter for a `?` use—has to cross it.
IoResult(a) : Try(a, [HostErr(Str)])

FallibleHostAlias := [].{
	str_ok! : {} => IoResult(Str)
}
