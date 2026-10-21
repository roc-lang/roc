# A recursive nominal holding a `Dict` and a list of itself, compared with
# `==`. Its call passes descriptors for the list and for the nominal; each is
# materialized from the descriptors already filled, so the two levels of the
# recursive type never read each other.
main! = |_args| Ok({})

T := { props : Dict(Str, List(Str)), kids : List(T) }.{
	is_eq : _
}

mk : {} -> T
mk = |_| { props: Dict.empty(), kids: [] }

expect mk({}) == { props: Dict.empty(), kids: [] }
