DerivedTagMapIneligible :: [].{}

AmbiguousNamed(a, b) := [NamedLeft(a), NamedRight(b)].{
	map : _
}

AllZstNamed(a, b) := [First(a), Second(b)].{
	map : _
}

OpaqueZst :: [OpaqueWrapped({})].{}

RepeatedParameter(a) := [RepeatedLeft(a), RepeatedRight(a)].{
	map : _
}

bad_named : AmbiguousNamed(U64, Str) -> AmbiguousNamed(U64, Str)
bad_named = |value| value.map(|payload| payload)

bad_structural : [StructuralLeft(U64), StructuralRight(Str)] -> [StructuralLeft(U64), StructuralRight(Str)]
bad_structural = |value| value.map(|payload| payload)

bad_all_zst : [ZstLeft({}), ZstRight({})] -> [ZstLeft({}), ZstRight({})]
bad_all_zst = |value| value.map(|payload| payload)

bad_all_zst_named : AllZstNamed({}, {}) -> AllZstNamed({}, {})
bad_all_zst_named = |value| value.map(|payload| payload)

bad_opaque_payload : [OpaqueSelected(U64), OpaqueIgnored(OpaqueZst)] -> [OpaqueSelected(U64), OpaqueIgnored(OpaqueZst)]
bad_opaque_payload = |value| value.map(|payload| payload)

bad_repeated_parameter : RepeatedParameter(U64) -> RepeatedParameter(U64)
bad_repeated_parameter = |value| value.map(|payload| payload)

bad_same_tag_same_type : [SameTagSame(U64, U64)] -> [SameTagSame(U64, U64)]
bad_same_tag_same_type = |value| value.map(|payload| payload)

bad_same_tag_different_types : [SameTagDifferent(U64, Str)] -> [SameTagDifferent(U64, Str)]
bad_same_tag_different_types = |value| value.map(|payload| payload)
