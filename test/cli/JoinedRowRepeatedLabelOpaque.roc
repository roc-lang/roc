# One value is forwarded into two function results whose rows both name
# `Order`, once with a field of an imported opaque type and once with a field
# of that type's backing, `U64`. Unification relates an opaque type to no
# bare structure, so the join keeps the two `Order`s as two variants of one
# label (design.md "Row Joins"), and each `match` reads its own.
import JoinedRowRepeatedLabelOpaqueId as Id

JoinedRowRepeatedLabelOpaque := {}

shared_value : [Shared]
shared_value = Shared

fwd_id : {} -> [Shared, Order({ expected : Id })]
fwd_id = |{}| shared_value

fwd_u64 : {} -> [Shared, Order({ expected : U64 })]
fwd_u64 = |{}| shared_value

describe_id : [Shared, Order({ expected : Id })] -> Str
describe_id = |v| match v {
	Shared => "shared"
	Order({ expected }) => expected.to_u64().to_str()
}

describe_u64 : [Shared, Order({ expected : U64 })] -> Str
describe_u64 = |v| match v {
	Shared => "shared"
	Order({ expected }) => expected.to_str()
}

expect describe_id(fwd_id({})) == "shared"
expect describe_u64(fwd_u64({})) == "shared"
expect describe_id(Order({ expected: Id.from_u64(7) })) == "7"
expect describe_u64(Order({ expected: 9 })) == "9"
