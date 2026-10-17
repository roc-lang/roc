# The same repeated label reached through one nominal value: `job` is read by
# two where-method uses whose results name `Conflict` at two payload types, so
# the field's row joins both and holds both variants (design.md "Row Joins").
JoinedRowRepeatedLabelField := {}

Job := [Pending([Shared])].{
	status : Job -> [Shared]
	status = |job| match job { Pending(s) => s }
}

use_a : a -> [Shared, Conflict({ a : U64 })] where [a.status : a -> [Shared]]
use_a = |x| x.status()

use_b : a -> [Shared, Conflict({ b : Str })] where [a.status : a -> [Shared]]
use_b = |x| x.status()

describe_a : [Shared, Conflict({ a : U64 })] -> Str
describe_a = |v| match v {
	Shared => "shared"
	Conflict({ a }) => a.to_str()
}

describe_b : [Shared, Conflict({ b : Str })] -> Str
describe_b = |v| match v {
	Shared => "shared"
	Conflict({ b }) => b
}

job = Job.Pending(Shared)

expect describe_a(use_a(job)) == "shared"
expect describe_b(use_b(job)) == "shared"
