# The rejected examples of the langref's "Methods on Open Records" and
# "Methods on Open Tag Unions", each reported at its call or, for the
# annotation without a where clause, at the method call it lacks.

LangrefOpenRowMethodsRejected :: [].{}

same_account : { id : U64, ..others }, { id : U64, ..others } -> Bool where [others.is_eq : others, others -> Bool]
same_account = |a, b| a.id == b.id and a == b

Account := { id : U64, name : Str }

describe_with_id : { id : U64, ..others } -> Str where [others.describe : others -> Str]
describe_with_id = |record| "${record.id.to_str()}: ${record.describe()}"

same_account_without_where : { id : U64, ..others }, { id : U64, ..others } -> Bool
same_account_without_where = |a, b| a.id == b.id and a == b

is_red : [Red, ..others] -> Bool where [others.is_eq : others, others -> Bool]
is_red = |color| color == Red

expect same_account({ id: 1, log: |msg| msg }, { id: 1, log: |msg| msg })
expect same_account(Account.{ id: 1, name: "Sam" }, Account.{ id: 1, name: "Sam" })
expect describe_with_id({ id: 1, name: "Sam" }) == "1: Sam"
expect is_red(Fn(|x| x))
