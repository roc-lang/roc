# The accepted examples of the langref's "Methods on Open Records" and
# "Methods on Open Tag Unions".

LangrefOpenRowMethods :: [].{}

same_account : { id : U64, ..others }, { id : U64, ..others } -> Bool where [others.is_eq : others, others -> Bool]
same_account = |a, b| a.id == b.id and a == b

Customer := { id : U64, name : Str }.{
	is_eq : Customer, Customer -> Bool
	is_eq = |a, b| a.id == b.id

	describe : Customer -> Str
	describe = |customer| "customer ${customer.name}"
}

get_id : { id : U64, ..others } -> U64
get_id = |record| record.id

describe_with_id : { id : U64, ..others } -> Str where [others.describe : others -> Str]
describe_with_id = |record| "${record.id.to_str()}: ${record.describe()}"

is_red : [Red, ..others] -> Bool where [others.is_eq : others, others -> Bool]
is_red = |color| color == Red

expect same_account({ id: 1, name: "Sam" }, { id: 1, name: "Sam" })
expect same_account(Customer.{ id: 1, name: "Sam" }, Customer.{ id: 1, name: "Samantha" })
expect get_id({ id: 7, log: |msg| msg }) == 7
expect describe_with_id(Customer.{ id: 1, name: "Sam" }) == "1: customer Sam"
expect !is_red(Green)
