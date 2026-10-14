# Support module for lambda_set_dispatch_matrix.roc: closures built in another
# module, passed to the importer's higher-order function beside local ones.

LambdaSetDispatchClosures := [].{
	adder : I64 -> (I64 -> I64)
	adder = |amount| |n| n + amount

	multiplier : I64 -> (I64 -> I64)
	multiplier = |factor| |n| n * factor
}
