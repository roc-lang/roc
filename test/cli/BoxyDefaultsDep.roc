## Dependency module for BoxyOptionalRecordFields.roc: declares a nominal
## whose defaulted field carries a block expression with a pattern binding,
## so a consuming module materializes a foreign default that binds locally.
BoxyDefaultsDep := { n : U64 ?? {
	base = 2
	base * 5
} }.{}
