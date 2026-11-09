# Completed zero-sized records have no physical fields to load.
Policy : { first : [One], second : [Two], third : [Three] }
policy : Policy
policy = { first: One, second: Two, third: Three }
wrapped : { inner : Policy }
wrapped = { inner: policy }

expect policy.first == One
expect policy.second == Two
expect policy.third == Three
expect wrapped.inner.second == Two
expect {
	{ first, second, third } = policy
	first == One and second == Two and third == Three
}
