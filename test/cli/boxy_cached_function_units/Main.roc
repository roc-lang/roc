# The imported code is unchanged when a caller changes these captures.
import Factory
import Other

add = Factory.offset(7)
number = Factory.make({ value: 91.I64, words: ["first", "second"] })
text = Other.make({ value: "fresh", flags: [True, False] })
nested = Factory.make(Other.make("nested"))

expect add(5) == 12
expect number({}) == { value: 91.I64, words: ["first", "second"] }
expect text({}) == { inner: { value: "fresh", flags: [True, False] } }
expect (nested({}))({}) == { inner: "nested" }
