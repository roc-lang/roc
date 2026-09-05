module [greet, double, base]

# Names bound by top-level destructures are exposed like plainly named values:
# a destructured function and destructured values, with an unexposed sibling.
{ greet, punctuation } = { greet: |name| Str.concat(Str.concat("Hi, ", name), punctuation), punctuation: "!" }
(double, base) = (|n| n * 2, 21)
