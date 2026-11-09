import Factory

# The completed function crosses the module boundary with Local's scoped
# descriptor in both its table and its dynamic capture-record descriptor.
saved = Factory.make(7.U64)
closed_saved = Factory.closed({})

expect saved({}) == "Local(7)"
expect closed_saved({}) == "Local(7)"
