module [decode, encode]

decode = |line| Ok(line)

encode = |_| "broken\nreply"
