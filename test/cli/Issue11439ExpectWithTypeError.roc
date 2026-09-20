# The third expect does not type check, so the run summary must account for it
# instead of reporting that every test passed.

identity = |x| x

expect identity(1) == 1
expect identity("a") == "a"
expect identity("a") == 1
