# META
~~~ini
description=Test == and != operators with payload-carrying nominal types that have is_eq methods
type=snippet
~~~
# SOURCE
~~~roc
# Define a UserId type that wraps an I64 with custom equality
UserId := [Id(I64)].{
    is_eq : UserId, UserId -> Bool
    is_eq = |a, b| match a {
        Id(id_a) => match b {
            Id(id_b) => id_a == id_b
        }
    }
}

user1 : UserId
user1 = UserId.Id(100)

user2 : UserId
user2 = UserId.Id(100)

user3 : UserId
user3 = UserId.Id(200)

# Test equality - same IDs should be equal
expect user1 == user2

# Test inequality - different IDs should not be equal
expect user1 != user3
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
