interface Order
    exposes [ Order, Ordering, lt, gt, eq, compare, reverse ]
    imports []

Order := U8

# -1 = Less
#  0 = Equal
#  1 = Greater
#
# Representing Order as a signed integer instead of a tag
# means we can reverse an Order by negating it.

Ordering is
    compare : a, a -> Order | a supports Equating

## The first value is **less than** the second.
lt : Order
lt = $Order -1

## The first value is **greater than** the second.
gt : Order
gt = $Order 1

## The first value is **equal to** the second.
eq : Order
eq = $Order 0

## Reverse the given [Order].
##
## So [Order.lt] becomes [Order.gt] and vice versa.
## [Order.eq] is unaffected.
reverse : Order -> Order
#reverse = \$Order order -> $Order (Num.neg order)
