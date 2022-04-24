interface Order
    exposes [ Order, Ordering, before, after, same, compare, reverse ]
    imports []

Order := I8

# -1 = Before
#  0 = Same
#  1 = After
#
# Representing Order as a signed integer instead of a tag
# means we can reverse an Order by negating it.

Ordering is
    compare : a, a -> Order | a supports Ordering

## The first value should be positioned **before** the second.
before : Order
before = $Order -1

## The first value should be positioned **after** the second.
after : Order
after = $Order 1

## The first value should have the **same** position as the second.
## (Meaning either order is acceptable.)
same : Order
same = $Order 0

# Design note: the reason it's called "same" and not (for example) "equal"
# is floats. The default ordering for floats is that `NaNs` go before everything
# else, and are considered the same as each other for ordering purposes.
#
# However, it's still the case that (by definition) NaNs are not *equal* to
# each other, even if they do sort to the *same* positions for ordering purposes.

## Reverses the given [Order].
##
## So [Order.before] becomes [Order.after] and vice versa.
## [Order.same] is unaffected.
reverse : Order -> Order
#reverse = \$Order order -> $Order (Num.neg order)
