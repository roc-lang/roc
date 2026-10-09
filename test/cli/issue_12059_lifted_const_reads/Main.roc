import First
import Second
import Hooks
import Other

Main := [].{}

# One constant read at two nominals, and at its own type.
expect First.size(Hooks.hooks) == 7
expect Second.size(Hooks.hooks) == 7
expect First.name(Hooks.hooks) == "seven"
expect Hooks.hooks.size == 7

# Two constants of one shape, each read at a different nominal.
expect Second.size(Other.thing) == 9
expect First.name(Other.thing) == "nine"

# Lifts beneath a record field and a list element.
expect First.size(Hooks.nested.inner) == 3
expect Second.name(Hooks.nested.inner) == "three"
expect Hooks.nested.items.map(First.size) == [4, 5]
expect Hooks.nested.items.map(Second.name) == ["four", "five"]

# A function-bearing constant, lifted and unlifted.
expect First.call(Hooks.step, 1) == 2
expect Second.call(Hooks.step, 2) == 3
expect (Hooks.step.run)(5) == 6

# Lifts beneath a function arrow.
expect First.go(Hooks.handler, 5) == 6
expect Second.go(Hooks.handler, 8) == 9
expect (Hooks.handler.make)(3).size == 3
