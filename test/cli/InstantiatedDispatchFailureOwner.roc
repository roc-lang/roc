## A method constraint `f` inherits from `g` fails only at the use that
## instantiates `f` with a receiver whose `is_ok` returns `Bool` rather than a
## record. That use is the rejected expression; `g` stays valid, so the other
## expect still runs and passes.
g = |r| r.is_ok()
f = |a| g(a).x

expect g([1].first())
expect f([1].first())
