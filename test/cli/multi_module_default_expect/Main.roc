import Cfg

main! = |_args| Ok({})

# Omitting `n` inlines Cfg's default block (with its failing inline expect)
# into this module's compile-time root; the failure must be attributed to the
# DECLARING module's expect, not rendered as if it were in this module.
cfg : Cfg.Cfg
cfg = Cfg.Cfg.{}
