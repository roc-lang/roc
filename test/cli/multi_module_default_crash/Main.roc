import Cfg

main! = |_args| Ok({})

# Omitting `n` inlines Cfg's default (200 + 100) into this module's
# compile-time root; the U8 overflow crash must be reported against the
# DECLARING module's source, not this module's.
cfg : Cfg.Cfg
cfg = Cfg.Cfg.{}
