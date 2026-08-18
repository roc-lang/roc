# Same bare module name as the app's own Cfg module. Omitting `n` inlines
# this default (200 + 100 overflows U8) into the consuming module's
# compile-time root; the crash must be attributed to THIS module even though
# the finalized module has the same bare name.
Cfg := { n : U8 ?? big_a + big_b }

big_a : U8
big_a = 200

big_b : U8
big_b = 100
