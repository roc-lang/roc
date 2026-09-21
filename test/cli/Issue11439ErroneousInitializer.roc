# Recovery keeps these roots executable; both runtime failures still count.
bad : U64
bad = "bad"

expect bad == 1
expect bad == 2
expect Bool.True
