# `roc test` must run expects in a default app even when `main!` is absent.
app [main!] {}

expect 1 + 1 == 2
