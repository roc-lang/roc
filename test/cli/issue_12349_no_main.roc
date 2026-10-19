# Repro for https://github.com/roc-lang/roc/issues/12349: a headerless file with
# no `main!`. Diagnostics about the whole file must not point at a position past
# the file's last line (4:1 here) with a caret that highlights nothing.
