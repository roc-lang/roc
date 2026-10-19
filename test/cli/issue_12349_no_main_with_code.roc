# Repro for https://github.com/roc-lang/roc/issues/12349: a headerless file
# with code but no `main!`. The report is about the file as a whole, so it
# must not print the body of the file as its source excerpt.

issue_12349_body_marker = 1

another_body_line = issue_12349_body_marker + 1
