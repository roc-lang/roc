# https://github.com/roc-lang/roc/issues/13215
# Pure module tests must work even when their imports also wrap hosted functions.
import pf.Output

answer = Output.answer({})

expect answer == 42
