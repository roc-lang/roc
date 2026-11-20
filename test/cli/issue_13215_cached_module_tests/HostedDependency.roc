# The hosted module's imports must not become unscheduled dependencies.
import pf.Host

answer = Host.answer({})

expect answer == 42
