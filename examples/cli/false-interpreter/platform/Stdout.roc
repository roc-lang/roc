module [line, raw]

import pf.PlatformTasks

line : Str -> Task {} *
line = PlatformTasks.putLine

raw : Str -> Task {} *
raw = PlatformTasks.putRaw
