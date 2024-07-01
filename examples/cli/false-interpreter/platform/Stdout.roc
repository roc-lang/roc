module [line, raw]

import pf.PlatformTask

line : Str -> Task {} *
line = PlatformTask.putLine

raw : Str -> Task {} *
raw = PlatformTask.putRaw
