app [make_glue] { pf: platform glue }

import pf.Types
import pf.File

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |_types| Ok([])
