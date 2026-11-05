# Repro for https://github.com/roc-lang/roc/issues/12853: `page` reads the
# constant `Attr.island`, whose value is a closure returning a tag. A build
# with `--specialize=no` fills the cache, and a build with the default
# strategy on the same cache must succeed.
app [run!] { pf: platform "../../alloc-count/platform/main.roc" }

import Attr

page : Attr({})
page = Attr.island

run! = |_| "hi"
