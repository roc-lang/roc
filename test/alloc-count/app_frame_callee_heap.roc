app [run!] { pf: platform "./platform/main.roc" }

import pf.Host
import FrameCallee

# The closure handed to FrameCallee.apply_both is also stored in a list, so
# it is a heap allocation. FrameCallee's object is the same as when an
# importer passes it a callable built in its own frame (app_frame_closures.roc).
run! : Str => Str
run! = |input| {
	offset = Str.count_utf8_bytes(input)
	add = |n| n + offset
	adders = [add]
	before = Host.alloc_count!()
	both = FrameCallee.apply_both(add, 1)
	allocs = Host.alloc_count!() - before
	"heap callee: ${both.to_str()} ${List.len(adders).to_str()} ${allocs.to_str()}"
}
