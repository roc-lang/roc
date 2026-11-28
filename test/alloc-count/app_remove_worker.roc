app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# A hit on the sole entry, or a miss on the now-empty table, never probes a
# second bucket. The counters are independent of hash seed and tail-call ABI.
run! : Str => Str
run! = |input| {
	k = input.count_utf8_bytes()
	d = Dict.single(k, input)
	s = Set.single(k)
	a = Host.alloc_count!()
	dh = d.remove(k)
	sh = s.remove(k)
	b = Host.alloc_count!()
	dm = dh.remove(k)
	sm = sh.remove(k)
	c = Host.alloc_count!()
	if d.get(k) != Ok(input) or !s.contains(k) {
		crash "changed original"
	}
	if !dh.is_empty() or !sh.is_empty() {
		crash "kept hit"
	}
	if !dm.is_empty() or !sm.is_empty() {
		crash "kept miss"
	}
	e = Host.alloc_count!()
	dh2 = d.remove(k)
	sh2 = s.remove(k)
	f = Host.alloc_count!()
	dm2 = dh2.remove(k)
	sm2 = sh2.remove(k)
	g = Host.alloc_count!()
	if !dm2.is_empty() or !sm2.is_empty() {
		crash "dying remove"
	}
	"kept miss ${(c - b).to_str()}, kept hit ${(b - a).to_str()}, dying miss ${(g - f).to_str()}, dying hit ${(f - e).to_str()}"
}
