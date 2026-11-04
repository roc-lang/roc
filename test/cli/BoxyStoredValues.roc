app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout

# A defaulted field's function default, stored in a compile-time constant.
Cfg := { f : U8 -> U8 ?? |n| n + 5 }

cfg : Cfg
cfg = Cfg.{}

# The same with a default-local capture.
CapturingCfg := { f : U8 -> U8 ?? { k = 5
                                    |n| n + k } }

capturing_cfg : CapturingCfg
capturing_cfg = CapturingCfg.{}

# A stored recursive local function whose numeral types only its methods fix.
counter : U64 -> U64
counter = {
	step = 2
	go = |n| if n == 0 { 0 } else { go(n - 1) + step }
	go
}

# A stored closure whose evidence holds a derived map.
NamedMaybe(a) := [NamedJust(a), NamedNothing].{
	map : _
}

make_mapper = |_| |m| m.map(|x| x + 1)

apply_map : NamedMaybe(U64) -> NamedMaybe(U64)
apply_map = make_mapper({})

# Stored closures capturing records with optional and defaulted fields.
Opts : { n : U32, m ?: U32 }

make_opts : Opts -> (U32 -> U32)
make_opts = |o| |x| x + o.n + o.?m.ok_or(0)

with_opts : U32 -> U32
with_opts = make_opts({ n: 1 })

Settings := { n : U32, m : U32 ?? 2 }

make_settings : Settings -> (U32 -> U32)
make_settings = |o| |x| x + o.n + o.m

with_settings : U32 -> U32
with_settings = make_settings(Settings.{ n: 1 })

main! = || {
	for n in [1.U8, 3] {
		f = cfg.f
		g = capturing_cfg.f
		Stdout.line!("${Str.inspect(f(n))} ${Str.inspect(g(n))}")
	}
	for n in [1.U64, 3] {
		Stdout.line!("${Str.inspect(counter(n))} ${Str.inspect(apply_map(NamedMaybe.NamedJust(n)))}")
	}
	for n in [1.U32, 3] {
		Stdout.line!("${Str.inspect(with_opts(n))} ${Str.inspect(with_settings(n))}")
	}
}
