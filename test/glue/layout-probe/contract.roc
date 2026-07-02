app [main!] { pf: platform "./main.roc" }

import pf.Probe exposing [LayoutProbe]

main! = || {
	wide = Wide(
		{
			label: "layout-wide",
			a: Box.box(1),
			b: Box.box(2),
			c: Box.box(3),
			d: Box.box(4),
			e: Box.box(5),
			f: Box.box(6),
			g: Box.box(7),
			h: Box.box(8),
		},
	)
	wide_back = Probe.roundtrip!(wide)
	expect
		match wide_back {
			Wide(payload) => payload.label == "layout-wide"
			_ => False
		}

	aligned_back = Probe.roundtrip!(Aligned({ marker: 99, token: Box.box(123), flag: 7, tiny: 3 }))
	expect
		match aligned_back {
			Aligned(payload) => payload.marker == 99 and payload.flag == 7 and payload.tiny == 3
			_ => False
		}

	empty_back = Probe.roundtrip!(Empty)
	expect
		match empty_back {
			Empty => True
			_ => False
		}

	{}
}
