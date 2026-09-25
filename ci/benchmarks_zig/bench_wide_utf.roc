# Benchmark the public Roc path, including its SIMD lowering and ARC.
# Arguments: unit width (16/32), mode (strict/lossy), case, units, iterations.
main! = |args| {
	width = args.get(0).ok_or("16")
	mode = args.get(1).ok_or("strict")
	kind = args.get(2).ok_or("ascii")
	len = U64.from_str(args.get(3).ok_or("1048576")).ok_or(1048576)
	iterations = U64.from_str(args.get(4).ok_or("64")).ok_or(64)
	var $units = List.with_capacity(len)
	var $index = 0.U64
	while $index < len {
		unit = match kind {
			"ascii" => 65.U32
			"mostly_ascii" => if $index % 64 == 63 {
				0xE9
			} else {
				65
			}
			"bmp" => 0x4E2D
			"supplementary" => if width == "16" {
				if $index % 2 == 0 {
					0xD83D
				} else {
					0xDC26
				}
			} else {
				0x1F426
			}
			"invalid_tail" => if $index + 1 == len {
				0xD800
			} else {
				65
			}
			"invalid_head" => if $index == 0 {
				0xD800
			} else {
				65
			}
			_ => crash "Unknown wide UTF benchmark case"
		}
		$units = $units.append(unit)
		$index = $index + 1
	}
	var $checksum = 0.U64
	var $iteration = 0.U64
	if width == "16" {
		units = $units.map(|unit| unit.to_u16_wrap())
		while $iteration < iterations {
			text = if mode == "lossy" {
				Str.from_utf16_lossy(units)
			} else {
				Str.from_utf16(units).ok_or("")
			}
			$checksum = $checksum.plus_wrap(text.count_utf8_bytes())
			$iteration = $iteration + 1
		}
	} else {
		while $iteration < iterations {
			text = if mode == "lossy" {
				Str.from_utf32_lossy($units)
			} else {
				Str.from_utf32($units).ok_or("")
			}
			$checksum = $checksum.plus_wrap(text.count_utf8_bytes())
			$iteration = $iteration + 1
		}
	}
	echo!("${U64.to_str($checksum)}\n")
	Ok({})
}
