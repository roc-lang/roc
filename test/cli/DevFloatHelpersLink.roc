app [main!] { pf: platform "../fx/platform/main.roc" }

main! = || {
	var $f32 = -7.5.F32
	var $f64 = -7.5.F64
	var $u128 = 16.U128
	var $i128 = -256.I128

	if F32.div_trunc_by($f32, 2.0) != -3.0 {
		crash "wrong F32 div_trunc_by result"
	}
	if F64.div_trunc_by($f64, 2.0) != -3.0 {
		crash "wrong F64 div_trunc_by result"
	}
	if F32.floor_to_i32($f32) != -8 {
		crash "wrong F32 floor result"
	}
	if F64.floor_to_i32($f64) != -8 {
		crash "wrong F64 floor result"
	}
	if F32.ceiling_to_i32($f32) != -7 {
		crash "wrong F32 ceiling result"
	}
	if F64.ceiling_to_i32($f64) != -7 {
		crash "wrong F64 ceiling result"
	}
	if $u128.shift_left_by(3) != 128 {
		crash "wrong U128 shift-left result"
	}
	if $u128.shift_right_by(2) != 4 {
		crash "wrong U128 shift-right result"
	}
	if $i128.shift_right_by(4) != -16 {
		crash "wrong I128 shift-right result"
	}
}
