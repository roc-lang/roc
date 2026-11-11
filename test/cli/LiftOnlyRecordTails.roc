LiftOnlyRecordTails :: [].{}

# A record pattern an unannotated function takes, or a record it both takes
# and returns, has a lift-only uncommitted tail: each use may pass a nominal
# type whose backing has exactly the record's fields. `Rect` has an unnamed
# field, so its layout differs from the record the functions were compiled
# at, and every value converts on its way in and out, including through
# function values and at depth.

Rect := { w : U64, _ : U32, h : U64 }

Unit := {}

W(a) := a.{
	encoder_for : _
}

area = |{ w, h }| w * h

total = |rs| List.fold(rs, 0, |acc, { w, h }| acc + w * h)

keep = |r| if r.w > 0 r else { w: 1, h: 1 }

apply = |f, r| f(r)

returned_local = |r| {
	g = |{}| Json.to_str(W.(r))
	g
}

nested_local = |r| {
	h = |{}| {
		g = |{}| Json.to_str(W.(r))
		g
	}
	h({})
}

rect : Rect
rect = { w: 3, h: 4 }

rects : List(Rect)
rects = [{ w: 1, h: 2 }, { w: 5, h: 6 }]

unit : Unit
unit = {}

expect area(rect) == 12

expect area({ w: 2, h: 2 }) == 4

expect total(rects) == 32

expect {
	k : Rect
	k = keep(rect)
	k.w == 3 and k.h == 4
}

expect {
	k : Rect
	k = keep({ w: 0, h: 9 })
	k.w == 1 and k.h == 1
}

expect List.map(rects, area) == [2, 30]

expect apply(area, rect) == 12

expect returned_local(7.U64)(unit) == "7"

expect returned_local("a")({}) == "\"a\""

expect nested_local(7.U64)(unit) == "7"
