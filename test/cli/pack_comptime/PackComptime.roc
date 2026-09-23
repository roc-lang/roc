# Compile-time roots that reach closed module functions, so a rebuild served
# from the object cache splices the functions' entries into the compile-time
# evaluator's image. The runtime reads the evaluated constants and calls the
# same functions with a value from stdin.
app [main!] { pf: platform "../../fx/platform/main.roc" }

import Shapes
import pf.Stdin
import pf.Stdout

hexagon_sides = Shapes.sides_of("hexagon")

unit_perimeters = Shapes.perimeters(1)

roster = Shapes.roster({})

square_from_index = Dict.get(Shapes.side_index({}), "square") ?? 0

mean_sides = Shapes.mean_sides({})

main! = || {
	side = Str.count_utf8_bytes(Stdin.line!())
	runtime_total = List.sum(Shapes.perimeters(side)) + Shapes.sides_of("pentagon")
	comptime_total = hexagon_sides + List.sum(unit_perimeters) + square_from_index
	Stdout.line!("${roster} ${U64.to_str(comptime_total)} ${U64.to_str(runtime_total)} ${F64.to_str(mean_sides)}")
}
