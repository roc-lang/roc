# Passes values of the app's own nominal type through Lib's higher-order
# function. An edit elsewhere in this module leaves the type's identity, and
# so the identity of Lib's specializations at it, unchanged; an edit to the
# type's structure changes them.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Lib

Board := { score : U64, moves : U64 }.{
	step : Board -> Board
	step = |board| Board.{ score: board.score * 2 + 1, moves: board.moves + 1 }
}

main! = |args| {
	start = Board.{ score: List.len(args), moves: 0 }
	done = Lib.apply_steps_on(Board.step, start, 5)
	Stdout.line!("score ${done.score.to_str()} after ${done.moves.to_str()} moves")
	Ok({})
}
