# Independent modules prepare generic captured closures before this importer
# stores a function that reads both. Run with one and several compiler workers.
import Left
import Right

saved = |n| { count: Left.apply(n), text: Right.apply("!") }

expect saved(5) == { count: 22, text: "stored!" }
expect saved(8) == { count: 25, text: "stored!" }
