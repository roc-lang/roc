# Closures, records and tag unions built in one module and consumed in another; a self tail call
# (not a group) called across modules.
import Base
import Mid

main! = |args| {
	zu = args.len() * 0
	k = zu.to_i64_wrap() + 4
	r = Mid.run([1, 2, 3], k)
	t = match Base.shape(r.a) {
		P(n) => Str.concat("P", n.to_str())
		Q(s) => Str.concat("Q", s)
		W => "W"
	}
	lines = [r.a.to_str(), r.b, t, Mid.sum_to(10 + zu, 0).to_str()]
	echo!(Str.concat(Str.join_with(lines, "\n"), "\n"))
	Ok({})
}
