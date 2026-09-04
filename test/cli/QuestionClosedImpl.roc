# polarity_phase_two.md W6b: `?` widens the closed Try error row selected by
# a where-method implementation through the same checked adapter mechanism.
load : a -> Try(Str, [NotFound, Other]) where [a.fetch : a -> Try(Str, [NotFound])]
load = |x| {
	s = x.fetch()?
	Ok(s)
}

closed_try : Try(Str, [NotFound])
closed_try = Ok("hit")

Src := [S].{
	fetch : Src -> Try(Str, [NotFound])
	fetch = |_| closed_try
}

expect match load(Src.S) { Ok(s) => s == "hit", Err(_) => False }
