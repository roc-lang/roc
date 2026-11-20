Io(s) :: { write! : (s -> s) => {} }.{
	new : ((s -> s) => {}) -> Io(s)
	new = |w| { write!: w }

	write_at! : Io(s), (s -> s) => {}
	write_at! = |io, f| (io.write!)(f)
}
