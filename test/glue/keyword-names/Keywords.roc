Keywords := [].{
	fields! : U8 => { abstract : U8, async : U8, await : U8, become : U8, box : U8, const : U8, crate : U8, do : U8, dyn : U8, enum : U8, extern : U8, false : U8, final : U8, fn : U8, gen : U8, impl : U8, let : U8, loop : U8, macro : U8, mod : U8, move : U8, mut : U8, override : U8, priv : U8, pub : U8, ref : U8, self : U8, static : U8, struct : U8, super : U8, trait : U8, true : U8, try : U8, type : U8, typeof : U8, unsafe : U8, unsized : U8, use : U8, virtual : U8, yield : U8 }
	tags! : U8 => [Crate(U64), Gen(Str), Self(U8), Struct(U64, U32), Super, Try(U16)]
}
