View := { n : U64, m : U64 }.{
	shown : View -> List(Str)
	shown = |_view| [greeting]
}

greeting : Str
greeting = "hi"
