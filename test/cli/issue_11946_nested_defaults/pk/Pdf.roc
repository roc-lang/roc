import Theme

Pdf :: [].{
	Mode := [Off, On]
	Options := {
		mode : Mode ?? Off,
		theme : Theme ?? {},
	}
}
