import View

Page :: [].{
	greet : U64 -> List(Str)
	greet = |n| {
		view : View
		view = { n, m: 0 }
		view.shown()
	}
}
