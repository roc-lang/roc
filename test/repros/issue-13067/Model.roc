Model := { items : List({ name : Str }) }.{
	Other : Str

	empty : Model
	empty = Model.{ items: [] }
}
