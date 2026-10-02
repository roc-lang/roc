import Font
import Layout

Theme := {
	body : BodyStyle ?? {},
	bullet_indent : Layout.Unit ?? 18,
	face : Font.FaceId ?? Font.FaceId.from_index(0),
}.{
	BodyStyle := { size : U64 ?? 11 }
}
