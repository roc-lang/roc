import Assets
import Color

Element := [].{
	FontMeasure : { text : Str, size : F32, spacing : F32 }

	FontDraw : {
		pos : { x : F32, y : F32 },
		text : Str,
		size : F32,
		spacing : F32,
		color : Color,
	}

	FontResource :: {
		key : U64,
		measure : Box(FontMeasure => { width : F32, height : F32 }),
		draw : Box(FontDraw => {}),
	}

	Font : [DefaultFont, CustomFont(FontResource)]

	CanvasPoint : { x : F32, y : F32 }

	CanvasLine : { start : CanvasPoint, end : CanvasPoint, thickness : F32, color : Color }

	CanvasCircle : { center : CanvasPoint, radius : F32, color : Color }

	CanvasPolygon : { points : List(CanvasPoint), color : Color }

	CanvasTextureQuad : {
		texture : Assets.Texture,
		top_left : CanvasPoint,
		bottom_left : CanvasPoint,
		bottom_right : CanvasPoint,
		top_right : CanvasPoint,
		tint : Color,
	}
}
