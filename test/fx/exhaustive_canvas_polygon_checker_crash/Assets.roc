import Color

Assets := [].{
	TextureRect : {
		source : { x : F32, y : F32, width : F32, height : F32 },
		dest : { x : F32, y : F32, width : F32, height : F32 },
		origin : { x : F32, y : F32 },
		rotation : F32,
		tint : Color,
	}

	TextureQuad : {
		top_left : { x : F32, y : F32 },
		bottom_left : { x : F32, y : F32 },
		bottom_right : { x : F32, y : F32 },
		top_right : { x : F32, y : F32 },
		tint : Color,
	}

	TextureCommand : [DrawRect(TextureRect), DrawQuad(TextureQuad)]

	Texture :: {
		width : F32,
		height : F32,
		draw : Box(TextureCommand => {}),
	}
}
