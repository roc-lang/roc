Font :: [].{
	FaceId :: U64.{
		from_index : U64 -> FaceId
		from_index = |i| FaceId.(i)
	}
}
