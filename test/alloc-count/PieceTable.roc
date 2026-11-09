PieceTable := { original : List(U8), added : List(U8), pieces : List(Piece) }.{
	Piece : { source : [Original, Added], start : U64, len : U64 }

	from_bytes : List(U8) -> PieceTable
	from_bytes = |bytes| { original: bytes, added: [], pieces: [{ source: Original, start: 0, len: bytes.len() }] }

	insert : PieceTable, U64, List(U8) -> PieceTable
	insert = |table, at, bytes| {
		{ original, added, pieces } = table
		new_piece = { source: Added, start: added.len(), len: bytes.len() }
		var $offset = 0
		var $index = 0
		var $result = List.with_capacity(pieces.len() + 2)
		var $done = Bool.False
		while $index < pieces.len() {
			piece = pieces.get($index) ?? new_piece
			if !$done and at >= $offset and at <= $offset + piece.len {
				split = at - $offset
				if split > 0 {
					$result = $result.append({ ..piece, len: split })
				}
				$result = $result.append(new_piece)
				if piece.len - split > 0 {
					$result = $result.append({ ..piece, start: piece.start + split, len: piece.len - split })
				}
				$done = Bool.True
			} else {
				$result = $result.append(piece)
			}
			$offset = $offset + piece.len
			$index = $index + 1
		}
		{ original, added: added.concat(bytes), pieces: $result }
	}

	len : PieceTable -> U64
	len = |table| table.pieces.len()
}
