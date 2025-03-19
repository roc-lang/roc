type layout_content =
  | Str
  | Int
  | Struct of layout list
  | Union of layout list
  | Box of layout
  | OpaquePtr
  | INTERNAL__Unfilled (* Internal only *)

and layout = layout_content ref
