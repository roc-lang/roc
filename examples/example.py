example = "(1+(2*3)+((8)/4))+1"

solve = (str) ->
  max_depth = 0
  depth = 0
  for char in string.gmatch(str, ".")
    if char == '('
      depth += 1
      if max_depth < depth
        max_depth = depth
    else if char == ")"
      depth -= 1
  max_depth

print solve(example)
