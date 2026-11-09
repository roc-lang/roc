# Two tuple components whose types settle to the same type only after their
# literals' open error rows meet the closed annotated rows compare and render
# through one specialization of their type's methods.
Pairs := [P].{}

pair : Bool -> (Try(Str, [Closed]), Try(Str, [Closed]))
pair = |closed| if closed { (Err(Closed), Ok("b")) } else { (Ok("a"), Ok("b")) }

expect pair(Bool.False) == (Ok("a"), Ok("b"))
expect pair(Bool.True) == (Err(Closed), Ok("b"))
expect pair(Bool.True) != (Ok("a"), Ok("b"))
expect pair(Bool.False) != (Ok("a"), Ok("c"))
expect Str.inspect(pair(Bool.True)) == Str.inspect((Err(Closed), Ok("b")))
