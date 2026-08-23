Result(a) :: { value : a }.{
    wrap : a -> Result(a)
    wrap = |v| Result.({ value: v })
}
