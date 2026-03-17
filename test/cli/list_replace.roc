main! = |_| {
  x = List.replace(['A', 'B', 'C'], 0, 'D')
  y = match x {
    Ok(r) => r.prev,
    Err(_) => {return Err(1)}
  }
  echo!(y.to_str())
  Ok({})
}
