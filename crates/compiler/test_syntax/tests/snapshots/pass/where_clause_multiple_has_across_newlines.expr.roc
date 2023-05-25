f : a -> (b -> c)
    | a implements Hash,
      b implements Eq,
      c implements Ord

f
