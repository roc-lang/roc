A := U8 has [Eq, Hash]

A := a | a has Other
     has [Eq, Hash]

A := a | a has Other
     has [Eq, Hash]

A := U8 has [Eq { eq }, Hash { hash }]

A := U8 has [Eq { eq, eq1 }]

A := U8 has [Eq { eq, eq1 }, Hash]

A := U8 has [Hash, Eq { eq, eq1 }]

A := U8 has []

A := a | a has Other
     has [Eq { eq }, Hash { hash }]

A := U8 has [Eq {}]

0