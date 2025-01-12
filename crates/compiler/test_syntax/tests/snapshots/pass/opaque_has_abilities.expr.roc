A := U8 implements [Eq, Hash]

A := a where a implements Other implements [Eq, Hash]

A := a where a implements Other
     implements [Eq, Hash]

A := U8 implements [Eq {eq}, Hash {hash}]

A := U8 implements [Eq {eq, eq1}]

A := U8 implements [Eq {eq, eq1}, Hash]

A := U8 implements [Hash, Eq {eq, eq1}]

A := U8 implements []

A := a where a implements Other
     implements [Eq {eq}, Hash {hash}]

A := U8 implements [Eq ()]

0
