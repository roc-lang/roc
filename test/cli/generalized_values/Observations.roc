# `made` is used at two concrete types (`List(U64)` twice, `List(Str)` once),
# so its `dbg` runs once per specialization: twice. `checked`'s inline expect
# fails at both of its specializations and is reported for each.
Observations := [].{}

import Values

expect List.append(Values.made, 1.U64) == [1]
expect List.append(Values.made, 2.U64) == [2]
expect List.append(Values.made, "w") == ["w"]
expect List.len(List.append(Values.checked, 1.U64)) + List.len(List.append(Values.checked, "x")) == 2
