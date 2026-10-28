module []

import Helper

expect {
    unused = 42
    dbg "test observation"
    expect Helper.positive(3)
    Helper.value == 5
}

expect True
