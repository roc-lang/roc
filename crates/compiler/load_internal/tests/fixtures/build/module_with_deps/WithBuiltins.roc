module [float_test, division_fn, division_test, int_test, constant_num, from_dep2, div_dep1_by_dep2]

import Dep1
import Dep2

float_test = Num.max_f64

division_fn = Num.div

x = 5.0

division_test = Num.max_f64 / x

int_test = Num.max_i64

constant_num = 5

from_dep2 = Dep2.two

div_dep1_by_dep2 = Dep1.three / from_dep2
