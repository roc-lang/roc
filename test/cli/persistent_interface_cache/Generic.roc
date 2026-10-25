import Defaults

Generic := [].{
    fresh = |factory| factory.make()
    composite = |_| fresh(Defaults.{})
    f0 = |x| {
        wrapped = fresh(Defaults.{})
        x + wrapped.value + wrapped.base.count + 1
    }
    f1 = |x| f0(x) + 1
    f2 = |x| f1(x) + 1
    f3 = |x| f2(x) + 1
    f4 = |x| f3(x) + 1
    f5 = |x| f4(x) + 1
    f6 = |x| f5(x) + 1
    f7 = |x| f6(x) + 1
}
