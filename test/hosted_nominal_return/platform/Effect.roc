import Foo
import Bar

Effect := [].{
    things! : () => List([
        Foo(Foo.Idx),
        Bar(Bar.Idx),
    ])
}
