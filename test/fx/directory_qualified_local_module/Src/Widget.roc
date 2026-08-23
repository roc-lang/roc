import ./Helper
import pf.Stdout

Widget :: [].{
    default : Str
    default = Helper.suffix()

    message : Str -> Str
    message = |value| value

    say! : Str => {}
    say! = |value| Stdout.line!(value)
}
