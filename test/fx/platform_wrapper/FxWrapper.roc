import pf.Host
import pf.Stdout

FxWrapper :: [].{
    WrappedHost : Host.Host

    new : Str -> WrappedHost
    new = Host.new

    print_greeting! : WrappedHost => {}
    print_greeting! = |host| {
        greeting = Host.get_greeting!(host)
        Stdout.line!(greeting)
    }
}
