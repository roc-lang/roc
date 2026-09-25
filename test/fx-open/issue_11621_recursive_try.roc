app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = |_args| {
    Stdout.line!("collect: ${Str.inspect(run({}))}")
    Stdout.line!("collect fails: ${Str.inspect(collect(7, []))}")
    Stdout.line!("ping: ${Str.inspect(ping(0, []))}")
    Stdout.line!("ping fails: ${Str.inspect(ping(8, []))}")
    Stdout.line!("apply step: ${Str.inspect(apply(step, 1))}")
    Stdout.line!("apply step fails: ${Str.inspect(apply(step, 7))}")
    Stdout.line!("apply step fails in callback: ${Str.inspect(apply(step, 6))}")
    Stdout.line!("countdown: ${Str.inspect(countdown(3))}")
    Stdout.line!("countdown fails: ${Str.inspect(countdown(9))}")
    Ok({})
}

run = |{}| collect(0, [])

step : U64 -> Try([More, Done], [StepFailed])
step = |n| if n == 7 { Err(StepFailed) } else if n < 3 { Ok(More) } else { Ok(Done) }

stop : U64 -> Try([More, Done], [StopFailed])
stop = |n| if n == 9 { Err(StopFailed) } else if n < 3 { Ok(More) } else { Ok(Done) }

collect = |n, out|
    match step(n)? {
        Done => Ok(out)
        More => collect(n + 1, out.append(n))
    }

ping = |n, out|
    match step(n)? {
        Done => pong(n + 1, out)
        More => pong(n + 1, out.append(n))
    }

pong = |n, out|
    match stop(n)? {
        Done => Ok(out)
        More => ping(n + 1, out.append(n))
    }

apply = |f, n| {
    _ = step(n)?
    f(n + 1)
}

countdown = |n| if n == 0 { Ok(Done) } else { apply(|m| countdown(m - 2), n) }
