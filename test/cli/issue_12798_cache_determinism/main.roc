app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import Maybe
import pf.Stderr
import pf.Stdout

main! = |_args| {
        args = {
                info!: |text| { Stdout.line!(text)
                    Ok({}) },
                error!: |text| { Stderr.line!(text)
                    Ok({}) },
        }
        match run!(args) {
                Ok(success) => Ok(success)
                Err(cause) => {
                        _ = Stdout.line!(Str.inspect(Failed(cause)))
                        Err(Failed)
                }
        }
}

run! : _ => Try({}, _)
run! = |args| {
        info! = args.info!
        error! = args.error!

        info!("Starting")?

        orders = take!({})?

        _ =
                orders.map_try!(
                        |order|
                                match execute!(order) {
                                        Ok(_) => info!("Done.")
                                        Err(e) => {
                                                _ = error!("Failed with: ${Str.inspect(e)}")
                                                mark_as_failed!(order)?
                                                Err(e)
                                        }
                                },
                )

        Ok({})
}

execute! = |order| {
        _ = fetch!({}) ? |_e| FetchErr(order)
        Ok({})
}

mark_as_failed! = |order| update_error!(order.id)

fetch! : {} => Try({}, _)
fetch! = |_| Err(NoData)

take! : {} => Try(List({ id : I32, name : Maybe(Str) }), _)
take! = |_| Ok([{ id: 1, name: Maybe.just("x") }])

update_error! : I32 => Try({}, _)
update_error! = |id| if id == 1 Ok({}) else Err(OrderIdMissing(id))
