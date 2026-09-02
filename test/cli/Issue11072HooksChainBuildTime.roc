app [main!] { pf: platform "../fx-open/platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/11072
#
# Every link of this chain unwraps a fallible hook with `?` and logs an
# interpolated string. Each interpolation mints a private iterator whose closure
# type shares one lambda set with every other link's, so the lowered layout
# graph carries one interchangeable copy of the same recursive closure layout
# per link. The layout store must intern those copies as one recursive layout;
# interning them one by one costs work that grows with the fourth power of the
# chain length, which the timeout on this case guards against.

import pf.Stdout

Worker := [].{
	start! : _ => Try({}, _)
	start! = |hooks| Worker.h120!(hooks)

	h1! : _ => Try({}, _)
	h1! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h1 ${items.len().to_str()}")
		Ok({})
	}

	h2! : _ => Try({}, _)
	h2! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h2 ${items.len().to_str()}")
		Worker.h1!(hooks)
	}

	h3! : _ => Try({}, _)
	h3! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h3 ${items.len().to_str()}")
		Worker.h2!(hooks)
	}

	h4! : _ => Try({}, _)
	h4! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h4 ${items.len().to_str()}")
		Worker.h3!(hooks)
	}

	h5! : _ => Try({}, _)
	h5! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h5 ${items.len().to_str()}")
		Worker.h4!(hooks)
	}

	h6! : _ => Try({}, _)
	h6! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h6 ${items.len().to_str()}")
		Worker.h5!(hooks)
	}

	h7! : _ => Try({}, _)
	h7! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h7 ${items.len().to_str()}")
		Worker.h6!(hooks)
	}

	h8! : _ => Try({}, _)
	h8! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h8 ${items.len().to_str()}")
		Worker.h7!(hooks)
	}

	h9! : _ => Try({}, _)
	h9! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h9 ${items.len().to_str()}")
		Worker.h8!(hooks)
	}

	h10! : _ => Try({}, _)
	h10! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h10 ${items.len().to_str()}")
		Worker.h9!(hooks)
	}

	h11! : _ => Try({}, _)
	h11! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h11 ${items.len().to_str()}")
		Worker.h10!(hooks)
	}

	h12! : _ => Try({}, _)
	h12! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h12 ${items.len().to_str()}")
		Worker.h11!(hooks)
	}

	h13! : _ => Try({}, _)
	h13! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h13 ${items.len().to_str()}")
		Worker.h12!(hooks)
	}

	h14! : _ => Try({}, _)
	h14! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h14 ${items.len().to_str()}")
		Worker.h13!(hooks)
	}

	h15! : _ => Try({}, _)
	h15! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h15 ${items.len().to_str()}")
		Worker.h14!(hooks)
	}

	h16! : _ => Try({}, _)
	h16! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h16 ${items.len().to_str()}")
		Worker.h15!(hooks)
	}

	h17! : _ => Try({}, _)
	h17! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h17 ${items.len().to_str()}")
		Worker.h16!(hooks)
	}

	h18! : _ => Try({}, _)
	h18! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h18 ${items.len().to_str()}")
		Worker.h17!(hooks)
	}

	h19! : _ => Try({}, _)
	h19! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h19 ${items.len().to_str()}")
		Worker.h18!(hooks)
	}

	h20! : _ => Try({}, _)
	h20! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h20 ${items.len().to_str()}")
		Worker.h19!(hooks)
	}

	h21! : _ => Try({}, _)
	h21! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h21 ${items.len().to_str()}")
		Worker.h20!(hooks)
	}

	h22! : _ => Try({}, _)
	h22! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h22 ${items.len().to_str()}")
		Worker.h21!(hooks)
	}

	h23! : _ => Try({}, _)
	h23! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h23 ${items.len().to_str()}")
		Worker.h22!(hooks)
	}

	h24! : _ => Try({}, _)
	h24! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h24 ${items.len().to_str()}")
		Worker.h23!(hooks)
	}

	h25! : _ => Try({}, _)
	h25! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h25 ${items.len().to_str()}")
		Worker.h24!(hooks)
	}

	h26! : _ => Try({}, _)
	h26! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h26 ${items.len().to_str()}")
		Worker.h25!(hooks)
	}

	h27! : _ => Try({}, _)
	h27! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h27 ${items.len().to_str()}")
		Worker.h26!(hooks)
	}

	h28! : _ => Try({}, _)
	h28! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h28 ${items.len().to_str()}")
		Worker.h27!(hooks)
	}

	h29! : _ => Try({}, _)
	h29! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h29 ${items.len().to_str()}")
		Worker.h28!(hooks)
	}

	h30! : _ => Try({}, _)
	h30! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h30 ${items.len().to_str()}")
		Worker.h29!(hooks)
	}

	h31! : _ => Try({}, _)
	h31! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h31 ${items.len().to_str()}")
		Worker.h30!(hooks)
	}

	h32! : _ => Try({}, _)
	h32! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h32 ${items.len().to_str()}")
		Worker.h31!(hooks)
	}

	h33! : _ => Try({}, _)
	h33! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h33 ${items.len().to_str()}")
		Worker.h32!(hooks)
	}

	h34! : _ => Try({}, _)
	h34! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h34 ${items.len().to_str()}")
		Worker.h33!(hooks)
	}

	h35! : _ => Try({}, _)
	h35! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h35 ${items.len().to_str()}")
		Worker.h34!(hooks)
	}

	h36! : _ => Try({}, _)
	h36! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h36 ${items.len().to_str()}")
		Worker.h35!(hooks)
	}

	h37! : _ => Try({}, _)
	h37! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h37 ${items.len().to_str()}")
		Worker.h36!(hooks)
	}

	h38! : _ => Try({}, _)
	h38! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h38 ${items.len().to_str()}")
		Worker.h37!(hooks)
	}

	h39! : _ => Try({}, _)
	h39! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h39 ${items.len().to_str()}")
		Worker.h38!(hooks)
	}

	h40! : _ => Try({}, _)
	h40! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h40 ${items.len().to_str()}")
		Worker.h39!(hooks)
	}

	h41! : _ => Try({}, _)
	h41! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h41 ${items.len().to_str()}")
		Worker.h40!(hooks)
	}

	h42! : _ => Try({}, _)
	h42! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h42 ${items.len().to_str()}")
		Worker.h41!(hooks)
	}

	h43! : _ => Try({}, _)
	h43! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h43 ${items.len().to_str()}")
		Worker.h42!(hooks)
	}

	h44! : _ => Try({}, _)
	h44! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h44 ${items.len().to_str()}")
		Worker.h43!(hooks)
	}

	h45! : _ => Try({}, _)
	h45! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h45 ${items.len().to_str()}")
		Worker.h44!(hooks)
	}

	h46! : _ => Try({}, _)
	h46! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h46 ${items.len().to_str()}")
		Worker.h45!(hooks)
	}

	h47! : _ => Try({}, _)
	h47! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h47 ${items.len().to_str()}")
		Worker.h46!(hooks)
	}

	h48! : _ => Try({}, _)
	h48! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h48 ${items.len().to_str()}")
		Worker.h47!(hooks)
	}

	h49! : _ => Try({}, _)
	h49! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h49 ${items.len().to_str()}")
		Worker.h48!(hooks)
	}

	h50! : _ => Try({}, _)
	h50! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h50 ${items.len().to_str()}")
		Worker.h49!(hooks)
	}

	h51! : _ => Try({}, _)
	h51! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h51 ${items.len().to_str()}")
		Worker.h50!(hooks)
	}

	h52! : _ => Try({}, _)
	h52! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h52 ${items.len().to_str()}")
		Worker.h51!(hooks)
	}

	h53! : _ => Try({}, _)
	h53! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h53 ${items.len().to_str()}")
		Worker.h52!(hooks)
	}

	h54! : _ => Try({}, _)
	h54! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h54 ${items.len().to_str()}")
		Worker.h53!(hooks)
	}

	h55! : _ => Try({}, _)
	h55! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h55 ${items.len().to_str()}")
		Worker.h54!(hooks)
	}

	h56! : _ => Try({}, _)
	h56! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h56 ${items.len().to_str()}")
		Worker.h55!(hooks)
	}

	h57! : _ => Try({}, _)
	h57! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h57 ${items.len().to_str()}")
		Worker.h56!(hooks)
	}

	h58! : _ => Try({}, _)
	h58! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h58 ${items.len().to_str()}")
		Worker.h57!(hooks)
	}

	h59! : _ => Try({}, _)
	h59! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h59 ${items.len().to_str()}")
		Worker.h58!(hooks)
	}

	h60! : _ => Try({}, _)
	h60! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h60 ${items.len().to_str()}")
		Worker.h59!(hooks)
	}

	h61! : _ => Try({}, _)
	h61! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h61 ${items.len().to_str()}")
		Worker.h60!(hooks)
	}

	h62! : _ => Try({}, _)
	h62! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h62 ${items.len().to_str()}")
		Worker.h61!(hooks)
	}

	h63! : _ => Try({}, _)
	h63! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h63 ${items.len().to_str()}")
		Worker.h62!(hooks)
	}

	h64! : _ => Try({}, _)
	h64! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h64 ${items.len().to_str()}")
		Worker.h63!(hooks)
	}

	h65! : _ => Try({}, _)
	h65! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h65 ${items.len().to_str()}")
		Worker.h64!(hooks)
	}

	h66! : _ => Try({}, _)
	h66! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h66 ${items.len().to_str()}")
		Worker.h65!(hooks)
	}

	h67! : _ => Try({}, _)
	h67! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h67 ${items.len().to_str()}")
		Worker.h66!(hooks)
	}

	h68! : _ => Try({}, _)
	h68! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h68 ${items.len().to_str()}")
		Worker.h67!(hooks)
	}

	h69! : _ => Try({}, _)
	h69! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h69 ${items.len().to_str()}")
		Worker.h68!(hooks)
	}

	h70! : _ => Try({}, _)
	h70! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h70 ${items.len().to_str()}")
		Worker.h69!(hooks)
	}

	h71! : _ => Try({}, _)
	h71! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h71 ${items.len().to_str()}")
		Worker.h70!(hooks)
	}

	h72! : _ => Try({}, _)
	h72! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h72 ${items.len().to_str()}")
		Worker.h71!(hooks)
	}

	h73! : _ => Try({}, _)
	h73! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h73 ${items.len().to_str()}")
		Worker.h72!(hooks)
	}

	h74! : _ => Try({}, _)
	h74! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h74 ${items.len().to_str()}")
		Worker.h73!(hooks)
	}

	h75! : _ => Try({}, _)
	h75! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h75 ${items.len().to_str()}")
		Worker.h74!(hooks)
	}

	h76! : _ => Try({}, _)
	h76! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h76 ${items.len().to_str()}")
		Worker.h75!(hooks)
	}

	h77! : _ => Try({}, _)
	h77! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h77 ${items.len().to_str()}")
		Worker.h76!(hooks)
	}

	h78! : _ => Try({}, _)
	h78! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h78 ${items.len().to_str()}")
		Worker.h77!(hooks)
	}

	h79! : _ => Try({}, _)
	h79! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h79 ${items.len().to_str()}")
		Worker.h78!(hooks)
	}

	h80! : _ => Try({}, _)
	h80! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h80 ${items.len().to_str()}")
		Worker.h79!(hooks)
	}

	h81! : _ => Try({}, _)
	h81! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h81 ${items.len().to_str()}")
		Worker.h80!(hooks)
	}

	h82! : _ => Try({}, _)
	h82! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h82 ${items.len().to_str()}")
		Worker.h81!(hooks)
	}

	h83! : _ => Try({}, _)
	h83! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h83 ${items.len().to_str()}")
		Worker.h82!(hooks)
	}

	h84! : _ => Try({}, _)
	h84! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h84 ${items.len().to_str()}")
		Worker.h83!(hooks)
	}

	h85! : _ => Try({}, _)
	h85! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h85 ${items.len().to_str()}")
		Worker.h84!(hooks)
	}

	h86! : _ => Try({}, _)
	h86! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h86 ${items.len().to_str()}")
		Worker.h85!(hooks)
	}

	h87! : _ => Try({}, _)
	h87! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h87 ${items.len().to_str()}")
		Worker.h86!(hooks)
	}

	h88! : _ => Try({}, _)
	h88! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h88 ${items.len().to_str()}")
		Worker.h87!(hooks)
	}

	h89! : _ => Try({}, _)
	h89! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h89 ${items.len().to_str()}")
		Worker.h88!(hooks)
	}

	h90! : _ => Try({}, _)
	h90! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h90 ${items.len().to_str()}")
		Worker.h89!(hooks)
	}

	h91! : _ => Try({}, _)
	h91! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h91 ${items.len().to_str()}")
		Worker.h90!(hooks)
	}

	h92! : _ => Try({}, _)
	h92! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h92 ${items.len().to_str()}")
		Worker.h91!(hooks)
	}

	h93! : _ => Try({}, _)
	h93! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h93 ${items.len().to_str()}")
		Worker.h92!(hooks)
	}

	h94! : _ => Try({}, _)
	h94! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h94 ${items.len().to_str()}")
		Worker.h93!(hooks)
	}

	h95! : _ => Try({}, _)
	h95! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h95 ${items.len().to_str()}")
		Worker.h94!(hooks)
	}

	h96! : _ => Try({}, _)
	h96! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h96 ${items.len().to_str()}")
		Worker.h95!(hooks)
	}

	h97! : _ => Try({}, _)
	h97! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h97 ${items.len().to_str()}")
		Worker.h96!(hooks)
	}

	h98! : _ => Try({}, _)
	h98! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h98 ${items.len().to_str()}")
		Worker.h97!(hooks)
	}

	h99! : _ => Try({}, _)
	h99! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h99 ${items.len().to_str()}")
		Worker.h98!(hooks)
	}

	h100! : _ => Try({}, _)
	h100! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h100 ${items.len().to_str()}")
		Worker.h99!(hooks)
	}

	h101! : _ => Try({}, _)
	h101! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h101 ${items.len().to_str()}")
		Worker.h100!(hooks)
	}

	h102! : _ => Try({}, _)
	h102! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h102 ${items.len().to_str()}")
		Worker.h101!(hooks)
	}

	h103! : _ => Try({}, _)
	h103! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h103 ${items.len().to_str()}")
		Worker.h102!(hooks)
	}

	h104! : _ => Try({}, _)
	h104! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h104 ${items.len().to_str()}")
		Worker.h103!(hooks)
	}

	h105! : _ => Try({}, _)
	h105! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h105 ${items.len().to_str()}")
		Worker.h104!(hooks)
	}

	h106! : _ => Try({}, _)
	h106! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h106 ${items.len().to_str()}")
		Worker.h105!(hooks)
	}

	h107! : _ => Try({}, _)
	h107! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h107 ${items.len().to_str()}")
		Worker.h106!(hooks)
	}

	h108! : _ => Try({}, _)
	h108! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h108 ${items.len().to_str()}")
		Worker.h107!(hooks)
	}

	h109! : _ => Try({}, _)
	h109! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h109 ${items.len().to_str()}")
		Worker.h108!(hooks)
	}

	h110! : _ => Try({}, _)
	h110! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h110 ${items.len().to_str()}")
		Worker.h109!(hooks)
	}

	h111! : _ => Try({}, _)
	h111! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h111 ${items.len().to_str()}")
		Worker.h110!(hooks)
	}

	h112! : _ => Try({}, _)
	h112! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h112 ${items.len().to_str()}")
		Worker.h111!(hooks)
	}

	h113! : _ => Try({}, _)
	h113! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h113 ${items.len().to_str()}")
		Worker.h112!(hooks)
	}

	h114! : _ => Try({}, _)
	h114! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h114 ${items.len().to_str()}")
		Worker.h113!(hooks)
	}

	h115! : _ => Try({}, _)
	h115! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h115 ${items.len().to_str()}")
		Worker.h114!(hooks)
	}

	h116! : _ => Try({}, _)
	h116! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h116 ${items.len().to_str()}")
		Worker.h115!(hooks)
	}

	h117! : _ => Try({}, _)
	h117! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h117 ${items.len().to_str()}")
		Worker.h116!(hooks)
	}

	h118! : _ => Try({}, _)
	h118! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h118 ${items.len().to_str()}")
		Worker.h117!(hooks)
	}

	h119! : _ => Try({}, _)
	h119! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h119 ${items.len().to_str()}")
		Worker.h118!(hooks)
	}

	h120! : _ => Try({}, _)
	h120! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h120 ${items.len().to_str()}")
		Worker.h119!(hooks)
	}
}

main! = |_args| {
	line! = Stdout.line!
	_ = Worker.start!({ log: { info!: |m| line!(m) }, fetch!: |{}| Ok([{ id: 1, name: "a" }]) })?
	Ok({})
}
