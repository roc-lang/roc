app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    input_list : List(Str)
    input_list = ["2"]
    
    run_sample : List(Try(I64, _))
    run_sample = input_list.map(I64.from_str)
    _b = run_sample.for_each!(my_fun!)

    Stdout.line!(match_tag_union(Ok({})))
}

my_fun! : Try(I64,_) => _
my_fun! = |r_elt| {
        dbg r_elt
        match r_elt {
            Ok(o) => Stdout.line!("Successful parse: ${Str.inspect(o)}")
            Err(e) => Stdout.line!("Failed to parse L2, raised error ${Str.inspect(e)}")
            _ => Stdout.line!("Parse Str -> I64 yielded ${Str.inspect(r_elt)}")
        }
}

match_tag_union : Try({}, [StdoutErr(Str), Other]) -> Str
match_tag_union = |try| {
	match try {
		Ok(_) =>
			"Success"

		Err(_) =>
			"Error"
	}
}