Issue9658ArcDiverge := {}

scan : Str -> U64
scan = |input| {
    var $a = input.concat("a")
    var $b = input.concat("b")
    var $c = input.concat("c")
    var $d = input.concat("d")
    var $e = input.concat("e")
    var $f = input.concat("f")
    var $keep = True
    var $n = 0
    while ($keep) {
        $n = $n + 1
        if $n == 1 { $a = $b }
        else if $n == 2 { $b = $c }
        else if $n == 3 { $c = $d }
        else if $n == 4 { $d = $e }
        else if $n == 5 { $e = $f }
        else if $n == 6 { $a = input.concat("a") }
        else if $n == 7 { $b = input.concat("b") }
        else if $n == 8 { $c = input.concat("c") }
        else if $n == 9 { $d = input.concat("d") }
        else if $n == 10 { $e = input.concat("e") }
        else if $n == 11 { $f = input.concat("f") }
        else { $keep = False }
    }
    $a.count_utf8_bytes() + $b.count_utf8_bytes() + $c.count_utf8_bytes() + $d.count_utf8_bytes() + $e.count_utf8_bytes() + $f.count_utf8_bytes()
}

expect scan("hi") > 0
