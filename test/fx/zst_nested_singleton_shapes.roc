app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

zstValue : { inner : [Outer({ nested : { one_field : [OneTag({})] } })] }
zstValue =
    { inner: Outer({ nested: { one_field: OneTag({}) } }) }

nonZstValueA : { inner : [Outer({ nested : { one_field : [OneTag({ n : U64 })] } })] }
nonZstValueA =
    { inner: Outer({ nested: { one_field: OneTag({ n: 1 }) } }) }

nonZstValueB : { inner : [Outer({ nested : { one_field : [OneTag({ n : U64 })] } })] }
nonZstValueB =
    { inner: Outer({ nested: { one_field: OneTag({ n: 2 }) } }) }

main! = || {
    zstList = List.append(List.append([zstValue], zstValue), zstValue)
    zstRepeat = List.repeat(zstValue, 3)

    match List.first(zstList) {
        Ok(first) => {
            if first == zstValue {
                Stdout.line!("zst first ok")
            } else {
                Stdout.line!("zst first bad")
            }
        }
        Err(ListWasEmpty) => Stdout.line!("zst first err")
    }

    match List.get(zstList, 1) {
        Ok(second) => {
            if second == zstValue {
                Stdout.line!("zst get ok")
            } else {
                Stdout.line!("zst get bad")
            }
        }
        Err(OutOfBounds) => Stdout.line!("zst get err")
    }

    if zstList == zstRepeat {
        Stdout.line!("zst repeat ok")
    } else {
        Stdout.line!("zst repeat bad")
    }

    match List.first(zstList) {
        Ok({ inner: Outer({ nested: { one_field: OneTag({}) } }) }) => {
            Stdout.line!("zst pattern ok")
        }
        _ => Stdout.line!("zst pattern bad")
    }

    nonZstList = [nonZstValueA, nonZstValueB, nonZstValueA]

    if nonZstValueA != nonZstValueB {
        Stdout.line!("non-zst distinct ok")
    } else {
        Stdout.line!("non-zst distinct bad")
    }

    match List.first(nonZstList) {
        Ok(first) => {
            if first == nonZstValueA {
                Stdout.line!("non-zst first ok")
            } else {
                Stdout.line!("non-zst first bad")
            }
        }
        Err(ListWasEmpty) => Stdout.line!("non-zst first err")
    }

    match List.get(nonZstList, 1) {
        Ok(second) => {
            if second == nonZstValueB {
                Stdout.line!("non-zst get ok")
            } else {
                Stdout.line!("non-zst get bad")
            }
        }
        Err(OutOfBounds) => Stdout.line!("non-zst get err")
    }

    match List.first(nonZstList) {
        Ok({ inner: Outer({ nested: { one_field: OneTag({ n: 1 }) } }) }) => {
            Stdout.line!("non-zst pattern ok")
        }
        _ => Stdout.line!("non-zst pattern bad")
    }
}
