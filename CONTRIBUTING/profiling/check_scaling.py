"""Generate one size of a `roc check --no-cache --timings` scaling workload."""

import argparse


def generate(case, n):
    arg = "List.len(args).to_i64_wrap()"
    if case == "generic-defs":  # #13308: deferred dispatch in a tree of generic defs
        for i in range(n):
            left = 2 * i + 1
            if left + 1 < n:
                body = f"f_{left}(x) + f_{left + 1}(x) + {i}"
            elif left < n:
                body = f"f_{left}(x) * 2 + {i}"
            else:
                body = f"x * {i % 7 + 1} + {i}"
            print(f"f_{i} = |x| {body}\n")
        result = f"f_0({arg})"
    elif case == "block":  # #13309: one lookup per sequential declaration
        print("long : I64 -> I64\nlong = |x0| {")
        for i in range(1, n + 1):
            print(f"\tx{i} = (x{i - 1} * {i % 5 + 1} + {i}) % 1000003")
        print(f"\tx{n}\n}}\n")
        result = f"long({arg})"
    elif case == "closures":  # #13309: distinct captures in the same block
        print("run : I64 -> I64\nrun = |x| {")
        for i in range(n):
            print(f"\tk{i} = x + {i}\n\tg{i} = |y| y * k{i}")
        print("\t" + " + ".join(f"g{i}({i})" for i in range(n)) + "\n}\n")
        result = f"run({arg})"
    elif case == "interpolation":  # #13310: requirements on the same receiver
        print("greet : Str -> Str\ngreet = |name| {\n\tparts = [")
        for i in range(n):
            print(f'\t\t"line {i} for ${{name}} and some literal text",')
        print('\t]\n\tStr.join_with(parts, "\\n")\n}\n')
        result = "greet(List.len(args).to_str()).count_utf8_bytes()"
    elif case == "constants":  # #13315: a chain, not N independent roots
        print("c_0 : I64\nc_0 = 1\n")
        for i in range(1, n):
            print(f"c_{i} : I64\nc_{i} = (c_{i - 1} * 3 + {i}) % 1000003\n")
        result = f"c_{n - 1} + {arg}"
    print('main! = |args| {\n\techo!("${Str.inspect(' + result + ')}\\n")\n\tOk({})\n}')


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("case", choices=["generic-defs", "block", "closures", "interpolation", "constants"])
    parser.add_argument("size", type=int)
    args = parser.parse_args()
    if args.size < 1:
        parser.error("size must be positive")
    generate(args.case, args.size)
