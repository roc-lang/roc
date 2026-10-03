#!/usr/bin/env python3
"""Generate a controlled stored-null lowering experiment in a scratch directory."""
import argparse
import re
from pathlib import Path

p = argparse.ArgumentParser()
p.add_argument("output", type=Path)
p.add_argument("--width", type=int, default=20)
p.add_argument("--depth", type=int, default=2)
p.add_argument("--uses", type=int, default=10)
p.add_argument("--mode", choices=["shared", "inline", "factory"], default="shared")
p.add_argument("--nominal", choices=["opaque", "public"], default="public")
p.add_argument("--inside", action="store_true")
p.add_argument("--node-source", type=Path)
p.add_argument("--reduce-node", action="store_true")
p.add_argument("--tags", type=int, default=10000)
p.add_argument("--defaults", action="store_true")
p.add_argument("--fan", action="store_true")
p.add_argument("--record", action="store_true")
a = p.parse_args()
a.output.mkdir(parents=True, exist_ok=True)
aliases = ["Level0 : { number : I64, child : Node }"]
for n in range(1, a.depth + 1):
    aliases.append(f"Level{n} : {{ " + ", ".join(f"f{i} : Level{n-1}" for i in range(a.width)) + " }")
node = f"Node {'::' if a.nominal == 'opaque' else ':='} [Null, Leaf(U64), Nodes(List(Node)), Other(Level{a.depth})].{{\n"
node += "\tnull : Node\n\tnull = Null\n\tmake_null : {} -> Node\n\tmake_null = |_| Null\n"
node += "\tdefault : Level0\n\tdefault = { number: 0, child: Node.null }\n"
node += "\tis_null : Node -> Bool\n\tis_null = |n| match n { Null => Bool.True; _ => Bool.False }\n}\n"
node = node.replace("Bool.True; _", "Bool.True\n\t\t_")
if a.fan:
    branches = ", ".join(f"Branch{i}(Node.R{i})" for i in range(a.width))
    node = f"Node {'::' if a.nominal == 'opaque' else ':='} [Null, Leaf(U64), " + branches + "].{\n"
    node += "\tnull : Node\n\tnull = Null\n\tmake_null : {} -> Node\n\tmake_null = |_| Null\n"
    node += "\tis_null : Node -> Bool\n\tis_null = |n| match n {\n\t\tNull => Bool.True\n\t\t_ => Bool.False\n\t}\n"
    for i in range(a.width):
        node += f"\tR{i} : {{ " + ", ".join(f"f{i}_{j} : Node" for j in range(a.depth)) + " }\n"
        if a.defaults:
            node += f"\tr{i}_default : Node.R{i}\n\tr{i}_default = {{ " + ", ".join(f"f{i}_{j}: Node.null" for j in range(a.depth)) + " }\n"
    node += "}\n"
    aliases = []
(a.output / "Node.roc").write_text(node + "\n" + "\n".join(aliases) + "\n")
value = {"shared": "Node.null", "inline": "Null", "factory": "Node.make_null({})"}[a.mode]
source = '''app [main!] {
    pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.27.0/HZanbveSUDoJF8LypR663eH7PpaKEKG36eErEQzmV1Qs.tar.zst",
}
import pf.Stdout
import Node

work : U64 -> Bool
work = |x| {
'''
for i in range(a.uses):
    if a.record:
        if a.node_source:
            expr = f"Node.ResTarget({{ ..Node.res_target_default, val: {value} }})"
        else:
            assert a.fan and a.defaults
            expr = f"Node.Branch0({{ ..Node.r0_default, f0_0: {value} }})"
    else:
        expr = value
    source += f"\tn{i} : Node\n\tn{i} = if x == {i} {expr} else Node.Leaf(x)\n"
source += "\tx >= 0 and " + " and ".join(f"Node.is_null(n{i})" for i in range(a.uses)) + "\n}\n"
source += 'answer = work(1)\nmain! = |_args| Stdout.line!(Str.inspect(answer))\n'
if a.inside:
    body = source[source.index("work :"):source.index("answer =")]
    node = node[:-2] + "\n" + "\n".join("\t" + line for line in body.splitlines()) + "\n}\n"
    (a.output / "Node.roc").write_text(node + "\n" + "\n".join(aliases) + "\n")
    source = source[:source.index("work :")] + 'answer = Node.work(1)\nmain! = |_args| Stdout.line!(Str.inspect(answer))\n'
(a.output / "main.roc").write_text(source)
if a.node_source:
    assert not a.inside
    original = a.node_source.read_text()
    if a.reduce_node:
        tags = original[original.index("Node := [") + len("Node := ["):original.index("].{")].strip().splitlines()
        selected = tags[:a.tags]
        for tag in ["\tNull,", "\tInteger(Node.Integer),"]:
            if tag.strip() not in [t.strip() for t in selected]:
                selected.append(tag)
        declarations = re.findall(r"^\t\w+ : \{[^\n]*", original, re.M)
        defaults = re.findall(r"^\t\w+_default : [^\n]*\n\t\w+_default = [^\n]*", original, re.M)
        original = "Node := [\n" + "\n".join(selected) + "\n].{\n"
        original += "\tnull : Node\n\tnull = Null\n\tmake_null : {} -> Node\n\tmake_null = |_| Null\n"
        original += "\tis_null : Node -> Bool\n\tis_null = |n| match n {\n\t\tNull => Bool.True\n\t\t_ => Bool.False\n\t}\n"
        original += "\tText : Try(Str, [Null])\n" + "\n".join(declarations) + "\n"
        if a.defaults:
            original += "\n".join(defaults) + "\n"
        original += "}\n"
    (a.output / "Node.roc").write_text(original)
    source = source.replace("Node.Leaf(x)", "Node.Integer({ ival: 1.I64 })")
    (a.output / "main.roc").write_text(source)
