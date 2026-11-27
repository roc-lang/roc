"""Parser for the fragment, so `check` can run the model on a .roc file.

It reads exactly the Roc that `lang.pp_module` prints (plus comments and
newlines in the usual places) and raises `ParseError` on anything outside the
fragment. `selftest` checks that `parse(pp(m)) == m` on generated modules.

A file is a module header `Name := [Name].{}` followed by definitions. Lines
of the form `# === label` split it into sections; each section is checked as
its own module under the same header. A `# expect: ...` line in a section is
read by `selftest` (see tests/).
"""
import re
from lang import (Var, Lit, BoolLit, Tag, Rec, Access, Lam, Call, If, Match, Block, Q, ListLit,
                  PVar, PWild, PTag, AVar, AHole, ACon, AFun, ATags, ARec, Def)


class ParseError(Exception):
    pass


TOKEN = re.compile(r"""
    (?P<nl>\n)
  | (?P<ws>[ \t\r]+)
  | (?P<comment>\#[^\n]*)
  | (?P<lit>\d+\.U64)
  | (?P<bool>Bool\.(?:True|False))
  | (?P<op>->|=>|\.\.|[|,()\[\]{}:=?.])
  | (?P<upper>[A-Z][A-Za-z0-9_]*)
  | (?P<lower>[a-z_][A-Za-z0-9_]*!?)
""", re.X)


def tokenize(src, first_line=1):
    """Tokens are (kind, text). A `(` with whitespace before it is kind
    `sparen`: `f(x)` is a call, `f (x)` is two expressions."""
    out, lines, pos, line, spaced = [], [], 0, first_line, True
    while pos < len(src):
        m = TOKEN.match(src, pos)
        if not m:
            raise ParseError("line %d: unexpected character %r" % (line, src[pos]))
        pos = m.end()
        k = m.lastgroup
        if k in ("ws", "comment"):
            spaced = True
            continue
        if m.group(0) == "(" and spaced:
            k = "sparen"
        out.append((k, m.group(0)))
        lines.append(line)
        line += k == "nl"
        spaced = k == "nl"
    out.append(("eof", ""))
    lines.append(line)
    return out, lines


class Parser:
    def __init__(self, src, first_line=1):
        self.toks, self.lines = tokenize(src, first_line)
        self.i = 0

    # ------------------------------------------------ token helpers
    def peek(self, k=0):
        return self.toks[min(self.i + k, len(self.toks) - 1)]

    def at(self, text):
        return self.peek()[1] == text and self.peek()[0] != "eof"

    def next(self):
        t = self.toks[self.i]
        self.i += 1
        return t

    def expect(self, text):
        t = self.next()
        if t[1] != text:
            raise ParseError("expected %r, found %r" % (text, t[1]))
        return t

    def skip_nl(self):
        while self.peek()[0] == "nl":
            self.i += 1

    def name(self):
        t = self.next()
        if t[0] != "lower":
            raise ParseError("expected a name, found %r" % t[1])
        return t[1]

    def comma_list(self, close, item):
        """Items separated by commas (newlines allowed) up to `close`."""
        out = []
        self.skip_nl()
        while not self.at(close):
            out.append(item())
            self.skip_nl()
            if not self.at(close):
                self.expect(",")
                self.skip_nl()
        self.expect(close)
        return out

    # ------------------------------------------------ module
    def defs(self):
        out = []
        self.skip_nl()
        while self.peek()[0] != "eof":
            name = self.name()
            ann = None
            if self.at(":"):
                self.next()
                ann = self.ann()
                self.skip_nl()
                if self.name() != name:
                    raise ParseError("annotation of %s is not followed by its definition" % name)
            self.expect("=")
            out.append(Def(name, ann, self.expr()))
            if self.peek()[0] not in ("nl", "eof"):
                raise ParseError("unexpected %r after definition of %s" % (self.peek()[1], name))
            self.skip_nl()
        return out

    # ------------------------------------------------ expressions
    def expr(self):
        if self.at("|"):
            self.next()
            params = []
            while not self.at("|"):
                params.append(self.pattern())
                if not self.at("|"):
                    self.expect(",")
            self.next()
            return Lam(params, self.expr())
        if self.at("if"):
            self.next()
            c = self.postfix()
            t = self.postfix()
            self.expect("else")
            return If(c, t, self.expr())
        if self.at("match"):
            self.next()
            scr = self.postfix()
            self.expect("{")
            return Match(scr, self.comma_list("}", self.branch))
        return self.postfix()

    def branch(self):
        p = self.pattern()
        self.expect("=>")
        return (p, self.expr())

    def postfix(self):
        e = self.atom()
        while True:
            if self.peek() == ("op", "("):
                self.next()
                e = Call(e, self.comma_list(")", self.expr))
            elif self.at("."):
                self.next()
                e = Access(e, self.name())
            elif self.at("?"):
                self.next()
                e = Q(e)
            else:
                return e

    def atom(self):
        k, s = self.peek()
        if k == "lit":
            self.next()
            v, ty = s.split(".")
            return Lit(ty, v)
        if k == "bool":
            self.next()
            return BoolLit(s == "Bool.True")
        if k == "upper":
            self.next()
            if self.peek() == ("op", "("):
                self.next()
                return Tag(s, self.comma_list(")", self.expr))
            return Tag(s, [])
        if k == "lower" and s not in ("if", "else", "match"):
            self.next()
            return Var(s)
        if s == "(":
            self.next()
            self.skip_nl()
            e = self.expr()
            self.skip_nl()
            self.expect(")")
            return e
        if s == "[":
            self.next()
            return ListLit(self.comma_list("]", self.expr))
        if s == "{":
            self.next()
            if self.peek()[0] == "nl":
                return self.block()
            return Rec(dict(self.comma_list("}", self.field)))
        if s in ("|", "if", "match"):
            return self.expr()
        raise ParseError("unexpected %r" % s)

    def field(self):
        k = self.name()
        self.expect(":")
        return (k, self.expr())

    def block(self):
        """`{` NEWLINE (stmt NEWLINE)* expr NEWLINE `}`: a statement is
        `name = expr`, optionally preceded by `name : ann`."""
        stmts = []
        while True:
            self.skip_nl()
            if self.peek()[0] == "lower" and self.peek(1)[1] in (":", "="):
                name = self.name()
                ann = None
                if self.at(":"):
                    self.next()
                    ann = self.ann()
                    self.skip_nl()
                    if self.name() != name:
                        raise ParseError("annotation of %s is not followed by its definition" % name)
                self.expect("=")
                stmts.append((name, ann, self.expr()))
                continue
            body = self.expr()
            self.skip_nl()
            self.expect("}")
            return Block(stmts, body)

    # ------------------------------------------------ patterns
    def pattern(self):
        k, s = self.next()
        if s == "_":
            return PWild()
        if k == "lower":
            return PVar(s)
        if k == "upper":
            if self.at("("):
                self.next()
                return PTag(s, self.comma_list(")", self.pattern))
            return PTag(s, [])
        raise ParseError("unexpected %r in a pattern" % s)

    # ------------------------------------------------ annotations
    def ann(self):
        """`a, b -> c`, `a => c`, or a single type."""
        args = [self.ann_atom()]
        while self.at(","):
            self.next()
            args.append(self.ann_atom())
        if self.at("->") or self.at("=>"):
            eff = self.next()[1] == "=>"
            return AFun(args, self.ann_atom(), eff)
        if len(args) != 1:
            raise ParseError("a comma-separated annotation needs an arrow")
        return args[0]

    def ann_item(self):
        """An annotation inside a comma-separated list: one type, or a
        one-argument function."""
        a = self.ann_atom()
        if self.at("->") or self.at("=>"):
            eff = self.next()[1] == "=>"
            return AFun([a], self.ann_atom(), eff)
        return a

    def ann_atom(self):
        k, s = self.next()
        if s == "_":
            return AHole()
        if k == "lower":
            return AVar(s)
        if k == "upper":
            if s in ("U64", "Bool"):
                return ACon(s, [])
            if s in ("Try", "List"):
                self.expect("(")
                args = self.comma_list(")", self.ann_item)
                if len(args) != (2 if s == "Try" else 1):
                    raise ParseError("%s takes %d arguments" % (s, 2 if s == "Try" else 1))
                return ACon(s, args)
            raise ParseError("type %s is outside the fragment" % s)
        if s == "(":
            a = self.ann()
            self.expect(")")
            return a
        if s == "[":
            tags, ext = {}, None
            for item in self.comma_list("]", self.tag_or_ext):
                if isinstance(item, str):
                    ext = item
                else:
                    tags[item[0]] = item[1]
            return ATags(tags, ext)
        if s == "{":
            fields, ext = {}, None
            for item in self.comma_list("}", self.field_or_ext):
                if isinstance(item, str):
                    ext = item
                else:
                    fields[item[0]] = item[1]
            return ARec(fields, ext)
        raise ParseError("unexpected %r in an annotation" % s)

    def ext(self):
        self.expect("..")
        if self.peek()[0] == "lower":
            return ".." + self.next()[1]
        return ".."

    def tag_or_ext(self):
        if self.at(".."):
            return self.ext()
        k, s = self.next()
        if k != "upper":
            raise ParseError("expected a tag, found %r" % s)
        if self.at("("):
            self.next()
            return (s, self.comma_list(")", self.ann_item))
        return (s, [])

    def field_or_ext(self):
        if self.at(".."):
            return self.ext()
        k = self.name()
        self.expect(":")
        return (k, self.ann_item())


HEADER = re.compile(r"^([A-Z][A-Za-z0-9_]*) := \[\1\]\.\{\}[ \t]*$")
SECTION = re.compile(r"^# === *(.*?)\s*$")


def split_file(text):
    """Returns (header line, [(label, section source, line number of its
    first line)]). A file without `# ===` lines is one section labeled by an
    empty string."""
    lines = text.splitlines()
    i = 0
    while i < len(lines) and (not lines[i].strip() or lines[i].lstrip().startswith("#")):
        i += 1
    if i == len(lines) or not HEADER.match(lines[i]):
        raise ParseError("the first line of code must be a type module header `Name := [Name].{}`")
    header = lines[i]
    sections, label, cur, start = [], "", [], i + 2
    for n, line in enumerate(lines[i + 1:], i + 2):
        m = SECTION.match(line)
        if m:
            if label or any(l.strip() and not l.lstrip().startswith("#") for l in cur):
                sections.append((label, "\n".join(cur) + "\n", start))
            label, cur, start = m.group(1), [], n + 1
        else:
            cur.append(line)
    sections.append((label, "\n".join(cur) + "\n", start))
    return header, sections


def parse_defs(src, first_line=1):
    p = Parser(src, first_line)
    try:
        return p.defs()
    except ParseError as ex:
        raise ParseError("line %d: %s" % (p.lines[min(p.i, len(p.lines) - 1)], ex))
