use std::cmp;
use std::fmt;
use std::io;
#[cfg(feature = "termcolor")]
use termcolor::{ColorSpec, WriteColor};

use crate::{Doc, DocPtr};

/// Trait representing the operations necessary to render a document
pub trait Render {
    type Error;

    fn write_str(&mut self, s: &str) -> Result<usize, Self::Error>;

    fn write_str_all(&mut self, mut s: &str) -> Result<(), Self::Error> {
        while !s.is_empty() {
            let count = self.write_str(s)?;
            s = &s[count..];
        }
        Ok(())
    }
}

/// Writes to something implementing `std::io::Write`
pub struct IoWrite<W> {
    upstream: W,
}

impl<W> IoWrite<W> {
    pub fn new(upstream: W) -> IoWrite<W> {
        IoWrite { upstream }
    }
}

impl<W> Render for IoWrite<W>
where
    W: io::Write,
{
    type Error = io::Error;

    fn write_str(&mut self, s: &str) -> io::Result<usize> {
        self.upstream.write(s.as_bytes())
    }

    fn write_str_all(&mut self, s: &str) -> io::Result<()> {
        self.upstream.write_all(s.as_bytes())
    }
}

/// Writes to something implementing `std::fmt::Write`
pub struct FmtWrite<W> {
    upstream: W,
}

impl<W> FmtWrite<W> {
    pub fn new(upstream: W) -> FmtWrite<W> {
        FmtWrite { upstream }
    }
}

impl<W> Render for FmtWrite<W>
where
    W: fmt::Write,
{
    type Error = fmt::Error;

    fn write_str(&mut self, s: &str) -> Result<usize, fmt::Error> {
        self.write_str_all(s).map(|_| s.len())
    }

    fn write_str_all(&mut self, s: &str) -> fmt::Result {
        self.upstream.write_str(s)
    }
}

/// Trait representing the operations necessary to write an annotated document.
pub trait RenderAnnotated<A>: Render {
    fn push_annotation(&mut self, annotation: &A) -> Result<(), Self::Error>;
    fn pop_annotation(&mut self) -> Result<(), Self::Error>;
}

impl<A, W> RenderAnnotated<A> for IoWrite<W>
where
    W: io::Write,
{
    fn push_annotation(&mut self, _: &A) -> Result<(), Self::Error> {
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl<A, W> RenderAnnotated<A> for FmtWrite<W>
where
    W: fmt::Write,
{
    fn push_annotation(&mut self, _: &A) -> Result<(), Self::Error> {
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

#[cfg(feature = "termcolor")]
pub struct TermColored<W> {
    color_stack: Vec<ColorSpec>,
    upstream: W,
}

#[cfg(feature = "termcolor")]
impl<W> TermColored<W> {
    pub fn new(upstream: W) -> TermColored<W> {
        TermColored {
            color_stack: Vec::new(),
            upstream,
        }
    }
}

#[cfg(feature = "termcolor")]
impl<W> Render for TermColored<W>
where
    W: io::Write,
{
    type Error = io::Error;

    fn write_str(&mut self, s: &str) -> io::Result<usize> {
        self.upstream.write(s.as_bytes())
    }

    fn write_str_all(&mut self, s: &str) -> io::Result<()> {
        self.upstream.write_all(s.as_bytes())
    }
}

#[cfg(feature = "termcolor")]
impl<W> RenderAnnotated<ColorSpec> for TermColored<W>
where
    W: WriteColor,
{
    fn push_annotation(&mut self, color: &ColorSpec) -> Result<(), Self::Error> {
        self.color_stack.push(color.clone());
        self.upstream.set_color(color)
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        self.color_stack.pop();
        match self.color_stack.last() {
            Some(previous) => self.upstream.set_color(previous),
            None => self.upstream.reset(),
        }
    }
}

macro_rules! make_spaces {
            () => { "" };
            ($s: tt $($t: tt)*) => { concat!("          ", make_spaces!($($t)*)) };
        }

pub(crate) const SPACES: &str = make_spaces!(,,,,,,,,,,);

#[inline]
pub fn best<'a, W, T, A>(doc: &Doc<'a, T, A>, width: usize, out: &mut W) -> Result<(), W::Error>
where
    T: DocPtr<'a, A> + 'a,
    W: ?Sized + RenderAnnotated<A>,
{
    #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
    enum Mode {
        Break,
        Flat,
    }

    type Cmd<'d, 'a, T, A> = (usize, Mode, &'d Doc<'a, T, A>);

    fn write_newline<W>(ind: usize, out: &mut W) -> Result<(), W::Error>
    where
        W: ?Sized + Render,
    {
        out.write_str_all("\n")?;
        write_spaces(ind, out)
    }

    fn write_spaces<W>(spaces: usize, out: &mut W) -> Result<(), W::Error>
    where
        W: ?Sized + Render,
    {
        let mut inserted = 0;
        while inserted < spaces {
            let insert = cmp::min(SPACES.len(), spaces - inserted);
            inserted += out.write_str(&SPACES[..insert])?;
        }

        Ok(())
    }

    fn fitting<'a, 'd, T, A>(
        temp_arena: &'d typed_arena::Arena<T>,
        next: &'d Doc<'a, T, A>,
        bcmds: &[Cmd<'d, 'a, T, A>],
        fcmds: &mut Vec<&'d Doc<'a, T, A>>,
        mut pos: usize,
        width: usize,
        ind: usize,
        newline_fits: fn(Mode) -> bool,
    ) -> bool
    where
        T: DocPtr<'a, A>,
    {
        let mut bidx = bcmds.len();
        fcmds.clear(); // clear from previous calls from best
        fcmds.push(next);

        let mut mode = Mode::Flat;
        loop {
            let mut doc = match fcmds.pop() {
                None => {
                    if bidx == 0 {
                        // All commands have been processed
                        return true;
                    } else {
                        bidx -= 1;
                        mode = Mode::Break;
                        bcmds[bidx].2
                    }
                }
                Some(cmd) => cmd,
            };

            loop {
                match *doc {
                    Doc::Nil => {}
                    Doc::Append(ref ldoc, ref rdoc) => {
                        fcmds.push(rdoc);
                        // Since appended documents often appear in sequence on the left side we
                        // gain a slight performance increase by batching these pushes (avoiding
                        // to push and directly pop `Append` documents)
                        doc = ldoc;
                        while let Doc::Append(ref l, ref r) = *doc {
                            fcmds.push(r);
                            doc = l;
                        }
                        continue;
                    }
                    // Newlines inside the group makes it not fit, but those outside lets it
                    // fit on the current line
                    Doc::Line => return newline_fits(mode),
                    Doc::BorrowedText(ref str) => {
                        pos += str.len();
                        if pos > width {
                            return false;
                        }
                    }
                    Doc::OwnedText(ref str) => {
                        pos += str.len();
                        if pos > width {
                            return false;
                        }
                    }
                    Doc::SmallText(ref str) => {
                        pos += str.len();
                        if pos > width {
                            return false;
                        }
                    }
                    Doc::FlatAlt(ref b, ref f) => {
                        doc = match mode {
                            Mode::Break => b,
                            Mode::Flat => f,
                        };
                        continue;
                    }

                    Doc::Column(ref f) => {
                        doc = temp_arena.alloc(f(pos));
                        continue;
                    }
                    Doc::Nesting(ref f) => {
                        doc = temp_arena.alloc(f(ind));
                        continue;
                    }
                    Doc::Nest(_, ref next)
                    | Doc::Group(ref next)
                    | Doc::Annotated(_, ref next)
                    | Doc::Union(_, ref next) => {
                        doc = next;
                        continue;
                    }
                }
                break;
            }
        }
    }

    let temp_arena = typed_arena::Arena::new();

    let mut pos = 0;
    let mut bcmds = vec![(0, Mode::Break, doc)];
    let mut fcmds = vec![];
    let mut annotation_levels = vec![];

    while let Some(mut cmd) = bcmds.pop() {
        loop {
            let (ind, mode, doc) = cmd;
            match *doc {
                Doc::Nil => {}
                Doc::Append(ref ldoc, ref rdoc) => {
                    bcmds.push((ind, mode, rdoc));
                    let mut doc = ldoc;
                    while let Doc::Append(ref l, ref r) = **doc {
                        bcmds.push((ind, mode, r));
                        doc = l;
                    }
                    cmd = (ind, mode, doc);
                    continue;
                }
                Doc::FlatAlt(ref b, ref f) => {
                    cmd = (
                        ind,
                        mode,
                        match mode {
                            Mode::Break => b,
                            Mode::Flat => f,
                        },
                    );
                    continue;
                }
                Doc::Group(ref doc) => match mode {
                    Mode::Flat => {
                        cmd = (ind, Mode::Flat, doc);
                        continue;
                    }
                    Mode::Break => {
                        cmd = if fitting(
                            &temp_arena,
                            doc,
                            &bcmds,
                            &mut fcmds,
                            pos,
                            width,
                            ind,
                            |mode| mode == Mode::Break,
                        ) {
                            (ind, Mode::Flat, &**doc)
                        } else {
                            (ind, Mode::Break, doc)
                        };
                        continue;
                    }
                },
                Doc::Nest(off, ref doc) => {
                    cmd = ((ind as isize).saturating_add(off) as usize, mode, doc);
                    continue;
                }
                Doc::Line => {
                    write_newline(ind, out)?;
                    pos = ind;
                }
                Doc::OwnedText(ref s) => {
                    out.write_str_all(s)?;
                    pos += s.len();
                }
                Doc::BorrowedText(ref s) => {
                    out.write_str_all(s)?;
                    pos += s.len();
                }
                Doc::SmallText(ref s) => {
                    out.write_str_all(s)?;
                    pos += s.len();
                }
                Doc::Annotated(ref ann, ref doc) => {
                    out.push_annotation(ann)?;
                    annotation_levels.push(bcmds.len());
                    cmd = (ind, mode, doc);
                    continue;
                }
                Doc::Union(ref l, ref r) => {
                    cmd = if fitting(&temp_arena, l, &bcmds, &mut fcmds, pos, width, ind, |_| {
                        true
                    }) {
                        (ind, mode, l)
                    } else {
                        (ind, mode, r)
                    };
                    continue;
                }
                Doc::Column(ref f) => {
                    cmd = (ind, mode, temp_arena.alloc(f(pos)));
                    continue;
                }
                Doc::Nesting(ref f) => {
                    cmd = (ind, mode, temp_arena.alloc(f(ind)));
                    continue;
                }
            }

            break;
        }
        while annotation_levels.last() == Some(&bcmds.len()) {
            annotation_levels.pop();
            out.pop_annotation()?;
        }
    }

    Ok(())
}
