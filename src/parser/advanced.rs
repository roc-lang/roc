// This is a port of elm/parser by Evan Czaplicki. It is licensed under
// the 3-Clause BSD License as follows:
//
// Copyright (c) 2017-present, Evan Czaplicki
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, this
//   list of conditions and the following disclaimer.
// 
// * Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the documentation
//   and/or other materials provided with the distribution.
// 
// * Neither the name of the {organization} nor the names of its
//   contributors may be used to endorse or promote products derived from
//   this software without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
// https://github.com/elm/parser/blob/1.1.0/src/Parser/Advanced.elm

use self::PStep::{Good, Bad};
use std::sync::Arc;

// TODO try smallbox crate and smallbox! macro for stack-allocated Box alternative.
pub struct Parser<Context, Problem, Value> (Arc<Fn(State<Context>) -> PStep<Context, Problem, Value>>);

impl<C, X, T: Clone> Parser<C, X, T> {
    // TODO someday this can probably be a const fn - tracking issue: https://github.com/rust-lang/rust/issues/57563
    pub fn succeed<Value: Clone>(val: &'static Value) -> Parser<C, X, Value> {
        Parser(Arc::new(move |state| Good(false, val.clone(), state)))
    }

    // TODO someday this can probably be a const fn - tracking issue: https://github.com/rust-lang/rust/issues/57563
    pub fn new() -> Parser<C, X, ()> {
        Parser(Arc::new(move |state| Good(false, (), state)))
    }

    fn keep<'a, B: 'a>(&'a self, other: &'a Parser<C, X, B>) -> Parser<C, X, (T, B)> {
        Parser(Box::new(move |s0| {
            let Parser(parseA) = self;
            let Parser(parseB) = other;

            match parseA(s0) {
                Bad(p, x) => Bad(p, x),
                Good(p1, a, s1) => {
                    match parseB(s1) {
                        Bad(p2, x) => Bad(p1 || p2, x),
                        Good(p2, b, s2) => Good(p1 || p2, (a, b), s2)
                    }
                }
            }
        }))
    }

    fn skip<'a, Ignored: 'a>(&'a self, other: &'a Parser<C, X, Ignored>) -> Parser<C, X, T> {
        Parser(Box::new(move |s0| {
            let Parser(parseA) = self;
            let Parser(parseB) = other;

            match parseA(s0) {
                Bad(p, x) => Bad(p, x),
                Good(p1, a, s1) => {
                    match parseB(s1) {
                        Bad(p2, x) => Bad(p1 || p2, x),
                        Good(p2, b, s2) => Good(p1 || p2, a, s2)
                    }
                }
            }
        }))
    }

    fn map<'a, After, Convert>(&'a self, convert: Convert) -> Parser<C, X, After>
    where Convert: Fn(T) -> After
    {
        Parser(Arc::new(|s0| {
            let Parser(parse) = self;

            match parse(s0) {
                Good(p, a, s1) => Good(p, convert(a), s1),
                Bad(p, x) => Bad(p, x)
            }
        }))
    }

    fn map2<B, Value, Convert>(self, other: Parser<C, X, B>, convert: Convert) -> Parser<C, X, Value>
    where Convert: Fn(T, B) -> Value, B: 'static, X: 'static
    {
        let Parser(parseA) = self;
        let Parser(parseB) = other;

        let callback = |s0| {
            match parseA(s0) {
                Bad(p, x) => Bad(p, x),
                Good(p1, a, s1) => {
                    match parseB(s1) {
                        Bad(p2, x) => Bad(p1 || p2, x),
                        Good(p2, b, s2) => Good(p1 || p2, convert(a, b), s2)
                    }
                }
            }
        };

        Parser(Arc::new(callback))
    }
}

fn map<C, X, T, Before, After, Convert>(me: Parser<C, X, T>, convert: Convert) -> Parser<C, X, After>
where Convert: Fn(Before) -> After
{
    Parser(Arc::new(|s0| {
        let Parser(parse) = self;

        match parse(s0) {
            Good(p, a, s1) => Good(p, convert(a), s1),
            Bad(p, x) => Bad(p, x)
        }
    }))
}

fn map2<C, X, T, B, Value, Convert>(me: Parser<C, X, T>, other: Parser<C, X, B>, convert: Convert) -> Parser<C, X, Value>
where Convert: Fn(T, B) -> Value
{
    let Parser(parseA) = me;
    let Parser(parseB) = other;

    let contents = Arc::new(|s0| {
        match parseA(s0) {
            Bad(p, x) => Bad(p, x),
            Good(p1, a, s1) => {
                match parseB(s1) {
                    Bad(p2, x) => Bad(p1 || p2, x),
                    Good(p2, b, s2) => Good(p1 || p2, convert(a, b), s2)
                }
            }
        }
    });

    Parser(contents)
}

enum PStep<Context, Problem, Value> {
    Good(bool, Value, State<Context>),
    Bad(bool, Bag<Context, Problem>),
}

struct State<Context> {
    src: String,
    offset: usize,
    indent: usize,
    context: Vec<Located<Context>>,
    row: usize,
    col: usize
}

struct Located<Context> {
    row: usize,
    col: usize,
    context: Context,
}

enum Bag<C, X> {
    Empty,
    AddRight(Box<Bag<C, X>>, DeadEnd<C, X>),
    Append(Box<Bag<C, X>>, Box<Bag<C, X>>),
}

struct DeadEnd<Context, Prob> {
    row: usize,
    col: usize,
    problem: Prob,
    context_stack: Vec<Located<Context>>,
}

pub struct Token<X: Clone> (String, X);

pub fn symbol<'a, C, X: Clone + 'a>(token: Token<X>) -> Parser<C, X, ()> {
    let Token(str, expecting) = token;
    let progress = !str.is_empty();

    Parser(Box::new(move |s| {
        let (potential_new_offset, new_row, new_col) = 
            is_substring(&str, s.offset, s.row, s.col, s.src);

        match potential_new_offset {
            None => Bad(false, from_state(s, expecting.clone())),
            Some(new_offset) =>
                Good(progress, (), State {
                    src: s.src,
                    offset: new_offset,
                    indent: s.indent,
                    context: s.context,
                    row: new_row,
                    col: new_col,
                })
        }
    }))
}

fn from_state<C, X>(s: State<C>, x: X) -> Bag<C, X> {
    Bag::AddRight(
        Box::new(Bag::Empty),
        DeadEnd { row: s.row, col: s.col, problem: x, context_stack: s.context }
    )
}

fn is_substring<'a>(small_string: &'a str, offset: usize, row: usize, col: usize, big_string: &'a str) -> (Option<usize>, usize, usize) {
    panic!("blah");
	// var smallLength = smallString.length;
	// var isGood = offset + smallLength <= bigString.length;

	// for (var i = 0; isGood && i < smallLength; )
	// {
	// 	var code = bigString.charCodeAt(offset);
	// 	isGood =
	// 		smallString[i++] === bigString[offset++]
	// 		&& (
	// 			code === 0x000A /* \n */
	// 				? ( row++, col=1 )
	// 				: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
	// 		)
	// }

	// return __Utils_Tuple3(isGood ? offset : -1, row, col);
}

fn example<C, X> () -> Parser<C, String, ((), ())> {
    let token:Token<String> = Token("(".to_owned(), "blah".to_owned());
    let orig: Parser<C, String, ()> = Parser::<C, String, ()>::new();
    let sym: Parser<C, String, ()> = symbol(token);
    let sym2: Parser<C, String, ()> = symbol(Token("(".to_owned(), "blah".to_owned()));

    orig
        .keep(&sym)
        .skip(&sym2)
    // .skip(symbol("("))
    // .skip(spaces)
    // .keep(float)
    // .skip(spaces)
    // .skip(symbol(","))
    // .skip(spaces)
    // .keep(float)
    // .skip(spaces)
    // .skip(symbol(","))
    // .map(finish)
}



    // Parser::new()
    //     .skip(symbol("("))
    //     .skip(spaces)
    //     .keep(float)
    //     .skip(spaces)
    //     .skip(symbol(","))
    //     .skip(spaces)
    //     .keep(float)
    //     .skip(spaces)
    //     .skip(symbol(","))
    //     .map(finish)
