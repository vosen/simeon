#![feature(unsafe_destructor)]
#![feature(concat_idents)]
#![feature(core)]
#![feature(std_misc)]
#![feature(unicode)]
#![feature(hash)]
#![feature(collections)]

extern crate core;

/*
 * Rust's lexical grammar is not unambiguous.
 * 'a'b can parse either as <Lifetime><Lifetime>
 * or <CharLiteral><Ident>. Libsyntax lexer panic!()s on that.
 * I've took stats from servo and rust sources.
 * Rate of <Lifetime><Token>?<Lifetime> to <CharLiteral><Token>?<Ident>
 * is 378 to 485 in rust and 3 to 49 in servo.
 * That's why we go with the second choice (and raise IllegalToken).
 */
pub mod lexer;
pub mod parser;
pub mod ptr;

#[derive(PartialEq, Eq, Copy, Debug, Clone, Hash)]
pub struct Span {   
    pub start: u32,
    pub end: u32,
}