#![allow(dead_code)]
#![allow(unused_parens)]
#![allow(unstable)]
#![feature(box_syntax)]


extern crate syntax;

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
pub mod token;

#[derive(PartialEq, Eq, Copy, Show, Clone, Hash)]
pub struct Span {   
    pub start: u32,
    pub end: u32,
}