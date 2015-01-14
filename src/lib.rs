#![allow(dead_code)]
#![allow(unused_parens)]
#![allow(unstable)]
#![feature(box_syntax)]

extern crate syntax;

pub mod lexer;
pub mod token;

#[derive(Copy)]
pub struct Span {   
    pub start: u32,
    pub end: u32,
}