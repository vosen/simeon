use libc::c_void;
use std::mem;
use std::slice;

use lexer::{Lexer, Scanner, LexerIterator};
use lexer::token::Token;
use Span;

#[repr(u8)]
pub enum RawToken {
    Eof = 0,
    UnexpectedSequence = 1,
    Whitespace = 2,
    Newline = 3,
    DocComment = 4,
    Comment = 5,
    CharLiteral = 6,
    StringLiteral = 7,
    ByteLiteral = 8,
    ByteStringLiteral = 9,
    Ident = 10,
    Eq = 11,
    Lt = 12,
    Le = 13,
    EqEq = 14,
    Ne = 15,
    Ge = 16,
    Gt = 17,
    AndAnd = 18,
    OrOr = 19,
    Not = 20,
    Tilde = 21,
    BinOp = 22,
    BinOpEq = 23,
    At = 24,
    Dot = 25,
    DotDot = 26,
    DotDotDot = 27,
    Comma = 28,
    Semi = 29,
    Colon = 30,
    ModSep = 31,
    RightArrow = 32,
    LeftArrow = 33,
    FatArrow = 34,
    Pound = 35,
    Dollar = 36,
    Question = 37,
    LeftParen = 38,
    LeftBracket = 39,
    LeftBrace = 40,
    RightParen = 41,
    RightBracket = 42,
    RightBrace = 43,
    Underscore = 44,
    Lifetime = 45,
    Keyword = 46,
    IntegerLiteral = 47,
    FloatLiteral = 48,
}

#[repr(C)]
/// Result returned by the lexing function
pub struct LexerResult {
    /// Span of the parsed token
    pub span: Span,
    /// Parsed token 
    pub token: RawToken,
}

struct UTF16Scanner<'a> {
    idx: u32,
    arr: &'a [u16],
}

impl<'a> UTF16Scanner<'a> {
    fn new(buffer: *const u16, len: usize) -> UTF16Scanner<'a> {
        UTF16Scanner {
            idx: 0,
            arr: unsafe { slice::from_raw_parts(buffer, len) }
        }
    }

    // copied from rustc_unicode
    fn char_at(&self, i: usize) -> Option<char> {
        let u = match self.arr.get(i) {
            Some(u) => *u,
            None => return None
        };
        if u < 0xD800 || 0xDFFF < u {
            Some(unsafe {mem::transmute(u as u32)})
        } else if u >= 0xDC00 {
            Some('\u{fffd}')
        } else {
            let u2 = match self.arr.get(i + 1) {
                Some(u2) => *u2,
                None => return Some('\u{fffd}')
            };
            if u2 < 0xDC00 || u2 > 0xDFFF {
                return Some('\u{fffd}');
            }
            let c = (((u - 0xD800) as u32) << 10 | (u2 - 0xDC00) as u32) + 0x1_0000;
            Some(unsafe {mem::transmute(c)})
        }
    }
}

impl<'a> Scanner for UTF16Scanner<'a> {
    fn advance(&mut self) {
        match self.char_at(self.idx as usize) {
            None => {},
            Some(c) => self.idx += c.len_utf16() as u32
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.char_at(self.idx as usize)
    }

    fn peek2(&mut self) -> Option<char> {
        self.char_at(self.idx as usize)
            .and_then(|c| self.char_at(self.idx as usize + c.len_utf16()))
    }
    
    fn current_position(&self) -> u32 {
        self.idx
    }
}

#[no_mangle]
pub extern fn simeon_lexer_init(buffer: *const u16, len: usize) -> *mut c_void {
    let scanner = UTF16Scanner::new(buffer, len);
    unsafe { mem::transmute(Box::new(Lexer::new(scanner, None).scan())) }
}

#[no_mangle]
pub extern fn simeon_lexer_next(lexer: *mut c_void) -> LexerResult {
    let mut iter = unsafe { mem::transmute::<_, &mut LexerIterator<UTF16Scanner>>(lexer) };
    match iter.next() {
        None => LexerResult {
            span: Span { start: 0, end: 0 },
            token: RawToken::Eof
        },
        Some((token, span)) => LexerResult {
            span: span,
            token: convert_token(token)
        }
    }
}

#[no_mangle]
pub extern fn simeon_lexer_free(lexer: *mut c_void) {
    drop(unsafe { mem::transmute::<_, Box<LexerIterator<UTF16Scanner>>>(lexer) })
}

fn convert_token(token: Token) -> RawToken {
    match token {
        Token::UnexpectedSequence => RawToken::UnexpectedSequence,
        Token::Whitespace => RawToken::Whitespace,
        Token::Newline => RawToken::Newline,
        Token::DocComment => RawToken::DocComment,
        Token::Comment => RawToken::Comment,
        Token::CharLiteral => RawToken::CharLiteral,
        Token::StringLiteral(..) => RawToken::StringLiteral,
        Token::ByteLiteral => RawToken::ByteLiteral,
        Token::ByteStringLiteral(..) => RawToken::ByteStringLiteral,
        Token::Ident => RawToken::Ident,
        Token::Eq => RawToken::Eq,
        Token::Lt => RawToken::Lt,
        Token::Le => RawToken::Le,
        Token::EqEq => RawToken::EqEq,
        Token::Ne => RawToken::Ne,
        Token::Ge => RawToken::Ge,
        Token::Gt => RawToken::Gt,
        Token::AndAnd => RawToken::AndAnd,
        Token::OrOr => RawToken::OrOr,
        Token::Not => RawToken::Not,
        Token::Tilde => RawToken::Tilde,
        Token::BinOp(..) => RawToken::BinOp,
        Token::BinOpEq(..) => RawToken::BinOpEq,
        Token::At => RawToken::At,
        Token::Dot => RawToken::Dot,
        Token::DotDot => RawToken::DotDot,
        Token::DotDotDot => RawToken::DotDotDot,
        Token::Comma => RawToken::Comma,
        Token::Semi => RawToken::Semi,
        Token::Colon => RawToken::Colon,
        Token::ModSep => RawToken::ModSep,
        Token::RightArrow => RawToken::RightArrow,
        Token::LeftArrow => RawToken::LeftArrow,
        Token::FatArrow => RawToken::FatArrow,
        Token::Pound => RawToken::Pound,
        Token::Dollar => RawToken::Dollar,
        Token::Question => RawToken::Question,
        Token::LeftParen => RawToken::LeftParen,
        Token::LeftBracket => RawToken::LeftBracket,
        Token::LeftBrace => RawToken::LeftBrace,
        Token::RightParen => RawToken::RightParen,
        Token::RightBracket => RawToken::RightBracket,
        Token::RightBrace => RawToken::RightBrace,
        Token::Underscore => RawToken::Underscore,
        Token::Lifetime => RawToken::Lifetime,
        Token::Keyword(..) => RawToken::Keyword,
        Token::IntegerLiteral(..) => RawToken::IntegerLiteral,
        Token::FloatLiteral(..) => RawToken::FloatLiteral,
    }
}