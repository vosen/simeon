use libc::{c_void, uint8_t, uint32_t};
use std::mem;
use std::char;

use lexer::{Lexer, StringScanner};
use lexer::token::Token;

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
/// Result returned by the scanning function
pub struct ScannerResult {
    /// Four byte unicode representation of the parsed character
    pub char: uint32_t,
    /// Size of the scanned character in bytes, should be set to 0 in case of EOF
    pub length: uint8_t
}

struct FFIScanner {
    scanner: fn(*mut c_void) -> ScannerResult,
    scanner_state: *mut c_void,
    curr_char: Option<u32>,
    curr_pos: u32,
}

// Only because c_void is not Send
unsafe impl Send for FFIScanner {}


impl StringScanner for FFIScanner {
    fn advance(&mut self) {
        let result = (self.scanner)(self.scanner_state);
        if result.length > 0 {
        	self.curr_char = Some(result.char);
        	self.curr_pos += result.length as u32;
        }
        else {
        	self.curr_char = None;
        }
    }

    fn peek(&self) -> Option<char> {
    	self.curr_char.map(|ch| char::from_u32(ch).unwrap_or('\u{fffd}'))
    }
    
    fn current_position(&self) -> u32 {
        self.curr_pos
    }
}

#[no_mangle]
pub extern fn simeon_lexer_init(scanner: fn(*mut c_void) -> ScannerResult,
                                scanner_state: *mut c_void) -> *mut c_void {
	let mut scanner = FFIScanner {
        scanner: scanner,
        scanner_state: scanner_state,
        curr_char: None,
        curr_pos: 0
    };
    scanner.advance();
    unsafe { mem::transmute(Box::new(Lexer::new(scanner, None))) }
}

#[no_mangle]
pub extern fn simeon_lexer_next(lexer: *mut c_void) -> RawToken {
    let lexer = unsafe { mem::transmute::<_, &Lexer<FFIScanner>>(lexer) };
    unimplemented!()
}

#[no_mangle]
pub extern fn simeon_lexer_free(lexer: *mut c_void) {
    drop(unsafe { mem::transmute::<_, Box<Lexer<FFIScanner>>>(lexer) })
}

fn convert_token(token: Option<Token>) -> RawToken {
    match token {
        None => RawToken::Eof,
        Some(token) => match token {
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
}