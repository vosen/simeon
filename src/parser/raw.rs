use super::super::Span;
use lexer::token::Token;
use libc::{size_t, c_void, c_int};
use super::YYCODETYPE;
use std::ptr::copy_nonoverlapping_memory;
use std::mem::uninitialized;

pub fn token_type(token: Token) -> c_int {
	match token {
		Token::Pound => 1,
		Token::Not => 2,
		Token::LeftBracket => 3,
		Token::RightBracket => 4,
		Token::Ident => 5,
		Token::Eq => 6,
		Token::StringLiteral(_) => 7,
		Token::LeftParen => 8,
		Token::RightParen => 9,
		Token::Comma => 10,
		_ => {
			println!("{:?}", token);
			unimplemented!()
		}
	}
}


#[repr(C)]
#[derive(PartialEq, Eq, Copy, Debug, Clone, Hash)]
pub struct ParsedToken {
	span: Span,
	token: u32,
}

#[repr(C)]
#[derive(PartialEq, Eq, Copy, Debug, Clone, Hash)]
struct TokenData {
	token: ParsedToken,
	skipped: &'static ParsedToken
}

impl ParsedToken {
	pub fn new(tup: (Token, Span)) -> ParsedToken {
		let mut result = ParsedToken {
			span: tup.1,
			token: 0
		};
		unsafe { copy_nonoverlapping_memory(&mut result.token  as *mut u32 as *mut Option<_>, &Some(tup.0), 1) };
		result
	}

	pub fn new_eof(pos: u32) -> ParsedToken {
		ParsedToken {
			span: Span { start: pos, end: pos },
			token: 0
		}
	}
}

fn unpickle_token(pickled: u32) -> Option<Token> {
	let mut result : Option<Token> = unsafe { uninitialized() };
	unsafe { copy_nonoverlapping_memory(&mut result, &pickled as *const u32 as *const Option<_>, 1) };
	result
}

#[no_mangle]
pub extern fn meta_item_eq(i: ParsedToken, e: ParsedToken, s: ParsedToken) {
	println!("({:?}, {:?})", i.span, unpickle_token(i.token));
	println!("({:?}, {:?})", e.span, unpickle_token(e.token));
	println!("({:?}, {:?})", s.span, unpickle_token(s.token));
}

#[no_mangle]
pub extern fn meta_item_single(i: ParsedToken, l: ParsedToken, r: ParsedToken) {
	println!("{:?}", i);
	println!("{:?}", l);
	println!("{:?}", r);
}

#[no_mangle]
pub extern fn meta_item_multi(i: ParsedToken, l: ParsedToken, c: ParsedToken, r: ParsedToken) {
	println!("{:?}", i);
	println!("{:?}", l);
	println!("{:?}", c);
	println!("{:?}", r);
}

/* We don't really care about size of Token type, but for the purpose
 * of passing data to the 
 */
#[test]
fn token_fits_in_4_bytes() {
    assert!(::std::mem::size_of::<Option<Token>>() <= 4);
}