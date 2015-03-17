use Span;
use libc::{c_void, c_int, size_t, c_char, c_uchar, c_schar, malloc, free};
use core::nonzero::NonZero;
use std::slice;
use super::ptr::OpaqueBox;
use std::collections::HashMap;
use std::collections::hash_state::DefaultState;
use std::default::Default;
use super::lexer::token::Token;
use std::slice::from_raw_parts;
use super::lexer::{Lexer, StringScanner};
use std::borrow::Borrow;

macro_rules! syntax_node_impl(
	($name: ty, $parent: ty) => (
		impl SyntaxNode<$parent> for $name {
			fn span(&self) -> Span { self.node_data.sp }
			fn has_errors(&self) -> bool { self.node_data.has_errors }
			fn parent<'a>(&'a self) -> &'a $parent { unsafe { &*self.node_data.parent } }
			fn leading_aux<'a>(&'a self) -> &'a [AuxiliaryNode] { self._leading_aux() }
			fn trailing_aux<'a>(&'a self) -> &'a [AuxiliaryNode] { self._trailing_aux() }
		}
	)
);

macro_rules! typed_token_tuple(
	($name: ident, $parent: ident, $($arg: ident : $arg_type: ty),+) => (
		pub struct $name {
			$( $arg: $arg_type, )+
		}

		impl $name {
			$(pub fn $arg(&self) -> &$arg_type {
				&self.$arg
			})+

			#[doc(hidden)]
			pub fn __new($($arg: $arg_type ,)+ ) -> $name {
				$name {
					$($arg: $arg,)+
				}
			}
		}
	)
);

pub use self::attr::{InnerAttributeNode, MetaItem, MetaItemList, MetaItemPair};
pub mod attr;

pub mod token;
#[doc(hidden)] // otherwise functions from this module won't get exported
pub mod raw;

/*
 * Few notes on the structure and memory layout of the ast:
 * # Ast should be verbose enough to recover full text of the crate by 
 *   concatenating all the leaf nodes
 * # Ast root (Crate) must be sendable
 * # All uninteresting stuff: whitespace, comments, unlexable sequences
 *   is contained in aux nodes that are attached to SyntaxNodes.
 * # Nodes own only leading aux, for trailing aux we navigate the tree
 * # Aux is owned by the highest parent. This mean that accessing 
 *   aux might require traversing the tree
 * # Similarly to the libsyntax's ast we try to  be as specific as possible.
 *   That means no generic `Node` trait for generic tree traversal.
 *   This might require an internal macro or trait here and there.
 * # This a public API, so try to make the docs readable.
 *   That means no byzantine generics (Node<P:Parent, T:Content>)
 *   and no constructs that are poorly supported by rustdoc (typedefs).
 */

pub struct Crate {
	attrs: Vec<Box<InnerAttributeNode>>,
	leading_aux: Vec<AuxiliaryNode>,
	trailing_aux: Vec<AuxiliaryNode>,
}

impl Crate {
	pub fn attributes(&self) -> &[Box<InnerAttributeNode>] {
		self.attrs.as_slice()
	}

	pub fn leading_aux(&self) -> &[AuxiliaryNode] {
		self.leading_aux.as_slice()
	}

	pub fn trailing_aux(&self) -> &[AuxiliaryNode] {
		self.trailing_aux.as_slice()
	}

	#[doc(hidden)]
	pub fn __new(a: Box<Vec<Box<InnerAttributeNode>>>) -> Crate {
		Crate {
			attrs: *a,
			leading_aux: vec!(),
			trailing_aux: vec!()
		}
	}
}

#[derive(PartialEq, Eq, Debug, Hash)]
#[allow(missing_copy_implementations)]
#[allow(raw_pointer_derive)]
pub struct AuxiliaryNode {
	parent: *const Crate,
	sp: Span,
	data: AuxiliaryContent
}

#[derive(PartialEq, Eq, Debug, Hash)]
#[allow(missing_copy_implementations)]
#[allow(raw_pointer_derive)]
pub enum AuxiliaryContent {
	UnexpectedToken(super::lexer::token::Token),
	UnexpectedSequence,
	Whitespace,
	DocComment,
	Comment
}

pub trait SyntaxNode<P> {
	fn span(&self) -> Span;
	fn has_errors(&self) -> bool;
	fn parent<'a>(&'a self) -> &'a P;
	fn leading_aux<'a>(&'a self) -> &'a [AuxiliaryNode];
	fn trailing_aux<'a>(&'a self) -> &'a [AuxiliaryNode];
}

trait SyntaxNodeInternal {
	fn set_parent(&mut self, parent: *const c_void);
}

struct GenericNodeData<P> {
	parent: *const P,
	sp: Span,
	has_errors: bool,
}

impl<P> GenericNodeData<P> {
	pub fn new(sp: Span) -> GenericNodeData<P> {
		GenericNodeData {
			parent: ::std::ptr::null(),
			sp: sp,
			has_errors: false
		}
	}
}

type YYACTIONTYPE = c_uchar;
type YYCODETYPE = c_uchar;

#[repr(C)]
#[derive(Copy,Clone)]
struct viable_token {
	token: YYCODETYPE,
	action: YYACTIONTYPE
}

#[repr(C)]
#[derive(Copy,Clone)]
struct viable_token_list {
	count: YYACTIONTYPE,
	actions: *const viable_token
}

extern {
	static state_count: YYACTIONTYPE;
	static unchecked_actions_list: *const viable_token_list;
	static checked_actions_list: *const viable_token_list;
	fn current_state(pptr: *mut c_void) -> YYACTIONTYPE;
	fn top_custom_state_ptr(pptr: *mut c_void) -> OpaqueBox<Crate>;
	fn can_reduce_and_shift(pptr: *mut c_void, token: YYCODETYPE) -> c_int;
    fn ParseAlloc(mallocProc: unsafe extern "C" fn(arg1: size_t) -> *mut c_void) -> *mut c_void;
    fn ParseFree(p: *mut c_void, freeProc: unsafe extern "C" fn(arg1: *mut c_void)-> ()) -> ();
    fn Parse(yyp: *mut c_void, yymajor: c_int, yyminor: raw::ParsedToken) -> ();
}

pub struct Parser<'l, S: StringScanner> {
	raw_parser: NonZero<*mut c_void>,
	error_handler: RawErrorRecovery,
	lexer: Lexer<'l, S>,
}

/*
 * This struct implements DERP error recovery
 * For simple grammar that accepts string of tokens [A,B,C],
 * when encountering an unexpected token, we try following
 * four actions (stopping at the first successful one):
 * # Delete
 *   Given sequence [A,X,B,C], when reaching token X:
 *   ## Check in lookup tables if token B can be shifted
 *      OR
 *      Check in lookup tables if the current state can be reduced
 *      and token B shifted
 *   ## If yes, convert token X to aux node and skip it
 * # Embed
 *   Given sequence [A,C], when reaching token C:
 *   ## Create empty lists L1 and L2
 *   ## For every token T that is shiftable from the current state S to some state Z,
 *      add pair (T, Z) to the list L1
 *      AND
 *      For every token T that is reduce-shiftable from the current state state S to some state Z
 *      add tuple (T, Z, i) to the list L2. i is index of the state Z on the parser stack
 *   ## Create a new list that includes states that can be shifted or reduced-shifted
 *      from (T, Z) or (T, Z, i)
 *   ## Check resultant list, if it contains only a single element, trhen token T is injected between A and C
 * # Replace
 *   Given sequence [A,X,C]
 *   ## Delete token X
 *   ## Try embedding in sequence [A,C] (same process as in Embed action)
 * # Pop parsing stack
 *   ## Keep popping parsing stack until we find parsing rule that can be synchronized.
 *      In practice that means recursively bailing out of current parsing rule,
 *      until we find a parsing rule that lets us skip all the tokens until we find the closing token
 *      Things like blocks: { tokens_to_skip }, function arguments: ( tokens_to_skip ),
 *      generic parameters: < tokens_to_skip > are where this parsing rule works
 * This strategy of error handling is heavily inspired by
 * ANTLR and Xtext strategies:
 * http://zarnekow.blogspot.com/2012/11/xtext-corner-7-parser-error-recovery.html
 * http://www.antlr.org/api/Java/org/antlr/v4/runtime/DefaultErrorStrategy.html
 */
struct RawErrorRecovery {
	unchecked_tokens: &'static [viable_token_list],
	checked_tokens: &'static [viable_token_list],
}

impl RawErrorRecovery {
	fn new() -> RawErrorRecovery {
		RawErrorRecovery {
			unchecked_tokens: unsafe { from_raw_parts(unchecked_actions_list, state_count as usize) },
			checked_tokens: unsafe { from_raw_parts(checked_actions_list, state_count as usize) },
		}
	}

	fn get_token(list: viable_token_list, token: YYCODETYPE) -> Option<viable_token> {
		for i in range(0, list.count as isize) {
			let current_pair = unsafe { *list.actions.offset(i) };
			if current_pair.token == token {
				return Some(current_pair);
			}
		}
		return None;
	}

	fn try_delete(&self, pptr: *mut c_void, next: YYCODETYPE) -> bool {
		if RawErrorRecovery::get_token(self.unchecked_tokens[unsafe { current_state(pptr) } as usize], next).is_some() {
			return true;
		}
		else if let Some(ref token_pair) = RawErrorRecovery::get_token(self.checked_tokens[unsafe { current_state(pptr) } as usize], next) {
			return unsafe { can_reduce_and_shift(pptr, token_pair.token) } != 0;
		}
		return false;
	}

	fn try_embed(&self, pptr: *mut c_void, current: YYCODETYPE, target: YYCODETYPE) -> bool {
		return false;
	}

	fn try_replace(&self, pptr: *mut c_void, token: YYCODETYPE) -> bool {
		return false;
	}

	fn pop(&self) {
		unimplemented!()
	}
}

fn unwrap_nnz<T>(x: NonZero<*mut T>) -> *mut T {
	unsafe { ::std::mem::transmute::<NonZero<*mut T>, *mut T>(x) }
}

impl<'l, S:StringScanner> Parser<'l, S> {
	pub fn new<'a>(l: Lexer<'a, S>) -> Parser<'a, S> {
		Parser {
			raw_parser: unsafe { NonZero::new({ ParseAlloc(malloc) }) },
			error_handler: RawErrorRecovery::new(),
			lexer: l
		}
	}

	pub fn parse(&mut self) -> OpaqueBox<Crate> {
		for token_span in self.lexer.scan() {
			unsafe { Parse(unwrap_nnz(self.raw_parser), raw::token_type(token_span.0), raw::ParsedToken::new(token_span)) };
		}
		unsafe { Parse(unwrap_nnz(self.raw_parser), 0, raw::ParsedToken::new_eof(self.lexer.scanner().current_position())) };
		unsafe { top_custom_state_ptr(unwrap_nnz(self.raw_parser)) }
	}
}

#[unsafe_destructor]
impl<'l, S:StringScanner> Drop for Parser<'l, S> {
	fn drop(&mut self) {
		unsafe { ParseFree( unwrap_nnz(self.raw_parser), free) }
	}
}

#[cfg(test)]
mod test {
	use super::super::lexer::{Lexer, SimpleStringScanner};
	use super::{Parser, Crate, SyntaxNode, MetaItem};
	use std::borrow::Borrow;
	use ptr::OpaqueBox;
	use Span;

	#[test]
	fn parse_simple_attr() {
		let text = "#![ident=\"asd\"]".to_string();
		let mut parser = Parser::new(
			Lexer::new(
				SimpleStringScanner::new(text),
				Some(Box::new(|_,_,_| { assert!(false); }))
			)
		);
		let _crate : OpaqueBox<Crate> = parser.parse();
		assert!((&*_crate).attributes().len() == 1);
		let attr = &(&*_crate).attributes()[0];
		assert!(attr.pound().span() == Span { start: 0, end: 1 });
		assert!(attr.not().span() == Span { start: 1, end: 2 });
		assert!(attr.left_bracket().span() == Span { start: 2, end: 3 });
		assert!(attr.right_bracket().span() == Span { start: 14, end: 15 });
		match attr.content() {
			&MetaItem::NameValuePair(ref meta_pair) => {
				assert!(meta_pair.ident().span() == Span { start: 3, end: 8 });
				assert!(meta_pair.eq().span() == Span { start: 8, end: 9 });
				assert!(meta_pair.string_literal().span() == Span { start: 9, end: 14 });
			}
			_ => assert!(false)
		};
	}

	#[test]
	fn recover_from_extra_token() {
		let text = "#!![ident=\"asd\"]".to_string();
		let mut parser = Parser::new(
			Lexer::new(
				SimpleStringScanner::new(text),
				Some(Box::new(|_,_,_| { assert!(false); }))
			)
		);
		let _crate : OpaqueBox<Crate> = parser.parse();
		assert!((&*_crate).attributes().len() == 1);
		let attr = &(&*_crate).attributes()[0];
		assert!(attr.pound().span() == Span { start: 0, end: 1 });
		assert!(attr.not().span() == Span { start: 1, end: 2 });
		assert!(attr.left_bracket().span() == Span { start: 3, end: 4 });
		assert!(attr.right_bracket().span() == Span { start: 15, end: 16 });
		match attr.content() {
			&MetaItem::NameValuePair(ref meta_pair) => {
				assert!(meta_pair.ident().span() == Span { start: 4, end: 9 });
				assert!(meta_pair.eq().span() == Span { start: 9, end: 10 });
				assert!(meta_pair.string_literal().span() == Span { start: 10, end: 15 });
			}
			_ => assert!(false)
		};
	}
}