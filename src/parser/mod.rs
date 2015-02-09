use super::Span;
use self::token::{IdentNode, LeftParenNode, RightParenNode, CommaNode, EqNode, PoundNode, NotNode, LeftBracketNode, RightBracketNode};

pub mod token;

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
	attrs: Vec<InnerAttributeNode>,
	leading_aux: Vec<AuxiliaryNode>,
	trailing_aux: Vec<AuxiliaryNode>,
}

impl Crate {
	pub fn attributes(&self) -> &[InnerAttributeNode] {
		self.attrs.as_slice()
	}

	pub fn leading_aux(&self) -> &[AuxiliaryNode] {
		self.leading_aux.as_slice()
	}

	pub fn trailing_aux(&self) -> &[AuxiliaryNode] {
		self.trailing_aux.as_slice()
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

macro_rules! syntax_node_impl(
	($name: ty, $parent: ty) => (
		impl SyntaxNode<$parent> for $name {
			fn span(&self) -> Span { self.node_data.sp }
			fn has_errors(&self) -> bool { self.node_data.has_errors }
			fn parent<'a>(&'a self) -> &'a $parent { & unsafe { *self.node_data.parent } }
		}
	)
);

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

struct GenericNodeData<P> {
	parent: *const P,
	sp: Span,
	has_errors: bool,
}

// atribute node
pub struct InnerAttributeNode {
    node_data: GenericNodeData<Crate>,
    leading_aux: Option<Vec<AuxiliaryNode>>,
	index: usize,
	pound: PoundNode<InnerAttributeNode>,
	not: NotNode<InnerAttributeNode>,
	left_bracket: LeftBracketNode<InnerAttributeNode>,
	content: MetaItem,
	right_bracket: RightBracketNode<InnerAttributeNode>
}
syntax_node_impl!(InnerAttributeNode, Crate);
impl InnerAttributeNode {
	fn _leading_aux<'a>(&'a self) -> &'a [AuxiliaryNode] {
		if let Some(ref vec) = self.leading_aux {
			return vec.as_slice();
		}
		self.parent().leading_aux.as_slice()
	}
	fn _trailing_aux<'a>(&'a self) -> &'a [AuxiliaryNode] {
		if self.parent().attrs.len() == self.index - 1 {
			return self.parent().trailing_aux.as_slice();
		}
		self.parent().attrs[self.index + 1].leading_aux()
	}
	pub fn pound(&self) -> &PoundNode<InnerAttributeNode> { &self.pound }
	pub fn not(&self) -> &NotNode<InnerAttributeNode> { &self.not }
	pub fn left_bracket(&self) -> &LeftBracketNode<InnerAttributeNode> { &self.left_bracket }
	pub fn content(&self) -> &MetaItem { &self.content }
	pub fn right_bracket(&self) -> &RightBracketNode<InnerAttributeNode> { &self.right_bracket }
}

pub enum MetaItem {
	Word(IdentNode<MetaItem>),
	List(IdentNode<MetaItem>, LeftParenNode<MetaItem>, /* TODO: seq node will go here */ Option<CommaNode<MetaItem>>, RightParenNode<MetaItem>),
	NameValuePair(IdentNode<MetaItem>, EqNode<MetaItem>, /* TODO: literal node will go here */ )
}