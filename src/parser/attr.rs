use Span;
use parser::{GenericNodeData, Crate, AuxiliaryNode, SyntaxNode, SyntaxNodeInternal};
use parser::token::{IdentNode, LeftParenNode, CommaNode, RightParenNode, EqNode, StringLiteralNode, PoundNode, NotNode, LeftBracketNode, RightBracketNode};

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
	#[doc(hidden)]
	pub fn __new(pound: PoundNode<InnerAttributeNode>,
				 not: NotNode<InnerAttributeNode>,
				 lbr: LeftBracketNode<InnerAttributeNode>,
				 content: MetaItem,
				 rbr: RightBracketNode<InnerAttributeNode>) -> InnerAttributeNode {
		InnerAttributeNode {
			node_data: GenericNodeData::new(Span::range(pound.span(), rbr.span())),
			leading_aux: None,
			index: 0,
			pound: pound,
			not: not,
			left_bracket: lbr,
			content: content,
			right_bracket: rbr,
		}
	}
}

#[repr(C, usize)]
pub enum MetaItem {
	Word(Box<IdentNode<InnerAttributeNode>>),
	List(Box<MetaItemList>),
	NameValuePair(Box<MetaItemPair>)
}

impl SyntaxNodeInternal for MetaItem {
	fn set_parent(&mut self, parent: *const ::libc::c_void) {
		unimplemented!()
		/*
		match self {
			&MetaItem::Word(_) => panic!(),
			_ => panic!()
		}
		*/
	}
}

typed_token_tuple!(
	MetaItemList,
	InnerAttributeNode,
	ident: IdentNode<MetaItem>,
	left_paren: LeftParenNode<MetaItem>,
	trailing_comma: Option<CommaNode<MetaItem>>,
	right_paren: RightParenNode<MetaItem>);

typed_token_tuple!(
	MetaItemPair,
	InnerAttributeNode,
	ident: IdentNode<MetaItem>,
	eq: EqNode<MetaItem>,
	string_literal: StringLiteralNode<MetaItem>);