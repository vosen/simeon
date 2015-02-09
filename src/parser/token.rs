use super::super::Span;
use super::super::lexer::LexingError;
use super::{SyntaxNode, AuxiliaryNode};

macro_rules! node_type_for_token(
	($tok: ident) => (
		pub struct $tok<P> {
			parent: *const P,
			sp: Span,
			error: Option<LexingError>,
			// I'm equally horrified and amazed
			leading_aux_fn: Box<for<'a> Fn(&'a $tok<P>) -> &'a [AuxiliaryNode] + 'static>,
			trailing_aux_fn: Box<for<'a> Fn(&'a $tok<P>) -> &'a [AuxiliaryNode] + 'static>,
		}
		impl<P> SyntaxNode<P> for $tok<P> {
			fn span(&self) -> Span { self.sp }
			fn has_errors(&self) -> bool { self.error.is_some() }
			fn parent<'a>(&'a self) -> &'a P { unsafe { &*self.parent } }
			fn leading_aux<'a>(&'a self) -> &'a [AuxiliaryNode] { (self.leading_aux_fn)(self) }
			fn trailing_aux<'a>(&'a self) -> &'a [AuxiliaryNode] { (self.trailing_aux_fn)(self) }
		}
		impl<P> $tok<P> {
			pub fn error(&self) -> Option<LexingError> { self.error }
		}
	)
);
node_type_for_token!(IdentNode);
node_type_for_token!(LeftParenNode);
node_type_for_token!(RightParenNode);
node_type_for_token!(CommaNode);
node_type_for_token!(EqNode);
node_type_for_token!(PoundNode);
node_type_for_token!(NotNode);
node_type_for_token!(LeftBracketNode);
node_type_for_token!(RightBracketNode);