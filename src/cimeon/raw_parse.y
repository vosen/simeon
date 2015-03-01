%include {
    #define YYNOERRORRECOVERY
    #define assert(x)
    #define TOKEN_COUNT 10
}

crate ::= attrs.

attrs ::= inner_attr.
attrs ::= attrs inner_attr.

inner_attr ::= POUND NOT LBRACKET meta_item RBRACKET.
meta_item ::= IDENT EQ STRING_LIT.
meta_item ::= IDENT LPAREN RPAREN.
meta_item ::= IDENT LPAREN COMMA RPAREN.