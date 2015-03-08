%include {
    #define YYNOERRORRECOVERY
    #define assert(x)
    #define TOKEN_COUNT 10
    /* those two structs can't be forward-delcared because they are used in an union */
    struct span {
        uint32_t start;
        uint32_t length;
    };
    struct parsed_token {
        struct span span;
        uint32_t token;
    };
}
%token_type { struct parsed_token }

crate ::= attrs.

attrs ::= inner_attr.
attrs ::= attrs inner_attr.

inner_attr ::= POUND NOT LBRACKET meta_item RBRACKET.
meta_item ::= IDENT(I) EQ(E) STRING_LIT(S). { meta_item_eq(I, E, S); }
meta_item ::= IDENT(I) LPAREN(L) RPAREN(R). { meta_item_single(I, L, R); }
meta_item ::= IDENT(I) LPAREN(L) COMMA(C) RPAREN(R). { meta_item_multi(I, L, C, R); }