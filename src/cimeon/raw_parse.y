%include {
    #include <string.h>
    #define YYNOERRORRECOVERY
    #define assert(x)
    #define TOKEN_COUNT 10
    /* those two structs can't be forward-declared because they are used in an union */
    struct span {
        uint32_t start;
        uint32_t length;
    };
    typedef struct parsed_token {
        struct span span;
        uint32_t token;
    } parsed_token;
    typedef struct node_variant {
        size_t discriminant;
        void* data;
    } node_variant;
    typedef struct node {
        void* data;
    } node;

    node crate_(node attrs);
    node attrs(node inner_a);
    node append_attr(node attrs, node attr);
    node inner_attr(parsed_token p, parsed_token n, parsed_token l, node_variant meta_i, parsed_token r);
    node_variant meta_item_eq(parsed_token i, parsed_token e, parsed_token s);
    node_variant meta_item_single(parsed_token i, parsed_token l, parsed_token r);
    node_variant meta_item_multi(parsed_token i, parsed_token l, parsed_token c, parsed_token r);
}

%token_type { parsed_token }


%type crate { node }
crate(node) ::= attrs(as). {
    node = crate_(as);
    memcpy(&yypParser->yystack[0].minor, &node, sizeof(node));
}

%type attrs { node }
attrs(node) ::= inner_attr(a). { node = attrs(a); }
attrs(node) ::= attrs(as) inner_attr(a). { node = append_attr(as, a); }

%type inner_attr { node }
inner_attr(node) ::= POUND(P) NOT(N) LBRACKET(L) meta_item(variant) RBRACKET(R). { node = inner_attr(P, N, L, variant, R); }

%type meta_item { node_variant }
meta_item(node_variant) ::= IDENT(I) EQ(E) STRING_LIT(S). { node_variant = meta_item_eq(I, E, S); }
meta_item(node_variant) ::= IDENT(I) LPAREN(L) RPAREN(R). { node_variant = meta_item_single(I, L, R); }
meta_item(node_variant) ::= IDENT(I) LPAREN(L) COMMA(C) RPAREN(R). { node_variant = meta_item_multi(I, L, C, R); }