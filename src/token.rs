#[derive(PartialEq, Eq, Copy, Show, Clone, Hash)]
pub enum LiteralKind {
    Normal,
    //Raw,
}

#[derive(PartialEq, Eq, Copy, Show, Clone, Hash)]
pub enum BinOpKind {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    And,
    Or,
    Shl,
    Shr,
}

#[derive(PartialEq, Eq, Copy, Show, Clone, Hash)]
pub enum Token {
    Error,
    Whitespace,
    CharLiteral, // evaluates to char
    StringLiteral(LiteralKind), // evaluates to &str
    ByteLiteral, // evaluates to u8
    ByteStringLiteral(LiteralKind), // evaluates to &'static [u8]
    Ident,
    Eq,
    Lt,
    Le,
    EqEq,
    Ne,
    Ge,
    Gt,
    AndAnd,
    OrOr,
    Not,
    Tilde,
    BinOp(BinOpKind),
    BinOpEq(BinOpKind),
    At,
    Dot,
    DotDot,
    DotDotDot,
    Comma,
    Semi,
    Colon,
    ModSep,
    RightArrow,
    LeftArrow, // unused token
    FatArrow,
    Pound,
    Dollar,
    Question,
    LeftParen,
    LeftBracket,
    LeftBrace,
    RightParen,
    RightBracket,
    RightBrace,
    Underscore,
    Lifetime,
    /*
    Interpolated,
    DocComment,
    MatchNt,
    SubstNt,
    SpecialVarNt,
    Whitespace,
    Comment,
    Shebang,
    */
}