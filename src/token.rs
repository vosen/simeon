#[derive(PartialEq, Eq, Copy)]
pub enum LiteralKind {
    Normal,
    Raw,
}

#[derive(PartialEq, Eq, Copy)]
pub enum Token {
    Error,
    Whitespace,
    CharLiteral, // evaluates to char
    StringLiteral(LiteralKind), // evaluates to &str
    ByteLiteral, // evaluates to u8
    ByteStringLiteral(LiteralKind), // evaluates to &'static [u8]
    Ident,
    /*
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
    BinOp,
    BinOpEq,
    At,
    Dot,
    DotDot,
    DotDotDot,
    Comma,
    Semi,
    Colon,
    ModSep,
    RArrow,
    LArrow,
    FatArrow,
    Pound,
    Dollar,
    Question,
    OpenDelim,
    CloseDelim,
    StringLiteral,
    Ident,
    Underscore,
    Lifetime,
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