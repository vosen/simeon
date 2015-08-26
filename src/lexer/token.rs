#[derive(PartialEq, Eq, Copy, Debug, Clone, Hash)]
pub enum StringLiteralKind {
    Normal,
    Raw,
}

#[derive(PartialEq, Eq, Copy, Debug, Clone, Hash)]
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

#[derive(PartialEq, Eq, Copy, Debug, Clone, Hash)]
pub enum KeywordKind {
    // strict
    As,
    Break,
    Crate,
    Else,
    Enum,
    Extern,
    False,
    Fn,
    For,
    If,
    Impl,
    In,
    Let,
    Loop,
    Match,
    Mod,
    Move,
    Mut,
    Pub,
    Ref,
    Return,
    Static,
    LowerSelf,
    UpperSelf,
    Struct,
    Super,
    True,
    Trait,
    Type,
    Unsafe,
    Use,
    Virtual,
    While,
    Continue,
    Proc,
    Box,
    Const,
    Where,
    // reserved
    Alignof,
    Be,
    Offsetof,
    Priv,
    Pure,
    Sizeof,
    Typeof,
    Unsized,
    Yield,
    Do,
    Abstract,
    Final,
    Override,
    Macro,
}

#[derive(PartialEq, Eq, Copy, Debug, Clone, Hash)]
pub enum IntegerLiteralBase {
    Decimal,
    Hex,
    Octal,
    Binary
}

#[derive(PartialEq, Eq, Copy, Debug, Clone, Hash)]
pub enum IntegerLiteralSuffix {
    None,
    Isize, Usize,
    U8, I8,
    U16, I16,
    U32, I32,
    U64, I64
}

#[derive(PartialEq, Eq, Copy, Debug, Clone, Hash)]
pub enum FloatLiteralSuffix {
    None,
    F32,
    F64
}

#[derive(PartialEq, Eq, Copy, Debug, Clone, Hash)]
pub enum Token {
    UnexpectedSequence,
    Whitespace,
    Newline,
    DocComment,
    Comment,
    CharLiteral, // evaluates to char
    StringLiteral(StringLiteralKind), // evaluates to &str
    ByteLiteral, // evaluates to u8
    ByteStringLiteral(StringLiteralKind), // evaluates to &'static [u8]
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
    Keyword(KeywordKind),
    IntegerLiteral(IntegerLiteralBase, IntegerLiteralSuffix),
    FloatLiteral(FloatLiteralSuffix),
}