use super::Span; 
use super::token::{Token, StringLiteralKind, BinOpKind, KeywordKind, IntegerLiteralKind};
use std::ascii::AsciiExt;

pub trait StringScanner : Send {
    // returns eof: \u0003 at the end of the text
    fn advance(&mut self);
    fn peek(&self) -> Option<char>;
    fn current_position(&self) -> u32;
    fn next(&mut self) -> Option<char> {
        let val = self.peek();
        self.advance();
        return val;
    }
}

const MAX_KEYWORD_LENGTH: usize = 8;
const MAX_INT_SUFFIX_LENGTH: usize = 3;

#[derive(PartialEq, Eq, Copy, Show, Clone, Hash)]
pub enum LexingError {
    Eof, // we were expecting something but the file ended
    IllegalToken, // special error for 'a'b
    NonAsciiByte, // special error for bytes with unicode bytes, eg b'ę'
    InvalidEscapeSeq, // for semantically wrong seqs like '\xff'
    IllegalEscapeSeq, // for '\u8' inside byte literals
    MalformedEscapeSeq, // we started to lex escape seq but it is broken in some syntactic way, eg. '\u{}' or '\x0'
    TokenTooLong, // for char/byte literals that exceed our expectations, eg. 'ab'
    TokenTooShort, // for char/byte literals that have the form ''
    UnescapedLiteral, // byte and char literals don't allow unescaped ', \t, \n, \r
    MalformedLiteral, // for things like `0b ` or `0b9`
    IllegalSuffix // for illegal integer suffixes: things like `1i16us`
}

pub struct SimpleStringScanner {
    idx: u32,
    s: String,
}

impl SimpleStringScanner {
    pub fn new(s: String) -> SimpleStringScanner {
        if ::std::mem::size_of::<usize>() < 4 || s.len() > (::std::u32::MAX as usize) {
            panic!();
        }
        SimpleStringScanner {
            idx: 0,
            s: s
        }
    }

    // this fn is mostly for debugging
    #[allow(deprecated)]
    pub fn slice_at(&self, sp: Span) -> &str {
        self.s.slice(sp.start as usize, sp.end as usize)
    }

    fn is_eof(&self, idx: u32) -> bool { (idx as usize) >= self.s.len() }
    fn peek_to(&self, idx: u32) -> Option<char> {
        if self.is_eof(idx) {
            return None;
        }
        else {
            return Some(self.s.char_at(idx as usize));
        }
    }
}

impl StringScanner for SimpleStringScanner {
    fn peek(&self) -> Option<char> {
        self.peek_to(self.idx)
    }

    fn advance(&mut self) {
        if self.is_eof(self.idx) {
            panic!("Can't advance beyond the scanned string");
        }
        else {
            match self.s.char_at(self.idx as usize) as u32 {
                0x0000u32...0x007Fu32 => self.idx += 1,
                0x0080u32...0x07FFu32 => self.idx += 2,
                0x0800u32...0xFFFFu32 => self.idx += 3,
                0x10000u32...0xFFFFFFFFu32 => self.idx += 4,
                _ => panic!("rustc error"),
            }
        }
    }

    fn current_position(&self) -> u32 {
        self.idx
    }
}
pub struct Lexer<'this, S:StringScanner> {
    r: S,
    err_fn: Option<Box<FnMut(&Lexer<S>, LexingError, u32)+'this>>,
    err_recovery: bool, // set to true during first lexing error in a token
}

// Actual lexing
impl<'this, S:StringScanner> Lexer<'this, S> {
    pub fn new(scanner: S, err_handler:Option<Box<FnMut(&Lexer<S>, LexingError, u32)+'this>>) -> Lexer<'this, S> {
        Lexer { r:scanner, err_fn: err_handler, err_recovery: false }
    }
}

fn match_keyword(ident: &str) -> Option<KeywordKind> {
    match ident {
        "as" => Some(KeywordKind::As),
        "break" => Some(KeywordKind::Break),
        "crate" => Some(KeywordKind::Crate),
        "else" => Some(KeywordKind::Else),
        "enum" => Some(KeywordKind::Enum),
        "extern" => Some(KeywordKind::Extern),
        "false" => Some(KeywordKind::False),
        "fn" => Some(KeywordKind::Fn),
        "for" => Some(KeywordKind::For),
        "if" => Some(KeywordKind::If),
        "impl" => Some(KeywordKind::Impl),
        "in" => Some(KeywordKind::In),
        "let" => Some(KeywordKind::Let),
        "loop" => Some(KeywordKind::Loop),
        "match" => Some(KeywordKind::Match),
        "mod" => Some(KeywordKind::Mod),
        "move" => Some(KeywordKind::Move),
        "mut" => Some(KeywordKind::Mut),
        "pub" => Some(KeywordKind::Pub),
        "ref" => Some(KeywordKind::Ref),
        "return" => Some(KeywordKind::Return),
        "static" => Some(KeywordKind::Static),
        "self" => Some(KeywordKind::Self),
        "struct" => Some(KeywordKind::Struct),
        "super" => Some(KeywordKind::Super),
        "true" => Some(KeywordKind::True),
        "trait" => Some(KeywordKind::Trait),
        "type" => Some(KeywordKind::Type),
        "unsafe" => Some(KeywordKind::Unsafe),
        "use" => Some(KeywordKind::Use),
        "virtual" => Some(KeywordKind::Virtual),
        "while" => Some(KeywordKind::While),
        "continue" => Some(KeywordKind::Continue),
        "proc" => Some(KeywordKind::Proc),
        "box" => Some(KeywordKind::Box),
        "const" => Some(KeywordKind::Const),
        "where" => Some(KeywordKind::Where),
        "alignof" => Some(KeywordKind::Alignof),
        "be" => Some(KeywordKind::Be),
        "offsetof" => Some(KeywordKind::Offsetof),
        "priv" => Some(KeywordKind::Priv),
        "pure" => Some(KeywordKind::Pure),
        "sizeof" => Some(KeywordKind::Sizeof),
        "typeof" => Some(KeywordKind::Typeof),
        "unsized" => Some(KeywordKind::Unsized),
        "yield" => Some(KeywordKind::Yield),
        "do" => Some(KeywordKind::Do),
        "abstract" => Some(KeywordKind::Abstract),
        "final" => Some(KeywordKind::Final),
        "override" => Some(KeywordKind::Override),
        "macro" => Some(KeywordKind::Macro),
        _ => None
    }
}

fn is_valid_int_suffix(ident: &str) -> bool {
    match ident {
          "is"  | "us"
        | "u8"  | "i8"
        | "u16" | "i16"
        | "u32" | "i32"
        | "u64" | "i64" => true,
        _ => false
    }
}

impl<'this, S:StringScanner> Lexer<'this, S> {
    fn on_error(&mut self, err: LexingError) {
        let curr_pos = self.r.current_position();
        self.on_error_at(err, curr_pos)
    }

    fn on_error_at(&mut self, err: LexingError, pos: u32) {
        if self.err_recovery {
            return;
        }
        else {
            self.err_recovery = true;
        }
        if let Some(ref mut callback) = self.err_fn {
            callback(unsafe { &*(self as *mut _) }, err, pos)
        }
    }

    fn scan_whitespace(&mut self) -> Token {
        loop {
            self.r.advance();
            match self.r.peek() {
                Some(c) => if !c.is_whitespace() { break; },
                None => { break; }
            }
        }
        return Token::Whitespace;
    }

    fn advance_literal_or_lifetime(&mut self, c_end: char, allow_unicode: bool, mut look_for_lifetime: bool) -> bool {
        debug_assert!(self.r.peek() == Some(c_end));
        let start = self.r.current_position();
        self.r.advance();
        let mut first_error = None;
        let mut length = 0us;
        loop {
            match self.r.peek() {
                None => {
                    if !look_for_lifetime || length == 0 {
                        self.on_error(LexingError::Eof);
                        return false;
                    }
                    return true;
                }
                Some(c) => {
                    match c {
                        '\\' => {
                            first_error = first_error.or(self.advance_escape_seq(start, allow_unicode));
                            look_for_lifetime = false;
                        },
                        c if c == c_end => {
                            let second_quote_pos = self.r.current_position();
                            self.r.advance();
                            if c_end == '\'' && length == 0 {
                                match self.r.peek() {
                                    Some(c) if c == c_end => {
                                        self.on_error_at(LexingError::UnescapedLiteral, second_quote_pos);
                                        self.r.advance();
                                    }
                                    _ => self.on_error_at(LexingError::TokenTooShort, start)
                                }
                            }
                            if let Some((err,idx)) = first_error {
                                // InvalidEscapeSeq is a semantic err, so we delay it in favour of syntactic errors
                                if err != LexingError::InvalidEscapeSeq {
                                    self.on_error_at(err, idx);
                                }
                            }
                            if c_end == '\'' && length > 1 {
                                self.on_error_at(LexingError::TokenTooLong, start);
                            }
                            if let Some((err,idx)) = first_error {
                                if err == LexingError::InvalidEscapeSeq {
                                    self.on_error_at(err, idx);
                                }
                            }
                            return false;
                        },
                        c if !allow_unicode && !c.is_ascii() => {
                            first_error = first_error.or(Some((LexingError::NonAsciiByte, self.r.current_position())));
                            self.r.advance();
                        },
                        '\r' | '\n' | '\u{2028}' | '\u{2029}' if c_end  == '\'' => {
                            if look_for_lifetime && length > 0 {
                                return true;
                            }
                            else {
                                self.on_error(LexingError::UnescapedLiteral);
                                return false;
                            }
                        },
                        '\t' | '\'' if c_end  == '\'' => {
                            first_error = first_error.or(Some((LexingError::UnescapedLiteral, self.r.current_position())));
                            self.r.advance();
                        },
                        c if c.is_whitespace() && look_for_lifetime => {
                            return true;
                        },
                        c if !c.is_xid_continue() => {
                            look_for_lifetime = false;
                            self.r.advance();
                        },
                        _ => self.r.advance()
                    }
                }
            }
            length += 1;
        }
    }

    // stairs of doom
    fn advance_hex_digits(&mut self, start: u32) -> Option<(LexingError, u32)> {
        debug_assert!(self.r.peek() == Some('x'));
        self.r.advance();
        let mut back_buffer = [0; 2];
        for i in range(0, 2) {
            match self.r.peek() {
                Some(c) if c.is_digit(16) => {
                    back_buffer[i] = c.to_digit(16).unwrap() as u8;
                    self.r.advance();
                }
                Some(_) => return Some((LexingError::MalformedEscapeSeq, self.r.current_position())),
                None => return Some((LexingError::Eof, self.r.current_position())),
            }
        }
        if (back_buffer[0] * 16) + back_buffer[1] > 0x7f {
            Some((LexingError::InvalidEscapeSeq, start))
        }
        else {
            None
        }
    }

    fn advance_unicode_digits(&mut self, token_start: u32) -> Option<(LexingError, u32)> {
        debug_assert!(self.r.peek() == Some('u'));
        self.r.advance();
        let mut back_buffer = [0; 6];
        match self.r.peek() {
            None => return Some((LexingError::Eof, self.r.current_position())),
            Some('{') => self.r.advance(),
            Some(_) => return Some((LexingError::MalformedEscapeSeq, self.r.current_position())),
        }
        let mut length = 0;
        for i in range(0, 6) {
            match self.r.peek() {
                Some('}') => {
                    length = i;
                    break;
                }
                Some(c) if c.is_digit(16) => {
                    back_buffer[i] = c.to_digit(16).unwrap() as u8;
                    self.r.advance();
                }
                Some(_) => return Some((LexingError::MalformedEscapeSeq, self.r.current_position())),
                None => return Some((LexingError::Eof, self.r.current_position())),
            }
        }
        if length == 0 {
            return Some((LexingError::MalformedEscapeSeq, self.r.current_position()));
        }
        match self.r.peek() {
            Some('}') => {
                self.r.advance();
                let sum : u64 = (back_buffer[0] << 5) as u64 + (back_buffer[2] << 4) as u64
                                + (back_buffer[2] << 3) as u64 + (back_buffer[3] << 2) as u64
                                + (back_buffer[4] << 1) as u64 + (back_buffer[5] << 0) as u64;
                if sum > 0x10ffffu64 || (sum >= 0xdc00u64 && sum <= 0xdfffu64) {
                    return Some((LexingError::InvalidEscapeSeq, token_start));
                }
                else {
                    return None
                }

            },
            Some(_) => return Some((LexingError::MalformedEscapeSeq, self.r.current_position())),
            None => return Some((LexingError::Eof, self.r.current_position())),
        }
    }

    fn advance_escape_seq(&mut self, token_start: u32, unicode: bool) -> Option<(LexingError, u32)> {
        debug_assert!(self.r.peek() == Some('\\'));
        self.r.advance();
        match self.r.peek() {
            None => return Some((LexingError::Eof, self.r.current_position())),
            Some(esc_mark) => {
                match esc_mark {
                    'n' | 'r' | 't' | '\\' | '\'' | '"' | '0' => self.r.advance(),
                    'x' => return self.advance_hex_digits(token_start),
                    'u' => {
                        let u_loc = self.r.current_position();
                        let err = self.advance_unicode_digits(token_start);
                        if !unicode {
                            return Some((LexingError::IllegalEscapeSeq, u_loc));
                        }
                        else {
                            return err;
                        }
                    },
                    _ => {
                        return Some((LexingError::MalformedEscapeSeq, self.r.current_position()));
                    }
                }
            }
        };
        None
    }

    // doc_comment flag is passed to handle the case of /**/
    fn scan_block_comment(&mut self, doc_comment: bool) -> Token {
        debug_assert!(self.r.peek() == Some('*'));
        let mut first_loop = false;
        let mut depth = 0;
        loop {
            self.r.advance();
            match self.r.peek() {
                Some('/') => {
                    if doc_comment && first_loop {
                        return Token::Comment
                    }
                    self.r.advance();
                    match self.r.peek() {
                        Some('*') => depth += 1,
                        Some(_) => { },
                        None => {
                            self.on_error(LexingError::Eof);
                            break;
                        }
                    }
                },
                Some('*') => {
                    self.r.advance();
                    match self.r.peek() {
                        Some('/') => {
                            if depth == 0 {
                                self.r.advance();
                                break;
                            }
                            depth -= 1;
                        } ,
                        Some(_) => { },
                        None => {
                            self.on_error(LexingError::Eof);
                            break;
                        }
                    }
                },
                Some(_) => { },
                None => {
                    self.on_error(LexingError::Eof);
                    break;
                }
            }
            if first_loop {
                first_loop = false;
            }
        }
        if doc_comment { Token::DocComment } else { Token::Comment }
    }

    fn scan_ident_or_keyword_core<F:FnMut(char)>(&mut self, mut f: F) {
        loop {
            match self.r.peek() {
                Some(x) if !x.is_xid_continue() => break,
                None => break,
                Some(c) => {
                    f(c);
                    self.r.advance();
                }
            }
        }
    }

    fn scan_ident(&mut self) -> Token {
        self.scan_ident_or_keyword_core(|_| {});
        Token::Ident
    }

    // this function can be called after we've eaten single 'b' char,
    // that's why this weird flag is here
    fn scan_ident_or_keyword(&mut self, prepend_b: bool) -> Token {
        let mut back_buffer = [0u8; MAX_KEYWORD_LENGTH]; // all keywords are ASCII
        let mut length = 0;
        if prepend_b {
            back_buffer[0] = b'b';
            length += 1;
        }
        self.scan_ident_or_keyword_core(|c| {
            if length <= MAX_KEYWORD_LENGTH {
                if c.is_ascii() {
                    back_buffer[length] = c as u8;
                        length += 1;
                }
                else {
                    length = MAX_KEYWORD_LENGTH + 1;
                }
            }
        });
        if length <= MAX_KEYWORD_LENGTH {
            let text_slice = unsafe { ::std::str::from_utf8_unchecked(&back_buffer[0..length]) }; // this is safe because back_buffer contains bytes we've just copied
            match match_keyword(text_slice) {
                Some(keyword_kind) => return Token::Keyword(keyword_kind),
                None => return Token::Ident
            }
        }
        return Token::Ident;
    }

    fn advance_single_line_comment(&mut self) {
        debug_assert!(self.r.peek() == Some('/'));
        loop {
            self.r.advance();
            match self.r.peek() {
                Some(c) => {
                    match c {
                        '\r' | '\n' | '\u{2028}' | '\u{2029}' => break,
                        _ => continue,
                    }
                }
                None => break
            }
        }
    }

    fn advance_digits(&mut self, max_digit: char, mut allow_zero_length: bool) {
        loop {
            self.r.advance();
            match self.r.peek() {
                Some(c) if c == '_' || (c >= '0' && c <= '9' && c <= max_digit) || (c >= 'a' && c <= max_digit) => {
                    if !allow_zero_length {
                        allow_zero_length = true;
                    }
                },
                _ => {
                    if !allow_zero_length {
                        self.on_error(LexingError::MalformedLiteral);
                        return;
                    }
                    break;
                }
            }
        }
        let suffix_start = self.r.current_position();
        match self.r.peek() {
            Some(c) if c.is_xid_continue() => {
                let mut back_buffer = [0u8; MAX_INT_SUFFIX_LENGTH]; // suffixes are ASCII
                let mut length = 0;
                loop {
                    match self.r.peek() {
                        Some(c) if c.is_xid_continue() => {
                            if c.is_ascii() && length < MAX_INT_SUFFIX_LENGTH {
                                back_buffer[length] = c as u8;
                            }
                            else {
                                length = MAX_INT_SUFFIX_LENGTH + 1;
                            }
                        }
                        _ => {
                            length += 1;
                            break;
                        }
                    }
                    self.r.advance();
                }
                if length > MAX_INT_SUFFIX_LENGTH {
                    self.on_error_at(LexingError::IllegalSuffix, suffix_start);
                }
                else {
                    let suffix = unsafe { ::std::str::from_utf8_unchecked(&back_buffer[0..length]) }; // safe, we've checked earlier that all u8s are ascii
                    if !is_valid_int_suffix(suffix) {
                        self.on_error_at(LexingError::IllegalSuffix, suffix_start);
                    }
                }
            },
            _ => { }
        }
    }

    fn eat(&mut self, t: Token) -> Token {
        self.r.advance();
        t
    }

    fn scan_token(&mut self) -> Option<(Token, Span)> {
        let start = self.r.current_position();
        let result = self.scan_token_inner();
        self.err_recovery = false;
        let end = self.r.current_position();
        return result.map(|t| (t, Span { start: start, end: end }));
    }

    fn scan_token_inner(&mut self) -> Option<Token> {
        let curr = self.r.peek();
        if curr.is_none() {
            return None;
        }
        let curr = curr.unwrap();
        if curr.is_whitespace() {
            return Some(self.scan_whitespace());
        }
        let token = match curr {
            '"' => {
                self.advance_literal_or_lifetime('"', true, false);
                Token::StringLiteral(StringLiteralKind::Normal)
            },
            '\'' => {
                let is_lifetime = self.advance_literal_or_lifetime('\'', true, true);
                if is_lifetime { Token::Lifetime } else { Token::CharLiteral }
            },
            'b' => {
                self.r.advance();
                match self.r.peek() {
                    Some('\'') => {
                        self.advance_literal_or_lifetime('\'', false, false);
                        Token::ByteLiteral
                    },
                    Some('"') => {
                        self.advance_literal_or_lifetime('"', false, false);
                        Token::ByteStringLiteral(StringLiteralKind::Normal)
                    },
                    Some(_) | None => self.scan_ident_or_keyword(true)
                }
            },
            '=' => {
                self.r.advance();
                match self.r.peek() {
                    Some('>') => self.eat(Token::FatArrow),
                    Some('=') => self.eat(Token::EqEq),
                    _ => Token::Lt
                }
            },
            '<' => {
                self.r.advance();
                match self.r.peek() {
                    Some('=') => self.eat(Token::Le),
                    Some('-') => self.eat(Token::LeftArrow),
                    Some('<') => {
                        self.r.advance();
                        match self.r.peek() {
                            Some('=') => self.eat(Token::BinOpEq(BinOpKind::Shl)),
                            _ => Token::BinOp(BinOpKind::Shl)
                        }
                    }
                    _ => Token::Eq,
                }
            },
            '>' => {
                self.r.advance();
                match self.r.peek() {
                    Some('=') => self.eat(Token::Ge),
                    Some('>') => {
                        self.r.advance();
                        match self.r.peek() {
                            Some('=') => self.eat(Token::BinOpEq(BinOpKind::Shr)),
                            _ => Token::BinOp(BinOpKind::Shr)
                        }
                    }
                    _ => Token::Gt,
                }
            },
            '&' => {
                self.r.advance();
                match self.r.peek() {
                    Some('&') => self.eat(Token::AndAnd),
                    Some('=') => self.eat(Token::BinOpEq(BinOpKind::And)),
                    _ => Token::BinOp(BinOpKind::And)
                }
            },
            '|' => {
                self.r.advance();
                match self.r.peek() {
                    Some('|') => self.eat(Token::OrOr),
                    Some('=') => self.eat(Token::BinOpEq(BinOpKind::Or)),
                    _ => Token::BinOp(BinOpKind::Or)
                }
            },
            '!' => self.eat(Token::Not),
            '~' => self.eat(Token::Tilde),
            '+' => {
                self.r.advance();
                match self.r.peek() {
                    Some('=') =>  self.eat(Token::BinOpEq(BinOpKind::Plus)),
                    _ => Token::BinOp(BinOpKind::Plus)
                }
            },
            '-' => {
                self.r.advance();
                match self.r.peek() {
                    Some('>') => self.eat(Token::RightArrow),
                    Some('=') => self.eat(Token::BinOpEq(BinOpKind::Minus)),
                    _ => Token::BinOp(BinOpKind::Minus)
                }
            },
            '*' => {
                self.r.advance();
                match self.r.peek() {
                    Some('=') => self.eat(Token::BinOpEq(BinOpKind::Star)),
                    _ => Token::BinOp(BinOpKind::Star)
                }
            },
            '/' => {
                self.r.advance();
                match self.r.peek() {
                    Some('=') => self.eat(Token::BinOpEq(BinOpKind::Slash)),
                    Some('*') => {
                        self.r.advance();
                        match self.r.peek() {
                            Some('*') => self.scan_block_comment(true),
                            _ =>  self.scan_block_comment(false),
                        }
                    }
                    Some('/') => {
                        self.r.advance();
                        match self.r.peek() {
                            Some('/') => {
                                self.advance_single_line_comment();
                                Token::DocComment
                            }
                            _ =>  {
                                self.advance_single_line_comment();
                                Token::Comment
                            }
                        }
                    }
                    _ => Token::BinOp(BinOpKind::Slash)
                }
            },
            '%' => {
                self.r.advance();
                match self.r.peek() {
                    Some('=') => self.eat(Token::BinOpEq(BinOpKind::Percent)),
                    _ => Token::BinOp(BinOpKind::Percent)
                }
            },
            '^' => {
                self.r.advance();
                match self.r.peek() {
                    Some('=') => self.eat(Token::BinOpEq(BinOpKind::Caret)),
                    _ => Token::BinOp(BinOpKind::Caret)
                }
            },
            '@' => self.eat(Token::At),
            '.' => {
                self.r.advance();
                match self.r.peek() {
                    Some('.') => {
                        self.r.advance();
                        match self.r.peek() {
                            Some('.') => self.eat(Token::DotDotDot),
                            _ => Token::DotDot
                        }
                    }
                    _ => Token::Dot
                }
            }, 
            ',' => self.eat(Token::Comma),
            ';' => self.eat(Token::Semi),
            ':' => {
                self.r.advance();
                match self.r.peek() {
                    Some(':') => self.eat(Token::ModSep),
                    _ => Token::Colon
                }
            },
            '#' => self.eat(Token::Pound),
            '$' => self.eat(Token::Dollar),
            '?' => self.eat(Token::Question),
            '(' => self.eat(Token::LeftParen),
            '[' => self.eat(Token::LeftBracket),
            '{' => self.eat(Token::LeftBrace),
            ')' => self.eat(Token::RightParen),
            ']' => self.eat(Token::RightBracket),
            '}' => self.eat(Token::RightBrace),
            '_' => {
                self.r.advance();
                match self.r.peek() {
                    Some(x) if x.is_xid_continue() => self.scan_ident(),
                    _ => Token::Underscore
                }
            },
            '0' => {
                self.r.advance();
                match self.r.peek() {
                    None => Token::IntegerLiteral(IntegerLiteralKind::Decimal),
                    Some(c) => {
                        match c {
                            'x' => {
                                self.advance_digits('f', false);
                                Token::IntegerLiteral(IntegerLiteralKind::Hex)
                            }
                            'o' => {
                                self.advance_digits('7', false);
                                Token::IntegerLiteral(IntegerLiteralKind::Octal)
                            },
                            'b' => {
                                self.advance_digits('1', false);
                                Token::IntegerLiteral(IntegerLiteralKind::Binary)
                            },
                            '0'...'9' | '_' => {
                                self.advance_digits('9', false);
                                Token::IntegerLiteral(IntegerLiteralKind::Decimal)
                            },
                            _ => Token::IntegerLiteral(IntegerLiteralKind::Decimal)
                        }
                    }
                }
            },
            '1'...'9' => {
                self.advance_digits('9', true);
                Token::IntegerLiteral(IntegerLiteralKind::Decimal)
            },
            x if x.is_xid_start() => {
                self.scan_ident_or_keyword(false)
            },
            _ => unimplemented!()
        };
        return Some(token);
    }

    pub fn scan<'a>(&'a mut self) -> LexerIterator<'a, 'this, S>  {
        LexerIterator { lexer: self, seen_char: false }
    }
}

pub struct LexerIterator<'this, 'lexer: 'this, S:StringScanner> {
    lexer: &'this mut Lexer<'lexer, S>,
    seen_char: bool
}

impl<'this, 'lexer, S:StringScanner> Iterator for LexerIterator<'this, 'lexer, S> {
    type Item = (Token, Span);

    // Most of this code is proper error handling of 'a'b
    fn next(&mut self) -> Option<(Token, Span)> {
        let result = self.lexer.scan_token();
        if self.seen_char { 
            if let Some((Token::Ident, ident_span)) = result {
                self.lexer.on_error_at(LexingError::IllegalToken, ident_span.start);
                self.lexer.err_recovery = false;
            }
        }
        match result {
            Some((Token::CharLiteral, _)) => { self.seen_char = true; },
            _ => { self.seen_char = false; },
        }
        return result;
    }
}

#[allow(dead_code)]
fn fail_on_parse_error(_: &Lexer<SimpleStringScanner>, er: LexingError, i: u32) {
    panic!(format!("{:?}", (er,i)));
}

// Testing

#[allow(dead_code)]
fn prepare_lexer<'a>(ss: String) -> Lexer<'a, SimpleStringScanner> {
    let scanner = SimpleStringScanner::new(ss);
    Lexer::new(scanner, Some(Box::new(fail_on_parse_error)))
}

#[test]
fn lex_whitespace() {
    let text = "   \t \n \r";
    let mut lexer = prepare_lexer(text.to_string());
    let span_opt = lexer.scan_token();
    assert!(span_opt.is_some());
    let (token, span) = span_opt.unwrap();
    assert!(token == Token::Whitespace);
    assert!(span.start as usize == 0);
    assert!(span.end as usize == text.len());
    assert!(lexer.scan_token().is_none());
}


#[test]
fn lex_string() {
    let text = "\"adąęa\"";
    let mut lexer = prepare_lexer(text.to_string());
    let span_opt = lexer.scan_token();
    assert!(span_opt.is_some());
    assert!(lexer.scan_token().is_none());
    let (token, span) = span_opt.unwrap();
    assert!(if let Token::StringLiteral(StringLiteralKind::Normal) = token { true } else { false });
    assert!(span.start as usize == 0);
    assert!(span.end as usize == text.len());
    assert!(lexer.scan_token().is_none());
}

#[test]
fn fail_on_eof() {    
    let text = "\"abc";
    let scanner = SimpleStringScanner::new(text.to_string());
    let mut error = None;
    let span_opt = {
        let mut lexer = Lexer::new(scanner, Some(Box::new(|_, er, _| { error = Some(er); } )));
        let span_opt = lexer.scan_token();
        assert!(lexer.scan_token().is_none());
        span_opt
    };
    assert!(error.is_some());
    assert!(error.unwrap() == LexingError::Eof);
    assert!(span_opt.is_some());
    let (token, span) = span_opt.unwrap();
    assert!(if let Token::StringLiteral(StringLiteralKind::Normal) = token { true } else { false });
    assert!(span.start as usize == 0);
    assert!(span.end as usize == text.len());
}

#[test]
fn fail_on_non_ascii() {    
    let text = "b\"aębc\"";
    let scanner = SimpleStringScanner::new(text.to_string());
    let mut error = None;
    let mut err_idx = 0;
    let mut err_count = 0i32;
    let span_opt = {
        let mut lexer = Lexer::new(scanner, Some(Box::new(|&mut: _, er, idx| {
            error = Some(er);
            err_idx = idx;
            err_count += 1;
        })));
        let span_opt = lexer.scan_token();
        assert!(lexer.scan_token().is_none());
        span_opt
    };
    assert!(err_count == 1);
    assert!(error.is_some());
    assert!(error.unwrap() == LexingError::NonAsciiByte);
    assert!(err_idx == 3);
    assert!(span_opt.is_some());
    let (token, span) = span_opt.unwrap();
    assert!(if let Token::ByteStringLiteral(StringLiteralKind::Normal) = token { true } else { false });
    assert!(span.start as usize == 0);
    assert!(span.end as usize == text.len());
}

#[test]
fn hex_eofs() { 
    let text = ["'\\x", "'\\xf", "'\\xff"];
    let i = ::std::cell::Cell::new(3);
    for t in text.iter() {
        let scanner = SimpleStringScanner::new(t.to_string());
        let mut lexer = Lexer::new(scanner, Some(Box::new(|_, er, idx| {
            assert!((idx as usize) == i.get());
            assert!(er == LexingError::Eof);
        })));
        lexer.scan_token();
        i.set(i.get()+1);
    }
}