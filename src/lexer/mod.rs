use super::Span; 
use std::ascii::AsciiExt;
use self::token::*;
use unicode_xid::UnicodeXID;

pub mod token;

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
const MAX_INT_SUFFIX_LENGTH: usize = 5;

#[derive(PartialEq, Eq, Copy, Debug, Clone, Hash)]
pub enum LexingError {
    Eof, /// we were expecting something but the file ended
    IllegalToken, // special error for 'a'b
    NonAsciiByte, // special error for bytes with unicode bytes, eg b'ę'
    InvalidEscapeSeq, // for semantically wrong seqs like '\xff'
    IllegalEscapeSeq, // for '\u8' inside byte literals
    MalformedEscapeSeq, // we started to lex escape seq but it is broken in some syntactic way, eg. '\u{}' or '\x0'
    TokenTooLong, // for char/byte literals that exceed our expectations, eg. 'ab'
    TokenTooShort, // for char/byte literals that have the form ''
    UnescapedLiteral, // byte and char literals don't allow unescaped ', \t, \n, \r
    MalformedLiteral, // for things like `0b ` or `0b9`
    IllegalSuffix, // for illegal integer suffixes: things like `1i16usize`
    MalformedExponent, // for broken exponent in floats: eg. `12e` or `12E!1`
    MissingQuote, // for raw literals that miss their quotation marks: eg. r###asd 
    UnbalancedRawLiteral, // for raw literals with wrong number of hash sings: eg. r###"asdf"#
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

    fn char_at(s: &str, i: usize) -> char {
        s[i..].chars().next().unwrap()
    }

    // this fn is mostly for debugging
    #[allow(deprecated)]
    pub fn slice_at(&self, sp: Span) -> &str {
        &self.s[sp.start as usize..sp.end as usize]
    }

    fn is_eof(&self, idx: u32) -> bool { (idx as usize) >= self.s.len() }
    fn peek_to(&self, idx: u32) -> Option<char> {
        if self.is_eof(idx) {
            None
        }
        else {
            Some(SimpleStringScanner::char_at(&*self.s, idx as usize))
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
            match SimpleStringScanner::char_at(&*self.s, self.idx as usize) as u32 {
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
pub struct Lexer<'a, S:StringScanner> {
    r: S,
    err_fn: Option<Box<for<'b> FnMut(&'b Lexer<S>, LexingError, u32)+'a>>,
    err_recovery: bool, // set to true during first lexing error in a token
    /*
     * Lexing float literal tokens require two chars lookahead
     * eg. when lexing `12...13` we start by entering lexing rule for integer literals
     * and after consuming the dot we have to abandon it and on the next call to the iterator
     * restart from this abandoned dot.
     * We could solve this by forcing StringScanner to provide us with two-tokens lookahead but I
     * want StringScanner to be as simple as possible (it is implemented by the user)
     * at the cost of some messiness in the Lexer.
    */
    abandoned_char: Option<(char, u32)>,
}

// Actual lexing
impl<'a, S:StringScanner> Lexer<'a, S> {
    pub fn new(scanner: S, err_handler:Option<Box<FnMut(&Lexer<S>, LexingError, u32)+'a>>) -> Lexer<'a, S> {
        Lexer { r:scanner, err_fn: err_handler, err_recovery: false, abandoned_char: None }
    }

    pub fn scanner(&self) -> &S {
        &self.r
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
        "self" => Some(KeywordKind::LowerSelf),
        "Self" => Some(KeywordKind::UpperSelf),
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

fn can_be_token_start(c: char) -> bool {
    match c {
        c if c.is_whitespace() => true,
        '"' | '\'' | 'b' | '='
        | '<' | '>' | '&' | '|'
        | '!' | '~' | '+' | '-'
        | '*' | '/' | '%' | '^'
        | '@' | '.' | ',' | ';'
        | ':' | '#' | '$' | '?'
        | '(' | '[' | '{' | ')'
        | ']' | '}' | '_' => true,
        '0'...'9' => true,
        c if UnicodeXID::is_xid_start(c) => true,
        _ => false
    }
}

fn is_single_char_newline(c: char) -> bool {
    c == '\n' || c == '\u{0085}' || c == '\u{2028}' || c == '\u{2029}'
}

impl<'a, S:StringScanner> Lexer<'a, S> {
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
        // HACK ALERT! HACK ALERT! HACK ALERT! HACK ALERT!
        // Borrow checker might disagree, but passing &self to a field that is FnMut is safe.
        let this : &Lexer<S> = unsafe { &*(self as *mut _) };
        if let Some(ref mut callback) = self.err_fn {
            callback(this, err, pos)
        }
    }

    fn advance_long_newline(&mut self, c: char) -> bool {
        if c == '\r' {
            self.advance_abandoned();
            if self.r.peek() == Some('\n') {
                return true;
            }
        }
        false
    }

    fn scan_whitespace(&mut self, c: char) -> (Token, u32) {
        if is_single_char_newline(c) {
            self.r.advance();
            return (Token::Newline, self.r.current_position());
        }
        else if self.advance_long_newline(c) {
            self.r.advance();
            return (Token::Newline, self.r.current_position());
        }
        else {            
            self.r.advance();
        }
        loop {
            let last = self.r.current_position();
            match self.r.peek() {
                Some(c) => {
                    if !c.is_whitespace() || is_single_char_newline(c) {
                        break;
                    }
                    let curr = self.r.current_position();
                    if self.advance_long_newline(c) {
                        self.abandon_char('\r', last);
                        return (Token::Whitespace, curr)
                    }
                    else if c == '\r' {
                        continue;
                    }
                }
                None => { break; }
            }
            self.r.advance();
        }
        (Token::Whitespace, self.r.current_position())
    }

    fn advance_literal_or_lifetime(&mut self, c_end: char, allow_unicode: bool, mut look_for_lifetime: bool) -> bool {
        debug_assert!(self.r.peek() == Some(c_end));
        let start = self.r.current_position();
        self.r.advance();
        let mut first_error = None;
        let mut length = 0usize;
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
                        c if !UnicodeXID::is_xid_continue(c) => {
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
        for i in 0..2 {
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
        for i in 0..6 {
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
                Some(x) if !UnicodeXID::is_xid_continue(x) => break,
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
    fn scan_ident_or_keyword(&mut self, prepend: &'static str) -> Token {
        let mut back_buffer = [0u8; MAX_KEYWORD_LENGTH]; // all keywords are ASCII
        for (idx, b) in prepend.bytes().enumerate() {
            back_buffer[idx] = b;
        }
        let mut length = prepend.len();
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

    fn advance_float_from_dot(&mut self, float_start: u32) -> (Token, u32) {
        let prev = self.r.current_position();
        self.r.advance();
        match self.r.peek() {
            None => return (Token::FloatLiteral(FloatLiteralSuffix::None), self.r.current_position()),
            Some(c) => {
                match c {
                    '0'...'9' => {
                        let suffix = self.advance_float_from_fractional();
                        return (Token::FloatLiteral(suffix), self.r.current_position());
                    }
                    c if UnicodeXID::is_xid_start(c) || c == '.' => { // field access, <Dot> or <DotDot>, abandon and rewind
                        self.abandon_char('.', prev);
                        return (Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::None), float_start);
                    },
                    _ => return (Token::FloatLiteral(FloatLiteralSuffix::None), self.r.current_position()),
                }
            }
        }
    }

    fn advance_float_from_fractional(&mut self) -> FloatLiteralSuffix {
        debug_assert!(
            self.r.peek() == Some('0')
            || self.r.peek() == Some('1')
            || self.r.peek() == Some('2')
            || self.r.peek() == Some('3')
            || self.r.peek() == Some('4')
            || self.r.peek() == Some('5')
            || self.r.peek() == Some('6')
            || self.r.peek() == Some('7')
            || self.r.peek() == Some('8')
            || self.r.peek() == Some('9')
        );
        loop {
            self.r.advance();
            match self.r.peek() {
                None => return FloatLiteralSuffix::None,
                Some(c) => { 
                    match c {
                        '0'...'9' | '_' => { },
                        _ => break
                    }
                }
            }
        }
        match self.r.peek().unwrap() {
            'e' | 'E' => self.advance_float_from_exponent(),
            c if UnicodeXID::is_xid_continue(c) => self.advance_float_suffix(),
            _ => return FloatLiteralSuffix::None,
        }
    }

    fn advance_float_from_exponent(&mut self) -> FloatLiteralSuffix {
        debug_assert!(self.r.peek() == Some('e') || self.r.peek() == Some('E'));
        self.r.advance();
        match self.r.peek() {
            Some(c) if c == '-' || c == '+' => self.r.advance(),
            None => self.on_error(LexingError::MalformedExponent),
            _ => {}
        }
        let mut first = true;
        loop {
            match self.r.peek() {
                Some(c) if (c >= '0' && c <= '9') || (!first && c == '_') => { },
                Some(c) if UnicodeXID::is_xid_start(c) => break,
                _ => return FloatLiteralSuffix::None,
            }
            if first { first = false };
            self.r.advance();
        }
        self.advance_float_suffix()
    }

    fn advance_float_suffix(&mut self) -> FloatLiteralSuffix {
        let suffix_start = self.r.current_position();
        let token = self.advance_float_or_decimal_suffix(suffix_start);
        if let Token::FloatLiteral(suffix) = token {
            suffix
        }
        else {
            self.on_error_at(LexingError::IllegalSuffix, suffix_start);
            FloatLiteralSuffix::None
        }
    }

    // no dot encountered, check suffix to disambiguate between int and float tokens
    fn advance_float_or_decimal_suffix(&mut self, suffix_start: u32) -> Token {
        debug_assert!(UnicodeXID::is_xid_start(self.r.peek().unwrap()));
        let mut length = 1;
        let mut back_buffer = [0u8; MAX_INT_SUFFIX_LENGTH]; // suffixes are ASCII
        {
            let c = self.r.peek().unwrap();
            if !c.is_ascii() { length = MAX_INT_SUFFIX_LENGTH + 1; }
            else { back_buffer[0] = c as u8; }
        }
        loop {
            self.r.advance();
            match self.r.peek() {
                Some(c) if UnicodeXID::is_xid_continue(c) => {
                    if c.is_ascii() && length < MAX_INT_SUFFIX_LENGTH {
                        back_buffer[length] = c as u8;
                        length += 1;
                    }
                    else {
                        length = MAX_INT_SUFFIX_LENGTH + 1;
                    }
                }
                _ => {
                    break;
                }
            }
        }
        if length > MAX_INT_SUFFIX_LENGTH {
            self.on_error_at(LexingError::IllegalSuffix, suffix_start);
            Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::None)
        }
        else {
            let suffix_text = unsafe { ::std::str::from_utf8_unchecked(&back_buffer[0..length]) }; // safe, we've checked earlier that all u8s are ascii
            self.scan_float_or_int_suffix(suffix_text)
        }
    }

    fn scan_float_or_int_suffix(&mut self, ident: &str) -> Token {
        match ident {
            "isize" => Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::Isize),
            "usize" => Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::Usize),
            "u8" => Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::U8),
            "i8" => Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::I8),
            "u16" => Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::U16),
            "i16" => Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::I16),
            "u32" => Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::U32),
            "i32" => Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::I32),
            "u64" => Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::U64),
            "i64" => Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::I64),
            "f32" => Token::FloatLiteral(FloatLiteralSuffix::F32),
            "f64" => Token::FloatLiteral(FloatLiteralSuffix::F64),
            "" => panic!(),
            _ => {
                self.on_error(LexingError::IllegalSuffix);
                Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::None)
            }
        }
    }

    fn scan_float_or_decimal_integer(&mut self) -> (Token, u32) {
        debug_assert!(
            self.r.peek() == Some('0')
            || self.r.peek() == Some('1')
            || self.r.peek() == Some('2')
            || self.r.peek() == Some('3')
            || self.r.peek() == Some('4')
            || self.r.peek() == Some('5')
            || self.r.peek() == Some('6')
            || self.r.peek() == Some('7')
            || self.r.peek() == Some('8')
            || self.r.peek() == Some('9')
        );
        loop {
            self.r.advance();
            match self.r.peek() {
                Some(c) if c == '_' || (c >= '0' && c <= '9') => continue,
                Some(_) => break,
                None => return (Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::None), self.r.current_position()),
            }
        }
        let float_start = self.r.current_position(); // point where decimal literal ended
        match self.r.peek().unwrap() {
            '.' => self.advance_float_from_dot(float_start),
            'e' | 'E' => {
                let suffix = self.advance_float_from_exponent();
                return (Token::FloatLiteral(suffix), self.r.current_position());
            },
            c if UnicodeXID::is_xid_start(c) => {
                let token = self.advance_float_or_decimal_suffix(float_start);
                return (token, self.r.current_position());
            }
            _ => return (Token::FloatLiteral(FloatLiteralSuffix::None), self.r.current_position())
        }
    }

    fn advance_digits(&mut self, max_digit: char, mut allow_zero_length: bool) -> IntegerLiteralSuffix {
        loop {
            self.r.advance();
            match self.r.peek() {
                Some(c) if c == '_' || (c >= '0' && c <= '9' && c <= max_digit) || (c >= 'a' && c <= max_digit) => {
                    if !allow_zero_length { allow_zero_length = true }
                },
                _ => {
                    if !allow_zero_length {
                        self.on_error(LexingError::MalformedLiteral);
                        return IntegerLiteralSuffix::None;
                    }
                    break;
                }
            }
        }
        let suffix_start = self.r.current_position();
        match self.r.peek() {
            Some(c) if UnicodeXID::is_xid_start(c) =>{
                if let Token::IntegerLiteral(_, suffix) = self.advance_float_or_decimal_suffix(suffix_start) {
                    suffix
                }
                else {
                    self.on_error_at(LexingError::IllegalSuffix, suffix_start);
                    IntegerLiteralSuffix::None
                }
            }
            _ => IntegerLiteralSuffix::None
        }
    }

    fn eat(&mut self, t: Token) -> Token {
        self.r.advance();
        t
    }

    fn abandon_char(&mut self, c: char, pos: u32) {
        debug_assert!(self.abandoned_char.is_none());
        self.abandoned_char = Some((c, pos));
    }

    fn peek_abandoned(&mut self) -> Option<char> {
        if let Some((c,_)) = self.abandoned_char { Some(c) }
        else { self.r.peek() }
    }

    fn advance_abandoned(&mut self) {
        if self.abandoned_char.is_some() { self.abandoned_char = None }
        else { self.r.advance() }
    }

    fn scan_token(&mut self) -> Option<(Token, Span)> {
        let start = self.abandoned_char.map(|(_, pos)| pos).unwrap_or_else(|| self.r.current_position());
        let result = self.scan_token_inner();
        self.err_recovery = false;
        return result.map(|(token, end)| (token, Span { start: start, end: end }));
    }

    fn scan_raw_literal(&mut self) {
        let mut hash_count = 0;
        loop {
            if self.r.peek() != Some('#') { break; }
            self.r.advance();
            hash_count += 1;
        }
        match self.r.peek() {
            Some(c) if c != '"' => {
                return self.on_error(LexingError::MissingQuote);
            }
            None => {
                return self.on_error(LexingError::Eof);
            }
            Some(_) => { }
        }
        self.advance_literal_or_lifetime('"', true, false);
        loop {
            if self.r.peek() != Some('#') { break; }
            self.r.advance();
            hash_count -= 1;
        }
        if hash_count != 0 {
            self.on_error(LexingError::UnbalancedRawLiteral);
        }
    }

    fn scan_token_inner(&mut self) -> Option<(Token, u32)> {
        let curr = self.peek_abandoned();
        if curr.is_none() {
            return None;
        }
        let curr = curr.unwrap();
        if curr.is_whitespace() {
            return Some(self.scan_whitespace(curr));
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
            '=' => {
                self.r.advance();
                match self.r.peek() {
                    Some('>') => self.eat(Token::FatArrow),
                    Some('=') => self.eat(Token::EqEq),
                    _ => Token::Eq
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
                    _ => Token::Lt,
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
                self.advance_abandoned();
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
                    Some(x) if UnicodeXID::is_xid_continue(x) => self.scan_ident(),
                    _ => Token::Underscore
                }
            },
            '0' => {
                let pre_dot_position = self.r.current_position();
                self.r.advance();
                match self.r.peek() {
                    None => Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::None),
                    Some(c) => {
                        match c {
                            'x' => Token::IntegerLiteral(IntegerLiteralBase::Hex, self.advance_digits('f', false)),
                            'o' => {
                                Token::IntegerLiteral(IntegerLiteralBase::Octal, self.advance_digits('7', false))
                            },
                            'b' => {
                                Token::IntegerLiteral(IntegerLiteralBase::Binary, self.advance_digits('1', false))
                            },
                            '0'...'9' | '_' => return Some(self.scan_float_or_decimal_integer()),
                            '.' => return Some(self.advance_float_from_dot(pre_dot_position)),
                            'e' | 'E' => {
                                let suffix = self.advance_float_from_exponent();
                                Token::FloatLiteral(suffix)
                            },
                            _ =>Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::None)
                        }
                    }
                }
            },
            '1'...'9' => return Some(self.scan_float_or_decimal_integer()),
            'b' => {
                self.r.advance();
                match self.r.peek() {
                    Some('r') => {
                        self.r.advance();
                        match self.r.peek() {
                            Some('#') | Some('"') => {
                                self.scan_raw_literal();
                                Token::ByteStringLiteral(StringLiteralKind::Raw)
                            }
                            _ => self.scan_ident_or_keyword("br")
                        }
                    }
                    Some('\'') => {
                        self.advance_literal_or_lifetime('\'', false, false);
                        Token::ByteLiteral
                    },
                    Some('"') => {
                        self.advance_literal_or_lifetime('"', false, false);
                        Token::ByteStringLiteral(StringLiteralKind::Normal)
                    },
                    Some(_) | None => self.scan_ident_or_keyword("b")
                }
            },
            'r' => {
                self.r.advance();
                match self.r.peek() {
                    Some('#') | Some('"') => {
                        self.scan_raw_literal();
                        Token::StringLiteral(StringLiteralKind::Raw)
                    }
                    _ => self.scan_ident_or_keyword("")
                }
            },
            x if UnicodeXID::is_xid_start(x) => {
                self.scan_ident_or_keyword("")
            },
            _ => {
                loop {
                    self.r.advance();
                    match self.r.peek() {
                        None => break,
                        Some(c) if can_be_token_start(c) => break,
                        _ => { }
                    }
                 }
                 Token::UnexpectedSequence
            }
        };
        Some((token, self.r.current_position()))
    }

    pub fn scan<'lexer>(&'lexer mut self) -> LexerIterator<'lexer, 'a, S>  {
        LexerIterator { lexer: self, seen_char: false }
    }
}

pub struct LexerIterator<'lexer, 'e:'lexer, S:StringScanner+'lexer> {
    lexer: &'lexer mut Lexer<'e, S>,
    seen_char: bool
}

impl<'lexer, '_, S:StringScanner> LexerIterator<'lexer, '_, S> {
    fn raise_error_on_illegal_token(&mut self, result: Option<(Token, Span)>) {
        if self.seen_char { 
            if let Some((Token::Ident, ident_span)) = result {
                self.lexer.on_error_at(LexingError::IllegalToken, ident_span.start);
                self.lexer.err_recovery = false;
            }
        }
        match result {
            Some((Token::CharLiteral, _)) => {
                self.seen_char = true;
            },
            _ => {
                self.seen_char = false;
            },
        }
    }

    fn drop_state(&mut self) {
        self.lexer.abandoned_char = None;
    }
}

impl<'lexer, '_, S:StringScanner> Iterator for LexerIterator<'lexer, '_, S> {
    type Item = (Token, Span);

    // Most of this code is proper error handling of 'a'b
    fn next(&mut self) -> Option<(Token, Span)> {
        let result = self.lexer.scan_token();
        self.raise_error_on_illegal_token(result);
        if result.is_none() { self.drop_state() };
        return result;
    }
}

#[cfg(test)]
mod test {
    use super::{SimpleStringScanner, Lexer, LexingError};
    use super::token::{Token, StringLiteralKind};

    #[allow(dead_code)]
    fn fail_on_parse_error(_: &Lexer<SimpleStringScanner>, er: LexingError, i: u32) {
        panic!(format!("{:?}", (er,i)));
    }

    #[allow(dead_code)]
    fn prepare_lexer<'a>(ss: String) -> Lexer<'a, SimpleStringScanner> {
        let scanner = SimpleStringScanner::new(ss);
        Lexer::new(scanner, Some(Box::new(fail_on_parse_error)))
    }

    #[test]
    fn lex_whitespace() {
        let text = "   \t \r \r";
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
        let (mut error, span_opt) : (Option<_>, _);
        error = None;
        span_opt = {
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
            let mut lexer = Lexer::new(scanner, Some(Box::new(|_, er, idx| {
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
}