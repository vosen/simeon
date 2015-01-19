use super::Span; 
use super::token::{Token, LiteralKind, BinOpKind};

/*
 * A word about our error strategy.
 * Errors thrown during tokenization can be split into two categories:
 * Syntactical and semantical. Semantical errors are
 * # InvalidEscapeSeq:
 *   Raises for syntactically correct escape sequence that covers wrong range.
 *   For example, \xff raises this error.
 *   This error could be en UnexpectedChar in most cases, but It's nicer for the user this way.
 * # IllegalToken:
 *   Special treatment for 'a'b
 * They are raised at the start of the token.
 * All other errors are syntactic
 */


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

#[derive(PartialEq, Eq, Copy, Show, Clone, Hash)]
pub enum LexingError {
    Eof, // syntax
    UnexpectedChar, // syntax
    IllegalToken, // semantics
    UnterminatedLiteral, // syntax
    NonAsciiByte, // syntax
    InvalidEscapeSeq // semantics
}

pub struct SimpleStringScanner {
    idx: u32,
    s: String,
}

impl SimpleStringScanner {
    pub fn new(s: String) -> SimpleStringScanner {
        if(::std::mem::size_of::<usize>() < 4 || s.len() > (::std::u32::MAX as usize)) {
            panic!();
        }
        SimpleStringScanner {
            idx: 0,
            s: s
        }
    }

    // this fn is mostly for debuggign
    pub fn slice_at(&self, sp: Span) -> &str {
        self.s.slice(sp.start as usize, sp.end as usize)
    }

    fn is_eof(&self, idx: u32) -> bool { (idx as usize) >= self.s.len() }
    fn peek_to(&self, idx: u32) -> Option<char> {
        if(self.is_eof(idx)) {
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
        if(self.is_eof(self.idx)) {
            panic!("Can't advance beyond the scanned string");
        }
        else {
            match (self.s.char_at(self.idx as usize) as u32) {
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

fn is_ascii(c: char) -> bool {
    match c {
        '\x00'...'\x7f' => true,
        _ => false
    }
}

impl<'this, S:StringScanner> Lexer<'this, S> {
    fn on_error(&mut self, err: LexingError) {
        let mut curr_pos = self.r.current_position();
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

    fn advance_single(&mut self) -> u32 {
        self.r.advance();
        return self.r.current_position();
    }

    fn advance_whitespace(&mut self) -> Span {
        let start = self.r.current_position();
        loop {
            self.r.advance();
            match self.r.peek() {
                Some(c) => if !c.is_whitespace() { break; },
                None => { break; }
            }
        }
        return Span { start: start, end: self.r.current_position() };
    }


    fn advance_until_end_of_string(&mut self, c_end: char, accept_unicode: bool) {
        loop {
            let c_opt = self.r.peek();
            match c_opt {
                None => {
                    self.on_error(LexingError::Eof);
                    break;
                },
                Some(x) if x == c_end => {
                    self.r.advance();
                    break;
                }
                Some('\\') => {
                    loop { // This is kinda ugly but I don't want stack overflows on \\\\\\\\\\\\\\\\\\\\\\\\\\\
                        match self.r.next() {
                            Some('\\') => continue,
                            Some(x) => {
                                if(!accept_unicode && !is_ascii(x)) {
                                    self.on_error(LexingError::UnexpectedChar);
                                }
                                break;
                            },
                            None  => break,
                        }
                    }
                },
                Some(x) =>{                    
                    if(!accept_unicode && !is_ascii(x)) {
                        self.on_error(LexingError::UnexpectedChar);
                    }
                    self.r.advance();
                }
            }
        }
    }

    fn advance_literal(&mut self, c_end: char, accept_unicode: bool) -> Span {
        let start = self.r.current_position();
        self.r.advance();
        self.advance_until_end_of_string(c_end, accept_unicode);
        return Span { start: start, end: self.r.current_position() }
    }

    // returns if looks like a char, false if looks like a lifetime
    fn error_and_recover_to_char(&mut self, can_be_lifetime: bool) -> bool {
        let xid_continue_start = self.r.current_position();
        loop {
            match self.r.peek() {
                Some('\'') => {
                    self.on_error_at(LexingError::UnexpectedChar, xid_continue_start);
                    self.r.advance();
                    return true;
                }
                Some(c) if c.is_xid_continue() => self.r.advance(),
                Some(_) => return false,
                None => {
                    if !can_be_lifetime {
                        self.on_error(LexingError::Eof);
                    }
                    return false;
                }
            }
        }
    }

    fn check_unicode(&mut self, c: char) {
        if c > '\x7f' {
            self.on_error(LexingError::NonAsciiByte);
        }
    }

    fn advance_byte_literal_or_lifetime(&mut self, unicode: bool) -> (Token, Span) {
        debug_assert!(self.r.peek() == Some('\''));
        let start = self.r.current_position();
        self.r.advance();
        match self.r.peek() {
            Some('\\') => (Token::CharLiteral, Span { start: start, end: self.advance_escape_seq(start, unicode) }), // byte mode
            None => { // I'm tempted to do if ::std::rand::random::<bool>() { Token::Lifetime } else { Token::CharLiteral }
                self.on_error(LexingError::Eof);
                return (Token::CharLiteral, Span {start: start, end: self.r.current_position()});
            },
            Some(c) if !c.is_xid_continue()  => { // we are char, consume until '
                self.r.advance();
                self.advance_until_end_of_string('\'', true);
                return (Token::CharLiteral, Span {start: start, end: self.r.current_position()})
            }
            Some(_) => { // we are <'><XID_CONTINUE>, check the next char
                self.r.advance();
                match self.r.peek() {
                    Some('\'') => (Token::CharLiteral, Span {start: start, end: self.advance_single()}),
                    _ =>  {
                        let token = if self.error_and_recover_to_char(true) { Token::CharLiteral } else { Token::Lifetime };
                        return (token, Span {start: start, end: self.r.current_position()})
                    } 
                }
            }
        }
    }

    // stairs of doom
    fn advance_hex_digits(&mut self, start: u32) {
        debug_assert!(self.r.peek() == Some('x'));
        self.r.advance();
        let mut accum_int = 0;
        match self.r.peek() {
            Some(c1) => {
                self.r.advance();
                match c1.to_digit(16) {
                    Some(first_digit) => {
                        match self.r.peek() {
                            Some(c2) => {
                                self.r.advance();
                                match c2.to_digit(16) {
                                    Some(second_digit) => {
                                        if(self.r.peek() == Some('\'')) {
                                            self.r.advance();
                                            if (first_digit * 16) + second_digit > 0x7F {
                                                self.on_error_at(LexingError::InvalidEscapeSeq, start);
                                            }
                                        }
                                        else {
                                            if !self.error_and_recover_to_char(false) {
                                                self.on_error(LexingError::UnterminatedLiteral)
                                            }
                                        }
                                    },
                                    // second char is not a hex digit
                                    None => {
                                        if(c2 == '\'') {
                                            self.on_error_at(LexingError::InvalidEscapeSeq, start)
                                        }
                                        if !self.error_and_recover_to_char(false) {
                                            self.on_error(LexingError::UnterminatedLiteral)
                                        }
                                    }
                                }
                            }
                            // eof
                            None => {
                                if !self.error_and_recover_to_char(false) {
                                    self.on_error(LexingError::UnterminatedLiteral)
                                }
                            }
                        }
                    },
                    // first char is not a hex digit
                    None => {
                        if !self.error_and_recover_to_char(false) {
                            self.on_error(LexingError::UnterminatedLiteral)
                        }
                    }
                }
            },
            // eof
            None => {
                if !self.error_and_recover_to_char(false) {
                    self.on_error(LexingError::UnterminatedLiteral)
                }
            }
        }
    }

    /*
     * Reference documentation is incomplete about accepted byte escape sequences
     * accepted simple esc seq are \n, \r, \t, \\, \', \", \0
     * new unicode escape seq are \u{0} to \u{10ffff} (any number of digits, no spaces, inclusive range D800 to DFFF is invalid)
     * Possible errors:
     * unknown escape seq (eg. '\a'): UnexpectedChar
     * wrong escape seq (eg. '\u{}'): UnexpectedChar
     * too long (eg. 'abc'): UnexpectedChar
     * unterminated seq (spotting whitespace before `'`): UnterminatedLiteral
     * and eofs
     */
    fn advance_escape_seq(&mut self, token_start: u32, unicode: bool) -> u32 {
        debug_assert!(self.r.peek() == Some('\\'));
        self.r.advance();
        match self.r.peek() {
            None => {
                self.on_error(LexingError::Eof);
                return self.r.current_position();
            },
            Some(esc_mark) => {
                match esc_mark {
                    'n' | 'r' | 't' | '\\' | '\'' | '"' | '0' => self.advance_single(),
                    'x' => {
                        self.advance_hex_digits(token_start);
                        return self.r.current_position();
                    }
                    _ => unimplemented!()
                }
            }
        }
    }

    fn advance_ident(&mut self) -> u32 {
        loop {
            match self.r.peek() {
                Some(x) if !x.is_xid_continue() => break,
                None => break,
                _ => { self.r.advance(); }
            }
        }
        self.r.current_position()
    }

    fn advance_token(&mut self) -> Option<(Token, Span)> {
        let result = self.advance_token_inner();
        self.err_recovery = false;
        return result;
    }

    fn advance_token_inner(&mut self) -> Option<(Token, Span)> {
        let curr = self.r.peek();
        if curr.is_none() {
            return None;
        }
        let curr = curr.unwrap();
        if curr.is_whitespace() {
            return Some((Token::Whitespace, self.advance_whitespace()));
        }
        let token_start = self.r.current_position();
        match curr {
            '"' => return Some((Token::StringLiteral(LiteralKind::Normal), self.advance_literal('"', true))),
            '\'' => return Some(self.advance_byte_literal_or_lifetime(true)),
            'b' => {
                self.r.advance();
                match self.r.peek() {
                    Some('\'') => {
                        let quoted_span = self.advance_literal('\'', false);
                        return Some((Token::ByteLiteral, Span { start: token_start, end: quoted_span.end } ));
                    },
                    Some('"') => {
                        let quoted_span = self.advance_literal('"', false);
                        return Some((Token::ByteStringLiteral(LiteralKind::Normal), Span { start: token_start, end: quoted_span.end } ));
                    },
                    Some(_) | None => { }
                }
            },
            '=' => {
                let first_span_end = self.advance_single();
                match self.r.peek() {
                    Some('>') => return Some((Token::FatArrow, Span { start:token_start, end: self.advance_single() })),
                    Some('=') => return Some((Token::EqEq, Span { start:token_start, end: self.advance_single() })),
                    _ => return Some((Token::Lt, Span { start:token_start, end: first_span_end }))
                }
            },
            '<' => {
                let mut span_end = self.advance_single();
                match self.r.peek() {
                    Some('=') => return Some((Token::Le, Span { start:token_start, end: self.advance_single() })),
                    Some('-') => return Some((Token::LeftArrow, Span { start:token_start, end: self.advance_single() })),
                    Some('<') => {
                        span_end = self.advance_single();
                        match self.r.peek() {
                            Some('=') => return Some((Token::BinOpEq(BinOpKind::Shl), Span { start:token_start, end: self.advance_single()})),
                            _ => return Some((Token::BinOp(BinOpKind::Shl), Span { start:token_start, end: span_end})),
                        }
                    }
                    _ => return Some((Token::Eq, Span { start:token_start, end: span_end }))
                }
            },
            '>' => {
                let mut span_end = self.advance_single();
                match self.r.peek() {
                    Some('=') => return Some((Token::Ge, Span { start:token_start, end: self.advance_single() })),
                    Some('>') => {
                        span_end = self.advance_single();
                        match self.r.peek() {
                            Some('=') => return Some((Token::BinOpEq(BinOpKind::Shr), Span { start:token_start, end: self.advance_single()})),
                            _ => return Some((Token::BinOp(BinOpKind::Shr), Span { start:token_start, end: span_end})),
                        }
                    }
                    _ => return Some((Token::Gt, Span { start:token_start, end: span_end }))
                }
            },
            '&' => {
                let first_span_end = self.advance_single();
                match self.r.peek() {
                    Some('&') => return Some((Token::AndAnd, Span { start:token_start, end: self.advance_single() })),
                    Some('=') => return Some((Token::BinOpEq(BinOpKind::And), Span { start:token_start, end: self.advance_single() })),
                    _ => return Some((Token::BinOp(BinOpKind::And), Span { start:token_start, end: first_span_end }))
                }
            },
            '|' => {
                let first_span_end = self.advance_single();
                match self.r.peek() {
                    Some('|') => return Some((Token::OrOr, Span { start:token_start, end: self.advance_single() })),
                    Some('=') => return Some((Token::BinOpEq(BinOpKind::Or), Span { start:token_start, end: self.advance_single() })),
                    _ => return Some((Token::BinOp(BinOpKind::Or), Span { start:token_start, end: first_span_end }))
                }
            },
            '!' => return Some((Token::Not, Span { start:token_start, end: self.advance_single() })),
            '~' => return Some((Token::Tilde, Span { start:token_start, end: self.advance_single() })),
            '+' => {
                let first_span_end = self.advance_single();
                match self.r.peek() {
                    Some('=') => return Some((Token::BinOpEq(BinOpKind::Plus), Span { start:token_start, end: self.advance_single() })),
                    _ => return Some((Token::BinOp(BinOpKind::Plus), Span { start:token_start, end: first_span_end })),
                }
            },
            '-' => {
                let first_span_end = self.advance_single();
                match self.r.peek() {
                    Some('>') => return Some((Token::RightArrow, Span { start:token_start, end: self.advance_single() })),
                    Some('=') => return Some((Token::BinOpEq(BinOpKind::Minus), Span { start:token_start, end: self.advance_single() })),
                    _ => return Some((Token::BinOp(BinOpKind::Minus), Span { start:token_start, end: first_span_end })),
                }
            },
            '*' => {
                let first_span_end = self.advance_single();
                match self.r.peek() {
                    Some('=') => return Some((Token::BinOpEq(BinOpKind::Star), Span { start:token_start, end: self.advance_single() })),
                    _ => return Some((Token::BinOp(BinOpKind::Star), Span { start:token_start, end: first_span_end })),
                }
            },
            '/' => {
                let first_span_end = self.advance_single();
                match self.r.peek() {
                    Some('=') => return Some((Token::BinOpEq(BinOpKind::Slash), Span { start:token_start, end: self.advance_single() })),
                    _ => return Some((Token::BinOp(BinOpKind::Slash), Span { start:token_start, end: first_span_end })),
                }
            },
            '%' => {
                let first_span_end = self.advance_single();
                match self.r.peek() {
                    Some('=') => return Some((Token::BinOpEq(BinOpKind::Percent), Span { start:token_start, end: self.advance_single() })),
                    _ => return Some((Token::BinOp(BinOpKind::Percent), Span { start:token_start, end: first_span_end })),
                }
            },
            '^' => {
                let first_span_end = self.advance_single();
                match self.r.peek() {
                    Some('=') => return Some((Token::BinOpEq(BinOpKind::Caret), Span { start:token_start, end: self.advance_single() })),
                    _ => return Some((Token::BinOp(BinOpKind::Caret), Span { start:token_start, end: first_span_end })),
                }
            },
            '@' => return Some((Token::At, Span { start:token_start, end: self.advance_single() })),
            '.' => {
                let mut span_end = self.advance_single();
                match self.r.peek() {
                    Some('.') => {
                        span_end = self.advance_single();
                        match self.r.peek() {
                            Some('.') => return Some((Token::DotDotDot, Span { start:token_start, end: self.advance_single()})),
                            _ => return Some((Token::DotDot, Span { start:token_start, end: span_end})),
                        }
                    }
                    _ => return Some((Token::Dot, Span { start:token_start, end: span_end }))
                }
            },
            ',' => return Some((Token::Comma, Span { start:token_start, end: self.advance_single() })),
            ';' => return Some((Token::Semi, Span { start:token_start, end: self.advance_single() })),
            ':' => {
                let first_span_end = self.advance_single();
                match self.r.peek() {
                    Some(':') => return Some((Token::ModSep, Span { start:token_start, end: self.advance_single() })),
                    _ => return Some((Token::Colon, Span { start:token_start, end: first_span_end })),
                }
            },
            '#' => return Some((Token::Pound, Span { start:token_start, end: self.advance_single() })),
            '$' => return Some((Token::Dollar, Span { start:token_start, end: self.advance_single() })),
            '?' => return Some((Token::Question, Span { start:token_start, end: self.advance_single() })),
            '(' => return Some((Token::LeftParen, Span { start:token_start, end: self.advance_single() })),
            '[' => return Some((Token::LeftBracket, Span { start:token_start, end: self.advance_single() })),
            '{' => return Some((Token::LeftBrace, Span { start:token_start, end: self.advance_single() })),
            ')' => return Some((Token::RightParen, Span { start:token_start, end: self.advance_single() })),
            ']' => return Some((Token::RightBracket, Span { start:token_start, end: self.advance_single() })),
            '}' => return Some((Token::RightBrace, Span { start:token_start, end: self.advance_single() })),
            '_' => {
                let first_span_end = self.advance_single();
                match self.r.peek() {
                    Some(x) if x.is_xid_continue() => {},
                    _ => return Some((Token::Underscore, Span { start:token_start, end: first_span_end })),
                }
            },
            x if x.is_xid_start() => {
                self.r.advance();
                return Some((Token::Ident, Span { start:token_start, end: self.advance_ident() }));
            },
            x =>  panic!(format!("Unexpected char {:}", x))
        };
        // We reach this point when failing to match literals
        // For example, upon seeing `b` we hope that we are parsing byte literal
        // Obviously, sometimes this fails and we are actually parsing idents
        return Some((Token::Ident, Span { start:token_start, end: self.advance_ident() }));
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
        let result = self.lexer.advance_token();
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

#[allow(unused_variables)]
fn fail_on_parse_error(l: &Lexer<SimpleStringScanner>, er: LexingError, i: u32) {
    assert!(false);
}

// Testing

fn prepare_lexer<'a>(ss: String) -> Lexer<'a, SimpleStringScanner> {
    let scanner = SimpleStringScanner::new(ss);
    Lexer::new(scanner, Some(box fail_on_parse_error))
}

#[test]
fn lex_whitespace() {
    let text = "   \t \n \r";
    let mut lexer = prepare_lexer(text.to_string());
    let span_opt = lexer.advance_token();
    assert!(span_opt.is_some());
    let (token, span) = span_opt.unwrap();
    assert!(token == Token::Whitespace);
    assert!(span.start as usize == 0);
    assert!(span.end as usize == text.len());
    assert!(lexer.advance_token().is_none());
}


#[test]
fn lex_string() {
    let text = "\"adąęa\"";
    let mut lexer = prepare_lexer(text.to_string());
    let span_opt = lexer.advance_token();
    assert!(span_opt.is_some());
    assert!(lexer.advance_token().is_none());
    let (token, span) = span_opt.unwrap();
    assert!(if let Token::StringLiteral(LiteralKind::Normal) = token { true } else { false });
    assert!(span.start as usize == 0);
    assert!(span.end as usize == text.len());
    assert!(lexer.advance_token().is_none());
}

#[test]
fn fail_on_eof() {    
    let text = "\"abc";
    let scanner = SimpleStringScanner::new(text.to_string());
    let mut error = None;
    let span_opt = {
        let mut lexer = Lexer::new(scanner, Some(box |_, er, _| { error = Some(er); } ));
        let span_opt = lexer.advance_token();
        assert!(lexer.advance_token().is_none());
        span_opt
    };
    assert!(error.is_some());
    assert!(error.unwrap() == LexingError::Eof);
    assert!(span_opt.is_some());
    let (token, span) = span_opt.unwrap();
    assert!(if let Token::StringLiteral(LiteralKind::Normal) = token { true } else { false });
    assert!(span.start as usize == 0);
    assert!(span.end as usize == text.len());
}

#[test]
fn fail_on_non_ascii() {    
    let text = "b\"aębc";
    let scanner = SimpleStringScanner::new(text.to_string());
    let mut error = None;
    let mut err_idx = 0;
    let mut err_count = 0i32;
    let span_opt = {
        let mut lexer = Lexer::new(scanner, Some(box |&mut: _, er, idx| {
            error = Some(er);
            err_idx = idx;
            err_count += 1;
        }));
        let span_opt = lexer.advance_token();
        assert!(lexer.advance_token().is_none());
        span_opt
    };
    assert!(err_count == 1);
    assert!(error.is_some());
    assert!(error.unwrap() == LexingError::UnexpectedChar);
    assert!(err_idx == 3);
    assert!(span_opt.is_some());
    let (token, span) = span_opt.unwrap();
    assert!(if let Token::ByteStringLiteral(LiteralKind::Normal) = token { true } else { false });
    assert!(span.start as usize == 0);
    assert!(span.end as usize == text.len());
}

#[test]
fn hex_eofs() { 
    let text = ["'\\x", "'\\xf", "'\\xff"];
    let mut i = ::std::cell::Cell::new(3);
    for t in text.iter() {
        let scanner = SimpleStringScanner::new(text[i.get()-3].to_string());
        let mut lexer = Lexer::new(scanner, Some(box |_, er, idx| {
            assert!((idx as usize) == i.get());
            assert!(er == LexingError::Eof);
        }));
        lexer.advance_token();
        i.set(i.get()+1);
    }
}