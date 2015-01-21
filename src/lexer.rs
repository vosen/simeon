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

    fn advance_literal(&mut self, c_end: char, accept_unicode: bool) {
        let start = self.r.current_position();
        self.r.advance();
        self.advance_until_end_of_string(c_end, accept_unicode);
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

    // If use_byte_mode is on we return ByteLiteral token and dont return Lifetime token
    fn scan_char_literal_or_lifetime(&mut self, unicode: bool, use_byte_mode: bool) -> Token {
        debug_assert!(self.r.peek() == Some('\''));
        let start = self.r.current_position();
        self.r.advance();
        match self.r.peek() {
            Some('\\') => { // byte mode
                self.advance_escape_seq(start, unicode);
                Token::CharLiteral
            },
            None => { // I'm tempted to do if ::std::rand::random::<bool>() { Token::Lifetime } else { Token::CharLiteral }
                self.on_error(LexingError::Eof);
                Token::CharLiteral
            },
            Some(c) if !c.is_xid_continue()  => { // we are char, consume until '
                if !unicode { self.check_unicode(c) };
                self.r.advance();
                self.advance_until_end_of_string('\'', true);
                Token::CharLiteral
            }
            Some(_) => { // we are <'><XID_CONTINUE>, check the next char
                self.r.advance();
                match self.r.peek() {
                    Some('\'') => self.eat(Token::CharLiteral),
                    _ =>  {
                        if self.error_and_recover_to_char(true) { Token::CharLiteral } else { Token::Lifetime }
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
                                            self.on_error(LexingError::UnexpectedChar);
                                            self.error_and_recover_to_char(false);
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

    fn advance_unicode_digits(&mut self, token_start: u32) {
        let mut back_buffer = [0; 6];
        match self.r.peek() {
            None => {
                self.on_error(LexingError::Eof);
                return;
            },
            Some('{') => self.r.advance(),
            Some(_) => {
                self.error_and_recover_to_char(false);
                return;
            }
        }
        for i in range(0, 6) {
            match self.r.peek() {
                Some('}') => break,
                Some(c) if c.is_digit(16) => {
                    back_buffer[i] = (c.to_digit(16).unwrap() as u8);
                }
                Some(_) => {
                    self.error_and_recover_to_char(false);
                    return;
                }
                None => {
                    self.on_error(LexingError::Eof);
                    return;
                }
            }
        }
        match self.r.peek() {
            Some('}') => {
                self.r.advance();
                match self.r.peek() {
                    Some('\'') => {
                        self.r.advance();
                        let sum : u64 = (back_buffer[0] << 5) as u64 + (back_buffer[2] << 4) as u64
                                        + (back_buffer[2] << 3) as u64 + (back_buffer[3] << 2) as u64
                                        + (back_buffer[4] << 1) as u64 + (back_buffer[5] << 0) as u64;
                        if sum > 0x10ffffu64 || (sum >= 0xdc00u64 && sum <= 0xdfffu64) {
                            self.on_error(LexingError::InvalidEscapeSeq);
                        }
                    }
                    Some(_) => {
                        if! self.error_and_recover_to_char(false) {
                            self.on_error(LexingError::UnterminatedLiteral)
                        }
                    }
                    None => self.on_error(LexingError::Eof),
                }
            },
            Some(_) => { self.error_and_recover_to_char(false); },
            None => self.on_error(LexingError::Eof)
        }

    }

    /*
     * Reference documentation is incomplete about accepted byte escape sequences
     * accepted simple esc seq are \n, \r, \t, \\, \', \", \0
     * new unicode escape seq are \u{0} to \u{10ffff} (one to six digits, no spaces, inclusive range DC00 to DFFF is invalid)
     * Possible errors:
     * unknown escape seq (eg. '\a'): UnexpectedChar
     * wrong escape seq (eg. '\u{}'): UnexpectedChar
     * too long (eg. 'abc'): UnexpectedChar
     * unterminated seq (spotting whitespace before `'`): UnterminatedLiteral
     * and eofs
     */
    fn advance_escape_seq(&mut self, token_start: u32, unicode: bool) {
        debug_assert!(self.r.peek() == Some('\\'));
        self.r.advance();
        match self.r.peek() {
            None => {
                self.on_error(LexingError::Eof);
            },
            Some(esc_mark) => {
                match esc_mark {
                    'n' | 'r' | 't' | '\\' | '\'' | '"' | '0' => self.r.advance(),
                    'x' => { self.advance_hex_digits(token_start); },
                    'u' => {
                        if !unicode {
                            self.on_error(LexingError::UnexpectedChar);
                        }
                        self.advance_unicode_digits(token_start);
                    },
                    _ => {
                        self.on_error(LexingError::UnexpectedChar);
                        self.error_and_recover_to_char(false);
                    }
                }
            }
        };
    }

    fn scan_ident(&mut self) -> Token {
        loop {
            match self.r.peek() {
                Some(x) if !x.is_xid_continue() => break,
                None => break,
                _ => { self.r.advance(); }
            }
        }
        Token::Ident
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
                self.advance_literal('"', true);
                Token::StringLiteral(LiteralKind::Normal)
            }
            '\'' => self.scan_char_literal_or_lifetime(true, true),
            'b' => {
                self.r.advance();
                match self.r.peek() {
                    Some('\'') => self.scan_char_literal_or_lifetime(false, false),
                    Some('"') => {
                        self.advance_literal('"', false);
                        Token::ByteStringLiteral(LiteralKind::Normal)
                    },
                    Some(_) | None => self.scan_ident()
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
            x if x.is_xid_start() => {
                self.r.advance();
                self.scan_ident()
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
    assert!(if let Token::StringLiteral(LiteralKind::Normal) = token { true } else { false });
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
        let mut lexer = Lexer::new(scanner, Some(box |_, er, _| { error = Some(er); } ));
        let span_opt = lexer.scan_token();
        assert!(lexer.scan_token().is_none());
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
        let span_opt = lexer.scan_token();
        assert!(lexer.scan_token().is_none());
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
        lexer.scan_token();
        i.set(i.get()+1);
    }
}