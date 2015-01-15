/* Concrete lexer that can lex on string reading interface */
use super::Span;
use super::token::Token;
use super::token::LiteralKind;

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

#[derive(PartialEq, Eq, Copy, Show)]
pub enum LexingError {
    Eof,
    UnexpectedChar,

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

/*
    fn peek2(&self) -> Option<char> {
        self.peek_to(self.idx + 1)
    }
    */

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
        if self.err_recovery {
            return;
        }
        else {
            self.err_recovery = true;
        }
        if let Some(ref mut callback) = self.err_fn {
            callback(unsafe { &*(self as *mut _) }, err, self.r.current_position())
        }
    }

    fn advance_single(&mut self) -> Span {
        let start = self.r.current_position();
        self.r.advance();
        return Span { start: start, end: self.r.current_position() };
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


    fn advance_until_end_of_string(&mut self, c_end: char, escape: bool, accept_unicode: bool) {
        assert!(escape);
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

    fn advance_literal(&mut self, c_end: char, escape: bool, accept_unicode: bool) -> Span {
        let start = self.r.current_position();
        self.r.advance();
        self.advance_until_end_of_string(c_end, escape, accept_unicode);
        return Span { start: start, end: self.r.current_position() }
    }

    fn advance_raw_string(&mut self) -> Span {
        // Ignore number of # pairs when parsing,
        // but still count them for error-reporting
        unimplemented!()
    }

    fn advance_ident(&mut self) -> u32 {
        loop {
            match self.r.peek() {
                Some(x) if !x.is_xid_continue() => break,
                None => break,
                _ => { }
            }
        }
        self.r.current_position()
    }

    fn advance_token(&mut self) -> Option<(Token, Span)> {
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
            '"' => { return Some((Token::StringLiteral(LiteralKind::Normal), self.advance_literal('"', true, true))); },
            '\'' => { return Some((Token::CharLiteral, self.advance_literal('\'', true, true))); },
            'b' => {
                self.r.advance();
                match self.r.peek() {
                    Some('\'') => {
                        let quoted_span = self.advance_literal('\'', true, false);
                        return Some((Token::ByteLiteral, Span { start: token_start, end: quoted_span.end } ));
                    },
                    Some('"') => {
                        let quoted_span = self.advance_literal('"', true, false);
                        return Some((Token::ByteStringLiteral(LiteralKind::Normal), Span { start: token_start, end: quoted_span.end } ));
                    },
                    Some(_) | None => { }
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
        let result = Some((Token::Ident, Span { start:token_start, end: self.advance_ident() }));
        self.err_recovery = false;
        return result;
    }

    pub fn scan<'a>(&'a mut self) -> LexerIterator<'a, 'this, S>  {
        LexerIterator { lexer: self, marker: ::std::marker::ContravariantLifetime }
    }
}

// borrow checker will cry that ptr reference can be larger than lexer lifetime,
// which is simply untrue due to the way we construct our iterator
pub struct LexerIterator<'this, 'lexer, S:StringScanner> {
    lexer: *mut Lexer<'lexer, S>,
    marker: ::std::marker::ContravariantLifetime<'this>
}

impl<'this, 'lexer, S:StringScanner> Iterator for LexerIterator<'this, 'lexer, S> {
    type Item = (Token, Span);

    fn next(&mut self) -> Option<(Token, Span)> {
        unsafe { &mut*(self.lexer) }.advance_token()
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
    println!("{}", span.end);
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
    println!("{:?}", error.as_ref());
    assert!(error.unwrap() == LexingError::UnexpectedChar);
    assert!(err_idx == 3);
    assert!(span_opt.is_some());
    let (token, span) = span_opt.unwrap();
    assert!(if let Token::ByteStringLiteral(LiteralKind::Normal) = token { true } else { false });
    assert!(span.start as usize == 0);
    assert!(span.end as usize == text.len());
}