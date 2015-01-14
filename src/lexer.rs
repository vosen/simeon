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

#[derive(PartialEq, Eq, Copy)]
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
    err_fn: Option<Box<FnMut(&Lexer<S>, LexingError, u32)+'this>>
}

// Actual lexing
impl<'this, S:StringScanner> Lexer<'this, S> {
    pub fn new(scanner: S, err_handler:Option<Box<FnMut(&Lexer<S>, LexingError, u32)+'this>>) -> Lexer<'this, S> {
        Lexer { r:scanner, err_fn: err_handler }
    }
}

impl<'this, S:StringScanner> Lexer<'this, S> {
    fn on_error(&mut self, err: LexingError) {
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

    fn advance_until_end_of_string(&mut self) {
        loop {
            let c_opt = self.r.peek();
            match c_opt {
                None => {
                    self.on_error(LexingError::Eof);
                    break;
                },
                Some('"') => {
                    self.r.advance();
                    break;
                }
                Some('\\') => {
                    loop { // This is kinda ugly but I don't want stack overflows on \\\\\\\\\\\\\\\\\\\\\\\\\\\
                        match self.r.next() {
                            Some('\\') => continue,
                            None | Some(_) => break
                        }
                    }
                },
                Some(_) => self.r.advance()
            }
        }
    }

    fn advance_literal(&mut self, c_end: char) -> Span {
        let start = self.r.current_position();
        self.r.advance();
        self.advance_until_end_of_string();
        return Span { start: start, end: self.r.current_position() }
    }

    fn advance_raw_string(&mut self) -> Span {
        // Ignore number of # pairs when parsing,
        // but still count them for error-reporting
        unimplemented!()
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
        let mut looks_like_ident = false;
        match curr {
            '"' => { return Some((Token::StringLiteral(LiteralKind::Normal), self.advance_literal('"'))); },
            '\'' => { return Some((Token::CharLiteral, self.advance_literal('\''))); },
            /*
            'r' => {
                match self.r.peek2() {
                    Some('#') | Some('"') => { return Some((Token::StringLiteral(LiteralKind::Raw), self.advance_raw_string())); },
                    Some(_) => { looks_like_ident = true; },
                    None => { return Some((Token::Ident, self.advance_single())); }
                }
            }
            */
            //'b' => { return Some((Token::ByteStringLiteral, self.advance_literal('"'))); },
            _ => unimplemented!()
        };
        assert!(looks_like_ident);
        unimplemented!()
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