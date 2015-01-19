#![allow(unstable)]
#![feature(box_syntax)]

extern crate simeon;

#[cfg(test)]
mod lexer {
    use simeon::lexer::*;
    use simeon::token::*;

    macro_rules! lexer_test{
        ($name: ident ( $text: expr, [$($tokens: pat),+ ]) ) => (
            #[test]
            fn $name() {
                let text = $text;
                let scanner = SimpleStringScanner::new(text.to_string());
                let mut tokens = Vec::new();
                let mut lexer = Lexer::new(scanner, None);
                for t in lexer.scan() {
                    tokens.push(t);
                }
                let mut i = 0;
                $({
                    let temp = (tokens[i].0, (tokens[i].1.start, tokens[i].1.end));
                    match temp {
                        $tokens => { },
                        _ => { panic!(format!("wrong token at index {:}: {:?}", i, tokens[i])) }
                    }
                    i+=1;
                })+
                if i != tokens.len() {
                    panic!(format!("wrong number of tokens. expected: {:}, actual: {:}", i, tokens.len()))
                }
            }
        );
    }

    macro_rules! lexer_test_errors{
        ($name: ident ( $text: expr, [$($tokens: pat),+ ]) ) => (
            #[test]
            fn $name() {
                let text = $text;
                let scanner = SimpleStringScanner::new(text.to_string());
                let mut tokens = Vec::new();
                let last_error : ::std::cell::Cell<Option<(LexingError, u32)>> = ::std::cell::Cell::new(None);
                let mut lexer = Lexer::new(scanner, Some(box |&mut: _, er, idx| {
                    if let Some(err_unwrapped) = last_error.get() {
                        panic!("raised more than one error for a token. old: {:?}, new: {:?}", err_unwrapped, (er,idx));
                    }
                    last_error.set(Some((er,idx)));
                }));
                for t in lexer.scan() {
                    tokens.push((t.0, last_error.get()));
                    last_error.set(None);
                }
                let mut i = 0;
                $({
                    match tokens[i] {
                        $tokens => { },
                        _ => { panic!(format!("wrong token at index {:}: {:?}", i, tokens[i])) }
                    }
                    i+=1;
                })+
                if i != tokens.len() {
                    panic!(format!("wrong number of tokens. expected: {:}, actual: {:}", i, tokens.len()))
                }
            }
        );
    }
    lexer_test!(illegal_char_span("'a'b", [(Token::CharLiteral, (0, 3)), (Token::Ident, (3, 4)) ]));
    lexer_test_errors!(illegal_char_errors("'a'b", [(Token::CharLiteral, None), (Token::Ident, Some((LexingError::IllegalToken, 3))) ]));
    lexer_test!(lex_underscore("_abc _", [(Token::Ident, (0, 4)), (Token::Whitespace, (4,5)), (Token::Underscore, (5,6)) ]));
    lexer_test!(byte_char("b'a'b", [(Token::ByteLiteral, (0, 4)), (Token::Ident, (4, 5)) ]));
    lexer_test!(single_single_quote("'", [(Token::CharLiteral, (0, 1)) ]));
    lexer_test_errors!(single_single_quote_error("'", [(Token::CharLiteral, Some((LexingError::Eof, 1))) ]));
    lexer_test!(char_too_long("'ab'", [(Token::CharLiteral, (0, 4)) ]));
    lexer_test_errors!(char_too_long_error("'ab'", [(Token::CharLiteral, Some((LexingError::UnexpectedChar, 2))) ]));
    lexer_test_errors!(byte_unicode_escape("b'\\u{0}'", [(Token::ByteLiteral, Some((LexingError::UnexpectedChar, 3))) ])); // FAIL
    lexer_test_errors!(byte_non_unicode("b'ą'", [(Token::ByteLiteral, Some((LexingError::NonAsciiByte, 2))) ])); // FAIL
    lexer_test_errors!(char_broken_unicode_escape("'\\u{0 }'", [(Token::CharLiteral, Some((LexingError::UnexpectedChar, 5))) ])); // FAIL
    lexer_test_errors!(char_hex_escape_too_short("'\\x0'", [(Token::CharLiteral, Some((LexingError::InvalidEscapeSeq, 0))) ])); // FAIL
    lexer_test_errors!(char_hex_escape_too_long("'\\xfff'", [(Token::CharLiteral, Some((LexingError::UnexpectedChar, 5))) ]));
    lexer_test_errors!(char_hex_escape_too_large("'\\xff'", [(Token::CharLiteral, Some((LexingError::InvalidEscapeSeq, 0))) ]));
    lexer_test_errors!(char_unterminated("'\\xff ", [(Token::CharLiteral, Some((LexingError::UnterminatedLiteral, 5))), (Token::Whitespace, None)  ]));
}