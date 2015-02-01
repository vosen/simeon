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


    lexer_test!(illegal_char_span("'a'b", [(Token::CharLiteral, (0, 3)), (Token::Ident, (3, 4)) ]));
    lexer_test!(char_unicode_empty("'\\u{}' ", [ (Token::CharLiteral, (0, 6)), (Token::Whitespace, (6,7)) ]));
    lexer_test!(lex_underscore("_abc _", [(Token::Ident, (0, 4)), (Token::Whitespace, (4,5)), (Token::Underscore, (5,6)) ]));
    lexer_test!(byte_char("b'a'b", [(Token::ByteLiteral, (0, 4)), (Token::Ident, (4, 5)) ]));
    lexer_test!(single_single_quote("'", [(Token::CharLiteral, (0, 1)) ]));
    lexer_test!(char_too_long("'ab'", [(Token::CharLiteral, (0, 4)) ]));
    lexer_test!(single_lifetime("'ab", [(Token::Lifetime, (0, 3)) ]));
    lexer_test!(lifetime_newline("'ab\n", [(Token::Lifetime, (0, 3)), (Token::Whitespace, (3, 4)) ]));
    lexer_test!(keywords("continue as break", [(Token::Keyword(KeywordKind::Continue), (0, 8)),
                                               (Token::Whitespace, (8, 9)),
                                               (Token::Keyword(KeywordKind::As), (9, 11)),
                                               (Token::Whitespace, (11, 12)),
                                               (Token::Keyword(KeywordKind::Break), (12, 17))]));
    lexer_test!(doc_comm_line("///12345\r\noffsetof", [(Token::DocComment, (0, 8)),
                                                       (Token::Whitespace, (8, 10)),
                                                       (Token::Keyword(KeywordKind::Offsetof), (10, 18))]));
    lexer_test!(block_doc_comment("/ /** */", [(Token::BinOp(BinOpKind::Slash), (0, 1)),
                                                            (Token::Whitespace, (1, 2)),
                                                            (Token::DocComment, (2, 8))]));
    lexer_test!(block_comment_nested("/**/**/*/", [(Token::DocComment, (0, 9))]));
    lexer_test!(int_literals_short("0/0", [(Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::None), (0, 1)),
                                           (Token::BinOp(BinOpKind::Slash), (1, 2)),
                                           (Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::None), (2, 3))]));
    lexer_test!(int_literal_suffixed1("123is", [(Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::Isize), (0, 5))]));
    lexer_test!(int_literal_suffixed2("123us", [(Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::Usize), (0, 5))]));
    lexer_test!(int_literal_suffixed3("123_us", [(Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::Usize), (0, 6))]));
    lexer_test!(int_literal_suffixed4("0xff_u8", [(Token::IntegerLiteral(IntegerLiteralBase::Hex, IntegerLiteralSuffix::U8), (0, 7))]));
    lexer_test!(int_literal_suffixed5("0o70_i16", [(Token::IntegerLiteral(IntegerLiteralBase::Octal, IntegerLiteralSuffix::I16), (0, 8))]));
    lexer_test!(int_literal_suffixed6("0b1111_1111_1001_0000_i32", [(Token::IntegerLiteral(IntegerLiteralBase::Binary, IntegerLiteralSuffix::I32), (0, 25))]));
    lexer_test!(float_literal1("123.0f64", [(Token::FloatLiteral(FloatLiteralSuffix::F64), (0, 8))]));
    lexer_test!(float_literal2("0.1f64", [(Token::FloatLiteral(FloatLiteralSuffix::F64), (0, 6))]));
    lexer_test!(float_literal4("0.1f32", [(Token::FloatLiteral(FloatLiteralSuffix::F32), (0, 6))]));
    lexer_test!(float_literal3("12E+99_f64", [(Token::FloatLiteral(FloatLiteralSuffix::F64), (0, 10))]));
    lexer_test!(float_literal5("002.", [(Token::FloatLiteral(FloatLiteralSuffix::None), (0, 4))]));
    lexer_test!(illegal_token("``` ", [(Token::UnexpectedSequence, (0, 3)), (Token::Whitespace, (3, 4)) ]));

    mod error {
        use simeon::lexer::*;
        use simeon::token::*;

        macro_rules! lexer_test_errors {
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

        lexer_test_errors!(lifetime_newline("'a\n", [(Token::Lifetime, None), (Token::Whitespace, None) ]));
        lexer_test_errors!(illegal_char("'a'b", [(Token::CharLiteral, None), (Token::Ident, Some((LexingError::IllegalToken, 3))) ]));
        lexer_test_errors!(single_single_quote("'", [(Token::CharLiteral, Some((LexingError::Eof, 1))) ]));
        lexer_test_errors!(char_too_long("'ab'", [(Token::CharLiteral, Some((LexingError::TokenTooLong, 0))) ]));
        lexer_test_errors!(byte_unicode_escape("b'\\u{0}'", [(Token::ByteLiteral, Some((LexingError::IllegalEscapeSeq, 3))) ]));
        lexer_test_errors!(byte_non_unicode("b'ą'", [(Token::ByteLiteral, Some((LexingError::NonAsciiByte, 2))) ]));
        lexer_test_errors!(char_broken_unicode_escape("'\\u{0 }'", [(Token::CharLiteral, Some((LexingError::MalformedEscapeSeq, 5))) ]));
        lexer_test_errors!(char_hex_escape_too_short("'\\x0'", [(Token::CharLiteral, Some((LexingError::MalformedEscapeSeq, 4))) ]));
        lexer_test_errors!(char_hex_escape_too_long("'\\xfFf'", [(Token::CharLiteral, Some((LexingError::TokenTooLong, 0))) ]));
        lexer_test_errors!(char_hex_escape_too_large("'\\xff'", [(Token::CharLiteral, Some((LexingError::InvalidEscapeSeq, 0))) ]));
        lexer_test_errors!(char_hex_eof("'\\xff", [(Token::CharLiteral, Some((LexingError::Eof, 5))) ]));
        lexer_test_errors!(char_unterminated("'\\xff ", [(Token::CharLiteral, Some((LexingError::Eof, 6)))]));
        lexer_test_errors!(char_hex_escape_recover_from_unexpected("'\\xffz ", [(Token::CharLiteral, Some((LexingError::Eof, 7)))]));
        lexer_test_errors!(char_unicode_unterminated("'\\u{00} ", [(Token::CharLiteral, Some((LexingError::Eof, 8)))]));
        lexer_test_errors!(char_unescaped_newline("'\r\n'", [(Token::CharLiteral, Some((LexingError::UnescapedLiteral, 1))),
                                                             (Token::Whitespace, None),
                                                             (Token::CharLiteral, Some((LexingError::Eof, 4)))    ]));
        lexer_test_errors!(char_unicode_dont_recover_on_whitespace("'\\u{00}z ", [(Token::CharLiteral, Some((LexingError::Eof, 9))) ]));
        lexer_test_errors!(char_unicode_empty("'\\u{}' ", [(Token::CharLiteral, Some((LexingError::MalformedEscapeSeq, 4))), (Token::Whitespace, None)  ]));
        lexer_test_errors!(byte_unescaped_newline("b'\n'", [(Token::ByteLiteral, Some((LexingError::UnescapedLiteral, 2))),
                                                            (Token::Whitespace, None),
                                                            (Token::CharLiteral, Some((LexingError::Eof, 4))) ]));
        lexer_test_errors!(char_unescaped_char("'''", [(Token::CharLiteral, Some((LexingError::UnescapedLiteral, 1))) ]));
        lexer_test_errors!(illegal_int_suffix1("1i16us", [(Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::None), Some((LexingError::IllegalSuffix, 1))) ]));
        lexer_test_errors!(illegal_int_suffix2("1a16us", [(Token::IntegerLiteral(IntegerLiteralBase::Decimal, IntegerLiteralSuffix::None), Some((LexingError::IllegalSuffix, 1))) ]));
        lexer_test_errors!(mismatched_int_suffix("0b1f64", [(Token::IntegerLiteral(IntegerLiteralBase::Binary, IntegerLiteralSuffix::None), Some((LexingError::IllegalSuffix, 3))) ]));
        lexer_test_errors!(mismatched_float_suffix("0.1u32", [(Token::FloatLiteral(FloatLiteralSuffix::None), Some((LexingError::IllegalSuffix, 3))) ]));
    }
}