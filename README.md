Alternative Rust lexer
======================

Simeon is a Rust lexer. It's quite usable and MIT/ASL2 license. Feel free to use it.

As to why it was written and how it's different from ANTLR-generated or one in libsyntax:
* In [Visual Rust](https://github.com/pistondevelopers/visualrust), this will let us drop Java dependency and dogfood some parts of the project
* It's nice to have a lexer that is not encoding-dependant
* Better control over error handling: libsyntax lexer will simply panic!(). ANTLR can be more or less forced to handle the errors the way I want, but it's tedious
* Because this seemed like an interesting side project

# What doesn't work

* Raw literals
* Obsolete syntax (eg. old-style unicode escapes `\u7FFF`)
* Shebangs (I think they should go away or be part of a parser)

# What works but maybe shouldn't

* `\u{2028}` and `\u{2029}` as line breaks

# What works

Everything else (probably)