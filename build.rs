extern crate gcc;

fn main() {
    gcc::Config::new()
                .file("src/cimeon/raw_parse.c")
                .file("src/cimeon/token_lookup.c")
                .flag("-std=c99")
                .flag("-pedantic")
                .flag("-Wall")
                //.flag("-Werror")
                .flag("-Wshadow")
                .flag("-Wpointer-arith")
                .flag("-Wcast-qual")
                .flag("-Wstrict-prototypes")
                .flag("-Wno-unused-but-set-variable")
                .compile("libcimeon.a");
}