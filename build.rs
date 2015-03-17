extern crate gcc;

fn main() {
    gcc::Config::new()
                .file("src/cimeon/token_lookup.c")
                .file("src/cimeon/cimeon.c")
                .flag("-std=c99")
                .flag("-pedantic")
                .flag("-Wall")
                .flag("-Wshadow")
                .flag("-Wpointer-arith")
                .flag("-Wcast-align")
                .flag("-Wcast-qual")
                .flag("-Wstrict-prototypes")
                .flag("-Wno-unused-but-set-variable")
                //.flag("-Werror")
                //.flag("-g")
                //.flag("-O0")
                .compile("libcimeon.a");
}