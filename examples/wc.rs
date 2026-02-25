// run command:
//    $ cargo run --example wc -- examples/wc.rs

use macro_lisp::lisp;

lisp!(use std::env);
lisp!(use std::process::exit);
lisp!(use std::io::Read);

lisp!(fn is_whitespace ((b u8)) bool
    (match b
        (0x20 | 0x09 | 0x85 | 0x0a | 0x0b | 0x0c | 0x0d => (true))
        (_ => (false) ))
);

lisp!(fn main () ()
    (let (args Vec<String>) env::args().collect())
    (if (< (len args) 2)
        (block
            (println "usage: wc file")
            (exit 0)))

    (let mut char_count 0)
    (let mut word_count 0)
    (let mut line_count 0)
    (let mut in_word false)

    (let path (ref (index args 1)))
    (rust
        let mut file = std::fs::File::open(path).unwrap()
    )
    (rust
        let mut contents = Vec::new()
    )
    (rust
        file.read_to_end(&mut contents).unwrap()
    )
    (for byte in contents
        (+= char_count 1)

        (let b byte)
        (if (== b 0x0a)
            (+= line_count 1))

        (if in_word
            (if (is_whitespace b)
                (set in_word false))
            (if (! (is_whitespace b))
                (block
                    (set in_word true)
                    (+= word_count 1)))))

    (println "{:>10} {:>10} {:>10} {}" line_count word_count char_count path)
);
