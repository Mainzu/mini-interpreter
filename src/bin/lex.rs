use std::io::{self, BufRead, Read, Write};

use mini_interpreter::{
    lex,
    source::{export::SourceFile, Span},
};

fn main() -> io::Result<()> {
    let mut stdin = io::stdin();
    let mut stdout = io::stdout();

    // let text = stdin
    //     .lock()
    //     .lines()
    //     .collect::<Result<Vec<_>, _>>()?
    //     .join("\n");
    let mut text = String::new();
    stdin.read_line(&mut text)?;
    let text = text.trim();

    let source = SourceFile::new("stdin", text);

    let mut lexer = lex::Lexer::new(source.cursor());

    for token in lexer.iter_full() {
        let span = Span::from(&token);
        let segment = &source.text()[span.range()];

        match token {
            Ok(tok) => {
                writeln!(stdout, "{:?}: \"{}\"", tok.kind, segment)?;
            }
            Err(e) => {
                writeln!(stdout, "{:?}: \"{}\"", e.kind, segment)?;
            }
        }
    }

    Ok(())
}
