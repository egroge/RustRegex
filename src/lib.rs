#[path = "utils/lexer.rs"]
mod lexer;

#[path = "utils/parser.rs"]
mod parser;

#[path = "utils/matcher.rs"]
mod matcher;

pub fn search(regex: &parser::Expr, search_space: &str) -> Option<(String, u32)> {
    matcher::find(regex, search_space)
}

pub fn compile_regex(regex_str: &str) -> parser::Expr {
    let lexed = lexer::lex(regex_str).expect("Malformed regex");
    parser::parse(lexed).expect("Malformed regex")
}

#[cfg(test)]
mod full_test {
    use super::{compile_regex, search};

    #[test]
    fn search_test() {
        assert_eq!(
            search(&compile_regex("yee*t"), "better yet, jam!"),
            Some((String::from("yet"), 7))
        );

        let kenobi_regex = compile_regex("(hello)? there[!?]*");

        assert_eq!(
            search(&kenobi_regex, "hello there. general kenobi."),
            Some((String::from("hello there"), 0))
        );

        assert_eq!(
            search(&kenobi_regex, "why hello there!!!. general kenobi."),
            Some((String::from("hello there!!!"), 4))
        );

        assert_eq!(
            search(&kenobi_regex, "is he there?! general kenobi."),
            Some((String::from(" there?!"), 5))
        );
        assert_eq!(
            search(&kenobi_regex, "is he here?! general kenobi?"),
            None
        );
    }
}
