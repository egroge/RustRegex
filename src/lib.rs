#[path = "utils/lexer.rs"]
mod lexer;

#[path = "utils/parser.rs"]
mod parser;

#[path = "utils/matcher.rs"]
mod matcher;

pub fn search(regex: &str, search_space: &str) -> Option<(String, u32)> {
    let lexed = lexer::lex(regex).expect("Unable to lex regex");
    let regex = parser::parse(lexed).expect("Unable to parse regex");
    Some(matcher::find(regex, search_space)?)
}

#[cfg(test)]
mod full_test {
    use super::search;

    #[test]
    fn search_test() {
        assert_eq!(
            search("yee*t", "better yet, jam!"),
            Some((String::from("yet"), 7))
        );
        assert_eq!(
            search("(hello)? there[!?]*", "hello there. general kenobi."),
            Some((String::from("hello there"), 0))
        );

        assert_eq!(
            search("(hello)? there[!?]*", "why hello there!!!. general kenobi."),
            Some((String::from("hello there!!!"), 4))
        );

        assert_eq!(
            search("(hello)? there[!?]*", "is he there?! general kenobi."),
            Some((String::from(" there?!"), 5))
        );
        assert_eq!(
            search("(hello)? there[!?]*", "is he here?! general kenobi?"),
            None
        );
    }
}
