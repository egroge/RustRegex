use std::collections::VecDeque;
use Lexeme::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Lexeme {
    Ch(char),
    Meta(char),
    Op(char),
    LSquare,
    RSquare,
    LRound,
    RRound,
}

fn lex_class(s: &str) -> Result<(String, Vec<Lexeme>), &'static str> {
    let mut lexed: Vec<Lexeme> = vec![];
    let mut iterator = s.chars();

    while let Some(c) = iterator.next() {
        match c {
            ']' => return Ok(("]".to_string() + iterator.as_str(), lexed)),
            '\\' => match iterator.next() {
                Some(next) => {
                    if "wbdsWBDS".contains(next) {
                        lexed.push(Meta(next));
                    } else {
                        lexed.push(Ch(next));
                    }
                    continue;
                }
                None => return Err("Class cannot end with single backslash"),
            },
            _ => (),
        };

        let lexeme = match c {
            '-' | '^' => Op(c),
            _ => Ch(c),
        };

        lexed.push(lexeme);
    }

    Ok((String::new(), lexed))
}

pub fn lex(s: &str) -> Result<Vec<Lexeme>, &'static str> {
    let mut lexed: Vec<Lexeme> = vec![];
    let mut bracket_stack = VecDeque::new();

    let mut s = s.to_string();
    let mut iterator = s.chars();

    while let Some(c) = iterator.next() {
        if c == '[' {
            lexed.push(LSquare);
            bracket_stack.push_back(LSquare);

            let (remainder, subclass) = lex_class(iterator.as_str())?;
            lexed.extend(subclass);
            s = remainder.clone(); // TODO this is unsatisfactory. Probably a fix with lifetimes
            iterator = s.chars();

            continue;
        }

        if c == '\\' {
            match iterator.next() {
                Some(next) => {
                    if "wbdsWBDS".contains(next) {
                        lexed.push(Meta(next));
                    } else {
                        lexed.push(Ch(next));
                    }
                }
                None => return Err("Class cannot end with single backslash"),
            }
            continue;
        }

        let lexeme = match c {
            '(' => {
                bracket_stack.push_back(LRound);
                LRound
            }
            ')' => {
                if let Some(LRound) = bracket_stack.back() {
                    bracket_stack.pop_back();
                    RRound
                } else {
                    return Err("Mismatched round brackets");
                }
            }
            ']' => {
                if let Some(LSquare) = bracket_stack.back() {
                    bracket_stack.pop_back();
                    RSquare
                } else {
                    return Err("Mismatched square brackets");
                }
            }
            '+' | '?' | '*' => Op(c),
            '.' => Meta(c),
            _ => Ch(c),
        };

        lexed.push(lexeme);
    }
    if !bracket_stack.is_empty() {
        Err("Mismatched brackets")
    } else {
        Ok(lexed)
    }
}

#[cfg(test)]
mod lexing_tests {
    use super::Lexeme::*;
    use super::{lex, lex_class};

    #[test]
    fn lex_class_test() {
        let (remains, lexed) = lex_class("A-Za-z").expect("Failure lexing");
        assert_eq!(remains, String::new());
        assert_eq!(
            lexed,
            vec![Ch('A'), Op('-'), Ch('Z'), Ch('a'), Op('-'), Ch('z')]
        );

        let (remains, lexed) = lex_class("A-Z_]world").expect("Failure lexing");
        assert_eq!(remains, String::from("]world"));
        assert_eq!(lexed, vec![Ch('A'), Op('-'), Ch('Z'), Ch('_')]);

        let (remains, lexed) = lex_class(r"\w]hmm").expect("Failure lexing");
        assert_eq!(remains, String::from("]hmm"));
        assert_eq!(lexed, vec![Meta('w')]);
    }

    #[test]
    fn lex_test() {
        let lexed = lex("yee+t").expect("Problem lexing");
        assert_eq!(lexed, vec![Ch('y'), Ch('e'), Ch('e'), Op('+'), Ch('t')]);

        let lexed = lex("g[A-Z+.]+.").expect("Problem lexing");
        assert_eq!(
            lexed,
            vec![
                Ch('g'),
                LSquare,
                Ch('A'),
                Op('-'),
                Ch('Z'),
                Ch('+'),
                Ch('.'),
                RSquare,
                Op('+'),
                Meta('.')
            ]
        );

        let lexed = lex(r"h?m+\.[\w]\?[\d]?").expect("Problem lexing");
        assert_eq!(
            lexed,
            vec![
                Ch('h'),
                Op('?'),
                Ch('m'),
                Op('+'),
                Ch('.'),
                LSquare,
                Meta('w'),
                RSquare,
                Ch('?'),
                LSquare,
                Meta('d'),
                RSquare,
                Op('?')
            ]
        );

        let lexed = lex("(a)?b(c)*").expect("Problem lexing");
        assert_eq!(
            lexed,
            vec![
                LRound,
                Ch('a'),
                RRound,
                Op('?'),
                Ch('b'),
                LRound,
                Ch('c'),
                RRound,
                Op('*')
            ]
        );

        let lexed = lex("h[");
        assert!(lexed.is_err());

        let lexed = lex(r"h\\h\w").expect("Problem lexing");
        assert_eq!(lexed, vec![Ch('h'), Ch('\\'), Ch('h'), Meta('w')]);

        let lexed = lex(r"[[\]]").expect("Problem lexing");
        assert_eq!(lexed, vec![LSquare, Ch('['), Ch(']'), RSquare]);

        let lexed = lex(r"([]))");
        assert!(lexed.is_err());

        let lexed = lex(r"]the[");
        assert!(lexed.is_err());

        let lexed = lex(r"([)]");
        assert!(lexed.is_err());
    }
}
