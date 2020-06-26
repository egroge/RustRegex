#[derive(Debug, PartialEq)]
enum Lexeme {
    Ch(char),
    Meta(char),
    Op(char),
    LSquare,
    RSquare,
    LRound,
    RRound,
}

use Lexeme::*;
fn lex_class(s: &str) -> (u32, Vec<Lexeme>) {
    let mut backslash = false;
    let mut lexed: Vec<Lexeme> = vec![];
    let mut i = 0;

    for c in s.chars() {
        if backslash {
            if "wbdsWBDS".contains(c) {
                lexed.push(Meta(c));
            } else {
                lexed.push(Ch(c));
            }
            i += 1;
            backslash = false;
            continue;
        }
        assert!(!backslash);

        match c {
            ']' => return (i, lexed),
            '\\' => {
                i += 1;
                backslash = true;
                continue;
            }
            _ => (),
        }

        i += 1;

        let lexeme = match c {
            '-' | '^' => Op(c),
            _ => Ch(c),
        };

        lexed.push(lexeme);
        backslash = false;
    }

    (i, lexed)
}

// If we have a backslash, whatever comes after is a meta, or a ch

fn lex(s: &str) -> Result<Vec<Lexeme>, &'static str> {
    let mut lexed: Vec<Lexeme> = vec![];
    let mut backslash = false;

    let mut num_round = 0;
    let mut num_square = 0;

    let s: Vec<char> = s.chars().collect();

    let mut i = 0;
    while i < s.len() {
        let c = s[i];
        i += 1;
        if backslash {
            if "wbdsWBDS".contains(c) {
                lexed.push(Meta(c));
            } else {
                lexed.push(Ch(c));
            }
            backslash = false;
            continue;
        }

        assert!(!backslash);

        if c == '[' {
            lexed.push(LSquare);
            num_square += 1;
            let (skipped, subclass) = lex_class(s[i..].iter().collect::<String>().as_str());
            lexed.extend(subclass);
            i += skipped as usize;
            continue;
        }

        if c == '\\' {
            backslash = true;
            continue;
        }

        let lexeme = match c {
            '(' => {
                num_round += 1;
                LRound
            }
            ')' => {
                num_round -= 1;
                RRound
            }
            ']' => {
                num_square -= 1;
                RSquare
            }
            '+' | '?' | '*' => Op(c),
            '.' => Meta(c),
            _ => Ch(c),
        };

        lexed.push(lexeme);
        backslash = false;
    }
    if num_round != 0 || num_square != 0 {
        Err("Mismatched brackets")
    } else {
        Ok(lexed)
    }
}

#[cfg(test)]
mod tests {
    mod lexer_tests {
        use super::super::Lexeme::*;
        use super::super::{lex, lex_class};

        #[test]
        fn lex_class_test() {
            let (remains, lexed) = lex_class("A-Za-z");
            assert_eq!(remains, 6);
            assert_eq!(
                lexed,
                vec![Ch('A'), Op('-'), Ch('Z'), Ch('a'), Op('-'), Ch('z')]
            );

            let (remains, lexed) = lex_class("A-Z_]world");
            assert_eq!(remains, 4);
            assert_eq!(lexed, vec![Ch('A'), Op('-'), Ch('Z'), Ch('_')]);
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
        }
    }
}
