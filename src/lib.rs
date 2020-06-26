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
// Assumes well formed
fn lex_class(s: &str) -> (String, Vec<Lexeme>) {
    let mut backslash = false;
    let mut lexed: Vec<Lexeme> = vec![];
    let mut i = 0;

    for c in s.chars() {
        match c {
            ']' => return (s[i..].to_string(), lexed),
            '\\' => {
                i += 1;
                backslash = true;
                continue;
            }
            _ => (),
        }

        i += 1;

        let lexeme = match c {
            'w' | 'W' | 'b' | 'B' | 'd' | 'D' | 's' | 'S' => {
                if backslash {
                    Meta(c)
                } else {
                    Ch(c)
                }
            }
            '-' | '^' => {
                if backslash {
                    Ch(c)
                } else {
                    Op(c)
                }
            }
            _ => Ch(c),
        };

        lexed.push(lexeme);
        backslash = false;
    }

    (s[i..].to_string(), lexed)
}

fn lex(s: &str) -> Vec<Lexeme> {
    if s == "" {
        return vec![];
    }
    let mut lexed: Vec<Lexeme> = vec![];
    let mut i = 0;
    let mut backslash = false;
    for c in s.chars() {
        if !backslash && c == '[' {
            lexed.push(LSquare);
            let (remaining, subclass) = lex_class(&s[i + 1..]);
            lexed.extend(subclass);
            lexed.extend(lex(remaining.as_str()));
            return lexed;
        }

        i += 1;
        if c == '\\' {
            backslash = true;
            continue;
        }

        let lexeme = match c {
            '(' => LRound,
            ')' => RRound,
            // TODO this is bad, but should work because already checked
            // for backslash at start of loop
            '[' => Ch('['),
            ']' => RSquare,
            '+' | '?' | '*' => {
                if backslash {
                    Ch(c)
                } else {
                    Op(c)
                }
            }
            '.' => {
                if backslash {
                    Ch(c)
                } else {
                    Meta(c)
                }
            }
            _ => Ch(c),
        };

        lexed.push(lexeme);
        backslash = false;
    }
    lexed
}

#[cfg(test)]
mod tests {
    mod lexer_tests {
        use super::super::Lexeme::*;
        use super::super::{lex, lex_class};

        #[test]
        fn lex_class_test() {
            let (remains, lexed) = lex_class("A-Za-z");
            assert_eq!(remains, "");
            assert_eq!(
                lexed,
                vec![Ch('A'), Op('-'), Ch('Z'), Ch('a'), Op('-'), Ch('z')]
            );

            let (remains, lexed) = lex_class("A-Z_]world");
            assert_eq!(remains, "]world");
            assert_eq!(lexed, vec![Ch('A'), Op('-'), Ch('Z'), Ch('_')]);
        }

        #[test]
        fn lex_test() {
            let lexed = lex("yee+t");
            assert_eq!(lexed, vec![Ch('y'), Ch('e'), Ch('e'), Op('+'), Ch('t')]);

            let lexed = lex("g[A-Za-z+0-9.]+@.");
            assert_eq!(
                lexed,
                vec![
                    Ch('g'),
                    LSquare,
                    Ch('A'),
                    Op('-'),
                    Ch('Z'),
                    Ch('a'),
                    Op('-'),
                    Ch('z'),
                    Ch('+'),
                    Ch('0'),
                    Op('-'),
                    Ch('9'),
                    Ch('.'),
                    RSquare,
                    Op('+'),
                    Ch('@'),
                    Meta('.')
                ]
            );

            let lexed = lex(r"h?m+\.@[\w]\?[\d]?");
            assert_eq!(
                lexed,
                vec![
                    Ch('h'),
                    Op('?'),
                    Ch('m'),
                    Op('+'),
                    Ch('.'),
                    Ch('@'),
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

            let lexed = lex("(a)?b(c)*");
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
            assert_eq!(lexed, vec![Ch('h'), LSquare]);
        }
    }
}
