use std::collections::VecDeque;

#[derive(Debug, PartialEq, Clone)]
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

type Expr = Vec<Term>;

enum Term {
    TAtom(Atom),
    Plus,
    Star,
    Questioned,
}

enum AllowedChars {
    Unrestricted,
    Restricted(Vec<char>),
}

enum Atom {
    ACh(char),
    Class(bool, AllowedChars),
    SubExpr(bool, Expr),
}

fn parse_range(lexed: &Vec<Lexeme>) -> Option<Vec<char>> {
    if let Op('-') = lexed[1] {
        if let Ch(lower) = lexed[0] {
            if let Ch(upper) = lexed[2] {
                let range: Vec<char> = (lower as u8..upper as u8).map(|c| c as char).collect();

                return Some(range);
            }
        }
    }
    None
}

// TODO have the initial ^ checked for before calling this
fn parse_class_member(lexed: &Vec<Lexeme>) -> Result<(u32, Vec<char>), &'static str> {
    let digits = (0..10)
        .map(|x| std::char::from_digit(x, 10).unwrap())
        .collect();

    let latin = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        .chars()
        .collect();

    if lexed.len() >= 3 {
        // Possible that there is a range
        let possible_range = parse_range(&lexed);
        if possible_range.is_some() {
            return Ok((3, possible_range.unwrap()));
        }
    }

    // Cannot be a range, so must be a single char
    let tokens_used = 1;
    match &lexed[0] {
        RSquare => Ok((tokens_used, vec![])),
        Meta(c) => match c {
            'd' => Ok((tokens_used, digits)),
            's' => Ok((tokens_used, vec![' ', '\t'])), // idk what else
            'w' => Ok((tokens_used, vec![digits, latin].concat())),
            'b' => Ok((tokens_used, vec!['\n'])), // pretty sure this is wrong
            _ => Err("Unknown meta token in character class!"),
        },
        Ch(c) => Ok((tokens_used, vec![*c])),
        _ => Err("Misplaced token in character class!"),
    }
}

use Atom::*;
fn parse_character_class(lexed: Vec<Lexeme>) -> Result<(Vec<Lexeme>, Atom), &'static str> {
    assert!(!lexed.is_empty());

    let mut i = 0;
    // Rust does not seem to support inverted = let Op = lexed[0]; :(
    let inverted: bool;
    if let Op('^') = lexed[0] {
        i += 1;
        inverted = true
    } else {
        inverted = false
    };

    // Should not have to check i < len() since bracketing should be correct
    let mut all_chars = vec![];
    while lexed[i] != RSquare {
        let (skipped, chars) = parse_class_member(&lexed)?;
        i += skipped as usize;
        all_chars.extend(chars);
    }

    Ok((
        lexed[i + 1..].to_vec(),
        Class(inverted, AllowedChars::Restricted(all_chars)),
    ))
}

fn parse_atom(lexed: Vec<Lexeme>) -> Result<(Vec<Lexeme>, Atom), &'static str> {
    use Atom::*;
    return match &lexed[0] {
        LSquare => Ok(parse_character_class(lexed)?),
        Ch(c) => Ok((lexed[1..].to_vec(), ACh(*c))),
        Meta(c) => {
            return if *c == '.' {
                Ok((
                    lexed[1..].to_vec(),
                    Class(false, AllowedChars::Unrestricted),
                ))
            } else {
                let (skipped, chars) = parse_class_member(&lexed)?;
                Ok((
                    lexed[skipped as usize..].to_vec(),
                    Class(false, AllowedChars::Restricted(chars)),
                ))
            };
        }
        LRound => Ok((vec![], ACh('q'))), // dummy value for now
        _ => Err("Non atom")?,
    };
}

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
// TODO fix bracketing. At the moment, /]the[/ would be allowed
fn lex(s: &str) -> Result<Vec<Lexeme>, &'static str> {
    let mut lexed: Vec<Lexeme> = vec![];
    let mut backslash = false;

    let mut bracket_stack = VecDeque::new();
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
            bracket_stack.push_back(LSquare);
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
        backslash = false;
    }
    if !bracket_stack.is_empty() {
        Err("Mismatched brackets")
    } else {
        Ok(lexed)
    }
}

#[cfg(test)]
mod tests {
    mod parser_tests {
        use super::super::*;

        #[test]
        fn parse_range_test() {
            let possible_range = parse_range(&vec![Ch('A'), Op('-'), Ch('a')]);
            let expected_range = "ABCDEFGHIJKLMNOPQRSTUVWXYZa".chars().collect();
            assert_eq!(possible_range, Some(expected_range));

            let possible_range = parse_range(&vec![Ch('a'), Ch('-'), Ch('z')]);
            assert!(possible_range.is_none());
        }

        #[test]
        fn parse_class_member_test() {
            let (skipped, chars) = parse_class_member(&vec![Ch('m')]).expect("Failure");
            assert_eq!(skipped, 1);
            assert_eq!(chars, vec!['m']);

            let (skipped, chars) = parse_class_member(&vec![Meta('w')]).expect("Failure");
            let expected_range = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".chars().collect::<Vec<char>>();
            assert_eq!(skipped, 1);
            assert_eq!(chars, expected_range);

            let (skipped, chars) =
                parse_class_member(&vec![Ch('A'), Op('-'), Ch('Z')]).expect("Failure");

            assert_eq!(skipped, 3);
            assert_eq!(chars, expected_range);

            let result = parse_class_member(&vec![Meta('z'), Ch('o'), Ch('d')]);
            assert!(result.is_err());
        }

        #[test]
        fn parse_atom_test() {
            // TODO implement
        }
    }

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

            let lexed = lex(r"]the[");
            assert!(lexed.is_err());

            let lexed = lex(r"([)]");
            assert!(lexed.is_err());
        }
    }
}
