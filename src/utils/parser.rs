mod lexer;

use lexer::{Lexeme, Lexeme::*};
use std::collections::HashSet;
use Atom::*;

type Expr = Vec<Term>;

#[derive(Debug, PartialEq)]
enum Term {
    TAtom(Atom),
    TOp(Operation),
}

#[derive(Debug, PartialEq)]
enum Operation {
    Plus(Atom),
    Star(Atom),
    Questioned(Atom),
}

#[derive(Debug, PartialEq)]
enum AllowedChars {
    Unrestricted,
    Restricted(HashSet<char>),
}

#[derive(Debug, PartialEq)]
enum Atom {
    ACh(char),
    Class(bool, AllowedChars),
    SubExpr(bool, Expr),
}

fn parse_range(lexed: &Vec<Lexeme>) -> Option<HashSet<char>> {
    if let Op('-') = lexed[1] {
        if let Ch(lower) = lexed[0] {
            if let Ch(upper) = lexed[2] {
                let range: HashSet<char> =
                    (lower as u8..upper as u8 + 1).map(|c| c as char).collect();

                return Some(range);
            }
        }
    }
    None
}

fn parse_class_member(lexed: &Vec<Lexeme>) -> Result<(u32, HashSet<char>), &'static str> {
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
        RSquare => Ok((tokens_used, HashSet::new())),
        Meta(c) => match c {
            'd' => Ok((tokens_used, digits)),
            's' => Ok((tokens_used, [' ', '\t'].iter().cloned().collect())), // idk what else
            'w' => Ok((tokens_used, digits.union(&latin).cloned().collect())),
            'b' => Ok((tokens_used, ['\n'].iter().cloned().collect())), // pretty sure this is wrong
            _ => Err("Unknown meta token in character class!"),
        },
        Ch(c) => Ok((tokens_used, [*c].iter().cloned().collect())),
        t => {
            println!("Token: {:?}", t);
            Err("Misplaced token in character class!")
        }
    }
}

fn parse_character_class(lexed: Vec<Lexeme>) -> Result<(Vec<Lexeme>, Atom), &'static str> {
    assert!(!lexed.is_empty());

    let mut lexed = lexed;
    // Rust does not seem to support inverted = let Op = lexed[0]; :(
    let inverted: bool;
    if let Op('^') = &lexed[0] {
        lexed = lexed[1..].to_vec();
        inverted = true;
    } else {
        inverted = false;
    };

    // Should not have to check i < len() since bracketing should be correct
    let mut all_chars = HashSet::new();
    let mut i = 0;
    while lexed[i] != RSquare {
        let (skipped, chars) = parse_class_member(&lexed[i..].to_vec())?;
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
        LSquare => Ok(parse_character_class(lexed[1..].to_vec())?),
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

fn parse_operation(
    lexed: Vec<Lexeme>,
) -> Result<(Vec<Lexeme>, Box<dyn Fn(Atom) -> Operation>), &'static str> {
    let remaining = lexed[1..].to_vec();
    match lexed[0] {
        Op('+') => Ok((remaining, Box::new(|a| Operation::Plus(a)))),
        Op('?') => Ok((remaining, Box::new(|a| Operation::Questioned(a)))),
        Op('*') => Ok((remaining, Box::new(|a| Operation::Star(a)))),
        Op(_) => Err("Unsupported operation"),
        _ => Err("Not an operation"),
    }
}

#[cfg(test)]
mod tests {
    mod parser_tests {
        use super::super::*;

        #[test]
        fn parse_range_test() {
            let possible_range = parse_range(&vec![Ch('A'), Op('-'), Ch('Z')]);
            let expected_range = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".chars().collect();
            assert_eq!(possible_range, Some(expected_range));

            let possible_range = parse_range(&vec![Ch('a'), Ch('-'), Ch('z')]);
            assert!(possible_range.is_none());
        }

        #[test]
        fn parse_class_member_test() {
            let (skipped, chars) = parse_class_member(&vec![Ch('m')]).expect("Failure");
            assert_eq!(skipped, 1);
            assert_eq!(chars, ['m'].iter().cloned().collect());

            let (skipped, chars) = parse_class_member(&vec![Meta('w')]).expect("Failure");
            let expected_range = "abcdefghijklmnopqrstuvwxyz0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                .chars()
                .collect::<HashSet<char>>();
            assert_eq!(skipped, 1);
            assert_eq!(chars, expected_range);

            let expected_range = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                .chars()
                .collect::<HashSet<char>>();
            let (skipped, chars) =
                parse_class_member(&vec![Ch('A'), Op('-'), Ch('Z')]).expect("Failure");

            assert_eq!(skipped, 3);
            assert_eq!(chars, expected_range);

            let result = parse_class_member(&vec![Meta('z'), Ch('o'), Ch('d')]);
            assert!(result.is_err());
        }

        #[test]
        fn parse_class_test() {
            let (remaining, class) =
                parse_character_class(vec![Op('^'), Ch(')'), Ch('8'), RSquare])
                    .expect("Failure with char class");

            let disallowed_chars = "8)".chars().collect::<HashSet<char>>();
            assert!(remaining.is_empty());
            assert_eq!(
                class,
                Class(true, AllowedChars::Restricted(disallowed_chars))
            );

            let (remaining, class) =
                parse_character_class(vec![Ch('?'), Ch('a'), Op('-'), Ch('d'), RSquare, Ch('r')])
                    .expect("Failure with char class");
            assert_eq!(remaining, vec![Ch('r')]);
            let allowed = "?abcd".chars().collect::<HashSet<char>>();

            if let Class(b, AllowedChars::Restricted(chars)) = class {
                assert!(!b);
                assert_eq!(chars, allowed);
            } else {
                panic!("Incorrect pattern");
            }
        }

        #[test]
        fn parse_atom_test() {
            let (remaining, atom) = parse_atom(vec![Meta('w')]).expect("Failure parsing meta char");
            let mut expected_range =
                "abcdefghijklmnopqrstuvwxyz0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                    .chars()
                    .collect::<HashSet<char>>();

            assert!(remaining.is_empty());
            assert_eq!(
                atom,
                Class(false, AllowedChars::Restricted(expected_range.clone()))
            );

            let (remaining, atom) = parse_atom(vec![Ch('a'), LSquare, Meta('w'), Ch('?'), RSquare])
                .expect("Failure parsing char");

            expected_range.insert('?');
            assert_eq!(remaining, vec![LSquare, Meta('w'), Ch('?'), RSquare]);
            assert_eq!(atom, ACh('a'));

            let (remaining, atom) = parse_atom(remaining).expect("Failure parsing char class");
            assert!(remaining.is_empty());

            // TODO this is unsatisfying, I would prefer commented line after this but for some reason that doesnt
            // seem to agree on equality
            if let Class(b, AllowedChars::Restricted(actual_chars)) = atom {
                assert!(!b);
                assert_eq!(actual_chars, expected_range);
            } else {
                panic!("Did not return character class");
            }

            // assert_eq!(atom, Class(false, AllowedChars::Restricted(expected_range)));

            let (remaining, atom) = parse_atom(vec![
                Meta('.'),
                LSquare,
                Op('^'),
                Ch(')'),
                Ch('8'),
                RSquare,
                Op('*'),
            ])
            .expect("Failure parsing Unrestricted char");

            assert_eq!(
                remaining,
                vec![LSquare, Op('^'), Ch(')'), Ch('8'), RSquare, Op('*')]
            );

            assert_eq!(atom, Class(false, AllowedChars::Unrestricted));

            let (remaining, atom) =
                parse_atom(remaining).expect("Failure parsing inverted char class");

            let disallowed_chars = ")8".chars().collect();
            assert_eq!(remaining, vec![Op('*')]);
            assert_eq!(
                atom,
                Class(true, AllowedChars::Restricted(disallowed_chars))
            );
        }

        fn parse_operation_test() {
            let results = [Op('*'), Op('?'), Op('+')]
                .iter()
                .map(|op| parse_operation(vec![op.clone()]).expect("Parse failed").1)
                .map(|f| f(ACh('a')))
                .collect::<Vec<Operation>>();

            assert_eq!(
                vec![
                    Operation::Star(ACh('a')),
                    Operation::Questioned(ACh('a')),
                    Operation::Plus(ACh('a'))
                ],
                results
            );
        }

        fn parse_term(lexed: Vec<Lexeme>) -> Result<(Vec<Lexeme>, Term), &'static str> {
            Err("Not yet implemented")
        }
    }
}
