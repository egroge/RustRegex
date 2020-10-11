use crate::lexer;

use lexer::{Lexeme, Lexeme::*};
use std::collections::HashSet;
use Atom::*;

pub type Expr = Vec<Term>;
type OpFunc = dyn Fn(Atom) -> Operation;

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    TAtom(Atom),
    TOp(Operation),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operation {
    Plus(Atom),
    Star(Atom),
    Questioned(Atom),
}

#[derive(Debug, PartialEq, Clone)]
pub enum AllowedChars {
    Unrestricted,
    Restricted(HashSet<char>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    ACh(char),
    Class(bool, AllowedChars),
    SubExpr(Expr), // Does not currently support non capture groups
}

fn parse_range(lexed: &[Lexeme]) -> Option<HashSet<char>> {
    // PRE: lexed has size >= 3
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

fn parse_class_member(lexed: &[Lexeme]) -> Result<(Vec<Lexeme>, HashSet<char>), &'static str> {
    let digits = "0123456789".chars().collect();

    let latin = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        .chars()
        .collect();

    let whitespace = " \t".chars().collect();

    if lexed.len() >= 3 {
        // Possible that there is a range
        if let Some(range) = parse_range(&lexed) {
            return Ok((lexed[3..].to_vec(), range));
        }
    }

    // Cannot be a range, so must be a single char
    let remaining = lexed[1..].to_vec();
    match &lexed[0] {
        RSquare => Ok((remaining, HashSet::new())),
        Meta(c) => match c {
            'd' => Ok((remaining, digits)),
            's' => Ok((remaining, whitespace)),
            'w' => Ok((remaining, digits.union(&latin).cloned().collect())),
            'b' => Ok((remaining, "\n".chars().collect())),
            _ => Err("Unknown meta token in character class!"),
        },
        Ch(c) => {
            let mut set = HashSet::new();
            set.insert(*c);
            Ok((remaining, set))
        }
        t => {
            println!("Token: {:?}", t);
            Err("Misplaced token in character class!")
        }
    }
}

fn parse_character_class(lexed: Vec<Lexeme>) -> Result<(Vec<Lexeme>, Atom), &'static str> {
    assert!(!lexed.is_empty());

    let (mut lexed, inverted) = if let Op('^') = &lexed[0] {
        (lexed[1..].to_vec(), true)
    } else {
        (lexed, false)
    };

    // Should not have to check i < len() since bracketing should be correct
    let mut all_chars = HashSet::new();
    while lexed[0] != RSquare {
        let (remaining, chars) = parse_class_member(&lexed)?;
        lexed = remaining;
        all_chars.extend(chars);
    }

    Ok((
        lexed[1..].to_vec(),
        Class(inverted, AllowedChars::Restricted(all_chars)),
    ))
}

fn parse_atom(lexed: Vec<Lexeme>) -> Result<(Vec<Lexeme>, Atom), &'static str> {
    use Atom::*;
    match &lexed[0] {
        LSquare => Ok(parse_character_class(lexed[1..].to_vec())?),
        Ch(c) => Ok((lexed[1..].to_vec(), ACh(*c))),
        Meta(c) => {
            if *c == '.' {
                Ok((
                    lexed[1..].to_vec(),
                    Class(false, AllowedChars::Unrestricted),
                ))
            } else {
                let (remaining, chars) = parse_class_member(&lexed)?;
                Ok((remaining, Class(false, AllowedChars::Restricted(chars))))
            }
        }
        LRound => {
            let (remaining, expr) = parse_expression(lexed[1..].to_vec())?;
            Ok((remaining, SubExpr(expr)))
        }
        _ => Err("Non atom found"),
    }
}

fn parse_operation(lexed: Vec<Lexeme>) -> Result<(Vec<Lexeme>, Box<OpFunc>), &'static str> {
    let remaining = lexed[1..].to_vec();

    match lexed[0] {
        Op('+') => Ok((remaining, Box::new(Operation::Plus))),
        Op('?') => Ok((remaining, Box::new(Operation::Questioned))),
        Op('*') => Ok((remaining, Box::new(Operation::Star))),
        Op(_) => Err("Unsupported operation"),
        _ => Err("Not an operation"),
    }
}

fn parse_term(lexed: Vec<Lexeme>) -> Result<(Vec<Lexeme>, Term), &'static str> {
    assert!(!lexed.is_empty());

    let (remaining, atom) = parse_atom(lexed)?;

    if remaining.is_empty() {
        return Ok((remaining, Term::TAtom(atom)));
    }

    if let Op(_) = remaining[0] {
        let (remaining, func) = parse_operation(remaining)?;
        return Ok((remaining, Term::TOp(func(atom))));
    }

    Ok((remaining, Term::TAtom(atom)))
}

fn parse_expression(lexed: Vec<Lexeme>) -> Result<(Vec<Lexeme>, Expr), &'static str> {
    // NOTE: If this is called due to a subexpr, the LRound starting it should already have been removed
    let mut expr = vec![];
    let mut lexed = lexed;

    while !lexed.is_empty() {
        if let RRound = lexed[0] {
            return Ok((lexed[1..].to_vec(), expr));
        }

        // Unfortunately rust does not yet support the more elegant pattern matched version of
        // this
        let result = parse_term(lexed)?;
        lexed = result.0;
        expr.push(result.1);
    }

    Ok((lexed, expr))
}

pub fn parse(lexed: Vec<Lexeme>) -> Result<Expr, &'static str> {
    let (remaining, parsed) = parse_expression(lexed)?;

    if !remaining.is_empty() {
        Err("Some of expression was left over after parsing. Is it malformed?")
    } else {
        Ok(parsed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let (remaining, chars) = parse_class_member(&vec![Ch('m')]).expect("Failure");
        assert_eq!(remaining, vec![]);
        assert_eq!(chars, ['m'].iter().cloned().collect());

        let (remaining, chars) = parse_class_member(&vec![Meta('w')]).expect("Failure");
        let expected_range = "abcdefghijklmnopqrstuvwxyz0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            .chars()
            .collect::<HashSet<char>>();
        assert_eq!(remaining, vec![]);
        assert_eq!(chars, expected_range);

        let expected_range = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            .chars()
            .collect::<HashSet<char>>();
        let (remaining, chars) =
            parse_class_member(&vec![Ch('A'), Op('-'), Ch('Z'), Ch('m')]).expect("Failure");

        assert_eq!(remaining, vec![Ch('m')]);
        assert_eq!(chars, expected_range);

        let result = parse_class_member(&vec![Meta('z'), Ch('o'), Ch('d')]);
        assert!(result.is_err());
    }

    #[test]
    fn parse_class_test() {
        let (remaining, class) = parse_character_class(vec![Op('^'), Ch(')'), Ch('8'), RSquare])
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

        let allowed = "?abcd".chars().collect::<HashSet<char>>();
        assert_eq!(remaining, vec![Ch('r')]);
        assert_eq!(class, Class(false, AllowedChars::Restricted(allowed)));
    }

    #[test]
    fn parse_atom_test() {
        let (remaining, atom) = parse_atom(vec![Meta('w')]).expect("Failure parsing meta char");
        let mut expected_range = "abcdefghijklmnopqrstuvwxyz0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ"
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

        assert_eq!(atom, Class(false, AllowedChars::Restricted(expected_range)));

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

        let (remaining, atom) = parse_atom(remaining).expect("Failure parsing inverted char class");

        let disallowed_chars = ")8".chars().collect();
        assert_eq!(remaining, vec![Op('*')]);
        assert_eq!(
            atom,
            Class(true, AllowedChars::Restricted(disallowed_chars))
        );

        // Extended tests for parsing subexpressions
        let (remaining, subexpr) = parse_atom(vec![
            LRound,
            Ch('I'),
            Ch('C'),
            Ch('L'),
            Op('?'),
            RRound,
            Op('?'),
        ])
        .expect("Failure parsing subexpression");

        use Operation::*;
        use Term::*;

        let expected_expr = SubExpr(vec![
            TAtom(ACh('I')),
            TAtom(ACh('C')),
            TOp(Questioned(ACh('L'))),
        ]);

        assert_eq!(remaining, vec![Op('?')]);
        assert_eq!(subexpr, expected_expr);
    }

    #[test]
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

    #[test]
    fn parse_term_test() {
        use Operation::*;
        use Term::*;
        let (remaining, atom) = parse_term(vec![Meta('w'), Op('*'), Meta('.')])
            .expect("Failure parsing meta char star");
        let expected_range = "abcdefghijklmnopqrstuvwxyz0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            .chars()
            .collect::<HashSet<char>>();

        assert_eq!(remaining, vec![Meta('.')]);
        assert_eq!(
            atom,
            TOp(Star(Class(false, AllowedChars::Restricted(expected_range))))
        );
    }

    #[test]
    fn parse_expr_test() {
        use Atom::*;
        use Operation::*;
        use Term::*;

        let (remaining, expr) = parse_expression(vec![
            Ch('a'),
            LSquare,
            Ch('0'),
            Op('-'),
            Ch('2'),
            RSquare,
            Op('+'),
            RRound,
            Ch('a'),
        ])
        .expect("Failure parsing expression");

        let expected_expr = vec![
            TAtom(ACh('a')),
            TOp(Plus(Class(
                false,
                AllowedChars::Restricted("012".chars().collect::<HashSet<char>>()),
            ))),
        ];

        assert_eq!(remaining, vec![Ch('a')]);
        assert_eq!(expr, expected_expr);

        let (remaining, expr) =
            parse_expression(remaining).expect("Failure paring single char expression");
        assert!(remaining.is_empty());
        assert_eq!(expr, vec![TAtom(ACh('a'))]);
    }

    #[test]
    fn parse_test() {
        use Atom::*;
        use Operation::*;
        use Term::*;

        let expr = parse(vec![
            Meta('s'),
            LRound,
            Ch('I'),
            Ch('C'),
            Ch('L'),
            Op('?'),
            RRound,
            Op('*'),
        ])
        .expect("Failure parsing expression");

        let expected_expr = vec![
            TAtom(Class(
                false,
                AllowedChars::Restricted(" \t".chars().collect::<HashSet<char>>()),
            )),
            TOp(Star(SubExpr(vec![
                TAtom(ACh('I')),
                TAtom(ACh('C')),
                TOp(Questioned(ACh('L'))),
            ]))),
        ];

        assert_eq!(expr, expected_expr);
    }
}
