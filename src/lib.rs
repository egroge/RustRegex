#[path = "utils/parser.rs"]
mod parser;

use parser::{AllowedChars, Atom, Atom::*, Expr, Operation, Operation::*, Term, Term::*};

fn match_character(ch: Atom, s: &str) -> Option<(String, String)> {
    match ch {
        ACh(c) => {
            let first = s.chars().next()?;
            if c == first {
                Some((c.to_string(), s[1..].to_string()))
            } else {
                None
            }
        }
        _ => None, // TODO should this be a panic instead?
    }
}

fn match_character_class(class: Atom, s: &str) -> Option<(String, String)> {
    let first = s.chars().next()?;
    let matched = (first.to_string(), s[1..].to_string());
    match class {
        Class(inverted, AllowedChars::Unrestricted) => {
            if inverted {
                // no character accepted
                None
            } else {
                Some(matched) // Any character accepted
            }
        }
        Class(inverted, AllowedChars::Restricted(chars)) => {
            if chars.contains(&first) ^ inverted {
                Some(matched)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn match_atom(atom: Atom, s: &str) -> Option<(String, String)> {
    match atom {
        ACh(_) => match_character(atom, s),
        Class(_, _) => match_character_class(atom, s),
        SubExpr(expr) => match_expression(expr, s),
    }
}

fn match_op(op: Operation, s: &str) -> Option<(String, String)> {
    match op {
        Plus(atom) => {
            let (first_match, remaining) = match_atom(atom.clone(), s)?;
            let (other_matches, remaining) =
                match_op(Star(atom), remaining.as_str()).unwrap_or(("".to_string(), remaining));
            Some((first_match + &other_matches, remaining))
        }
        Star(atom) => {
            let (mut matched, mut remaining) = (String::new(), s.to_string());
            for _ in s.chars() {
                let result = match_atom(atom.clone(), remaining.as_str());
                if result.is_some() {
                    let result = result.unwrap();
                    matched = format!("{}{}", matched, result.0);
                    remaining = result.1;
                } else {
                    break;
                }
            }
            Some((matched.to_string(), remaining.to_string()))
        }
        Questioned(atom) => {
            Some(match_atom(atom, s).unwrap_or((String::new(), s.to_string())))
        }
    }
}

fn match_term(term: Term, s: &str) -> Option<(String, String)> {
    match term {
        TAtom(atom) => match_atom(atom, s),
        TOp(op) => match_op(op, s),
    }
}

fn match_expression(expr: Expr, s: &str) -> Option<(String, String)> {
    expr.iter()
        .fold(Some((String::new(), s.to_string())), |acc, term| {
            let (matched, remaining) = acc?;
            let (next_match, remaining) = match_term(term.clone(), remaining.as_str())?;
            Some((format!("{}{}", matched, next_match), remaining))
        })
}

fn match_regex(expr: Expr, s: &str) -> Option<String> {
    Some(match_expression(expr, s)?.0)
}

pub fn find(expr: Expr, s: &str) -> Option<(String, u32)> {
    let mut i = 0;
    while i < s.len() {
        if let Some(matched) = match_regex(expr.clone(), &s[i..]) {
            return Some((matched, i as u32));
        }

        i += 1;
    }
    None
}

// TODO write tests
#[cfg(test)]
mod match_tests {
    use super::*;

    #[test]
    fn match_character_test() {
        let (matched, remaining) =
            match_character(ACh('a'), "apple").expect("Failure matching character");

        assert_eq!(matched, String::from("a"));
        assert_eq!(remaining, String::from("pple"));

        assert!(match_character(ACh('l'), "apple").is_none());
        assert!(match_character(Class(false, AllowedChars::Unrestricted), "some string").is_none());
    }

    #[test]
    fn match_character_class_test() {
        use std::collections::HashSet;

        let chars = "iclICL".chars().collect::<HashSet<char>>();
        let (matched, remaining) =
            match_character_class(Class(false, AllowedChars::Restricted(chars.clone())), "icl")
                .expect("Failure with char class");

        assert_eq!(matched, String::from("i"));
        assert_eq!(remaining, String::from("cl"));

        assert!(
            match_character_class(Class(true, AllowedChars::Restricted(chars)), "icl").is_none()
        );

        let (matched, remaining) =
            match_character_class(Class(false, AllowedChars::Unrestricted), "icl")
                .expect("Failure with char class");

        assert_eq!(matched, String::from("i"));
        assert_eq!(remaining, String::from("cl"));

        let chars = "iclICL".chars().collect::<HashSet<char>>();
        assert!(match_character_class(
            Class(false, AllowedChars::Restricted(chars.clone())),
            "(icl)"
        )
        .is_none());
    }

    #[test]
    fn match_atom_test() {
        use std::collections::HashSet;

        let (matched, remaining) = match_atom(ACh('a'), "apple").expect("Failure with AChr");
        assert_eq!(matched, String::from("a"));
        assert_eq!(remaining, String::from("pple"));

        let chars = "iclICL".chars().collect::<HashSet<char>>();
        let (matched, remaining) = match_atom(
            Class(false, AllowedChars::Restricted(chars.clone())),
            "icarus",
        )
        .expect("Failure with char class");

        assert_eq!(matched, String::from("i"));
        assert_eq!(remaining, String::from("carus"));

        let (matched, remaining) = match_atom(
            SubExpr(vec![TAtom(ACh('c')), TAtom(ACh('a')), TAtom(ACh('r'))]),
            remaining.as_str(),
        )
        .expect("Failure on subexpression");

        assert_eq!(matched, "car");
        assert_eq!(remaining, "us");

        assert!(match_atom(ACh('a'), "bell").is_none());
    }

    #[test]
    fn match_op_test() {
        // Plus
        let (matched, remaining) = match_op(Plus(ACh('a')), "aab").expect("Failure on plus op");
        assert_eq!(matched, String::from("aa"));
        assert_eq!(remaining, String::from("b"));

        let (matched, remaining) = match_op(Plus(ACh('a')), "ab").expect("Failure on plus op");
        assert_eq!(matched, String::from("a"));
        assert_eq!(remaining, String::from("b"));

        assert!(match_op(Plus(ACh('a')), "b").is_none());

        // Star
        let (matched, remaining) = match_op(Star(ACh('a')), "aab").expect("Failure on star op");
        assert_eq!(matched, String::from("aa"));
        assert_eq!(remaining, String::from("b"));

        let (matched, remaining) = match_op(Star(ACh('a')), "ab").expect("Failure on star op");
        assert_eq!(matched, String::from("a"));
        assert_eq!(remaining, String::from("b"));

        let (matched, remaining) = match_op(Star(ACh('a')), "b").expect("Failure on star op");
        assert_eq!(matched, String::from(""));
        assert_eq!(remaining, String::from("b"));

        // Questioned
        let (matched, remaining) =
            match_op(Questioned(ACh('a')), "aab").expect("Failure on questioned op");
        assert_eq!(matched, String::from("a"));
        assert_eq!(remaining, String::from("ab"));

        let (matched, remaining) =
            match_op(Questioned(ACh('a')), "ab").expect("Failure on questioned op");
        assert_eq!(matched, String::from("a"));
        assert_eq!(remaining, String::from("b"));

        let (matched, remaining) =
            match_op(Questioned(ACh('a')), "b").expect("Failure on questioned op");
        assert_eq!(matched, String::from(""));
        assert_eq!(remaining, String::from("b"));
    }

    #[test]
    fn match_term_test() {
        let (matched, remaining) =
            match_term(TAtom(ACh('a')), "apple").expect("Failure with TAtom");
        assert_eq!(matched, String::from("a"));
        assert_eq!(remaining, String::from("pple"));

        let (matched, remaining) = match_term(TOp(Plus(ACh('a'))), "aab").expect("Failure on TOp");
        assert_eq!(matched, String::from("aa"));
        assert_eq!(remaining, String::from("b"));
    }

    #[test]
    fn match_expression_test() {
        let yeet_regex = vec![
            TAtom(ACh('y')),
            TAtom(ACh('e')),
            TOp(Plus(ACh('e'))),
            TAtom(ACh('t')),
        ];
        let (matched, remaining) = match_expression(
            yeet_regex.clone(),
            "yeeeeeeeeeeeet is a thing lame people say",
        )
        .expect("Failure with expression");

        assert_eq!(matched, String::from("yeeeeeeeeeeeet"));
        assert_eq!(remaining, String::from(" is a thing lame people say"));

        assert!(match_expression(yeet_regex, "yet is a thing normal people say").is_none());
    }

    #[test]
    fn match_regex_test() {
        let yeet_regex = vec![
            TAtom(ACh('y')),
            TAtom(ACh('e')),
            TOp(Plus(ACh('e'))),
            TAtom(ACh('t')),
        ];

        assert!(match_regex(
            yeet_regex.clone(),
            "The word 'yeet' is a thing lame people say"
        )
        .is_none());
        assert_eq!(
            match_regex(yeet_regex.clone(), "yeeet!"),
            Some(String::from("yeeet"))
        );
    }

    #[test]
    fn find_test() {
        let yeet_regex = vec![
            TAtom(ACh('y')),
            TAtom(ACh('e')),
            TOp(Plus(ACh('e'))),
            TAtom(ACh('t')),
        ];

        assert_eq!(
            find(
                yeet_regex.clone(),
                "The word 'yeet' is a thing lame people say"
            ),
            Some((String::from("yeet"), 10))
        );

        assert!(find(
            yeet_regex.clone(),
            "The word 'yet' is a thing normal people say"
        )
        .is_none());
    }
}
