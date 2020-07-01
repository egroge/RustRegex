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
            for c in s.chars() {
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
            Some(match_atom(atom, s).unwrap_or((String::new(), s[1..].to_string())))
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
    let mut expr = expr;
    expr.iter()
        .fold(Some((String::new(), s.to_string())), |acc, term| {
            let (matched, remaining) = acc?;
            let (next_match, remaining) = match_term(term.clone(), remaining.as_str())?;
            Some((format!("{}{}", matched, next_match), remaining))
        })
}

fn match_regex(expr: Expr, s: &str) -> Option<String> {
    let (matched, remaining) = match_expression(expr, s)?;
    if !remaining.is_empty() {
        panic!("Some regex left over");
    } else {
        Some(matched)
    }
}

// TODO use references instead of cloning everything
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
