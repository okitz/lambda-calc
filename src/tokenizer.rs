use std::collections::VecDeque;
use std::error::Error;
use std::fmt;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub struct InvalidToken;
impl fmt::Display for InvalidToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid token")
    }
}
impl Error for InvalidToken {}

fn is_var(s: &str) -> bool {
    let c = s.chars().next().unwrap();
    c.is_ascii_lowercase() || "\\λ.():".contains(c)
}
fn is_symbol(s: &str) -> bool {
    let c = s.chars().next().unwrap();
    c.is_ascii_lowercase() || "\\λ.():".contains(c)
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Var(String),
    Symbol(String),
    EOF,
}
impl Token {
    pub fn new(s: String) -> Result<Token, InvalidToken> {
        if s.is_empty() {
            Ok(Token::EOF)
        } else if is_var(&s) {
            Ok(Token::Var(s))
        } else if is_symbol(&s) {
            Ok(Token::Symbol(s))
        } else {
            Err(InvalidToken)
        }
    }
    pub fn new_from_chars(input: &mut Chars<'_>) -> Result<Token, InvalidToken> {
        let input = input.skip_while(|c| *c == ' ');
        let s = input.take(1).collect::<String>();
        Token::new(s)
    }
}

pub fn tokenize(input: &str) -> Result<VecDeque<Token>, InvalidToken> {
    println!("tokenize '{}'", input);
    let mut input: Chars<'_> = input.chars();
    let mut tstream = VecDeque::new();
    loop {
        let tok = Token::new_from_chars(&mut input)?;
        let is_eof = tok == Token::EOF;
        tstream.push_back(tok);
        if is_eof {
            break;
        }
    }
    Ok(tstream)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_single_symbols() {
        assert_eq!(Err(InvalidToken), tokenize("1"));
        let c2t = |c| Token::Var(String::from(c));
        let test_str = "\\x.(x y)";
        let mut expect: VecDeque<Token> = test_str.chars().filter(|c| *c != ' ').map(c2t).collect();
        expect.push_back(Token::EOF);
        assert_eq!(expect, tokenize(test_str).unwrap());
    }
}
