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
impl Error for InvalidToken {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

fn is_valid_char(s: &str) -> bool {
    let c = s.chars().next().unwrap();
    return ('a' <= c && c <= 'z') || "\\λ.():".contains(c);
}

#[derive(Debug, PartialEq)]
pub enum Token {
    TOKEN(String),
    EOF,
}
impl Token {
    fn new(input: &mut Chars<'_>) -> Result<Token, InvalidToken> {
        let input = input.skip_while(|c| *c == ' ');
        let s = input.take(1).collect::<String>();

        if s == "" {
            Ok(Token::EOF)
        } else if is_valid_char(&s) {
            Ok(Token::TOKEN(s))
        } else {
            Err(InvalidToken)
        }
    }
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, InvalidToken> {
    println!("tokenize `{}`", input);
    let mut input: Chars<'_> = input.chars();
    let mut v = Vec::new();
    loop {
        let tok = Token::new(&mut input)?;
        let is_eof = tok == Token::EOF;
        v.push(tok);
        if is_eof {
            break;
        }
    }
    return Ok(v);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_single_symbols() {
        assert_eq!(Err(InvalidToken), tokenize("1"));
        let c2t = |c| Token::TOKEN(String::from(c));
        let test_str = "\\x.(x y)";
        let mut expect: Vec<Token> = test_str
            .chars()
            .into_iter()
            .filter(|c| *c != ' ')
            .map(c2t)
            .collect();
        expect.push(Token::EOF);
        assert_eq!(expect, tokenize(test_str).unwrap());
    }
}
