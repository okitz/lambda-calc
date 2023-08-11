use crate::tokenizer::Token;
use std::collections::VecDeque;

// Grammar
// term = primary+
//      | ("λ" | "\") var "." term
// primary = var | "(" term ")"

#[derive(Debug)]
struct UnexpectedTokenError;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken,
    SyntaxError,
}

impl std::error::Error for ParseError {}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken => write!(f, "unexpected token"),
            ParseError::SyntaxError => write!(f, "syntax error"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Tree {
    Var(String),
    Abs(String, Box<Tree>),
    Apply(Box<Tree>, Box<Tree>),
}

fn consume_token(tok: &mut VecDeque<Token>, target: &str) -> Result<(), ParseError> {
    if tok.len() > 0 && tok[0] == Token::Token(String::from(target)) {
        tok.pop_front();
        Ok(())
    } else {
        Err(ParseError::UnexpectedToken)
    }
}

fn at_eof(tok: &VecDeque<Token>) -> bool {
    tok[0] == Token::EOF
}

pub fn parse(mut tstream: VecDeque<Token>) -> Result<Option<Tree>, ParseError> {
    let tree = term(&mut tstream)?;
    Ok(tree)
}

fn term(tok: &mut VecDeque<Token>) -> Result<Option<Tree>, ParseError> {
    if consume_token(tok, "λ").is_ok() || consume_token(tok, "\\").is_ok() {
        let mut params = VecDeque::new();
        while let Some(Tree::Var(param_name)) = primary(tok)? {
            params.push_back(param_name);
        }

        // consume_token(tok, ".")?;
        if let Some(subt) = term(tok)? {
            let mut tr = Tree::Abs(params.pop_back().expect("no parameter!"), Box::new(subt));
            while let Some(param_name) = params.pop_back() {
                tr = Tree::Abs(param_name, Box::new(tr));
            }
            Ok(Some(tr))
        } else {
            Err(ParseError::SyntaxError)
        }
    } else if tok.len() > 0 {
        let mut subterms = VecDeque::new();
        while let Some(subt) = primary(tok)? {
            subterms.push_back(subt);
        }

        match subterms.len() {
            0 => Err(ParseError::SyntaxError),
            1 => Ok(Some(subterms.pop_front().unwrap())),
            _ => {
                let mut tr = Tree::Apply(
                    Box::new(subterms.pop_front().unwrap()),
                    Box::new(subterms.pop_front().unwrap()),
                );
                while let Some(subt) = subterms.pop_front() {
                    tr = Tree::Apply(Box::new(tr), Box::new(subt));
                }
                Ok(Some(tr))
            }
        }
    } else {
        Ok(None)
    }
}

fn primary(tok: &mut VecDeque<Token>) -> Result<Option<Tree>, ParseError> {
    if consume_token(tok, "(").is_ok() {
        let t = term(tok);
        t
    } else if consume_token(tok, ")").is_ok() || consume_token(tok, ".").is_ok() || at_eof(tok) {
        Ok(None)
    } else if let Some(Token::Token(s)) = tok.pop_front() {
        Ok(Some(Tree::Var(String::from(s))))
    } else {
        Err(ParseError::SyntaxError)
    }
}