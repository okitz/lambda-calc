use crate::tokenizer;
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

impl From<tokenizer::InvalidToken> for ParseError {
    fn from(_value: tokenizer::InvalidToken) -> Self {
        ParseError::UnexpectedToken
    }
}

#[derive(Debug, Clone)]
pub enum Tree {
    Var(String),
    Abs(String, Box<Tree>),
    Apply(Box<Tree>, Box<Tree>),
}

fn consume_token(tok: &mut VecDeque<Token>, target: &str) -> Result<(), ParseError> {
    if tok.is_empty() {
        Err(ParseError::SyntaxError)
    } else {
        let target = Token::new(target.to_string())?;
        match tok[0] {
            _ if tok[0] == target => {
                tok.pop_front();
                Ok(())
            }
            _ => Err(ParseError::UnexpectedToken),
        }
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
    } else if tok.is_empty() {
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
        term(tok)
    } else if consume_token(tok, ")").is_ok() || consume_token(tok, ".").is_ok() || at_eof(tok) {
        Ok(None)
    } else if let Some(Token::Var(s)) = tok.pop_front() {
        Ok(Some(Tree::Var(s)))
    } else {
        Err(ParseError::SyntaxError)
    }
}
