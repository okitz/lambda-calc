use crate::tokenizer;
use crate::tokenizer::Token;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::{Rc, Weak};

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
    Abs(AbsNode),
    Apply(ApplyNode),
    Var(VarNode),
}

#[derive(Debug, Clone)]
pub struct AbsNode {
    parent: RefCell<Weak<Tree>>,
    var: String,
    subterm: RefCell<Rc<Tree>>,
}

#[derive(Debug, Clone)]
pub struct ApplyNode {
    parent: RefCell<Weak<Tree>>,
    left: RefCell<Rc<Tree>>,
    right: RefCell<Rc<Tree>>,
    is_redex: bool,
}

#[derive(Debug, Clone)]
pub struct VarNode {
    parent: RefCell<Weak<Tree>>,
    var: String,
}

impl Tree {
    pub fn find_leftmost_redex(rc: &Rc<Tree>) -> Option<Rc<Tree>> {
        match **rc {
            Tree::Abs(ref node) => {
                let subt = node.subterm.borrow();
                Tree::find_leftmost_redex(&*subt)
            }
            Tree::Apply(ref node) => {
                if node.is_redex {
                    Some(Rc::clone(&rc))
                } else {
                    let left = node.left.borrow();
                    let right = node.right.borrow();
                    Tree::find_leftmost_redex(&*left)
                        .or_else(move || Tree::find_leftmost_redex(&*right))
                }
            }
            Tree::Var(_) => None,
        }
    }

    pub fn stringify(&self) -> String {
        match self {
            Tree::Abs(node) => {
                let subt = node.subterm.borrow();
                format!("λ{}.{}", node.var, subt.stringify())
            }
            Tree::Apply(node) => {
                let make_substr = |nd: &Tree| match *nd {
                    Tree::Var(_) => nd.stringify(),
                    _ => format!("({})", nd.stringify()),
                };
                let lstr = make_substr(&*node.left.borrow());
                let rstr = make_substr(&*node.right.borrow());
                lstr + " " + &rstr
            }
            Tree::Var(node) => node.var.clone(),
        }
    }

    // pub fn substitute(&self, param: &String, tr: Box<Tree>) {
    //     match self {
    //         Tree::Abs(node) => {
    //             if node.var != param {
    //                 node.subterm.substitute(param, tr);
    //             }
    //         }
    //         Tree::Apply(node) => {
    //             node.left.substitute(param, tr);
    //             node.right.substitute(param, tr);
    //         }
    //         Tree::Var(vname) => {
    //             if vname = param {
    //                 match node.parent {

    //                 }
    //             }
    //         }
    //     }
    // }
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

pub fn parse(mut tstream: VecDeque<Token>) -> Result<Option<Rc<Tree>>, ParseError> {
    let tree = term(&mut tstream)?;
    Ok(tree)
}

fn term(tok: &mut VecDeque<Token>) -> Result<Option<Rc<Tree>>, ParseError> {
    if consume_token(tok, "λ").is_ok() || consume_token(tok, "\\").is_ok() {
        let mut params = VecDeque::new();
        while let Some(ref rc) = primary(tok)? {
            if let Tree::Var(ref node) = **rc {
                params.push_back(node.var.clone());
            } else {
                break;
            }
        }

        if let Some(subt) = term(tok)? {
            let mut tr = Rc::new(Tree::Abs(AbsNode {
                parent: RefCell::new(Weak::new()),
                var: params.pop_back().expect("no parameter!"),
                subterm: RefCell::new(subt),
            }));
            while let Some(param_name) = params.pop_back() {
                tr = Rc::new(Tree::Abs(AbsNode {
                    parent: RefCell::new(Weak::new()),
                    var: param_name,
                    subterm: RefCell::new(tr),
                }));
            }
            Ok(Some(tr))
        } else {
            Err(ParseError::SyntaxError)
        }
    } else if !tok.is_empty() {
        let mut subterms = VecDeque::new();
        while let Some(subt) = primary(tok)? {
            subterms.push_back(subt);
        }

        match subterms.len() {
            0 => Err(ParseError::SyntaxError),
            1 => Ok(Some(subterms.pop_front().unwrap())),
            _ => {
                let left = subterms.pop_front().unwrap();
                let right = subterms.pop_front().unwrap();
                let mut tr = Rc::new(Tree::Apply(ApplyNode {
                    parent: RefCell::new(Weak::new()),
                    is_redex: matches!(*left, Tree::Abs(_)),
                    left: RefCell::new(left),
                    right: RefCell::new(right),
                }));
                while let Some(subt) = subterms.pop_front() {
                    tr = Rc::new(Tree::Apply(ApplyNode {
                        parent: RefCell::new(Weak::new()),
                        is_redex: matches!(*tr, Tree::Abs(_)),
                        left: RefCell::new(tr),
                        right: RefCell::new(subt),
                    }));
                }
                Ok(Some(tr))
            }
        }
    } else {
        Ok(None)
    }
}

fn primary(tok: &mut VecDeque<Token>) -> Result<Option<Rc<Tree>>, ParseError> {
    if consume_token(tok, "(").is_ok() {
        term(tok)
    } else if consume_token(tok, ")").is_ok() || consume_token(tok, ".").is_ok() || at_eof(tok) {
        Ok(None)
    } else if let Some(Token::Var(s)) = tok.pop_front() {
        Ok(Some(Rc::new(Tree::Var(VarNode {
            parent: RefCell::new(Weak::new()),
            var: s,
        }))))
    } else {
        Err(ParseError::SyntaxError)
    }
}
