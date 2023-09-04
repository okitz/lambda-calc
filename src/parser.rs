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
pub struct Ast {
    pub root: RccellNode,
}

#[derive(Debug, Clone)]
pub enum NodeType {
    Abs { var: String, subterm: RccellNode },
    Apply { left: RccellNode, right: RccellNode },
    Var { var: String },
    Root { term: RccellNode },
}

#[derive(Debug, Clone)]
pub struct Node {
    node_type: NodeType,
    parent: Option<WkcellNode>,
    self_ref: WkcellNode,
}

impl Node {
    fn new(node_type: NodeType) -> Rc<RefCell<Node>> {
        let rc = Rc::new(RefCell::new(Node {
            node_type,
            parent: None,
            self_ref: Weak::new(),
        }));
        {
            rc.borrow_mut().self_ref = Rc::downgrade(&rc);
        }
        {
            // new_node を子ノードの parent として設定
            match &rc.borrow().node_type {
                NodeType::Root { term } => {
                    term.borrow_mut().parent = Some(Weak::clone(&rc.borrow().self_ref))
                }
                NodeType::Abs { var: _, subterm } => {
                    subterm.borrow_mut().parent = Some(Weak::clone(&rc.borrow().self_ref))
                }
                NodeType::Apply { left, right } => {
                    left.borrow_mut().parent = Some(Weak::clone(&rc.borrow().self_ref));
                    right.borrow_mut().parent = Some(Weak::clone(&rc.borrow().self_ref));
                }
                NodeType::Var { .. } => {}
            };
        }
        rc
    }
}

type RccellNode = Rc<RefCell<Node>>;
type WkcellNode = Weak<RefCell<Node>>;
type ParseResult = Result<Option<RccellNode>, ParseError>;
type AstResult = Result<Option<Ast>, ParseError>;
impl Ast {
    pub fn evaluate(&self) {
        let redex = self.root.borrow().find_leftmost_redex();
        if let Some(redex) = redex {
            println!("redex: {:?}", redex.borrow().stringify());
            redex.borrow().reduce();
        };
    }
}

impl Node {
    fn deep_clone(&self) -> RccellNode {
        let node_type = match &self.node_type {
            NodeType::Root { term } => NodeType::Root {
                term: term.borrow().deep_clone(),
            },
            NodeType::Abs { var, subterm } => NodeType::Abs {
                var: var.clone(),
                subterm: subterm.borrow().deep_clone(),
            },
            NodeType::Apply { left, right } => NodeType::Apply {
                left: left.borrow().deep_clone(),
                right: right.borrow().deep_clone(),
            },
            NodeType::Var { var } => NodeType::Var { var: var.clone() },
        };
        Node::new(node_type)
    }

    pub fn stringify(&self) -> String {
        match &self.node_type {
            NodeType::Root { term } => term.borrow().stringify(),
            NodeType::Abs { var, subterm } => {
                let subt = subterm.borrow();
                format!("λ{}.{}", var, subt.stringify())
            }
            NodeType::Apply { left, right } => {
                let make_substr = |nd: &Node| match nd.node_type {
                    NodeType::Var { .. } => nd.stringify(),
                    _ => format!("({})", nd.stringify()),
                };
                let lstr = make_substr(&left.borrow());
                let rstr = make_substr(&right.borrow());
                lstr + " " + &rstr
            }
            NodeType::Var { var } => var.clone(),
        }
    }

    pub fn find_leftmost_redex(&self) -> Option<RccellNode> {
        match &self.node_type {
            NodeType::Root { term } => term.borrow().find_leftmost_redex(),
            NodeType::Abs { subterm, .. } => subterm.borrow().find_leftmost_redex(),
            NodeType::Apply { left, right } => {
                if let NodeType::Abs { .. } = left.borrow().node_type {
                    self.self_ref.upgrade()
                } else {
                    left.borrow()
                        .find_leftmost_redex()
                        .or_else(|| right.borrow().find_leftmost_redex())
                }
            }
            NodeType::Var { .. } => None,
        }
    }

    pub fn substitute(&self, param: &String, src: RccellNode) -> RccellNode {
        match &self.node_type {
            NodeType::Root { term } => {
                let substituted = term.borrow().substitute(param, Rc::clone(&src));
                Node::new(NodeType::Root { term: substituted })
            }
            NodeType::Abs { var, subterm } => {
                if var != param {
                    let subterm = subterm.borrow().substitute(param, Rc::clone(&src));
                    Node::new(NodeType::Abs {
                        var: var.clone(),
                        subterm,
                    })
                } else {
                    self.self_ref.upgrade().unwrap()
                }
            }
            NodeType::Apply { left, right } => {
                let left = left.borrow().substitute(param, Rc::clone(&src));
                let right = right.borrow().substitute(param, Rc::clone(&src));
                Node::new(NodeType::Apply { left, right })
            }
            NodeType::Var { var } => {
                if var == param {
                    src.borrow().deep_clone()
                } else {
                    self.self_ref.upgrade().unwrap()
                }
            }
        }
    }

    fn reduce(&self) {
        if let NodeType::Apply {
            left: abs,
            right: src,
        } = &self.node_type
        {
            let abs = Rc::clone(abs);
            let src = Rc::clone(src);
            if let NodeType::Abs {
                var: prm,
                subterm: tar,
            } = &abs.borrow().node_type
            {
                let tar = Rc::clone(tar);
                let substituted_subterm = tar.borrow().substitute(prm, src);
                {
                    println!(
                        "reduced subterm: {:?}",
                        substituted_subterm.borrow().stringify()
                    );
                }
                substituted_subterm.borrow_mut().parent = self.parent.clone();
                if let Some(parent_rc) = &self.parent {
                    let parent_rc = parent_rc.upgrade().unwrap();
                    match &mut parent_rc.borrow_mut().node_type {
                        NodeType::Root { term } => *term = Rc::clone(&substituted_subterm),
                        NodeType::Abs { var: _, subterm } => {
                            *subterm = Rc::clone(&substituted_subterm)
                        }
                        NodeType::Apply { left, right } => {
                            if std::ptr::eq(self, &*left.borrow()) {
                                *left = Rc::clone(&substituted_subterm)
                            } else {
                                *right = Rc::clone(&substituted_subterm)
                            }
                        }
                        NodeType::Var { .. } => {}
                    };
                }
            };
        }
    }
}

fn consume_token(tok: &mut VecDeque<Token>, target: &str) -> Result<(), ParseError> {
    if at_eof(tok) {
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

fn peek_token(tok: &mut VecDeque<Token>, target: &str) -> Result<(), ParseError> {
    if at_eof(tok) {
        Err(ParseError::SyntaxError)
    } else {
        let target = Token::new(target.to_string())?;
        match tok[0] {
            _ if tok[0] == target => Ok(()),
            _ => Err(ParseError::UnexpectedToken),
        }
    }
}

fn at_eof(tok: &VecDeque<Token>) -> bool {
    tok[0] == Token::Eof
}

pub fn parse(mut tstream: VecDeque<Token>) -> AstResult {
    let term_node = term(&mut tstream)?;
    if !at_eof(&tstream) {
        Err(ParseError::SyntaxError)
    } else if let Some(term_node) = term_node {
        Ok(Some(Ast {
            root: Node::new(NodeType::Root {
                term: Rc::clone(&term_node),
            }),
        }))
    } else {
        Ok(None)
    }
}

fn term(tok: &mut VecDeque<Token>) -> ParseResult {
    if consume_token(tok, "λ").is_ok() || consume_token(tok, "\\").is_ok() {
        // ABS
        // パラメタを読み込む
        let mut params = VecDeque::new();
        while let Some(rc) = primary(tok)? {
            if let NodeType::Var { var } = &rc.borrow().node_type {
                params.push_back(var.clone());
            } else {
                return Err(ParseError::SyntaxError);
            }
        }
        consume_token(tok, ".")?;
        if params.is_empty() {
            return Err(ParseError::SyntaxError);
        }
        if let Some(mut subt) = term(tok)? {
            // 各パラメタについてAbsNodeを作る
            while let Some(param_name) = params.pop_back() {
                let parent_rc = Node::new(NodeType::Abs {
                    var: param_name,
                    subterm: Rc::clone(&subt),
                });
                subt = parent_rc;
            }
            Ok(Some(subt))
        } else {
            Err(ParseError::SyntaxError)
        }
    } else if !at_eof(tok) {
        // primary+
        let mut subterms = VecDeque::new();
        while let Some(subt) = primary(tok)? {
            subterms.push_back(Rc::clone(&subt));
        }
        match subterms.len() {
            0 => Err(ParseError::SyntaxError),
            1 => Ok(Some(subterms.pop_front().unwrap())),
            _ => {
                let mut left = subterms.pop_front().unwrap();

                while let Some(right) = subterms.pop_front() {
                    let parent_rc = Node::new(NodeType::Apply {
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    });
                    left = parent_rc;
                }
                Ok(Some(left))
            }
        }
    } else {
        Ok(None)
    }
}

fn primary(tok: &mut VecDeque<Token>) -> ParseResult {
    if consume_token(tok, "(").is_ok() {
        let tm = term(tok);
        consume_token(tok, ")")?;
        tm
    } else if peek_token(tok, ")").is_ok() || peek_token(tok, ".").is_ok() || at_eof(tok) {
        Ok(None)
    } else if let Some(Token::Var(s)) = tok.pop_front() {
        Ok(Some(Node::new(NodeType::Var { var: s })))
    } else {
        Err(ParseError::SyntaxError)
    }
}
