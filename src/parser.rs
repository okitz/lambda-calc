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
pub struct AST {
    pub root: RccellNode,
}

#[derive(Debug, Clone)]
pub enum NodeType {
    Abs(AbsNode),
    Apply(ApplyNode),
    Var(VarNode),
    Root(RootNode),
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
                NodeType::Root(ref data) => {
                    data.term.borrow_mut().parent = Some(Weak::clone(&rc.borrow().self_ref))
                }
                NodeType::Abs(ref data) => {
                    data.subterm.borrow_mut().parent = Some(Weak::clone(&rc.borrow().self_ref))
                }
                NodeType::Apply(ref data) => {
                    data.left.borrow_mut().parent = Some(Weak::clone(&rc.borrow().self_ref));
                    data.right.borrow_mut().parent = Some(Weak::clone(&rc.borrow().self_ref));
                }
                NodeType::Var(_) => {}
            };
        }
        rc
    }
}

type RccellNode = Rc<RefCell<Node>>;
type WkcellNode = Weak<RefCell<Node>>;
type ParseResult = Result<Option<RccellNode>, ParseError>;
type ASTResult = Result<Option<AST>, ParseError>;

#[derive(Debug, Clone)]
pub struct AbsNode {
    var: String,
    subterm: RccellNode,
}

#[derive(Debug, Clone)]
pub struct ApplyNode {
    left: RccellNode,
    right: RccellNode,
}

#[derive(Debug, Clone)]
pub struct VarNode {
    var: String,
}

#[derive(Debug, Clone)]
pub struct RootNode {
    term: RccellNode,
}

impl AST {
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
            NodeType::Root(data) => NodeType::Root(RootNode {
                term: data.term.borrow().deep_clone(),
            }),
            NodeType::Abs(data) => NodeType::Abs(AbsNode {
                var: data.var.clone(),
                subterm: data.subterm.borrow().deep_clone(),
            }),
            NodeType::Apply(data) => NodeType::Apply(ApplyNode {
                left: data.left.borrow().deep_clone(),
                right: data.right.borrow().deep_clone(),
            }),
            NodeType::Var(data) => NodeType::Var(data.clone()),
        };
        Node::new(node_type)
    }

    pub fn stringify(&self) -> String {
        match &self.node_type {
            NodeType::Root(data) => data.term.borrow().stringify(),
            NodeType::Abs(data) => {
                let subt = data.subterm.borrow();
                format!("λ{}.{}", data.var, subt.stringify())
            }
            NodeType::Apply(data) => {
                let make_substr = |nd: &Node| match nd.node_type {
                    NodeType::Var(_) => nd.stringify(),
                    _ => format!("({})", nd.stringify()),
                };
                let lstr = make_substr(&*data.left.borrow());
                let rstr = make_substr(&*data.right.borrow());
                lstr + " " + &rstr
            }
            NodeType::Var(data) => data.var.clone(),
        }
    }

    pub fn find_leftmost_redex(&self) -> Option<RccellNode> {
        match self.node_type {
            NodeType::Root(ref data) => data.term.borrow().find_leftmost_redex(),
            NodeType::Abs(ref data) => data.subterm.borrow().find_leftmost_redex(),
            NodeType::Apply(ref data) => {
                if let NodeType::Abs(_) = data.left.borrow().node_type {
                    self.self_ref.upgrade()
                } else {
                    data.left
                        .borrow()
                        .find_leftmost_redex()
                        .or_else(|| data.right.borrow().find_leftmost_redex())
                }
            }
            NodeType::Var(_) => None,
        }
    }

    pub fn substitute(&self, param: &String, src: RccellNode) -> RccellNode {
        match &self.node_type {
            NodeType::Root(data) => {
                let substituted = data.term.borrow().substitute(param, Rc::clone(&src));
                Node::new(NodeType::Root(RootNode { term: substituted }))
            }
            NodeType::Abs(data) => {
                if data.var != *param {
                    let subterm = data.subterm.borrow().substitute(param, Rc::clone(&src));
                    Node::new(NodeType::Abs(AbsNode {
                        var: data.var.clone(),
                        subterm,
                    }))
                } else {
                    self.self_ref.upgrade().unwrap()
                }
            }
            NodeType::Apply(data) => {
                let left = data.left.borrow().substitute(param, Rc::clone(&src));
                let right = data.right.borrow().substitute(param, Rc::clone(&src));
                Node::new(NodeType::Apply(ApplyNode { left, right }))
            }
            NodeType::Var(data) => {
                if data.var == *param {
                    src.borrow().deep_clone()
                } else {
                    self.self_ref.upgrade().unwrap()
                }
            }
        }
    }

    fn reduce(&self) {
        if let NodeType::Apply(ref apply_data) = self.node_type {
            let abs = apply_data.left.clone();
            let src = apply_data.right.clone();
            if let NodeType::Abs(ref abs_data) = abs.borrow().node_type {
                let prm = abs_data.var.clone();
                let tar = abs_data.subterm.clone();
                let substituted_subterm = tar.borrow().substitute(&prm, src);
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
                        NodeType::Root(data) => data.term = substituted_subterm.clone(),
                        NodeType::Abs(data) => data.subterm = substituted_subterm.clone(),
                        NodeType::Apply(data) => {
                            if std::ptr::eq(self, &*data.left.borrow()) {
                                data.left = substituted_subterm.clone()
                            } else {
                                data.right = substituted_subterm.clone()
                            }
                        }
                        NodeType::Var(_) => {}
                    };
                }
            };
        }
    }
}

fn consume_token(tok: &mut VecDeque<Token>, target: &str) -> Result<(), ParseError> {
    if tok.is_empty() || at_eof(tok) {
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
    if tok.is_empty() || at_eof(tok) {
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
    tok[0] == Token::EOF
}

pub fn parse(mut tstream: VecDeque<Token>) -> ASTResult {
    let term_node = term(&mut tstream)?;
    if !at_eof(&tstream) {
        Err(ParseError::SyntaxError)
    } else if let Some(term_node) = term_node {
        Ok(Some(AST {
            root: Node::new(NodeType::Root(RootNode {
                term: Rc::clone(&term_node),
            })),
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
            if let NodeType::Var(ref data) = rc.borrow().node_type {
                params.push_back(data.var.clone());
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
                let parent_rc = Node::new(NodeType::Abs(AbsNode {
                    var: param_name,
                    subterm: subt.clone(),
                }));
                subt = parent_rc;
            }
            Ok(Some(subt))
        } else {
            Err(ParseError::SyntaxError)
        }
    } else if !tok.is_empty() {
        // primary+
        let mut subterms = VecDeque::new();
        while let Some(subt) = primary(tok)? {
            subterms.push_back(subt.clone());
        }
        match subterms.len() {
            0 => Err(ParseError::SyntaxError),
            1 => Ok(Some(subterms.pop_front().unwrap())),
            _ => {
                let mut left = subterms.pop_front().unwrap();

                while let Some(right) = subterms.pop_front() {
                    let parent_rc = Node::new(NodeType::Apply(ApplyNode {
                        left: left.clone(),
                        right: right.clone(),
                    }));
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
        Ok(Some(Node::new(NodeType::Var(VarNode { var: s }))))
    } else {
        Err(ParseError::SyntaxError)
    }
}
