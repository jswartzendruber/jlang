use std::fmt;
use crate::lexer::*;

#[derive(Debug)]
enum Type {
    I64,
    Void,
    String,
}

#[derive(Debug)]
pub enum Argument {
    Expression(Expression),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
}

impl Operation {
    fn from(ttype: &TType) -> Self {
	match ttype {
	    TType::PLUS => Operation::Add,
	    TType::MINUS => Operation::Sub,
	    TType::STAR => Operation::Mul,
	    TType::SLASH => Operation::Div,
	    _ => unreachable!(),
	}
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionValue {
    Operation(Operation),
    I64(i64),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub value: ExpressionValue,
    pub left: Option<Box<Expression>>,
    pub right: Option<Box<Expression>>,
}

impl Expression {
    fn new(value: ExpressionValue, left: Option<Box<Expression>>, right: Option<Box<Expression>>) -> Self {
	Self { value, left, right }
    }

    fn new_leaf(value: ExpressionValue) -> Self {
	Self { value, left: None, right: None }
    }

    fn count_nodes(expr: &Expression) -> usize {
	let mut total = 1;

	if let Some(left) = &expr.left {
	    total += Self::count_nodes(&left);
	}
	if let Some(right) = &expr.right {
	    total += Self::count_nodes(&right);
	}

	total
    }
}

#[derive(Debug)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Argument>,
    pub stack_bytes_needed: usize,
}

impl FunctionCall {
    fn new(name: String, args: Vec<Argument>, stack_bytes_needed: usize) -> Self {
	Self { name, args, stack_bytes_needed }
    }
}

impl fmt::Display for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	write!(f, "{}( {:#?} ) Stack: {}", self.name, self.args, self.stack_bytes_needed)
    }
}

#[derive(Debug)]
pub enum Statement {
    FunctionCall(FunctionCall),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	match self {
	    Statement::FunctionCall(fc) => write!(f, "{}", fc),
	}
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    args: Vec<Argument>,
    pub body: Vec<Statement>,
    return_type: Type,
}

impl Function {
    fn new(name: String, args: Vec<Argument>, body:Vec<Statement>, return_type: Type) -> Self {
	Self { name, args, body, return_type }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	write!(f, "{}( {:?} ) -> {:?}\n{{\n", self.name, self.args, self.return_type)?;
	for statement in &self.body {
	    write!(f, "{}", statement)?;
	}
	write!(f, "}}")
    }
}

#[derive(Debug)]
pub enum Node {
    Function(Function),
}

#[derive(Debug)]
pub struct AST {
    pub node: Node,
    left: Box<Option<AST>>,
    right: Box<Option<AST>>,
}

impl AST {
    fn new(node: Node, left: Box<Option<AST>>, right: Box<Option<AST>>) -> Self {
	Self { node, left, right }
    }
}

impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	match &self.node {
	    Node::Function(func) => write!(f, "{}", func),
	}
    }
}

pub struct Parser {
    file_contents: String,
    pub ast: AST,
}

impl Parser {
    fn new(file_contents: String) -> Self {
	Self {
	    file_contents,

	    // Immediately overwritten. Used to avoid Option<>
	    ast: AST::new(
		Node::Function(
		    Function::new("a".to_string(), vec![], vec![], Type::Void)
		), Box::new(None), Box::new(None),
	    ),
	}
    }

    pub fn parse(lexer: &mut Lexer) -> Self {
	let mut parser = Self::new(lexer.file_contents.clone());

	parser.ast = AST::new(
	    parser.parse_function(&mut lexer.tokens),
	    Box::new(None),
	    Box::new(None)
	);

	parser
    }

    fn parse_function(&mut self, tokens: &mut Tokens) -> Node {
	let func_name = tokens.advance().expect("Expected function name").copy_contents(&self.file_contents);

	tokens.expect(TType::COLONCOLON);
	tokens.expect(TType::LPAREN);

	// handle arguments here

	tokens.expect(TType::RPAREN);
	tokens.expect(TType::LCURLY);

	// Collect statements until end of function
	let mut statements = vec![];
	while tokens.current().ttype != TType::RCURLY {
	    statements.push(self.parse_statement(tokens));
	}

	tokens.expect(TType::RCURLY);

	Node::Function(Function::new(func_name, vec![], statements, Type::Void))
    }

    fn parse_statement(&mut self, tokens: &mut Tokens) -> Statement {
	// Assume function call
	Statement::FunctionCall(self.parse_function_call(tokens))
    }

    fn parse_function_call(&mut self, tokens: &mut Tokens) -> FunctionCall {
	let func = tokens.advance().expect("Expected function name").copy_contents(&self.file_contents);
	let mut stack_bytes_needed = 0;

	tokens.expect(TType::LPAREN);

	// Collect arguments, if any
	let mut arguments = vec![];
	while tokens.current().ttype != TType::RPAREN {
	    let (arg, bytes) = self.parse_argument(tokens);
	    arguments.push(arg);
	    stack_bytes_needed += bytes;
	}

	tokens.expect(TType::RPAREN);
	tokens.expect(TType::SEMICOLON);

	FunctionCall::new(func, arguments, stack_bytes_needed)
    }

    fn parse_argument(&mut self, tokens: &mut Tokens) -> (Argument, usize) {
	let curr = tokens.current().clone();

	let arg = match curr.ttype {
	    TType::STRING => {
		let arg = Argument::String(curr.copy_contents(&self.file_contents));
		tokens.advance();
		let bytes = 16; // char* = 8, length = 8
		(arg, bytes)
	    },
	    TType::NUMBER | TType::LPAREN => {
		let expr = self.parse_expression(tokens, 0);
		let count = Expression::count_nodes(&expr) * 8;
		(Argument::Expression(expr), count)
	    },
	    _ => {
		println!("Error: Unexpected argument: '{}'", curr.display(&self.file_contents));
		std::process::exit(1);
	    }
	};

	// Skip over comma if it exists, so we can collect other args
	if tokens.current().ttype == TType::COMMA {
	    tokens.expect(TType::COMMA);
	}

	arg
    }

    fn parse_expression(&mut self, tokens: &mut Tokens, min_bp: usize) -> Expression {
	let lhs_token = tokens.advance().expect("Expected expr");
	let mut lhs = match lhs_token.ttype {
	    TType::NUMBER => {
		let string_value = lhs_token.copy_contents(&self.file_contents);
		Expression::new_leaf(ExpressionValue::I64(string_value.parse().unwrap()))
	    },
	    TType::LPAREN => {
		let lhs = self.parse_expression(tokens, 0);
		tokens.expect(TType::RPAREN);
		lhs
	    },
	    _ => {
		println!("Error: Unexpected token in expression: '{}'", lhs_token.display(&self.file_contents));
		std::process::exit(1);
	    }
	};

	loop {
	    let op_token = tokens.current();
	    let op = match op_token.ttype {
		TType::PLUS | TType::MINUS | TType::STAR | TType::SLASH => op_token.ttype.clone(),
		TType::RPAREN => break,
		_ => {
		    println!("Error: Expected operation, got : '{}'", op_token.display(&self.file_contents));
		    std::process::exit(1);
		}
	    };

	    if let Some((lbp, rbp)) = infix_binding_power(&op) {
		if lbp < min_bp {
		    break;
		}

		tokens.advance();
		let rhs = self.parse_expression(tokens, rbp);

		lhs = Expression::new(ExpressionValue::Operation(Operation::from(&op)), Some(Box::new(lhs)), Some(Box::new(rhs)));
		continue;
	    }
	}

	lhs
    }
}

fn infix_binding_power(op: &TType) -> Option<(usize, usize)> {
    let res = match op {
	TType::PLUS | TType::MINUS => (1, 2),
	TType::STAR | TType::SLASH => (3, 4),
	_ => return None,
    };
    Some(res)
}
