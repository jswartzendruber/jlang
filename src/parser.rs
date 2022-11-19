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

#[derive(Debug)]
pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum ExpressionValue {
    Operation(Operation),
    I64(i64),
}

#[derive(Debug)]
pub struct Expression {
    value: ExpressionValue,
    left: Box<Option<Expression>>,
    right: Box<Option<Expression>>,
}

impl Expression {
    fn new(value: ExpressionValue, left: Box<Option<Expression>>, right: Box<Option<Expression>>) -> Self {
	Self { value, left, right }
    }

    fn new_leaf(value: ExpressionValue) -> Self {
	Self { value, left: Box::new(None), right: Box::new(None) }
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

    fn display(&self, ident: usize) {
	println!("{:ident$}{}( {:#?} )  Stack: {}", "", self.name, self.args, self.stack_bytes_needed);
    }
}

#[derive(Debug)]
pub enum Statement {
    FunctionCall(FunctionCall),
}

impl Statement {
    fn display(&self, indent: usize) {
	match self {
	    Statement::FunctionCall(fc) => fc.display(indent),
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

    fn display(&self, indent: usize) {
	println!("{}( {:?} ) -> {:?}\n{{", self.name, self.args, self.return_type);
	for statement in &self.body {
	    statement.display(indent);
	}
	println!("}}");
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

    pub fn print(&self) {
	match &self.node {
	    Node::Function(f) => f.display(2),
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
	    TType::NUMBER => (Argument::Expression(self.parse_expression(tokens, 0)), 8),
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
	    _ => {
		println!("Error: Unexpected token in expression: '{}'", lhs_token.display(&self.file_contents));
		std::process::exit(1);
	    }
	};

	loop {
	    let op_token = tokens.current();
	    let op = match op_token.ttype {
		TType::RPAREN => break,
		TType::PLUS => Operation::Add,
		TType::MINUS => Operation::Sub,
		TType::STAR => Operation::Mul,
		TType::SLASH => Operation::Div,
		_ => {
		    println!("Error: Expected operation, got : '{}'", op_token.display(&self.file_contents));
		    std::process::exit(1);
		}
	    };

	    let (lbp, rbp) = infix_binding_power(&op);
	    if lbp < min_bp {
		break;
	    }

	    tokens.advance();
	    let rhs = self.parse_expression(tokens, rbp);

	    lhs = Expression::new(ExpressionValue::Operation(op), Box::new(Some(lhs)), Box::new(Some(rhs)));
	}

	lhs
    }
}

fn infix_binding_power(op: &Operation) -> (usize, usize) {
    match op {
	Operation::Add | Operation::Sub => (1, 2),
	Operation::Mul | Operation::Div => (3, 4),
    }
}
