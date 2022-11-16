use crate::lexer::*;

#[derive(Debug)]
enum Type {
    I64,
    Void,
    String,
}

#[derive(Debug)]
pub enum Argument {
    I64(i64),
    String(String),
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
	println!("{:ident$}{}( {:?} )  Stack: {}", "", self.name, self.args, self.stack_bytes_needed);
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
	let curr = tokens.advance().expect("Expected function argument");

	let arg = match curr.ttype {
	    TType::STRING => {
		let arg = Argument::String(curr.copy_contents(&self.file_contents));
		let bytes = 16; // char* = 8, length = 8
		(arg, bytes)
	    },
	    _ => {
		println!("Error: Unexpected argument: '{:?}'", curr);
		std::process::exit(1);
	    }
	};

	// Skip over comma if it exists, so we can collect other args
	if tokens.current().ttype == TType::COMMA {
	    tokens.expect(TType::COMMA);
	}

	arg
    }
}
