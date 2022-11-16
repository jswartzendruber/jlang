use crate::lexer::*;

#[derive(Debug)]
enum Type {
    I64,
    Void,
    String,
}

#[derive(Debug)]
enum Argument {
    I64(i64),
    String(String),
}

#[derive(Debug)]
struct FunctionCall {
    name: String,
    args: Vec<Argument>,
}

impl FunctionCall {
    fn new(name: String, args: Vec<Argument>) -> Self {
	Self { name, args }
    }

    fn display(&self, ident: usize) {
	println!("{:ident$}{}( {:?} )", "", self.name, self.args);
    }
}

#[derive(Debug)]
enum Statement {
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
struct Function {
    name: String,
    args: Vec<Argument>,
    body: Vec<Statement>,
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
enum Node {
    Function(Function),
}

#[derive(Debug)]
pub struct AST {
    node: Node,
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
    pub ast: Option<AST>,
}

impl Parser {
    fn new(file_contents: String) -> Self {
	Self {
	    file_contents,
	    ast: None,
	}
    }

    pub fn parse(lexer: &mut Lexer) -> Self {
	let mut parser = Self::new(lexer.file_contents.clone());

	parser.ast = Some(
	    AST::new(
		parser.parse_function(&mut lexer.tokens),
		Box::new(None),
		Box::new(None)
	    )
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

	tokens.expect(TType::LPAREN);

	// Collect arguments, if any
	let mut arguments = vec![];
	while tokens.current().ttype != TType::RPAREN {
	    arguments.push(self.parse_argument(tokens));
	}

	tokens.expect(TType::RPAREN);
	tokens.expect(TType::SEMICOLON);

	FunctionCall::new(func, arguments)
    }

    fn parse_argument(&mut self, tokens: &mut Tokens) -> Argument {
	let curr = tokens.advance().expect("Expected function argument");
	match curr.ttype {
	    TType::STRING => Argument::String(curr.copy_contents(&self.file_contents)),
	    _ => {
		println!("Error: Unexpected argument: '{:?}'", curr);
		std::process::exit(1);
	    }
	}
    }
}
