use crate::lexer::*;
use std::fmt;

enum Type {
    I64,
    Void,
    String,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::I64 => write!(f, "i64"),
            Type::Void => write!(f, "Void"),
            Type::String => write!(f, "String"),
        }
    }
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
    EqEq,
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operation::Add => write!(f, "+"),
            Operation::Sub => write!(f, "-"),
            Operation::Mul => write!(f, "*"),
            Operation::Div => write!(f, "/"),
            Operation::EqEq => write!(f, "=="),
        }
    }
}

impl Operation {
    fn from(ttype: &TType) -> Self {
        match ttype {
            TType::Plus => Operation::Add,
            TType::Minus => Operation::Sub,
            TType::Star => Operation::Mul,
            TType::Slash => Operation::Div,
            TType::EqualEqual => Operation::EqEq,
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
    fn new(
        value: ExpressionValue,
        left: Option<Box<Expression>>,
        right: Option<Box<Expression>>,
    ) -> Self {
        Self { value, left, right }
    }

    fn new_leaf(value: ExpressionValue) -> Self {
        Self {
            value,
            left: None,
            right: None,
        }
    }

    fn count_nodes(expr: &Expression) -> usize {
        let mut total = 1;

        if let Some(left) = &expr.left {
            total += Self::count_nodes(left);
        }
        if let Some(right) = &expr.right {
            total += Self::count_nodes(right);
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
        Self {
            name,
            args,
            stack_bytes_needed,
        }
    }
}

impl fmt::Display for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}( {:#?} ) Stack: {}",
            self.name, self.args, self.stack_bytes_needed
        )
    }
}

#[derive(Debug)]
pub struct If {
    condition: Expression,
    if_true: Vec<Statement>,
    if_false: Option<Vec<Statement>>,
}

impl If {
    fn new(
        condition: Expression,
        if_true: Vec<Statement>,
        if_false: Option<Vec<Statement>>,
    ) -> Self {
        Self {
            condition,
            if_true,
            if_false,
        }
    }
}

impl fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "If ( {:?} ) {{\n  {:?}\n}}\nelse {{\n  {:?}\n}}\n",
            self.condition, self.if_true, self.if_false
        )
    }
}

#[derive(Debug)]
pub enum Statement {
    FunctionCall(FunctionCall),
    If(If),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::FunctionCall(fc) => write!(f, "{}", fc),
            Statement::If(i) => write!(f, "{}", i),
        }
    }
}

pub struct Function {
    pub name: String,
    args: Vec<Argument>,
    pub body: Vec<Statement>,
    return_type: Type,
}

impl Function {
    fn new(name: String, args: Vec<Argument>, body: Vec<Statement>, return_type: Type) -> Self {
        Self {
            name,
            args,
            body,
            return_type,
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}( {:?} ) -> {}\n{{\n",
            self.name, self.args, self.return_type
        )?;
        for statement in &self.body {
            write!(f, "{}", statement)?;
        }
        write!(f, "}}")
    }
}

pub enum Node {
    Function(Function),
}

pub struct Ast {
    pub node: Node,
    left: Box<Option<Ast>>,
    right: Box<Option<Ast>>,
}

impl Ast {
    fn new(node: Node, left: Box<Option<Ast>>, right: Box<Option<Ast>>) -> Self {
        Self { node, left, right }
    }
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.node {
            Node::Function(func) => write!(f, "{}", func),
        }
    }
}

pub struct Parser {
    file_contents: String,
    pub ast: Ast,
}

impl Parser {
    fn new(file_contents: String) -> Self {
        Self {
            file_contents,

            // Immediately overwritten. Used to avoid Option<>
            ast: Ast::new(
                Node::Function(Function::new("a".to_string(), vec![], vec![], Type::Void)),
                Box::new(None),
                Box::new(None),
            ),
        }
    }

    pub fn parse(lexer: &mut Lexer) -> Self {
        let mut parser = Self::new(lexer.file_contents.clone());

        parser.ast = Ast::new(
            parser.parse_function(&mut lexer.tokens),
            Box::new(None),
            Box::new(None),
        );

        parser
    }

    fn parse_function(&mut self, tokens: &mut Tokens) -> Node {
        let func_name = tokens
            .advance()
            .expect("Expected function name")
            .copy_contents(&self.file_contents);

        tokens.expect(TType::ColonColon);
        tokens.expect(TType::LParen);

        // handle arguments here

        tokens.expect(TType::RParen);
        tokens.expect(TType::LCurly);

        // Collect statements until end of function
        let mut statements = vec![];
        while tokens.current().ttype != TType::RCurly {
            statements.push(self.parse_statement(tokens));
        }

        tokens.expect(TType::RCurly);

        Node::Function(Function::new(func_name, vec![], statements, Type::Void))
    }

    fn parse_statement(&mut self, tokens: &mut Tokens) -> Statement {
        match tokens.peek().expect("Expected statement").ttype {
            TType::LParen => Statement::FunctionCall(self.parse_function_call(tokens)),
            _ => Statement::If(self.parse_if_statement(tokens)),
        }
    }

    fn parse_if_statement(&mut self, tokens: &mut Tokens) -> If {
        let mut curr_token = tokens.current();
        if curr_token.copy_contents(&self.file_contents) == "if" {
            tokens.expect(TType::Identifier);
        } else {
            println!(
                "Error: Unexpected identifier in if statement: '{}'",
                curr_token.display(&self.file_contents)
            );
            std::process::exit(1);
        }

        let condition = self.parse_expression(tokens, 0);
        tokens.expect(TType::LCurly);

        // TODO: Handle arbitrary number of statements
        let if_true = vec![self.parse_statement(tokens)];
        tokens.expect(TType::RCurly);

        curr_token = tokens.current();
        let if_false = if curr_token.copy_contents(&self.file_contents) == "else" {
            tokens.expect(TType::Identifier);
            let mut v = vec![];

            tokens.expect(TType::LCurly);
            v.push(self.parse_statement(tokens));
            tokens.expect(TType::RCurly);

            Some(v)
        } else {
            None
        };

        If::new(condition, if_true, if_false)
    }

    fn parse_function_call(&mut self, tokens: &mut Tokens) -> FunctionCall {
        let func = tokens
            .advance()
            .expect("Expected function name")
            .copy_contents(&self.file_contents);
        let mut stack_bytes_needed = 0;

        tokens.expect(TType::LParen);

        // Collect arguments, if any
        let mut arguments = vec![];
        while tokens.current().ttype != TType::RParen {
            let (arg, bytes) = self.parse_argument(tokens);
            arguments.push(arg);
            stack_bytes_needed += bytes;
        }

        tokens.expect(TType::RParen);
        tokens.expect(TType::Semicolon);

        FunctionCall::new(func, arguments, stack_bytes_needed)
    }

    fn parse_argument(&mut self, tokens: &mut Tokens) -> (Argument, usize) {
        let curr = tokens.current().clone();

        let arg = match curr.ttype {
            TType::String => {
                let arg = Argument::String(curr.copy_contents(&self.file_contents));
                tokens.advance();
                let bytes = 16; // char* = 8, length = 8
                (arg, bytes)
            }
            TType::Number | TType::LParen => {
                let expr = self.parse_expression(tokens, 0);
                let count = Expression::count_nodes(&expr) * 8;
                (Argument::Expression(expr), count)
            }
            _ => {
                println!(
                    "Error: Unexpected argument: '{}'",
                    curr.display(&self.file_contents)
                );
                std::process::exit(1);
            }
        };

        // Skip over comma if it exists, so we can collect other args
        if tokens.current().ttype == TType::Comma {
            tokens.expect(TType::Comma);
        }

        arg
    }

    fn parse_expression(&mut self, tokens: &mut Tokens, min_bp: usize) -> Expression {
        let lhs_token = tokens.advance().expect("Expected expr");
        let mut lhs = match lhs_token.ttype {
            TType::Number => {
                let string_value = lhs_token.copy_contents(&self.file_contents);
                Expression::new_leaf(ExpressionValue::I64(string_value.parse().unwrap()))
            }
            TType::LParen => {
                let lhs = self.parse_expression(tokens, 0);
                tokens.expect(TType::RParen);
                lhs
            }
            _ => {
                println!(
                    "Error: Unexpected token in expression: '{}'",
                    lhs_token.display(&self.file_contents)
                );
                std::process::exit(1);
            }
        };

        loop {
            let op_token = tokens.current();
            let op = match op_token.ttype {
                TType::Plus | TType::Minus | TType::Star | TType::Slash | TType::EqualEqual => {
                    op_token.ttype.clone()
                }
                _ => break,
            };

            if let Some((lbp, rbp)) = infix_binding_power(&op) {
                if lbp < min_bp {
                    break;
                }

                tokens.advance();
                let rhs = self.parse_expression(tokens, rbp);

                lhs = Expression::new(
                    ExpressionValue::Operation(Operation::from(&op)),
                    Some(Box::new(lhs)),
                    Some(Box::new(rhs)),
                );
                continue;
            }
        }

        lhs
    }
}

fn infix_binding_power(op: &TType) -> Option<(usize, usize)> {
    let res = match op {
        TType::Plus | TType::Minus => (10, 20),
        TType::Star | TType::Slash => (30, 40),
        TType::EqualEqual => (5, 6),
        _ => return None,
    };
    Some(res)
}
