use crate::lexer::*;
use std::collections::HashMap;
use std::fmt;

pub enum Type {
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

impl Type {
    fn from(t: &str) -> Self {
        match t {
            "i64" => Type::I64,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Argument {
    Expression(Expression),
    String(String),
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Argument::Expression(e) => {
                write!(f, "{}", e)
            }
            Argument::String(s) => {
                write!(f, "\"{}\"", s)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operation {
    Or,
    Add,
    Sub,
    Mul,
    Div,
    EqEq,
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
	    Operation::Or => write!(f, "||"),
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
	    TType::PipePipe => Operation::Or,
            TType::Plus => Operation::Add,
            TType::Minus => Operation::Sub,
            TType::Star => Operation::Mul,
            TType::Slash => Operation::Div,
            TType::EqualEqual => Operation::EqEq,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Variable {
    pub name: String,
    pub from_line: usize,
    line_start_idx: usize,
    start_idx: usize,
    end_idx: usize,
}

impl Variable {
    pub fn new(
        name: String,
        from_line: usize,
        line_start_idx: usize,
        start_idx: usize,
        end_idx: usize,
    ) -> Self {
        Self {
            name,
            from_line,
            line_start_idx,
            start_idx,
            end_idx,
        }
    }

    // pub fn copy_line(&self, file_contents: &str) -> String {
    //     let bytes = file_contents.as_bytes();
    //     let mut i = self.line_start_idx + 1;
    //     while i < file_contents.len() && bytes[i] != b'\n' {
    //         i += 1;
    //     }
    //     String::from(&file_contents[self.line_start_idx..i])
    // }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionValue {
    FunctionCall(FunctionCall, Scope),
    Operation(Operation),
    Variable(Variable),
    I64(i64),
}

impl fmt::Display for ExpressionValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExpressionValue::FunctionCall(fc, _) => {
                write!(f, "{}", fc)
            }
            ExpressionValue::Operation(o) => {
                write!(f, "{}", o)
            }
            ExpressionValue::Variable(v) => {
                write!(f, "{}", v)
            }
            ExpressionValue::I64(i) => {
                write!(f, "{}", i)
            }
        }
    }
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

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.left.is_some() && self.right.is_some() {
            write!(
                f,
                "({} {} {})",
                self.left.as_ref().unwrap(),
                self.value,
                self.right.as_ref().unwrap()
            )
        } else {
            write!(f, "{}", self.value)
        }
    }
}

#[derive(Debug, Clone)]
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
        write!(f, "{}( ", self.name).expect("Failed to write");
        self.args
            .iter()
            .for_each(|ref arg| write!(f, "{}", arg).expect("Failed to write"));
        write!(f, " ) Stack: {}", self.stack_bytes_needed)
    }
}

type Statements = Vec<Statement>;

pub struct Block {
    pub statements: Statements,
    pub scope: Scope,
}

impl Block {
    fn new(statements: Statements, scope: Scope) -> Self {
        Self { statements, scope }
    }
}

pub struct If {
    pub condition: Expression,
    pub if_true: Block,
    pub if_false: Option<Block>,
}

impl If {
    fn new(condition: Expression, if_true: Block, if_false: Option<Block>) -> Self {
        Self {
            condition,
            if_true,
            if_false,
        }
    }
}

impl fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "If ( {} ) {{", self.condition,).expect("Failed to write");
        self.if_true
            .statements
            .iter()
            .for_each(|ref stmt| write!(f, "{}", stmt).expect("Failed to write"));
        write!(f, "}} ").expect("Failed to write");
        writeln!(f, "else {{").expect("Failed to write");
        if self.if_false.is_some() {
            self.if_false
                .as_ref()
                .unwrap()
                .statements
                .iter()
                .for_each(|ref stmt| write!(f, "{}", stmt).expect("Failed to write"));
        }
        writeln!(f, "}}")
    }
}

pub struct VarDeclaration {
    pub var_type: Type,
    pub value: Expression,
    pub name: String,
    stack_bytes_needed: usize,
}

impl VarDeclaration {
    pub fn new(var_type: Type, value: Expression, name: String, stack_bytes_needed: usize) -> Self {
        Self {
            var_type,
            value,
            name,
            stack_bytes_needed,
        }
    }
}

impl fmt::Display for VarDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "let {}: {} = {}", self.name, self.var_type, self.value)
    }
}

pub struct Return {
    val: Expression,
}

impl fmt::Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "return {}", self.val)
    }
}

impl Return {
    fn new(val: Expression) -> Self {
	Self { val }
    }
}

pub enum Statement {
    VarDeclaration(VarDeclaration),
    FunctionCall(FunctionCall),
    Return(Return),
    If(If),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::VarDeclaration(v) => write!(f, "{}", v),
            Statement::FunctionCall(fc) => write!(f, "{}", fc),
	    Statement::Return(r) => write!(f, "{}", r),
            Statement::If(i) => write!(f, "{}", i),
        }
    }
}

impl Statement {
    fn stack_bytes_needed(&self) -> usize {
        match self {
            Statement::If(_) => unimplemented!(),
	    Statement::Return(_) => unimplemented!(),
            Statement::FunctionCall(_) => unimplemented!(),
            Statement::VarDeclaration(v) => v.stack_bytes_needed,
        }
    }
}

pub struct NamedArgument {
    arg_name: String,
    arg_type: Type,
}

impl fmt::Display for NamedArgument {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} : {}", self.arg_name, self.arg_type)
    }
}

impl NamedArgument {
    fn new(arg_name: String, arg_type: Type) -> Self {
        Self { arg_name, arg_type }
    }
}

pub struct Function {
    pub name: String,
    args: Vec<NamedArgument>,
    pub body: Block,
    return_type: Type,
    pub stack_bytes_needed: usize,
}

impl Function {
    fn new(
        name: String,
        args: Vec<NamedArgument>,
        body: Block,
        return_type: Type,
        stack_bytes_needed: usize,
    ) -> Self {
        Self {
            name,
            args,
            body,
            return_type,
            stack_bytes_needed,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub table: HashMap<String, Expression>,
    pub prev: Box<Option<Scope>>,
}

impl Scope {
    fn new(prev: Box<Option<Scope>>) -> Self {
        Self {
            table: HashMap::new(),
            prev,
        }
    }

    // Traverse scope from most recent to least, and return var if found.
    pub fn get_scoped_var(&self, var: &str) -> Option<&Expression> {
        let elem = self.table.get(var);
        match elem {
            Some(e) => Some(e),
            None => {
                if self.prev.is_some() {
                    self.prev.as_ref().as_ref().unwrap().get_scoped_var(var)
                } else {
                    None
                }
            }
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}( ", self.name,)?;
        self.args
            .iter()
            .for_each(|ref arg| write!(f, "{}", arg).expect("Failed to write"));
        write!(f, " ) -> {}\n{{\n", self.return_type)?;
        for statement in &self.body.statements {
            write!(f, "{}", statement)?;
        }
        write!(f, "}}")
    }
}

pub struct Module {
    name: String,
    pub functions: Vec<Function>,
}

impl Module {
    fn new(name: String) -> Self {
        Self {
            name,
            functions: vec![],
        }
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        for func in &self.functions {
            write!(f, "{}", func).expect("Failed to write");
        }
        writeln!(f)
    }
}

pub enum Node {
    Module(Module),
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
            Node::Module(m) => write!(f, "{}", m),
        }
    }
}

pub struct Parser {
    pub file_contents: String,
    pub ast: Option<Ast>,
    pub scope: Vec<Scope>, // stack
}

impl Parser {
    fn new(file_contents: String) -> Self {
        Self {
            file_contents,
            ast: None,
            scope: vec![],
        }
    }

    fn enter_scope(&mut self) {
        match self.scope.last() {
            None => self.scope.push(Scope::new(Box::new(None))),
            Some(s) => self.scope.push(Scope::new(Box::new(Some((*s).clone())))),
        };
    }

    fn leave_scope(&mut self) -> Scope {
        self.scope.pop().unwrap()
    }

    fn current_scope(&self) -> Scope {
        self.scope.last().unwrap().clone()
    }

    // Insert var in most recent scope. Return false if a var with that name exists.
    fn insert_scoped_var(&mut self, var: &str, expr: Expression) -> bool {
        let elem = self
            .scope
            .last_mut()
            .unwrap()
            .table
            .insert(var.to_string(), expr);
        elem.is_none()
    }

    pub fn parse(lexer: &mut Lexer) -> Self {
        let mut parser = Self::new(lexer.file_contents.clone());

        let mut module = Module::new("main".to_string());
        while lexer.tokens.curr_idx < lexer.tokens.tokens.len() - 1 {
            module
                .functions
                .push(parser.parse_function(&mut lexer.tokens));
        }
        parser.ast = Some(Ast::new(
            Node::Module(module),
            Box::new(None),
            Box::new(None),
        ));

        parser
    }

    fn parse_function(&mut self, tokens: &mut Tokens) -> Function {
        let func_name = tokens
            .advance()
            .expect("Expected function name")
            .copy_contents(&self.file_contents);

        tokens.expect(TType::ColonColon, &self.file_contents);
        tokens.expect(TType::LParen, &self.file_contents);

        let arguments = self.parse_function_arguments(tokens);

        tokens.expect(TType::RParen, &self.file_contents);

	let return_type = self.parse_function_return_type(tokens);

        tokens.expect(TType::LCurly, &self.file_contents);
        self.enter_scope();

        // Collect statements until end of function
        let mut statements = vec![];
        while tokens.current().ttype != TType::RCurly {
            statements.push(self.parse_statement(tokens));
        }
        let stack_bytes_needed = statements
            .iter()
            .filter(|e| matches!(e, Statement::VarDeclaration(_)))
            .map(|e| e.stack_bytes_needed())
            .sum(); // Stack space needed for local function vars

        tokens.expect(TType::RCurly, &self.file_contents);
        Function::new(
            func_name,
            arguments,
            Block::new(statements, self.leave_scope()),
            Type::Void,
            stack_bytes_needed,
        )
    }

    fn parse_function_return_type(&mut self, tokens: &mut Tokens) -> Type {
	if tokens.current().ttype != TType::LCurly {
	    tokens.expect(TType::Arrow, &self.file_contents);
	    Type::from(&tokens.advance().expect("Expected return type").copy_contents(&self.file_contents))
	} else {
	    Type::Void
	}
    }

    fn parse_function_arguments(&mut self, tokens: &mut Tokens) -> Vec<NamedArgument> {
        let mut arguments = vec![];

        while tokens.current().ttype != TType::RParen {
            arguments.push(self.parse_function_argument(tokens));
        }

        arguments
    }

    fn parse_function_argument(&mut self, tokens: &mut Tokens) -> NamedArgument {
        let arg_name = tokens
            .advance()
            .expect("Expected argument name")
            .copy_contents(&self.file_contents);
        tokens.expect(TType::Colon, &self.file_contents);
        let arg_type = tokens
            .advance()
            .expect("Expected argument type")
            .copy_contents(&self.file_contents);
        NamedArgument::new(arg_name, Type::from(&arg_type))
    }

    fn parse_statement(&mut self, tokens: &mut Tokens) -> Statement {
        let curr_token = tokens.current();
        let next_token = tokens.peek().expect("Expected statement");
        match next_token.ttype {
            TType::LParen => {
                let stmt = Statement::FunctionCall(self.parse_function_call(tokens));
                tokens.expect(TType::Semicolon, &self.file_contents);
                stmt
            }
            TType::Identifier => {
                let curr_contents = curr_token.copy_contents(&self.file_contents);
                if curr_contents == "if" {
                    Statement::If(self.parse_if_statement(tokens))
                } else if curr_contents == "let" {
                    Statement::VarDeclaration(self.parse_var_declaration(tokens))
                } else if curr_contents == "return" {
		    Statement::Return(self.parse_return_statement(tokens))
		} else {
		    println!("unimplemented: {}", curr_contents);
                    unimplemented!();
                }
            }
            TType::Number => Statement::If(self.parse_if_statement(tokens)),
            _ => unimplemented!(),
        }
    }

    fn parse_return_statement(&mut self, tokens: &mut Tokens) -> Return {
	tokens.expect(TType::Identifier, &self.file_contents); // return
	let expr = self.parse_expression(tokens, 0);
	tokens.expect(TType::Semicolon, &self.file_contents);
	Return::new(expr)
    }

    fn parse_var_declaration(&mut self, tokens: &mut Tokens) -> VarDeclaration {
        tokens.expect(TType::Identifier, &self.file_contents); // let
        let var_name_token = tokens.advance().expect("Expected variable name");
        let var_name = var_name_token.copy_contents(&self.file_contents);
        tokens.expect(TType::Colon, &self.file_contents);

        let var_type = Type::from(
            &tokens
                .advance()
                .expect("Expected variable type")
                .copy_contents(&self.file_contents),
        );
        tokens.expect(TType::Equal, &self.file_contents);

        let var_value = self.parse_expression(tokens, 0);
        let stack_bytes_needed = Expression::count_nodes(&var_value) * 8;
        tokens.expect(TType::Semicolon, &self.file_contents);

        self.insert_scoped_var(&var_name, var_value.clone());
        VarDeclaration::new(var_type, var_value, var_name, stack_bytes_needed)
    }

    fn parse_if_statement(&mut self, tokens: &mut Tokens) -> If {
        let mut curr_token = tokens.current();
        if curr_token.copy_contents(&self.file_contents) == "if" {
            tokens.expect(TType::Identifier, &self.file_contents);
        } else {
            self.error(curr_token, "identifier");
        }

        let condition = self.parse_expression(tokens, 0);
        tokens.expect(TType::LCurly, &self.file_contents);
        self.enter_scope();

        // TODO: Handle arbitrary number of statements
        let if_true_statements = vec![self.parse_statement(tokens)];
        let if_true = Block::new(if_true_statements, self.leave_scope());

        tokens.expect(TType::RCurly, &self.file_contents);

        curr_token = tokens.current();
        let if_false = if curr_token.copy_contents(&self.file_contents) == "else" {
            tokens.expect(TType::Identifier, &self.file_contents);
            tokens.expect(TType::LCurly, &self.file_contents);
            self.enter_scope();
            let if_false_statements = vec![self.parse_statement(tokens)];

            tokens.expect(TType::RCurly, &self.file_contents);

            Some(Block::new(if_false_statements, self.leave_scope()))
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

        tokens.expect(TType::LParen, &self.file_contents);

        // Collect arguments, if any
        let (arguments, stack_bytes_needed) = self.parse_arguments(tokens);

        tokens.expect(TType::RParen, &self.file_contents);

        FunctionCall::new(func, arguments, stack_bytes_needed)
    }

    fn parse_arguments(&mut self, tokens: &mut Tokens) -> (Vec<Argument>, usize) {
        let mut args = vec![];
        let mut bytes = 0;

        self.parse_argument(tokens, &mut args, &mut bytes);

        (args, bytes)
    }

    fn parse_argument(&mut self, tokens: &mut Tokens, args: &mut Vec<Argument>, bytes: &mut usize) {
        let curr = tokens.current();

        match curr.ttype {
            TType::String => {
                let arg = Argument::String(curr.copy_contents(&self.file_contents));
                tokens.advance();

                if tokens.current().ttype == TType::Comma {
                    tokens.expect(TType::Comma, &self.file_contents);
                    self.parse_argument(tokens, args, bytes);
                }

                args.push(arg);
                *bytes += 16; // char* = 8, length = 8
            }
            TType::Number | TType::Identifier | TType::LParen => {
                let expr = self.parse_expression(tokens, 0);

                if tokens.current().ttype == TType::Comma {
                    tokens.expect(TType::Comma, &self.file_contents);
                    self.parse_argument(tokens, args, bytes);
                }

                args.push(Argument::Expression(expr));
                *bytes += 8;
            }
            _ => {
                self.error(curr, "argument");
                unreachable!();
            }
        };
    }

    fn parse_expression(&mut self, tokens: &mut Tokens, min_bp: usize) -> Expression {
        let lhs_token = tokens.current().clone();
        let mut lhs = match lhs_token.ttype {
            TType::Number => {
                tokens.advance();
                let string_value = lhs_token.copy_contents(&self.file_contents);
                Expression::new_leaf(ExpressionValue::I64(string_value.parse().unwrap()))
            }
            TType::LParen => {
                tokens.advance();
                let lhs = self.parse_expression(tokens, 0);
                tokens.expect(TType::RParen, &self.file_contents);
                lhs
            }
            TType::Identifier => {
                if tokens.peek().expect("Expected expr").ttype == TType::LParen {
                    Expression::new_leaf(ExpressionValue::FunctionCall(
                        self.parse_function_call(tokens),
                        self.current_scope(),
                    ))
                } else {
                    tokens.advance();
                    let string_value = lhs_token.copy_contents(&self.file_contents);
                    Expression::new_leaf(ExpressionValue::Variable(Variable::new(
                        string_value,
                        lhs_token.from_line,
                        lhs_token.line_start_idx,
                        lhs_token.start_idx,
                        lhs_token.end_idx,
                    )))
                }
            }
            _ => {
                self.error(&lhs_token, "token in expression");
                unreachable!();
            }
        };

        loop {
            let op_token = tokens.current();
            let op = match op_token.ttype {
                TType::Plus | TType::Minus | TType::Star | TType::Slash | TType::EqualEqual | TType::PipePipe => {
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

    fn error(&self, token: &Token, thing: &str) {
        println!(
            "Error: Unexpected {} '{}' on line {}.",
            thing,
            token.copy_contents(&self.file_contents).trim(),
            token.from_line
        );
        println!("  {}", token.copy_line(&self.file_contents).trim());
        println!();
        std::process::exit(1);
    }
}

fn infix_binding_power(op: &TType) -> Option<(usize, usize)> {
    let res = match op {
        TType::Plus | TType::Minus => (10, 20),
        TType::Star | TType::Slash => (30, 40),
        TType::EqualEqual => (5, 6),
	TType::PipePipe => (3, 4),
        _ => return None,
    };
    Some(res)
}
