#[derive(Debug)]
enum TType {
    COLONCOLON,
    COLONEQUAL,
    SEMICOLON,
    NUMBER,
    RPAREN,
    LCURLY,
    RCURLY,
    LPAREN,
    STRING,
    ARROW,
    COLON,
    COMMA,
    IDENT,
    SLASH,
    MINUS,
    STAR,
    PLUS,
}

#[derive(Debug)]
pub struct Token {
    ttype: TType,
    start_idx: usize,
    end_idx: usize,
}

impl Token {
    fn new(ttype: TType, start_idx: usize, end_idx: usize) -> Self {
	Self { ttype, start_idx, end_idx }
    }

    pub fn display(&self, file_contents: &str) -> String {
	format!("{:?}: {}", self.ttype, &file_contents[self.start_idx..self.end_idx])
    }
}

pub struct Lexer {
    pub file_contents: String,
    pub tokens: Vec<Token>,
    curr_line: usize,
    curr_idx: usize,
}

impl Lexer {
    fn new(file_contents: String) -> Self {
	Self { file_contents, tokens: vec![], curr_line: 0, curr_idx: 0 }
    }

    pub fn lex(file_contents: String) -> Self {
	let mut lexer = Self::new(file_contents);

	let bytes = lexer.file_contents.as_bytes();
	while lexer.curr_idx < bytes.len() {
	    let c = bytes[lexer.curr_idx] as char;
	    if c == ' ' || c == '\n' || c == '\t' {
		if bytes[lexer.curr_idx] == b'\n' {
		    lexer.curr_line += 1;
		}
		lexer.curr_idx += 1;
	    } else if c == '(' {
		lexer.tokens.push(Token::new(TType::LPAREN, lexer.curr_idx, lexer.curr_idx + 1));
		lexer.curr_idx += 1;
	    } else if c == ')' {
		lexer.tokens.push(Token::new(TType::RPAREN, lexer.curr_idx, lexer.curr_idx + 1));
		lexer.curr_idx += 1;
	    } else if c == '{' {
		lexer.tokens.push(Token::new(TType::LCURLY, lexer.curr_idx, lexer.curr_idx + 1));
		lexer.curr_idx += 1;
	    } else if c == '}' {
		lexer.tokens.push(Token::new(TType::RCURLY, lexer.curr_idx, lexer.curr_idx + 1));
		lexer.curr_idx += 1
	    } else if c == ';' {
		lexer.tokens.push(Token::new(TType::SEMICOLON, lexer.curr_idx, lexer.curr_idx + 1));
		lexer.curr_idx += 1
	    } else if c == ',' {
		lexer.tokens.push(Token::new(TType::COMMA, lexer.curr_idx, lexer.curr_idx + 1));
		lexer.curr_idx += 1
	    } else if c == '+' {
		lexer.tokens.push(Token::new(TType::PLUS, lexer.curr_idx, lexer.curr_idx + 1));
		lexer.curr_idx += 1
	    } else if c == '*' {
		lexer.tokens.push(Token::new(TType::STAR, lexer.curr_idx, lexer.curr_idx + 1));
		lexer.curr_idx += 1
	    } else if c == '/' {
		if lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] == b'/' {
		    while lexer.curr_idx < bytes.len() && bytes[lexer.curr_idx] != b'\n' {
			lexer.curr_idx += 1;
		    }
		} else {
		    lexer.tokens.push(Token::new(TType::SLASH, lexer.curr_idx, lexer.curr_idx + 1));
		    lexer.curr_idx += 1;
		}
	    } else if c == ':' {
		if lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] == b':' {
		    lexer.tokens.push(Token::new(TType::COLONCOLON, lexer.curr_idx, lexer.curr_idx + 2));
		    lexer.curr_idx += 2;
		} else if lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] == b'=' {
		    lexer.tokens.push(Token::new(TType::COLONEQUAL, lexer.curr_idx, lexer.curr_idx + 2));
		    lexer.curr_idx += 2;
		} else {
		    lexer.tokens.push(Token::new(TType::COLON, lexer.curr_idx, lexer.curr_idx + 1));
		    lexer.curr_idx += 1;
		}
	    } else if c == '-' {
		if lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] == b'>' {
		    lexer.tokens.push(Token::new(TType::ARROW, lexer.curr_idx, lexer.curr_idx + 2));
		    lexer.curr_idx += 2;
		} else {
		    lexer.tokens.push(Token::new(TType::MINUS, lexer.curr_idx, lexer.curr_idx + 1));
		    lexer.curr_idx += 1;
		}
	    } else if c.is_ascii_digit() {
		let start = lexer.curr_idx;
		while lexer.curr_idx < bytes.len() && (bytes[lexer.curr_idx] as char).is_ascii_digit() {
		    lexer.curr_idx += 1;
		}
		lexer.tokens.push(Token::new(TType::NUMBER, start, lexer.curr_idx));
	    } else if c.is_ascii_alphanumeric() {
		let start = lexer.curr_idx;
		while lexer.curr_idx < bytes.len() && (bytes[lexer.curr_idx] as char).is_ascii_alphanumeric() {
		    lexer.curr_idx += 1;
		}
		lexer.tokens.push(Token::new(TType::IDENT, start, lexer.curr_idx));
	    } else if c == '"' {
		let start = lexer.curr_idx;
		while lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] != b'"' {
		    lexer.curr_idx += 1;
		}
		lexer.curr_idx += 2; // Skip quotes
		lexer.tokens.push(Token::new(TType::STRING, start, lexer.curr_idx));
	    } else {
		println!("bad tok");
		println!("{:?}", lexer.tokens);
		println!("{}", bytes[lexer.curr_idx]);
		todo!()
	    }
	}

	lexer
    }
}
