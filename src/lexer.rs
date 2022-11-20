#[derive(Debug, PartialEq, Clone)]
pub enum TType {
    COLONCOLON,
    COLONEQUAL,
    EQUALEQUAL,
    SEMICOLON,
    PIPEPIPE,
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
    pub ttype: TType,
    start_idx: usize,
    end_idx: usize,
}

impl Token {
    fn new(ttype: TType, start_idx: usize, end_idx: usize) -> Self {
        Self {
            ttype,
            start_idx,
            end_idx,
        }
    }

    pub fn display(&self, file_contents: &str) -> String {
        format!(
            "{:?}: {}",
            self.ttype,
            &file_contents[self.start_idx..self.end_idx]
        )
    }

    pub fn copy_contents(&self, file_contents: &str) -> String {
        String::from(&file_contents[self.start_idx..self.end_idx])
    }
}

#[derive(Debug)]
pub struct Tokens {
    pub tokens: Vec<Token>,
    curr_idx: usize,
}

impl Tokens {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            curr_idx: 0,
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.curr_idx >= self.tokens.len() - 1
    }

    pub fn current(&self) -> &Token {
        &self.tokens[self.curr_idx]
    }

    pub fn peek(&self) -> Option<&Token> {
        if self.curr_idx + 1 < self.tokens.len() {
            return Some(&self.tokens[self.curr_idx + 1]);
        } else {
            return None;
        }
    }

    // Returns current value, and then advances one token.
    pub fn advance(&mut self) -> Option<&Token> {
        if self.is_at_end() {
            return None;
        } else {
            self.curr_idx += 1;
            return Some(&self.tokens[self.curr_idx - 1]);
        }
    }

    // Advances if token type matches, otherwise throws error
    pub fn expect(&mut self, ttype: TType) {
        if self.current().ttype == ttype {
            self.advance();
        } else {
            println!(
                "Error: Expected '{:?}', got '{:?}'",
                ttype,
                self.current().ttype
            );
            std::process::exit(1);
        }
    }
}

pub struct Lexer {
    pub file_contents: String,
    pub tokens: Tokens,
    curr_line: usize,
    curr_idx: usize,
}

impl Lexer {
    fn new(file_contents: String) -> Self {
        Self {
            file_contents,
            tokens: Tokens::new(vec![]),
            curr_line: 0,
            curr_idx: 0,
        }
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
                lexer.tokens.tokens.push(Token::new(
                    TType::LPAREN,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1;
            } else if c == ')' {
                lexer.tokens.tokens.push(Token::new(
                    TType::RPAREN,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1;
            } else if c == '{' {
                lexer.tokens.tokens.push(Token::new(
                    TType::LCURLY,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1;
            } else if c == '}' {
                lexer.tokens.tokens.push(Token::new(
                    TType::RCURLY,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1
            } else if c == ';' {
                lexer.tokens.tokens.push(Token::new(
                    TType::SEMICOLON,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1
            } else if c == ',' {
                lexer.tokens.tokens.push(Token::new(
                    TType::COMMA,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1
            } else if c == '+' {
                lexer.tokens.tokens.push(Token::new(
                    TType::PLUS,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1
            } else if c == '*' {
                lexer.tokens.tokens.push(Token::new(
                    TType::STAR,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1
            } else if c == '/' {
                if lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] == b'/' {
                    while lexer.curr_idx < bytes.len() && bytes[lexer.curr_idx] != b'\n' {
                        lexer.curr_idx += 1;
                    }
                } else {
                    lexer.tokens.tokens.push(Token::new(
                        TType::SLASH,
                        lexer.curr_idx,
                        lexer.curr_idx + 1,
                    ));
                    lexer.curr_idx += 1;
                }
            } else if c == '=' {
                if lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] == b'=' {
                    lexer.tokens.tokens.push(Token::new(
                        TType::EQUALEQUAL,
                        lexer.curr_idx,
                        lexer.curr_idx + 2,
                    ));
                    lexer.curr_idx += 2;
                } else {
                    todo!();
                }
            } else if c == '|' {
                if lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] == b'|' {
                    lexer.tokens.tokens.push(Token::new(
                        TType::PIPEPIPE,
                        lexer.curr_idx,
                        lexer.curr_idx + 2,
                    ));
                    lexer.curr_idx += 2;
                } else {
                    todo!();
                }
            } else if c == ':' {
                if lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] == b':' {
                    lexer.tokens.tokens.push(Token::new(
                        TType::COLONCOLON,
                        lexer.curr_idx,
                        lexer.curr_idx + 2,
                    ));
                    lexer.curr_idx += 2;
                } else if lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] == b'=' {
                    lexer.tokens.tokens.push(Token::new(
                        TType::COLONEQUAL,
                        lexer.curr_idx,
                        lexer.curr_idx + 2,
                    ));
                    lexer.curr_idx += 2;
                } else {
                    lexer.tokens.tokens.push(Token::new(
                        TType::COLON,
                        lexer.curr_idx,
                        lexer.curr_idx + 1,
                    ));
                    lexer.curr_idx += 1;
                }
            } else if c == '-' {
                if lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] == b'>' {
                    lexer.tokens.tokens.push(Token::new(
                        TType::ARROW,
                        lexer.curr_idx,
                        lexer.curr_idx + 2,
                    ));
                    lexer.curr_idx += 2;
                } else {
                    lexer.tokens.tokens.push(Token::new(
                        TType::MINUS,
                        lexer.curr_idx,
                        lexer.curr_idx + 1,
                    ));
                    lexer.curr_idx += 1;
                }
            } else if c.is_ascii_digit() {
                let start = lexer.curr_idx;
                while lexer.curr_idx < bytes.len()
                    && (bytes[lexer.curr_idx] as char).is_ascii_digit()
                {
                    lexer.curr_idx += 1;
                }
                lexer
                    .tokens
                    .tokens
                    .push(Token::new(TType::NUMBER, start, lexer.curr_idx));
            } else if c.is_ascii_alphanumeric() || c == '_' {
                let start = lexer.curr_idx;
                while lexer.curr_idx < bytes.len()
                    && ((bytes[lexer.curr_idx] as char).is_ascii_alphanumeric()
                        || bytes[lexer.curr_idx] == b'_')
                {
                    lexer.curr_idx += 1;
                }
                lexer
                    .tokens
                    .tokens
                    .push(Token::new(TType::IDENT, start, lexer.curr_idx));
            } else if c == '"' {
                let start = lexer.curr_idx;
                while lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] != b'"' {
                    lexer.curr_idx += 1;
                }
                lexer.curr_idx += 2; // Skip quotes
                lexer
                    .tokens
                    .tokens
                    .push(Token::new(TType::STRING, start + 1, lexer.curr_idx - 1));
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
