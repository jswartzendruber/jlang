#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TType {
    ColonColon,
    ColonEqual,
    EqualEqual,
    Identifier,
    Semicolon,
    PipePipe,
    Number,
    RParen,
    LParen,
    RCurly,
    LCurly,
    String,
    Arrow,
    Colon,
    Comma,
    Slash,
    Minus,
    Star,
    Plus,
}

#[derive(Debug, Clone)]
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
            Some(&self.tokens[self.curr_idx + 1])
        } else {
            None
        }
    }

    // Returns current value, and then advances one token.
    pub fn advance(&mut self) -> Option<&Token> {
        if self.is_at_end() {
            None
        } else {
            self.curr_idx += 1;
            Some(&self.tokens[self.curr_idx - 1])
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
                    TType::LParen,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1;
            } else if c == ')' {
                lexer.tokens.tokens.push(Token::new(
                    TType::RParen,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1;
            } else if c == '{' {
                lexer.tokens.tokens.push(Token::new(
                    TType::LCurly,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1;
            } else if c == '}' {
                lexer.tokens.tokens.push(Token::new(
                    TType::RCurly,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1
            } else if c == ';' {
                lexer.tokens.tokens.push(Token::new(
                    TType::Semicolon,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1
            } else if c == ',' {
                lexer.tokens.tokens.push(Token::new(
                    TType::Comma,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1
            } else if c == '+' {
                lexer.tokens.tokens.push(Token::new(
                    TType::Plus,
                    lexer.curr_idx,
                    lexer.curr_idx + 1,
                ));
                lexer.curr_idx += 1
            } else if c == '*' {
                lexer.tokens.tokens.push(Token::new(
                    TType::Star,
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
                        TType::Slash,
                        lexer.curr_idx,
                        lexer.curr_idx + 1,
                    ));
                    lexer.curr_idx += 1;
                }
            } else if c == '=' {
                if lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] == b'=' {
                    lexer.tokens.tokens.push(Token::new(
                        TType::EqualEqual,
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
                        TType::PipePipe,
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
                        TType::ColonColon,
                        lexer.curr_idx,
                        lexer.curr_idx + 2,
                    ));
                    lexer.curr_idx += 2;
                } else if lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] == b'=' {
                    lexer.tokens.tokens.push(Token::new(
                        TType::ColonEqual,
                        lexer.curr_idx,
                        lexer.curr_idx + 2,
                    ));
                    lexer.curr_idx += 2;
                } else {
                    lexer.tokens.tokens.push(Token::new(
                        TType::Colon,
                        lexer.curr_idx,
                        lexer.curr_idx + 1,
                    ));
                    lexer.curr_idx += 1;
                }
            } else if c == '-' {
                if lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] == b'>' {
                    lexer.tokens.tokens.push(Token::new(
                        TType::Arrow,
                        lexer.curr_idx,
                        lexer.curr_idx + 2,
                    ));
                    lexer.curr_idx += 2;
                } else {
                    lexer.tokens.tokens.push(Token::new(
                        TType::Minus,
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
                    .push(Token::new(TType::Number, start, lexer.curr_idx));
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
                    .push(Token::new(TType::Identifier, start, lexer.curr_idx));
            } else if c == '"' {
                let start = lexer.curr_idx;
                while lexer.curr_idx + 1 < bytes.len() && bytes[lexer.curr_idx + 1] != b'"' {
                    lexer.curr_idx += 1;
                }
                lexer.curr_idx += 2; // Skip quotes
                lexer
                    .tokens
                    .tokens
                    .push(Token::new(TType::String, start + 1, lexer.curr_idx - 1));
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
