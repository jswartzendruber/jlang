use std::process;

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
    line_start_idx: usize,
    pub from_line: usize,
}

impl Token {
    fn new(
        ttype: TType,
        start_idx: usize,
        end_idx: usize,
        line_start_idx: usize,
        from_line: usize,
    ) -> Self {
        Self {
            ttype,
            start_idx,
            end_idx,
            line_start_idx,
            from_line,
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

    pub fn copy_line(&self, file_contents: &str) -> String {
        let bytes = file_contents.as_bytes();
        let mut i = self.line_start_idx + 1;
        while i < file_contents.len() && bytes[i] != b'\n' {
            i += 1;
        }
        String::from(&file_contents[self.line_start_idx..i])
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
    pub fn expect(&mut self, ttype: TType, file_contents: &str) {
        if self.current().ttype == ttype {
            self.advance();
        } else {
            println!(
                "Error: Expected '{:?}', got '{}' on line {}.",
                ttype,
                self.current().copy_contents(file_contents).trim(),
                self.current().from_line
            );
            println!("  {}", self.current().copy_line(file_contents));
            println!();
            std::process::exit(1);
        }
    }
}

pub struct Lexer {
    pub file_contents: String,
    pub tokens: Tokens,
    curr_line_start: usize,
    curr_line: usize,
    curr_idx: usize,
    found_error: bool,
}

impl Lexer {
    fn new(file_contents: String) -> Self {
        Self {
            file_contents,
            tokens: Tokens::new(vec![]),
            curr_line_start: 0,
            curr_line: 1,
            curr_idx: 0,
            found_error: false,
        }
    }

    pub fn add_token(&self, ttype: TType, length: usize) -> Token {
        Token::new(
            ttype,
            self.curr_idx,
            self.curr_idx + length,
            self.curr_line_start,
            self.curr_line,
        )
    }

    pub fn add_token_specific(&self, ttype: TType, start: usize, end: usize) -> Token {
        Token::new(ttype, start, end, self.curr_line_start, self.curr_line)
    }

    pub fn lex(file_contents: String) -> Self {
        let mut lexer = Self::new(file_contents);

        let len = lexer.file_contents.len();
        let bytes = lexer.file_contents.as_bytes();
        while lexer.curr_idx < len {
            let c = bytes[lexer.curr_idx] as char;
            if c == ' ' || c == '\n' || c == '\t' {
                if bytes[lexer.curr_idx] == b'\n' {
                    lexer.curr_line += 1;
                    lexer.curr_line_start = lexer.curr_idx + 1;
                }
                lexer.curr_idx += 1;
            } else if c == '(' {
                lexer.tokens.tokens.push(lexer.add_token(TType::LParen, 1));
                lexer.curr_idx += 1;
            } else if c == ')' {
                lexer.tokens.tokens.push(lexer.add_token(TType::RParen, 1));
                lexer.curr_idx += 1;
            } else if c == '{' {
                lexer.tokens.tokens.push(lexer.add_token(TType::LCurly, 1));
                lexer.curr_idx += 1;
            } else if c == '}' {
                lexer.tokens.tokens.push(lexer.add_token(TType::RCurly, 1));
                lexer.curr_idx += 1;
            } else if c == ';' {
                lexer
                    .tokens
                    .tokens
                    .push(lexer.add_token(TType::Semicolon, 1));
                lexer.curr_idx += 1;
            } else if c == ',' {
                lexer.tokens.tokens.push(lexer.add_token(TType::Comma, 1));
                lexer.curr_idx += 1;
            } else if c == '+' {
                lexer.tokens.tokens.push(lexer.add_token(TType::Plus, 1));
                lexer.curr_idx += 1;
            } else if c == '*' {
                lexer.tokens.tokens.push(lexer.add_token(TType::Star, 1));
                lexer.curr_idx += 1;
            } else if c == '/' {
                if lexer.curr_idx + 1 < len && bytes[lexer.curr_idx + 1] == b'/' {
                    while lexer.curr_idx < len && bytes[lexer.curr_idx] != b'\n' {
                        lexer.curr_idx += 1;
                    }
                } else {
                    lexer.tokens.tokens.push(lexer.add_token(TType::Slash, 1));
                    lexer.curr_idx += 1;
                }
            } else if c == '=' {
                if lexer.curr_idx + 1 < len && bytes[lexer.curr_idx + 1] == b'=' {
                    lexer
                        .tokens
                        .tokens
                        .push(lexer.add_token(TType::EqualEqual, 2));
                    lexer.curr_idx += 2;
                } else {
                    todo!();
                }
            } else if c == '|' {
                if lexer.curr_idx + 1 < len && bytes[lexer.curr_idx + 1] == b'|' {
                    lexer
                        .tokens
                        .tokens
                        .push(lexer.add_token(TType::PipePipe, 2));
                    lexer.curr_idx += 2;
                } else {
                    todo!();
                }
            } else if c == ':' {
                if lexer.curr_idx + 1 < len && bytes[lexer.curr_idx + 1] == b':' {
                    lexer
                        .tokens
                        .tokens
                        .push(lexer.add_token(TType::ColonColon, 2));
                    lexer.curr_idx += 2;
                } else if lexer.curr_idx + 1 < len && bytes[lexer.curr_idx + 1] == b'=' {
                    lexer
                        .tokens
                        .tokens
                        .push(lexer.add_token(TType::ColonEqual, 2));
                    lexer.curr_idx += 2;
                } else {
                    lexer.tokens.tokens.push(lexer.add_token(TType::Colon, 1));
                    lexer.curr_idx += 1;
                }
            } else if c == '-' {
                if lexer.curr_idx + 1 < len && bytes[lexer.curr_idx + 1] == b'>' {
                    lexer.tokens.tokens.push(lexer.add_token(TType::Arrow, 2));
                    lexer.curr_idx += 2;
                } else {
                    lexer.tokens.tokens.push(lexer.add_token(TType::Minus, 2));
                    lexer.curr_idx += 2;
                }
            } else if c.is_ascii_digit() {
                let start = lexer.curr_idx;
                while lexer.curr_idx < len && (bytes[lexer.curr_idx] as char).is_ascii_digit() {
                    lexer.curr_idx += 1;
                }
                lexer.tokens.tokens.push(lexer.add_token_specific(
                    TType::Number,
                    start,
                    lexer.curr_idx,
                ));
            } else if c.is_ascii_alphanumeric() || c == '_' {
                let start = lexer.curr_idx;
                while lexer.curr_idx < len
                    && ((bytes[lexer.curr_idx] as char).is_ascii_alphanumeric()
                        || bytes[lexer.curr_idx] == b'_')
                {
                    lexer.curr_idx += 1;
                }
                lexer.tokens.tokens.push(lexer.add_token_specific(
                    TType::Identifier,
                    start,
                    lexer.curr_idx,
                ));
            } else if c == '"' {
                let start = lexer.curr_idx;
                while lexer.curr_idx + 1 < len && bytes[lexer.curr_idx + 1] != b'"' {
                    lexer.curr_idx += 1;
                }
                lexer.curr_idx += 2; // Skip quotes
                lexer.tokens.tokens.push(lexer.add_token_specific(
                    TType::String,
                    start + 1,
                    lexer.curr_idx - 1,
                ));
            } else {
                println!(
                    "Error: Unexpected token '{}' on line {}.",
                    c, lexer.curr_line
                );
                let mut i = lexer.curr_line_start + 1;
                while i < len && bytes[i] != b'\n' {
                    i += 1;
                }
                println!(
                    "  {}",
                    &lexer.file_contents[lexer.curr_line_start..i].trim()
                );
                println!();
                lexer.curr_idx += 1; // Skip over error token
                lexer.found_error = true; // Halt program after lexing file
            }
        }

        if lexer.found_error {
            process::exit(1);
        }

        lexer
    }
}
