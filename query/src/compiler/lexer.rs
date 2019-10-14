use ast::common::Sign;
use error::{Error, Result};

pub struct Lexer {
    buf: Vec<char>,
    len: usize, // total character (not byte) count of input
    cursor: usize,
    pub col: usize,
    pub line: usize,
    is_eof: bool,
    cur_token: Option<InputToken>,
    peeked_token: Option<InputToken>,
    savepoint: Option<Savepoint>,
}

/// Savepoint captures the state of the lexer before some operation that may need to be undone
struct Savepoint {
    cursor: usize,
    col: usize,
    line: usize,
}

impl Lexer {
    pub fn from_string<S: Into<String>>(input: S) -> Lexer {
        let buf: Vec<char> = input
            .into()
            .chars()
            .into_iter()
            .map(|c| c.clone())
            .collect();
        let char_len = buf.len();
        let is_eof = char_len == 0;

        Lexer {
            buf: buf,
            len: char_len,
            cursor: 0,
            col: 0,
            line: 0,
            is_eof: is_eof,
            cur_token: None,
            peeked_token: None,
            savepoint: None,
        }
    }

    pub fn next_token(&mut self) -> Result<InputToken> {
        self.consume_whitespace();

        let next_token = match self.peek() {
            Some('+') | Some('-') => {
                self.snapshot();
                if let Some(num_token) = self.read_number().ok() {
                    Ok(num_token)
                } else {
                    self.undo();
                    self.read_operator()
                }
            }

            Some('*') | Some('/') | Some('=') | Some('<') | Some('>') | Some('!') | Some('%') => {
                self.read_operator()
            }

            Some('0'..='9') => self.read_number(),

            // TODO: Support unicode identifiers
            Some('A'..='z') => self.read_identifier_or_keyword(),

            Some('"') => self.read_quoted_identifier(),

            Some('\'') => self.read_string(),

            Some(',') => self.read_next_char_as_token(Token::Comma, TokenType::Punctuation),
            Some('.') => self.read_next_char_as_token(Token::Period, TokenType::Punctuation),
            Some('(') => self.read_next_char_as_token(Token::ParenL, TokenType::Punctuation),
            Some(')') => self.read_next_char_as_token(Token::ParenR, TokenType::Punctuation),
            Some(';') => self.read_next_char_as_token(Token::Semicolon, TokenType::Punctuation),
            Some(':') => self.read_next_char_as_token(Token::Colon, TokenType::Punctuation),
            Some('?') => self.read_next_char_as_token(Token::QuestionMark, TokenType::Punctuation),

            Some(_) => Err(self.err_unexpected_char()),

            None => {
                self.is_eof = true;
                Ok(self.token_eof(self.line, self.col))
            },
        };
        self.cur_token = next_token.ok();
        self.peeked_token = None;

        self.cur_token.clone().ok_or_else(|| self.err_unknown())
    }

    pub fn peek_token(&mut self) -> Result<InputToken> {
        if let Some(ref token) = self.peeked_token {
            return Ok(token.clone());
        }
        self.snapshot();
        let cur_token = self.cur_token.clone();
        let next_token = self.next_token()?;
        self.cur_token = cur_token;
        self.peeked_token = Some(next_token.clone());
        self.undo();

        Ok(next_token)
    }

    pub fn current_token(&self) -> Option<InputToken> {
        self.cur_token.clone()
    }

    pub fn current_line(&self) -> String {
        let start = self.cursor - self.col;
        let mut end = self.cursor;
        while end < self.len && self.buf[end] != '\n' {
            end += 1;
        }

        self.buf[start..end].into_iter().collect()
    }

    fn peek(&self) -> Option<char> {
        if self.cursor >= self.len {
            None
        } else {
            Some(self.buf[self.cursor])
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.peek().map(|c| {
            self.advance();
            c
        })
    }

    fn read_number(&mut self) -> Result<InputToken> {
        let line = self.line;
        let col = self.col;
        let mut sign = Sign::Pos;

        if let Some('-') = self.consume_any(vec!['+', '-']) {
            sign = Sign::Neg;
        }
        let int_start = self.cursor;
        while let Some('0'..='9') = self.peek() {
            self.next_char();
        }
        let s: String = self.buf[int_start..self.cursor].into_iter().collect();
        let integer = s.parse::<u64>().map_err(|_| Error::LexError {
            line: line,
            col: col,
            msg: format!("Cannot parse integer as digits: {}", s),
            context: self.current_line(),
        })?;

        // If the next character is a ".", try to lex as a numeric value
        if let Some(_) = self.consume_char('.') {
            let f_start = self.cursor;
            let f_line = self.line;

            while let Some('0'..='9') = self.peek() {
                self.next_char();
            }
            let s: String = self.buf[f_start..self.cursor].into_iter().collect();
            let fraction = s.parse::<u64>().map_err(|_| Error::LexError {
                line: line,
                col: col,
                msg: format!("Cannot parse fraction as digits: {}", s),
                context: self.current_line(),
            })?;

            return Ok(self.token_decimal(integer, fraction, sign, line, col));
        }

        // Otherwise, lex as an integer
        Ok(self.token_integer(integer, sign, line, col))
    }

    fn read_identifier_or_keyword(&mut self) -> Result<InputToken> {
        let start = self.cursor;
        let line = self.line;
        let col = self.col;

        loop {
            match self.peek() {
                Some('A'..='z') => {
                    self.next_char();
                }
                Some('0'..='9') => {
                    self.next_char();
                }
                _ => {
                    break;
                }
            }
        }
        let ident: String = self.buf[start..self.cursor].into_iter().collect();

        if let Some(bool_token) = self.try_match_boolean(&ident) {
            return Ok(self.token_boolean(bool_token, line, col));
        }

        if let Some(kw_token) = self.try_match_keyword(&ident) {
            return Ok(self.token_keyword(kw_token, line, col));
        }

        Ok(self.token_identifier(ident, line, col))
    }

    fn read_quoted_identifier(&mut self) -> Result<InputToken> {
        let line = self.line;
        let col = self.col;

        self.must_consume_char('"')?;
        let ident_start = self.cursor;
        loop {
            match self.peek() {
                Some('A'..='z') | Some('0'..='9') | Some(' ') => {
                    self.next_char();
                }
                _ => {
                    break;
                }
            }
        }
        let ident: String = self.buf[ident_start..self.cursor].into_iter().collect();
        self.must_consume_char('"')?;

        Ok(self.token_identifier(ident, line, col))
    }

    fn read_string(&mut self) -> Result<InputToken> {
        let line = self.line;
        let col = self.col;

        self.must_consume_char('\'')?;

        let mut string = String::new();
        let mut is_escape = false;
        loop {
            let next_char = self.peek().ok_or(self.err_eof())?;

            if is_escape {
                match next_char {
                    '\'' => string.push('\''),
                    'n' => string.push('\n'),
                    't' => string.push('\t'),
                    _ => {
                        return Err(self.err_unexpected_char());
                    }
                }

                is_escape = false;
            } else {
                match next_char {
                    '\'' => {
                        break;
                    } // Unescaped quote - end of string
                    '\\' => {
                        is_escape = true;
                    } // Start escape sequence
                    c => {
                        string.push(c);
                    } // Base case - append character
                }
            }

            self.advance();
        }

        self.must_consume_char('\'')?;

        Ok(self.token_string(string, line, col))
    }

    fn read_operator(&mut self) -> Result<InputToken> {
        let line = self.line;
        let col = self.col;
        let next_char = self.next_char().ok_or(self.err_eof())?;

        match next_char {
            '+' => Ok(self.token_operator("+", line, col)),
            '-' => Ok(self.token_operator("-", line, col)),
            '*' => Ok(self.token_operator("*", line, col)),
            '%' => Ok(self.token_operator("%", line, col)),
            '/' => Ok(self.token_operator("/", line, col)),
            '=' => Ok(self.token_operator("=", line, col)),
            '>' => {
                if let Some(_) = self.consume_char('=') {
                    Ok(self.token_operator(">=", line, col))
                } else {
                    Ok(self.token_operator(">", line, col))
                }
            }
            '<' => match self.consume_any(vec!['=', '>']) {
                Some('=') => Ok(self.token_operator("<=", line, col)),
                Some('>') => Ok(self.token_operator("<>", line, col)),
                _ => Ok(self.token_operator("<", line, col)),
            },
            '!' => {
                let _ = self.must_consume_char('=');
                Ok(self.token_operator("!=", line, col))
            }
            _ => Err(self.err_unexpected_char()),
        }
    }

    fn read_next_char_as_token(
        &mut self,
        token: Token,
        token_type: TokenType,
    ) -> Result<InputToken> {
        let line = self.line;
        let col = self.col;

        self.advance();

        Ok(InputToken {
            token,
            token_type,
            line,
            col,
        })
    }

    fn try_match_boolean(&self, ident: &str) -> Option<Token> {
        match ident.to_lowercase().as_str() {
            "true" => Some(Token::Boolean(true)),
            "false" => Some(Token::Boolean(false)),
            _ => None,
        }
    }

    fn try_match_keyword(&self, ident: &str) -> Option<Token> {
        match ident.to_lowercase().as_str() {
            "null" => Some(Token::Null),
            "not" => Some(Token::Not),
            "and" => Some(Token::And),
            "or" => Some(Token::Or),
            "like" => Some(Token::Like),
            "ilike" => Some(Token::Ilike),
            "between" => Some(Token::Between),
            "in" => Some(Token::In),
            "is" => Some(Token::Is),
            "select" => Some(Token::Select),
            "all" => Some(Token::All),
            "distinct" => Some(Token::Distinct),
            "as" => Some(Token::As),
            "from" => Some(Token::From),
            "inner" => Some(Token::Inner),
            "outer" => Some(Token::Outer),
            "left" => Some(Token::Left),
            "right" => Some(Token::Right),
            "natural" => Some(Token::Natural),
            "cross" => Some(Token::Cross),
            "join" => Some(Token::Join),
            "on" => Some(Token::On),
            "using" => Some(Token::Using),
            "group" => Some(Token::Group),
            "having" => Some(Token::Having),
            "order" => Some(Token::Order),
            "limit" => Some(Token::Limit),
            "where" => Some(Token::Where),
            _ => None,
        }
    }

    fn consume_whitespace(&mut self) -> Option<()> {
        let start = self.cursor;
        while let Some(_) = self.consume_any(vec![' ', '\n', '\r', '\0']) {}
        if self.cursor == start {
            // Nothing consumed
            None
        } else {
            Some(())
        }
    }

    /// Consumes a character if it is an exact match of any of the specified characters
    /// If a match is found, an `Option::Some` of the matching character is returned.
    /// Otherwise an `Option::None` is returned.
    fn consume_any(&mut self, cs: Vec<char>) -> Option<char> {
        for c in cs.into_iter() {
            if let Some(_) = self.consume_char(c) {
                return Some(c);
            }
        }

        None
    }

    fn consume_char(&mut self, m: char) -> Option<char> {
        match self.peek() {
            Some(c) if c == m => {
                self.advance();
                Some(c)
            }
            _ => None,
        }
    }

    fn must_consume_char(&mut self, c: char) -> Result<()> {
        if let Some(_) = self.consume_char(c) {
            Ok(())
        } else {
            let actual_char = self.peek().ok_or(self.err_eof())?;

            Err(Error::LexError {
                line: self.line,
                col: self.col,
                msg: format!("Expected: {}. Got: {}", c, actual_char),
                context: self.current_line(),
            })
        }
    }

    // Does not check bounds of self.buf - assumes it's called from a checked context
    fn advance(&mut self) {
        match self.buf[self.cursor] {
            '\n' => {
                self.line += 1;
                self.col = 0;
            }
            _ => {
                self.col += 1;
            }
        }
        self.cursor += 1;
    }

    fn err_eof(&self) -> Error {
        Error::LexError {
            line: self.line,
            col: self.col,
            msg: "Unexpected EOF".to_string(),
            context: self.current_line(),
        }
    }

    fn err_unexpected_char(&self) -> Error {
        if let Some(c) = self.peek() {
            Error::LexError {
                line: self.line,
                col: self.col,
                msg: format!("Unexpected character: {}", c),
                context: self.current_line(),
            }
        } else {
            self.err_eof()
        }
    }

    fn err_unknown(&self) -> Error {
        Error::LexError {
            line: self.line,
            col: self.col,
            msg: "Unknown error".to_string(),
            context: self.current_line(),
        }
    }

    fn snapshot(&mut self) {
        self.savepoint = Some(Savepoint {
            cursor: self.cursor,
            line: self.line,
            col: self.col,
        });
    }

    fn undo(&mut self) {
        if let Some(ref sp) = self.savepoint {
            self.cursor = sp.cursor;
            self.line = sp.line;
            self.col = sp.col;
        }
        self.savepoint = None;
    }

    fn token_eof(&self, line: usize, col: usize) -> InputToken {
        InputToken {
            token: Token::Eof,
            token_type: TokenType::Eof,
            line: line,
            col: col,
        }
    }

    fn token_integer(&self, integer: u64, sign: Sign, line: usize, col: usize) -> InputToken {
        InputToken {
            token: Token::Integer { integer, sign },
            token_type: TokenType::Integer,
            line: line,
            col: col,
        }
    }

    fn token_decimal(
        &self,
        integer: u64,
        fraction: u64,
        sign: Sign,
        line: usize,
        col: usize,
    ) -> InputToken {
        InputToken {
            token: Token::Decimal {
                integer,
                fraction,
                sign,
            },
            token_type: TokenType::Decimal,
            line: line,
            col: col,
        }
    }

    fn token_boolean(&self, boolean: Token, line: usize, col: usize) -> InputToken {
        InputToken {
            token: boolean,
            token_type: TokenType::Boolean,
            line: line,
            col: col,
        }
    }

    fn token_keyword(&self, keyword: Token, line: usize, col: usize) -> InputToken {
        InputToken {
            token: keyword,
            token_type: TokenType::Keyword,
            line: line,
            col: col,
        }
    }

    fn token_identifier(&self, ident: String, line: usize, col: usize) -> InputToken {
        InputToken {
            token: Token::Identifier(ident),
            token_type: TokenType::Identifier,
            line: line,
            col: col,
        }
    }

    fn token_string(&self, string: String, line: usize, col: usize) -> InputToken {
        InputToken {
            token: Token::String(string),
            token_type: TokenType::String,
            line: line,
            col: col,
        }
    }

    fn token_operator(&self, op: &'static str, line: usize, col: usize) -> InputToken {
        InputToken {
            token: Token::Operator(op.to_string()),
            token_type: TokenType::Operator,
            line,
            col,
        }
    }
}

impl Iterator for Lexer {
    type Item = InputToken;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_eof {
            return None;
        }
        self.next_token().ok()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InputToken {
    pub token: Token,
    pub token_type: TokenType,
    pub col: usize,
    pub line: usize,
    // TODO: Add a &str[] into input buffer
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    Integer,
    Decimal,
    Boolean,
    Identifier,
    String,
    Operator,
    Keyword,
    Punctuation,
    Eof,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Data Types
    Integer {
        integer: u64,
        sign: Sign,
    },
    Decimal {
        integer: u64,
        fraction: u64,
        sign: Sign,
    },
    Identifier(String),
    String(String),
    Operator(String),
    Boolean(bool),

    // Keywords - Not sure whether it makes more sense to identify keywords during lexing or parsing.
    // I think that deferring until parsing will introduce ambiguity.
    Null,
    Not,
    And,
    Or,
    Like,
    Ilike,
    Between,
    In,
    Is,
    Select,
    All,
    Distinct,
    As,
    From,
    Inner,
    Outer,
    Left,
    Right,
    Natural,
    Cross,
    Join,
    On,
    Using,
    Group,
    Having,
    Order,
    Limit,
    Where,

    // Punctuation
    Comma,
    Period,
    ParenL,
    ParenR,
    Colon,
    Semicolon,
    QuestionMark,

    // Misc
    Eof,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn lexes_integer() {
        let in_token = Lexer::from_string("12345").next_token().unwrap();
        assert_eq!(
            Token::Integer {
                integer: 12345,
                sign: Sign::Pos
            },
            in_token.token
        );
        assert_eq!(TokenType::Integer, in_token.token_type);

        let in_token = Lexer::from_string("-45").next_token().unwrap();
        assert_eq!(
            Token::Integer {
                integer: 45,
                sign: Sign::Neg
            },
            in_token.token
        );
        assert_eq!(TokenType::Integer, in_token.token_type);
    }

    #[test]
    fn lexes_decimal() {
        let in_token = Lexer::from_string("123.45").next_token().unwrap();
        assert_eq!(
            Token::Decimal {
                integer: 123,
                fraction: 45,
                sign: Sign::Pos,
            },
            in_token.token
        );

        let in_token = Lexer::from_string("-0.81235").next_token().unwrap();
        assert_eq!(
            Token::Decimal {
                integer: 0,
                fraction: 81235,
                sign: Sign::Neg,
            },
            in_token.token
        );
    }

    #[test]
    fn lexes_boolean() {
        let in_token = Lexer::from_string("true").next_token().unwrap();
        assert_eq!(Token::Boolean(true), in_token.token);

        let in_token = Lexer::from_string("false").next_token().unwrap();
        assert_eq!(Token::Boolean(false), in_token.token);
    }

    #[test]
    fn lexes_misc_keywords() {
        let in_token = Lexer::from_string("null").next_token().unwrap();
        assert_eq!(Token::Null, in_token.token);

        let in_token = Lexer::from_string("not").next_token().unwrap();
        assert_eq!(Token::Not, in_token.token);

        let in_token = Lexer::from_string("and").next_token().unwrap();
        assert_eq!(Token::And, in_token.token);

        let in_token = Lexer::from_string("or").next_token().unwrap();
        assert_eq!(Token::Or, in_token.token);

        let in_token = Lexer::from_string("like").next_token().unwrap();
        assert_eq!(Token::Like, in_token.token);

        let in_token = Lexer::from_string("ilike").next_token().unwrap();
        assert_eq!(Token::Ilike, in_token.token);

        let in_token = Lexer::from_string("between").next_token().unwrap();
        assert_eq!(Token::Between, in_token.token);

        let in_token = Lexer::from_string("in").next_token().unwrap();
        assert_eq!(Token::In, in_token.token);

        let in_token = Lexer::from_string("is").next_token().unwrap();
        assert_eq!(Token::Is, in_token.token);

        let in_token = Lexer::from_string("select").next_token().unwrap();
        assert_eq!(Token::Select, in_token.token);

        let in_token = Lexer::from_string("inner").next_token().unwrap();
        assert_eq!(Token::Inner, in_token.token);

        let in_token = Lexer::from_string("outer").next_token().unwrap();
        assert_eq!(Token::Outer, in_token.token);

        let in_token = Lexer::from_string("left").next_token().unwrap();
        assert_eq!(Token::Left, in_token.token);

        let in_token = Lexer::from_string("right").next_token().unwrap();
        assert_eq!(Token::Right, in_token.token);

        let in_token = Lexer::from_string("natural").next_token().unwrap();
        assert_eq!(Token::Natural, in_token.token);

        let in_token = Lexer::from_string("cross").next_token().unwrap();
        assert_eq!(Token::Cross, in_token.token);

        let in_token = Lexer::from_string("join").next_token().unwrap();
        assert_eq!(Token::Join, in_token.token);

        let in_token = Lexer::from_string("on").next_token().unwrap();
        assert_eq!(Token::On, in_token.token);

        let in_token = Lexer::from_string("using").next_token().unwrap();
        assert_eq!(Token::Using, in_token.token);

        let in_token = Lexer::from_string("all").next_token().unwrap();
        assert_eq!(Token::All, in_token.token);

        let in_token = Lexer::from_string("distinct").next_token().unwrap();
        assert_eq!(Token::Distinct, in_token.token);

        let in_token = Lexer::from_string("from").next_token().unwrap();
        assert_eq!(Token::From, in_token.token);

        let in_token = Lexer::from_string("as").next_token().unwrap();
        assert_eq!(Token::As, in_token.token);
    }

    #[test]
    fn lexes_unquoted_identifier() {
        let in_token = Lexer::from_string("foo bar").next_token().unwrap();
        assert_eq!(Token::Identifier("foo".to_string()), in_token.token);
    }

    #[test]
    fn lexes_quoted_identifier() {
        let in_token = Lexer::from_string("\"foo bar\"").next_token().unwrap();
        assert_eq!(Token::Identifier("foo bar".to_string()), in_token.token);
    }

    #[test]
    fn lexes_basic_string() {
        let in_token = Lexer::from_string("'foo bar'").next_token().unwrap();
        assert_eq!(Token::String("foo bar".to_string()), in_token.token);
    }

    #[test]
    fn lexes_escaped_string() {
        let in_token = Lexer::from_string("'\\'foo\\'\\nbar'")
            .next_token()
            .unwrap();
        assert_eq!(Token::String("\'foo\'\nbar".to_string()), in_token.token);
    }

    #[test]
    fn lexes_comma() {
        let in_token = Lexer::from_string(",").next_token().unwrap();
        assert_eq!(Token::Comma, in_token.token);
    }

    #[test]
    fn lexes_period() {
        let in_token = Lexer::from_string(".").next_token().unwrap();
        assert_eq!(Token::Period, in_token.token);
    }

    #[test]
    fn lexes_semicolon() {
        let in_token = Lexer::from_string(";").next_token().unwrap();
        assert_eq!(Token::Semicolon, in_token.token);
    }

    #[test]
    fn lexes_colon() {
        let in_token = Lexer::from_string(":").next_token().unwrap();
        assert_eq!(Token::Colon, in_token.token);
    }

    #[test]
    fn lexes_question_mark() {
        let in_token = Lexer::from_string("?").next_token().unwrap();
        assert_eq!(Token::QuestionMark, in_token.token);
    }

    #[test]
    fn lexes_parens() {
        let in_token = Lexer::from_string("(").next_token().unwrap();
        assert_eq!(Token::ParenL, in_token.token);

        let in_token = Lexer::from_string(")").next_token().unwrap();
        assert_eq!(Token::ParenR, in_token.token);
    }

    #[test]
    fn lexes_operators() {
        match Lexer::from_string("+").next_token() {
            Ok(in_token) => {
                assert_eq!(Token::Operator("+".to_string()), in_token.token);
            }
            Err(err) => panic!("{}", err),
        }

        let in_token = Lexer::from_string("-").next_token().unwrap();
        assert_eq!(Token::Operator("-".to_string()), in_token.token);

        let in_token = Lexer::from_string("*").next_token().unwrap();
        assert_eq!(Token::Operator("*".to_string()), in_token.token);

        let in_token = Lexer::from_string("/").next_token().unwrap();
        assert_eq!(Token::Operator("/".to_string()), in_token.token);

        let in_token = Lexer::from_string("%").next_token().unwrap();
        assert_eq!(Token::Operator("%".to_string()), in_token.token);

        let in_token = Lexer::from_string("=").next_token().unwrap();
        assert_eq!(Token::Operator("=".to_string()), in_token.token);

        let in_token = Lexer::from_string("<>").next_token().unwrap();
        assert_eq!(Token::Operator("<>".to_string()), in_token.token);

        let in_token = Lexer::from_string("!=").next_token().unwrap();
        assert_eq!(Token::Operator("!=".to_string()), in_token.token);

        let in_token = Lexer::from_string("<").next_token().unwrap();
        assert_eq!(Token::Operator("<".to_string()), in_token.token);

        let in_token = Lexer::from_string("<=").next_token().unwrap();
        assert_eq!(Token::Operator("<=".to_string()), in_token.token);

        let in_token = Lexer::from_string(">").next_token().unwrap();
        assert_eq!(Token::Operator(">".to_string()), in_token.token);

        let in_token = Lexer::from_string(">=").next_token().unwrap();
        assert_eq!(Token::Operator(">=".to_string()), in_token.token);
    }

    #[test]
    fn returns_error_on_open_string() {
        let lex_result = Lexer::from_string("'Hello, ").next_token();
        assert!(lex_result.is_err());
    }

    #[test]
    fn lexes_eof() {
        let in_token = Lexer::from_string("").next_token().unwrap();
        assert_eq!(Token::Eof, in_token.token);

        let in_token = Lexer::from_string("          ").next_token().unwrap();
        assert_eq!(Token::Eof, in_token.token);
    }

    #[test]
    fn lexes_whole_string() {
        let mut l = Lexer::from_string(
            "SELECT (12 + -42.7) as \"my column\", 'foo\\nbar' as my_str FROM tbl1;",
        );
        let mut tokens = Vec::new();
        loop {
            match l.next_token().unwrap() {
                InputToken {
                    token: Token::Eof, ..
                } => {
                    break;
                }
                InputToken { token, .. } => {
                    tokens.push(token);
                }
            }
        }

        assert_eq!(
            vec![
                Token::Select,
                Token::ParenL,
                Token::Integer {
                    integer: 12,
                    sign: Sign::Pos
                },
                Token::Operator("+".to_string()),
                Token::Decimal {
                    integer: 42,
                    fraction: 7,
                    sign: Sign::Neg
                },
                Token::ParenR,
                Token::As,
                Token::Identifier("my column".to_string()),
                Token::Comma,
                Token::String("foo\nbar".to_string()),
                Token::As,
                Token::Identifier("my_str".to_string()),
                Token::From,
                Token::Identifier("tbl1".to_string()),
                Token::Semicolon,
            ],
            tokens
        );
    }

    #[test]
    fn peeking_next() {
        let mut l = Lexer::from_string("a b");

        assert_eq!(
            Token::Identifier("a".to_string()),
            l.peek_token().unwrap().token
        );
        assert_eq!(
            Token::Identifier("a".to_string()),
            l.next_token().unwrap().token
        );
        assert_eq!(
            Token::Identifier("b".to_string()),
            l.peek_token().unwrap().token
        );
        assert_eq!(
            Token::Identifier("b".to_string()),
            l.next_token().unwrap().token
        );
    }

    #[test]
    fn getting_current() {
        let mut l = Lexer::from_string("a b");

        assert!(l.current_token().is_none());

        l.next_token();
        assert_eq!(
            Token::Identifier("a".to_string()),
            l.current_token().unwrap().token
        );

        // Peeking does not chage current
        l.peek_token();
        assert_eq!(
            Token::Identifier("a".to_string()),
            l.current_token().unwrap().token
        );
    }
}
