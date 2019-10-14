use super::lexer::{InputToken, Lexer, Token, TokenType};
use super::precedence_table::PrecedenceTable;
use ast::common::Sign;
use ast::expr::*;
use ast::query::*;
use error::{Error, Result};

pub struct Parser {
    lexer: Lexer,
    precedences: PrecedenceTable,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer,
            precedences: Default::default(),
        }
    }

    pub fn parse(&mut self) -> Result<Query> {
        self.lexer.next_token();
        match self.lexer.current_token() {
            Some(InputToken { token, .. }) => match token {
                Token::Select => Ok(Query::Select(Box::new(self.parse_select_query()?))),
                _ => Err(self.error("Not a known query type")),
            },
            _ => Err(self.error("Empty query")),
        }
    }

    fn parse_select_query(&mut self) -> Result<SelectQuery> {
        self.must_parse_token(Token::Select)?;
        let select_expr = self.parse_select()?;
        let from_expr = self.parse_from()?;
        let where_expr = self.parse_where()?;

        let query = SelectQuery {
            select: select_expr,
            from: from_expr.unwrap_or(Vec::new()),
            where_: where_expr,
        };

        Ok(query)
    }

    fn parse_select(&mut self) -> Result<SelectExpr> {
        let mut modifier: Option<SelectModifier> = None;
        if let Some(_) = self.try_parse_token(Token::All) {
            modifier = Some(SelectModifier::All);
        }
        if let Some(_) = self.try_parse_token(Token::Distinct) {
            modifier = Some(SelectModifier::Distinct);
        }

        let mut result_columns = Vec::new();
        loop {
            if let Some(result_column) = self.try_parse_result_column()? {
                result_columns.push(result_column);
            }
            if self.try_parse_token(Token::Comma).is_none() {
                break;
            }
        }

        Ok(SelectExpr {
            modifier,
            result_columns,
        })
    }

    fn parse_from(&mut self) -> Result<Option<Vec<TableRef>>> {
        if let None = self.try_parse_token(Token::From) {
            return Ok(None);
        }

        let mut table_refs = Vec::new();
        loop {
            if let Some(table_ref) = self.try_parse_table_ref()? {
                table_refs.push(table_ref);
            }
            if let None = self.try_parse_token(Token::Comma) {
                break;
            }
        }

        Ok(Some(table_refs))
    }

    fn parse_where(&mut self) -> Result<Option<Expr>> {
        if let None = self.try_parse_token(Token::Where) {
            return Ok(None);
        }

        self.try_parse_expr(0)
    }

    fn try_parse_result_column(&mut self) -> Result<Option<Expr>> {
        if let Some(op) = self.try_parse_wildcard() {
            return ok_some(Expr::Column(ColumnExpr::unqualified_any()));
        }

        if let Some(expr) = self.try_parse_expr(0)? {
            if let (Token::As, Token::Identifier(ref alias)) = self.current_and_lookahead()? {
                self.advance()?;
                self.advance()?;
                return ok_some(Expr::aliased(expr, alias));
            }

            if let Some(Token::Identifier(ref alias)) =
                self.try_parse_token_type(TokenType::Identifier)
            {
                return ok_some(Expr::aliased(expr, alias));
            }

            return ok_some(expr);
        }

        Ok(None)
    }

    fn try_parse_table_ref(&mut self) -> Result<Option<TableRef>> {
        match self.try_parse_materialized_table()? {
            Some(tbl) => {
                // inner cross left right natural join
                // Unwrap should be safe because the parser guarantees that it is never None after the first token
                match self.current_token().unwrap() {
                    Token::Join
                    | Token::Inner
                    | Token::Left
                    | Token::Right
                    | Token::Natural
                    | Token::Cross => ok_some(self.must_parse_joined_table(tbl)?),
                    _ => ok_some(tbl),
                }
            }
            None => Ok(None),
        }
    }

    fn try_parse_materialized_table(&mut self) -> Result<Option<TableRef>> {
        let mut table_ref = if let Some(subquery) = self.try_parse_subquery()? {
            subquery
        } else if let Some(Token::Identifier(ref tbl_name)) =
            self.try_parse_token_type(TokenType::Identifier)
        {
            TableRef::table(tbl_name.to_string())
        } else {
            return Ok(None);
        };

        if let (Token::As, Token::Identifier(ref alias)) = self.current_and_lookahead()? {
            self.advance()?;
            self.advance()?;

            table_ref = TableRef::aliased(table_ref, alias.to_string());
        } else if let Some(Token::Identifier(ref alias)) =
            self.try_parse_token_type(TokenType::Identifier)
        {
            table_ref = TableRef::aliased(table_ref, alias.to_string());
        }

        ok_some(table_ref)
    }

    fn try_parse_subquery(&mut self) -> Result<Option<TableRef>> {
        if let Some(_) = self.try_parse_token(Token::ParenL) {
            let inner_select = self.parse_select_query()?;
            let subquery = TableRef::subquery(inner_select);

            self.must_parse_token(Token::ParenR)?;
            return ok_some(subquery);
        }

        Ok(None)
    }

    fn must_parse_joined_table(&mut self, left: TableRef) -> Result<TableRef> {
        match self.current_token() {
            Some(token) => match token {
                Token::Join | Token::Inner | Token::Cross => {
                    self.must_parse_inner_or_cross_join(left)
                }
                Token::Left | Token::Right => self.must_parse_outer_join(left),
                Token::Natural => self.must_parse_natural_join(left),
                _ => Ok(left), // ε case
            },
            None => Ok(left),
        }
    }

    fn must_parse_inner_or_cross_join(&mut self, left: TableRef) -> Result<TableRef> {
        let join_op = match self.current_token() {
            Some(Token::Inner) => {
                self.advance()?;
                JoinOperator::Inner
            }
            Some(Token::Cross) => {
                self.advance()?;
                JoinOperator::Cross
            }
            _ => JoinOperator::Inner,
        };
        let _ = self.must_parse_token(Token::Join)?;
        let right = match self.try_parse_table_ref()? {
            Some(tbl) => tbl,
            None => {
                let join_type = match join_op {
                    JoinOperator::Inner => "inner",
                    JoinOperator::Cross => "cross",
                    _ => unreachable!(),
                };
                return Err(self.error(format!(
                    "expected table on righthand side of {} join",
                    join_type
                )));
            }
        };
        let constraint = self.try_parse_join_constraint()?;

        Ok(TableRef::joined(left, right, join_op, constraint))
    }

    fn must_parse_outer_join(&mut self, left: TableRef) -> Result<TableRef> {
        unimplemented!();
    }

    fn must_parse_natural_join(&mut self, left: TableRef) -> Result<TableRef> {
        unimplemented!();
    }

    fn try_parse_join_constraint(&mut self) -> Result<Option<JoinConstraint>> {
        match self.current_token() {
            Some(Token::On) => {
                self.advance()?;
                match self.try_parse_expr(0)? {
                    Some(expr) => ok_some(JoinConstraint::on(expr)),
                    None => Err(self.error("expected join condition after ON")),
                }
            }

            Some(Token::Using) => {
                self.advance()?;
                let _ = self.must_parse_token(Token::ParenL);
                let mut columns = Vec::new();
                loop {
                    if let Token::Identifier(ref col_name) =
                        self.must_parse_token_type(TokenType::Identifier)?
                    {
                        columns.push(col_name.to_string());
                    }

                    if self.try_parse_token(Token::Comma).is_none() {
                        break;
                    }
                }
                let _ = self.must_parse_token(Token::ParenR);

                ok_some(JoinConstraint::using(columns))
            }

            _ => Ok(None),
        }
    }

    fn parse_table_ref_ident(&mut self) -> Result<TableRef> {
        match self.must_parse_token_type(TokenType::Identifier)? {
            Token::Identifier(name) => Ok(TableRef::table(name)),
            _ => unreachable!(),
        }
    }

    // Try to parse an expression using the table-driven variant of Precedence Climbing
    // described in http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm
    fn try_parse_expr(&mut self, precedence: i8) -> Result<Option<Expr>> {
        match self.try_parse_base_expr_term()? {
            Some(mut term) => {
                let mut cur_prec = self.precedences.max_precedence();
                // Note that we do not currently support any postfix operators
                while let Some(op) = self.try_read_binary_operator_token() {
                    match self.precedences.lookup(op.clone()) {
                        Some(entry) => {
                            let op_precedence = entry.precedence();
                            if op_precedence < precedence || op_precedence > cur_prec {
                                break;
                            }
                            self.advance()?;

                            match self.try_parse_expr(entry.right())? {
                                Some(right_term) => {
                                    term = get_binary_expr(op, term, right_term)
                                        .ok_or_else(|| { self.error("No binary operation defined for operator") })?;
                                },
                                None => {
                                    return Err(self.error("Expected expresion on righthand side of binary operator"));
                                }
                            }

                            cur_prec = entry.next();
                        },
                        None => {
                            return Err(self.error(format!("No precedence found for operator, {:?}", op)));
                        }
                    }

                }
                ok_some(term)
            },
            None => Ok(None),
        }
    }

    fn try_parse_base_expr_term(&mut self) -> Result<Option<Expr>> {
        match self.try_parse_token_type(TokenType::Operator) {
            Some(Token::Operator(ref op)) if op == "+" => return self.try_parse_expr(2), // + expr is valid but a op-op - no need to record it in the AST
            Some(Token::Operator(ref op)) if op == "-" => {
                return match self.try_parse_expr(2)? { // TODO: Determine correct precedence for entry
                    Some(inner) => ok_some(Expr::negate(inner)),
                    None => Err(self.error("expected expression after unary: -"))
                };
            },
            Some(Token::Operator(ref op)) if op == "~" => unimplemented!("TODO: expr form: ~ expr"),
            None => {}
            _ => return Err(self.error("unexpected operator encountered while parsing expression")),
        };

        if let Some(_) = self.try_parse_token(Token::Not) {
            return self
                .try_parse_expr(1) // TODO: Determine correct precedence for entry
                .and_then(|maybe_inner_expr| match maybe_inner_expr {
                    Some(inner_expr) => ok_some(Expr::Not(Box::new(inner_expr))),
                    None => Err(self.error("expected expression after unary: NOT")),
                });
        }

        if let Some(_) = self.try_parse_token(Token::ParenL) {
            let expr = self
                .try_parse_expr(0)
                .and_then(|maybe_inner_expr| match maybe_inner_expr {
                    Some(inner_expr) => ok_some(inner_expr),
                    None => Err(self.error("expected parenthesized expression")),
                });
            self.must_parse_token(Token::ParenR)?;
            return expr;
        }

        if let Some(literal) = self.try_parse_literal()? {
            return ok_some(literal);
        }

        if let Some(qualified_column) = self.try_parse_qualified_column_ref()? {
            return ok_some(qualified_column);
        }

        if let Some(unqualified_column) = self.try_parse_unqualified_column_ref()? {
            return ok_some(unqualified_column);
        }

        Ok(None)
    }

    fn try_parse_qualified_column_ref(&mut self) -> Result<Option<Expr>> {
        match self.current_and_lookahead()? {
            (Token::Identifier(ref tbl_name), Token::Period) => {
                self.advance()?;
                self.advance()?;

                if let Some(_) = self.try_parse_wildcard() {
                    ok_some(Expr::Column(ColumnExpr::qualified_any(tbl_name)))
                } else if let Some(Token::Identifier(ref col_name)) =
                    self.try_parse_token_type(TokenType::Identifier)
                {
                    ok_some(Expr::Column(ColumnExpr::qualified_ref(tbl_name, col_name)))
                } else {
                    ok_some(Expr::Column(ColumnExpr::qualified_any(tbl_name)))
                }
            }
            _ => Ok(None),
        }
    }

    fn try_parse_unqualified_column_ref(&mut self) -> Result<Option<Expr>> {
        if let Some(Token::Identifier(ref col_name)) =
            self.try_parse_token_type(TokenType::Identifier)
        {
            return ok_some(Expr::Column(ColumnExpr::unqualified_ref(col_name)));
        }

        Ok(None)
    }

    fn try_read_binary_operator_token(&mut self) -> Option<Token> {
        match self.current_token().unwrap() {
            // t@ Token::Concat |
            t@ Token::And |
            t@ Token::Or => {
                Some(t)
            },
            t@ Token::Operator(..) => {
                if let Token::Operator(op) = t {
                    if op == "*" ||
                        op == "/" ||
                        op == "%" ||
                        op == "+" ||
                        op == "-" ||
                        op == "<" ||
                        op == "<=" ||
                        op == ">" ||
                        op == ">=" ||
                        op == "=" ||
                        op == "<>" ||
                        op == "!="
                    {
                        Some(Token::Operator(op))
                    } else {
                        None
                    }
                } else {
                    unreachable!("Must match operator");
                }
            },
            _ => None
        }
    }

    fn try_parse_literal(&mut self) -> Result<Option<Expr>> {
        if let Some(input_token) = self.lexer.current_token() {
            return Ok(match input_token.token {
                Token::Integer { .. } => Some(self.parse_expr_int()?),
                Token::String(_) => Some(self.parse_expr_string()?),
                Token::Decimal { .. } => Some(self.parse_expr_decimal()?),
                Token::Boolean(_) => Some(self.parse_expr_bool()?),
                Token::Null => Some(self.parse_expr_null()?),
                _ => None,
            });
        }

        Ok(None)
    }

    fn parse_expr_int(&mut self) -> Result<Expr> {
        match self.must_parse_token_type(TokenType::Integer)? {
            Token::Integer { integer, sign } => {
                let val = match sign {
                    Sign::Pos => integer as i64,
                    Sign::Neg => -(integer as i64),
                };

                Ok(Expr::Integer(val))
            }
            _ => unreachable!(),
        }
    }

    fn parse_expr_string(&mut self) -> Result<Expr> {
        match self.must_parse_token_type(TokenType::String)? {
            Token::String(val) => Ok(Expr::string(&val)),
            _ => unreachable!(),
        }
    }

    fn parse_expr_decimal(&mut self) -> Result<Expr> {
        match self.must_parse_token_type(TokenType::Decimal)? {
            Token::Decimal {
                integer,
                fraction,
                sign,
            } => {
                let val = match sign {
                    Sign::Pos => integer as i64,
                    Sign::Neg => -(integer as i64),
                };

                Ok(Expr::Integer(val))
            }
            _ => unreachable!(),
        }
    }

    fn parse_expr_bool(&mut self) -> Result<Expr> {
        match self.must_parse_token_type(TokenType::Boolean)? {
            Token::Boolean(val) => Ok(Expr::Boolean(val)),
            _ => unreachable!(),
        }
    }

    fn parse_expr_null(&mut self) -> Result<Expr> {
        self.must_parse_token(Token::Null).map(|_| Expr::Null)
    }

    fn maybe_parse_alias(&mut self, table_ref: TableRef) -> Result<TableRef> {
        if let Some(_) = self.try_parse_token(Token::As) {
            if let Token::Identifier(alias) = self.must_parse_token_type(TokenType::Identifier)? {
                return Ok(TableRef::aliased(table_ref, alias));
            }
        }

        Ok(table_ref)
    }

    fn try_parse_wildcard(&mut self) -> Option<Token> {
        if let Some(input_token) = self.lexer.current_token() {
            return match input_token.token {
                Token::Operator(ref c) if c == "*" => {
                    self.advance();
                    Some(input_token.token.clone())
                }
                _ => None,
            };
        }

        None
    }

    fn try_parse_token(&mut self, token: Token) -> Option<Token> {
        if let Some(current_token) = self.current_token() {
            if token == current_token {
                self.advance();
                return Some(current_token);
            }
        }

        None
    }

    fn must_parse_token(&mut self, token: Token) -> Result<Token> {
        self.try_parse_token(token.clone())
            .ok_or(self.error(format!("Expected token: {:?}", token)))
    }

    fn try_parse_token_type(&mut self, token_type: TokenType) -> Option<Token> {
        if let Some(input_token) = self.lexer.current_token() {
            if input_token.token_type == token_type {
                self.advance();
                return Some(input_token.token);
            }
        }

        None
    }

    fn must_parse_token_type(&mut self, token_type: TokenType) -> Result<Token> {
        self.try_parse_token_type(token_type)
            .ok_or(self.error(format!("Expected token of type: {:?}", token_type)))
    }

    fn current_token(&self) -> Option<Token> {
        self.lexer
            .current_token()
            .map(|input_token| input_token.token)
    }

    /// Get the current token along with a single token of lookahead
    fn current_and_lookahead(&mut self) -> Result<(Token, Token)> {
        match (self.lexer.current_token(), self.lexer.peek_token()) {
            (Some(InputToken { token: current, .. }), Ok(InputToken { token: next, .. })) => {
                Ok((current, next))
            }
            (_, Err(lex_error)) => {
                Err(self.error(format!("Lex error getting next token: {}", lex_error)))
            }
            _ => panic!("Should not have a lookahead token without a current token!"),
        }
    }

    fn advance(&mut self) -> Result<()> {
        self.lexer
            .next_token()
            .map_err(|lex_error| self.error(format!("Lex error getting next token: {}", lex_error)))
            .map(|_| ())
    }

    fn error<S: Into<String>>(&self, msg: S) -> Error {
        Error::ParseError {
            line: self.lexer.line,
            col: self.lexer.col,
            context: self.lexer.current_line(),
            msg: msg.into(),
        }
    }
}

fn ok_some<T>(val: T) -> Result<Option<T>> {
    Ok(Some(val))
}

// TODO: Consider moving into expr
fn get_binary_expr(op_token: Token, left: Expr, right: Expr) -> Option<Expr> {
    match op_token {
        Token::And => Some(Expr::and(left, right)),
        Token::Or => Some(Expr::or(left, right)),
        Token::Like => Some(Expr::like(left, right)),
        Token::Ilike => Some(Expr::ilike(left, right)),
        Token::Operator(op) => match op.as_ref() {
            "=" | "==" => Some(Expr::equal(left, right)),
            "<>" | "!=" => Some(Expr::not_equal(left, right)),
            "<" => Some(Expr::less_than(left, right)),
            "<=" => Some(Expr::less_than_or_equal(left, right)),
            ">" => Some(Expr::greater_than(left, right)),
            ">=" => Some(Expr::greater_than_or_equal(left, right)),
            "+" => Some(Expr::add(left, right)),
            "-" => Some(Expr::sub(left, right)),
            "*" => Some(Expr::mult(left, right)),
            "/" => Some(Expr::div(left, right)),
            "%" => Some(Expr::modulo(left, right)),
            "^" => Some(Expr::exp(left, right)),
            _ => None
        },
        _ => None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ast::builder::Builder;

    #[test]
    fn parses_trivial_select() {
        assert_parses_query(
            "SELECT 1",
            QueryBuilder::new()
                .with_selection(vec![QueryBuilder::select_integer(1)])
                .finish(),
        );
    }

    #[test]
    fn parses_single_column_select() {
        assert_parses_query(
            "SELECT id",
            QueryBuilder::new()
                .with_selection(vec![QueryBuilder::select_unqualified_column("id")])
                .finish(),
        );
    }

    #[test]
    fn parses_aliased_column_select() {
        assert_parses_query(
            "SELECT id as my_id",
            QueryBuilder::new()
                .with_selection(vec![QueryBuilder::select_aliased(
                    QueryBuilder::select_unqualified_column("id"),
                    "my_id",
                )]).finish(),
        );
    }

    #[test]
    fn parses_multi_column_select() {
        assert_parses_query(
            "SELECT user.first_name AS fname, false AS is_enabled, \"other tbl\".*",
            QueryBuilder::new()
                .with_selection(vec![
                    QueryBuilder::select_aliased(
                        QueryBuilder::select_qualified_column("user", "first_name"),
                        "fname",
                    ),
                    QueryBuilder::select_aliased(QueryBuilder::select_bool(false), "is_enabled"),
                    QueryBuilder::select_star_in_tbl("other tbl"),
                ]).finish(),
        );
    }

    #[test]
    fn parses_simple_from() {
        assert_parses_query(
            "SELECT * FROM my_tbl",
            QueryBuilder::new()
                .with_selection(vec![QueryBuilder::select_star()])
                .with_tables(vec![QueryBuilder::from_table("my_tbl")])
                .finish(),
        );
    }

    #[test]
    fn parses_multiple_tables() {
        assert_parses_query(
            "SELECT * FROM my_tbl, other AS \"other_tbl\"",
            QueryBuilder::new()
                .with_selection(vec![QueryBuilder::select_star()])
                .with_tables(vec![
                    QueryBuilder::from_table("my_tbl"),
                    QueryBuilder::from_aliased(QueryBuilder::from_table("other"), "other_tbl"),
                ]).finish(),
        );
    }

    #[test]
    fn parses_simple_subquery() {
        assert_parses_query(
            "SELECT inner_tbl.* FROM (SELECT id from user) as inner_tbl",
            QueryBuilder::new()
                .with_selection(vec![QueryBuilder::select_star_in_tbl("inner_tbl")])
                .with_tables(vec![QueryBuilder::from_aliased(
                    QueryBuilder::from_subquery(
                        QueryBuilder::new()
                            .with_selection(vec![QueryBuilder::select_unqualified_column("id")])
                            .with_tables(vec![QueryBuilder::from_table("user")])
                            .finish()
                            .as_select_query()
                            .unwrap(),
                    ),
                    "inner_tbl",
                )]).finish(),
        );
    }

    #[test]
    fn parses_simple_join() {
        assert_parses_query(
            "SELECT * FROM my_tbl JOIN other",
            QueryBuilder::new()
                .with_selection(vec![QueryBuilder::select_star()])
                .with_tables(vec![QueryBuilder::join(
                    QueryBuilder::from_table("my_tbl"),
                    QueryBuilder::from_table("other"),
                )]).finish(),
        );
    }

    #[test]
    fn parses_multiple_joins() {
        assert_parses_query(
            "SELECT * FROM a JOIN b JOIN c",
            QueryBuilder::new()
                .with_selection(vec![QueryBuilder::select_star()])
                .with_tables(vec![QueryBuilder::join(
                    QueryBuilder::from_table("a"),
                    QueryBuilder::join(
                        QueryBuilder::from_table("b"),
                        QueryBuilder::from_table("c"),
                    ),
                )]).finish(),
        );
    }

    #[test]
    fn parses_inner_join() {
        assert_parses_query(
            "SELECT * FROM my_tbl INNER JOIN other",
            QueryBuilder::new()
                .with_selection(vec![QueryBuilder::select_star()])
                .with_tables(vec![QueryBuilder::join(
                    QueryBuilder::from_table("my_tbl"),
                    QueryBuilder::from_table("other"),
                )]).finish(),
        );
    }

    #[test]
    fn parses_cross_join() {
        assert_parses_query(
            "SELECT * FROM my_tbl CROSS JOIN other",
            QueryBuilder::new()
                .with_selection(vec![QueryBuilder::select_star()])
                .with_tables(vec![
                    QueryBuilder::join(
                        QueryBuilder::from_table("my_tbl"),
                        QueryBuilder::from_table("other"),
                    ).with_join_operator(JoinOperator::Cross)
                    .unwrap(),
                ]).finish(),
        );
    }

    #[test]
    fn parses_join_with_on_constraint() {
        assert_parses_query(
            "SELECT * FROM my_tbl JOIN other ON my_tbl.id = other.my_tbl_id",
            QueryBuilder::new()
                .with_selection(vec![QueryBuilder::select_star()])
                .with_tables(vec![QueryBuilder::join_on(
                    QueryBuilder::from_table("my_tbl"),
                    QueryBuilder::from_table("other"),
                    Expr::equal(
                        Expr::column(ColumnExpr::qualified_ref("my_tbl", "id")),
                        Expr::column(ColumnExpr::qualified_ref("other", "my_tbl_id")),
                    ),
                )]).finish(),
        );
    }

    #[test]
    fn parses_join_with_using_constraint() {
        assert_parses_query(
            "SELECT * FROM users JOIN orders USING (city, state)",
            QueryBuilder::new()
                .with_selection(vec![QueryBuilder::select_star()])
                .with_tables(vec![QueryBuilder::join_using(
                    QueryBuilder::from_table("users"),
                    QueryBuilder::from_table("orders"),
                    vec!["city", "state"],
                )]).finish(),
        );
    }

    #[test]
    fn parses_where_clause() {
        assert_parses_query(
            "SELECT * FROM user WHERE first_name = 'Alexei'",
            QueryBuilder::new()
                .with_selection(vec![QueryBuilder::select_star()])
                .with_tables(vec![QueryBuilder::from_table("user")])
                .with_where(Expr::equal(
                    Expr::column(ColumnExpr::unqualified_ref("first_name")),
                    Expr::string("Alexei"),
                )).finish(),
        );
    }

    #[test]
    fn parses_complex_expressions_in_select() {
        assert_parses_query(
            "SELECT id > 0 AND first_name <> 'Bob' FROM \"user\"",
            QueryBuilder::new()
                .with_selection(vec![
                    Expr::and(
                        Expr::greater_than(
                            Expr::column(ColumnExpr::unqualified_ref("id")),
                            Expr::integer(0),
                        ),
                        Expr::not_equal(
                            Expr::column(ColumnExpr::unqualified_ref("first_name")),
                            Expr::string("Bob"),
                        )
                    )
                ])
                .with_tables(vec![QueryBuilder::from_table("user")])
                .finish(),
        );

    }

    fn assert_parses_query(sql: &str, expected_query: Query) {
        let lexer = Lexer::from_string(sql);
        let mut parser = Parser::new(lexer);

        match parser.parse() {
            Ok(parse_result) => {
                assert_eq!(parse_result, expected_query);
            }
            Err(err) => {
                panic!("{}", err);
            }
        }
    }

    // TODO: Test operator precedence and associativity
}
