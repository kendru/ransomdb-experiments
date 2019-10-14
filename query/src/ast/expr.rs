use error::{Error, Result};

#[derive(Debug, PartialEq, Clone)]
pub enum ExprType {
    Integer,
    Float,
    Decimal,
    Boolean,
    String,
    Unknown,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedExpr {
    expr_type: ExprType,
    expr: Expr,
}

impl TypedExpr {
    pub fn primitive_int(v: i64) -> TypedExpr {
        TypedExpr {
            expr_type: ExprType::Integer,
            expr: Expr::Integer(v),
        }
    }

    pub fn primitive_bool(v: bool) -> TypedExpr {
        TypedExpr {
            expr_type: ExprType::Boolean,
            expr: Expr::Boolean(v),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Null,
    Column(ColumnExpr),

    Equal(Box<Expr>, Box<Expr>),
    NotEqual(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    GreaterThanEqual(Box<Expr>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    LessThanEqual(Box<Expr>, Box<Expr>),

    Like(Box<Expr>, Box<Expr>),
    Ilike(Box<Expr>, Box<Expr>),

    IsNull(Box<Expr>),
    IsNotNull(Box<Expr>),

    And(Vec<Expr>),
    Or(Vec<Expr>),
    Not(Box<Expr>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Exp(Box<Expr>, Box<Expr>),
    Negate(Box<Expr>),

    FnCall(String, Vec<Expr>),

    Alias(Box<Expr>, String),
    Parenthesized(Box<Expr>),
    // TODO:
    // Exists
    // Date/Time
    // Misc. Functions
}

impl Expr {
    pub fn integer(n: i64) -> Expr {
        Expr::Integer(n)
    }

    pub fn boolean(b: bool) -> Expr {
        Expr::Boolean(b)
    }

    pub fn string(s: &str) -> Expr {
        Expr::String(s.to_string())
    }

    pub fn column(column: ColumnExpr) -> Expr {
        Expr::Column(column)
    }

    pub fn aliased(expr: Expr, alias: &str) -> Expr {
        Expr::Alias(Box::new(expr), alias.to_string())
    }

    pub fn equal(lh: Expr, rh: Expr) -> Expr {
        Expr::Equal(Box::new(lh), Box::new(rh))
    }

    pub fn not_equal(lh: Expr, rh: Expr) -> Expr {
        Expr::NotEqual(Box::new(lh), Box::new(rh))
    }

    pub fn less_than(lh: Expr, rh: Expr) -> Expr {
        Expr::LessThan(Box::new(lh), Box::new(rh))
    }

    pub fn less_than_or_equal(lh: Expr, rh: Expr) -> Expr {
        Expr::LessThanEqual(Box::new(lh), Box::new(rh))
    }

    pub fn greater_than(lh: Expr, rh: Expr) -> Expr {
        Expr::GreaterThan(Box::new(lh), Box::new(rh))
    }

    pub fn greater_than_or_equal(lh: Expr, rh: Expr) -> Expr {
        Expr::GreaterThanEqual(Box::new(lh), Box::new(rh))
    }

    pub fn like(lh: Expr, rh: Expr) -> Expr {
        Expr::Like(Box::new(lh), Box::new(rh))
    }

    pub fn ilike(lh: Expr, rh: Expr) -> Expr {
        Expr::Ilike(Box::new(lh), Box::new(rh))
    }

    pub fn negate(expr: Expr) -> Expr {
        Expr::Negate(Box::new(expr))
    }

    pub fn and(e1: Expr, e2: Expr) -> Expr {
        Expr::And(vec![e1, e2])
    }

    pub fn or(e1: Expr, e2: Expr) -> Expr {
        Expr::Or(vec![e1, e2])
    }

    pub fn add(e1: Expr, e2: Expr) -> Expr {
        Expr::Add(Box::new(e1), Box::new(e2))
    }

    pub fn sub(e1: Expr, e2: Expr) -> Expr {
        Expr::Sub(Box::new(e1), Box::new(e2))
    }

    pub fn mult(e1: Expr, e2: Expr) -> Expr {
        Expr::Mult(Box::new(e1), Box::new(e2))
    }

    pub fn div(e1: Expr, e2: Expr) -> Expr {
        Expr::Div(Box::new(e1), Box::new(e2))
    }

    pub fn modulo(e1: Expr, e2: Expr) -> Expr {
        Expr::Mod(Box::new(e1), Box::new(e2))
    }

    pub fn exp(e1: Expr, e2: Expr) -> Expr {
        Expr::Exp(Box::new(e1), Box::new(e2))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ColumnExpr {
    ColumnRef {
        table_alias: Option<String>,
        column: String,
    },
    AnyColumn {
        table_alias: Option<String>
    },
}

impl ColumnExpr {
    pub fn qualified_ref(table_alias: &str, column: &str) -> ColumnExpr {
        ColumnExpr::ColumnRef {
            table_alias: Some(table_alias.to_string()),
            column: column.to_string(),
        }
    }

    pub fn unqualified_ref(column: &str) -> ColumnExpr {
        ColumnExpr::ColumnRef {
            table_alias: None,
            column: column.to_string(),
        }
    }

    pub fn unqualified_any() -> ColumnExpr {
        ColumnExpr::AnyColumn {
            table_alias: None,
        }
    }

    pub fn qualified_any(table_alias: &str) -> ColumnExpr {
        ColumnExpr::AnyColumn {
            table_alias: Some(table_alias.to_string()),
        }
    }
}
