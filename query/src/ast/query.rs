use ast::builder::Builder;
use ast::expr::*;

// Numeric       : integer() | decimal()
// Boolean       : keyword(true) | keyword(false)
// Value         : string() | Numeric | Boolean | keyword(null)
// BindParameter : colon integer() | questionMark
// Name          : identifier()
// TableAlias    : Name
// Term          : Value | BindParameter | TableAlias period ColumnRef | ColumnRef
// SelectExpr    : operator(*) | Term keyword(as) identifier(alias) | Term identifier(alias) | Term
// Select        : keyword(select) SelectExpr+
//
// SQLStatement  : Select

#[derive(Debug, PartialEq, Clone)]
pub enum Query {
    Select(Box<SelectQuery>),
    InsertPlaceholder,
}

impl Query {
    pub fn as_select_query(self) -> Option<SelectQuery> {
        match self {
            Query::Select(boxed_select) => Some(*boxed_select),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelectQuery {
    pub select: SelectExpr,
    pub from: Vec<TableRef>,
    pub r#where: Option<Expr>,
    // group_by: Option<Vec<GroupExpr>>,
    // having: Option<PredicateExp>,
    // order_by: Option<Vec<OrderExpr>>,
    // limit: Option<LimitExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelectExpr {
    pub modifier: Option<SelectModifier>,
    pub result_columns: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SelectModifier {
    All,
    Distinct,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TableRef {
    Table {
        name: String,
    },
    AliasedTable {
        table: Box<TableRef>,
        alias: String,
    },
    JoinedTable {
        left: Box<TableRef>,
        right: Box<TableRef>,
        is_natural: bool,
        operator: JoinOperator,
        constraint: Option<JoinConstraint>,
    },
    Subquery {
        subquery: Box<SelectQuery>,
    },
}

impl TableRef {
    pub fn table(name: String) -> TableRef {
        TableRef::Table { name }
    }

    pub fn aliased(tbl: TableRef, alias: String) -> TableRef {
        TableRef::AliasedTable {
            table: Box::new(tbl),
            alias,
        }
    }

    pub fn subquery(query: SelectQuery) -> TableRef {
        TableRef::Subquery {
            subquery: Box::new(query),
        }
    }

    pub fn joined(
        l: TableRef,
        r: TableRef,
        operator: JoinOperator,
        constraint: Option<JoinConstraint>,
    ) -> TableRef {
        TableRef::JoinedTable {
            left: Box::new(l),
            right: Box::new(r),
            operator,
            constraint,
            is_natural: false,
        }
    }

    pub fn with_join_operator(self, operator: JoinOperator) -> Option<TableRef> {
        match self {
            TableRef::JoinedTable {
                left,
                right,
                constraint,
                is_natural,
                ..
            } => Some(TableRef::JoinedTable {
                left,
                right,
                constraint,
                is_natural,
                operator,
            }),
            _ => None,
        }
    }

    pub fn with_join_constraint(self, constraint: Option<JoinConstraint>) -> Option<TableRef> {
        match self {
            TableRef::JoinedTable {
                left,
                right,
                operator,
                is_natural,
                ..
            } => Some(TableRef::JoinedTable {
                left,
                right,
                constraint,
                is_natural,
                operator,
            }),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum JoinOperator {
    Inner,
    Cross,
    Left,
    Right,
    LeftOuter,
    RightOuter,
}

#[derive(Debug, Clone, PartialEq)]
pub enum JoinConstraint {
    Using(Vec<String>),
    On(Expr),
}

impl JoinConstraint {
    pub fn on(condition: Expr) -> JoinConstraint {
        JoinConstraint::On(condition)
    }

    pub fn using(columns: Vec<String>) -> JoinConstraint {
        JoinConstraint::Using(columns)
    }
}

pub struct QueryBuilder {
    select: Option<SelectExpr>,
    from: Option<Vec<TableRef>>,
    r#where: Option<Expr>,
    // group_by: Option<Vec<GroupExpr>>,
    // having: Option<PredicateExp>,
    // order_by: Option<Vec<OrderExpr>>,
    // limit: Option<LimitExpr>,
}

impl QueryBuilder {
    pub fn new() -> QueryBuilder {
        QueryBuilder {
            select: None,
            from: None,
            r#where: None,
        }
    }

    // SELECT

    pub fn select_integer(n: i64) -> Expr {
        Expr::integer(n)
    }

    pub fn select_bool(b: bool) -> Expr {
        Expr::boolean(b)
    }

    pub fn select_aliased(expr: Expr, alias: &str) -> Expr {
        Expr::aliased(expr, alias)
    }

    pub fn select_qualified_column(tbl: &str, col: &str) -> Expr {
        Expr::column(ColumnExpr::qualified_ref(tbl, col))
    }

    pub fn select_unqualified_column(col: &str) -> Expr {
        Expr::column(ColumnExpr::unqualified_ref(col))
    }

    pub fn select_star() -> Expr {
        Expr::column(ColumnExpr::unqualified_any())
    }

    pub fn select_star_in_tbl(tbl: &str) -> Expr {
        Expr::column(ColumnExpr::qualified_any(tbl))
    }

    pub fn with_selection(mut self, cols: Vec<Expr>) -> QueryBuilder {
        self.select = Some(SelectExpr {
            modifier: None,
            result_columns: cols,
        });
        self
    }

    // FROM/JOIN

    pub fn from_table(tbl: &str) -> TableRef {
        TableRef::table(tbl.to_string())
    }

    pub fn from_aliased(tbl: TableRef, alias: &str) -> TableRef {
        TableRef::aliased(tbl, alias.to_string())
    }

    pub fn join(left: TableRef, right: TableRef) -> TableRef {
        TableRef::joined(left, right, JoinOperator::Inner, None)
    }

    pub fn join_on(left: TableRef, right: TableRef, condition: Expr) -> TableRef {
        TableRef::joined(
            left,
            right,
            JoinOperator::Inner,
            Some(JoinConstraint::on(condition)),
        )
    }

    pub fn join_using<S: Into<String>>(
        left: TableRef,
        right: TableRef,
        columns: Vec<S>,
    ) -> TableRef {
        TableRef::joined(
            left,
            right,
            JoinOperator::Inner,
            Some(JoinConstraint::using(
                columns.into_iter().map(|c| c.into()).collect(),
            )),
        )
    }

    pub fn from_subquery(query: SelectQuery) -> TableRef {
        TableRef::subquery(query)
    }

    pub fn with_tables(mut self, tables: Vec<TableRef>) -> QueryBuilder {
        self.from = Some(tables);
        self
    }

    // WHERE

    pub fn with_where(mut self, pred: Expr) -> QueryBuilder {
        self.r#where = Some(pred);
        self
    }
}

impl Builder<Query> for QueryBuilder {
    fn finish(self) -> Query {
        Query::Select(Box::new(SelectQuery {
            select: self.select.unwrap(),
            from: self.from.unwrap_or(Vec::new()),
            r#where: self.r#where,
        }))
    }
}
