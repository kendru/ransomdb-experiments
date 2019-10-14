use ast::expr::*;
use catalog::schema::SchemaObject;

// IDEAS:
// - Allow table/column refs to reference both permanent and temporary objects. Maybe we could keep the referants on the QueryPlan itself and hold references within the plan nodes. Will this require pinning? Should we use logical references? (e.g. use a usize and store the referants in a vector on QueryPlan)
// - In logical plan, include physical properties on operators, and in physical plan, create explicit nodes to handle things like sorting

#[derive(Debug, PartialEq, Clone)]
pub struct QueryPlan {
    root: PlanNode,
    column_lookup: Vec<SchemaObject>,
    table_lookup: Vec<SchemaObject>,
}

impl QueryPlan {
    pub fn new(root: PlanNode) -> QueryPlan {
        QueryPlan {
            root,
            column_lookup: Vec::new(),
            table_lookup: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PlanNode {
    Scan(Box<TableScan>),
    Project(Box<Projection>),
    Join(Box<Join>),
    Select(Box<Selection>),
    Sort(Box<Ordering>),
    Limit(Box<Limit>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct TableScan {
    table: u8, // Figure out the right type for this
    predicate: Option<Box<TypedExpr>>,
    projection: Option<Box<Projection>>,
}

// Projection is an "extended projection" that encompasses both
// projection over a relation and evaluation of expressions that
// can reference columns of the relation. In the interest of
// effeciency, it also allows for (re)naming of the projected columns
// without a dedicated "rename" node.
#[derive(Debug, PartialEq, Clone)]
pub struct Projection {
    columns: Vec<ColumnProjection>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ColumnProjection {
    expr: TypedExpr, // May contain an Expr::Column
    r#as: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Selection;

#[derive(Debug, PartialEq, Clone)]
pub struct Join;

#[derive(Debug, PartialEq, Clone)]
pub struct Ordering {
    field: String,
    direction: SortDirection,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SortDirection {
    Asc,
    Desc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Limit {
    offset: u64,
    limit: u64,
}

// (distinct
//     (project
//         (filter
//             (join
//                 (join
//                     (scan product)
//                     (scan purchase)
//                     (= product.pid purchase.pid))
//                 (scan customer)
//                 (= purchase.cid customer.cid))
//             (and (> product.price, 100)
//                 (= customer.city "Seattle")))
//         (list product.name, purchase.name)))
