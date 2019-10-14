use error::{Error, Result};
use ast::query::*;
use ast::query_plan::*;
use ast::expr::*;
use catalog::schema::{SchemaObject, DbType};
use catalog::Catalog;

/// TODO: Since many of the query AST and query plan AST nodes are similar,
/// we should either factor out the common nodes or qualify the imports so that
/// we can quickly and visibly distinguish between the two modules.

struct Optimizer<'a> {
    catalog: &'a Catalog,
    optimization_level: OptimizationLevel,
}

impl<'a> Optimizer<'a> {

    fn new(catalog: &'a (impl Catalog + 'a)) -> Optimizer<'a> {
        Optimizer {
            catalog,
            optimization_level: OptimizationLevel::None,
        }
    }

    fn optimize(&self, query: &Query) -> RelationalOperator {
        RelationalOperator::Scan(RelationRef(1))
    }

    fn generate_logical_plan(&self, query: &Query) -> RelationalOperator {
        #[allow(unreachable_patterns)]
        match query {
            Query::Select{ select, from, .. } => {
                let scan_tbl = from.first()
                    .map(|tbl_| RelationalOperator::Scan(RelationRef(1)))
                    .unwrap_or(RelationalOperator::Empty);

                RelationalOperator::Select{
                    input: Box::new(scan_tbl),
                    projection: select.result_columns
                        .iter()
                        .cloned()
                        .map(|res_col| match res_col {
                            ColumnExpr::Expr(e) => ColumnExpr::Expr(e),
                            _ => { panic!("TODO: Complete pattern matching on result column"); }
                        })
                        .collect::<Vec<ColumnExpr>>(),
                    filter: None,
                }
            },
            _ => { panic!("Non-SELECT statements not supported"); }
        }
    }

    // fn generate_typed_expression(&self, col_expr: &Expr) -> TypedExpr {
    //     match col_expr {
    //         Expr::Integer(i) => TypedExpr::IntegerExpr(IntegerExpr::Integer(*i)),
    //         Expr::Boolean(b) => TypedExpr::BooleanExpr(BooleanExpr::Boolean(*b)),
    //     }
    // }
}

#[derive(Debug)]
enum OptimizationLevel {
    None,
}

// lex -> parse -> type -> validate -> optimize

#[cfg(test)]
mod tests {
    use super::*;
    use compiler::test_helpers::*;
    use ast::builder::Builder;

    #[test]
    fn generates_logical_plan_trivial_select() {
        let query = QueryBuilder::new()
            .with_selection(vec![
                QueryBuilder::select_integer(1),
            ])
            .finish();

        let expected_plan = RelationalOperator::Select{
            input: Box::new(RelationalOperator::Empty),
            projection: vec!(
                ColumnExpr::Expr(
                    TypedExpr::IntegerExpr(IntegerExpr::Integer(1))
                )
            ),
            filter: None,
        };

        let catalog = get_test_catalog();
        let optimizer = Optimizer::new(&catalog);
        let plan = optimizer.generate_logical_plan(&query);

        assert_eq!(expected_plan, plan);
    }

    // #[test]
    // fn generates_logical_plan_simple_select() {
    //     let query = QueryBuilder::new()
    //         .with_selection(vec![
    //             QueryBuilder::select_qualified_column("user", "first_name"),
    //         ])
    //         .with_tables(vec!( QueryBuilder::from_table("user") ))
    //         .finish();

    //     let expected_plan = RelationalOperator::Select{
    //         input: Box::new(RelationalOperator::Scan(RelationRef{ id: 1 })),
    //         projection: vec!(ProjectedColumn::new("first_name")),
    //         filter: None,
    //     };

    //     let catalog = TestCatalog::new();
    //     let optimizer = Optimizer::new(Box::new(catalog));

    //     let plan = optimizer.generate_logical_plan(&query);

    //     assert_eq!(expected_plan, plan);
    // }
}