use ast::expr::{ColumnExpr, Expr};
use ast::query::{Query, SelectExpr, SelectQuery, TableRef};
use ast::query_plan;
use ast::query_plan::{QueryPlan, PlanNode};
use catalog::schema::SchemaObject;
use catalog::Catalog;
use error::{Error, Result};
use std::borrow::BorrowMut;

// It seems like in order to effectively traverse the AST we need a general mechanism for walking
// and matching the structure of the tree - maybe something like a Visitor or navigators

type AliasedTableSchema = (String, SchemaObject);

/// The Analyzer is designed to bind references to schema objects within a query
/// (e.g. tables, columns, functions) to the schema objects that they reference.
struct Analyzer<'a, T: Catalog + 'a> {
    catalog: &'a T,
}

impl<'a, T: Catalog + 'a> Analyzer<'a, T> {
    pub fn new(catalog: &'a T) -> Analyzer<'a, T> {
        Analyzer { catalog }
    }

    pub fn bind(&self, query: &Query) -> Result<QueryPlan> {
        match query {
            Query::Select { .. } => self.bind_select_query(query),
            _ => unimplemented!(),
        }
    }

    fn bind_select_query(&self, query: &Query) -> Result<QueryPlan> {
        // box syntax in pattern matching would be a nice alternative to `if let ...ref x... = y { if let z == *x.borrow_mut() { ... } }`
        match *query {
            Query::Select(boxed_select) => {
                let SelectQuery {
                    select,
                    from,
                    ..
                } = *boxed_select;
                let 
                let join_clause = self.from_clause_to_join(from);
                if from.len() > 0 {

                } else {

                }
                let mut join_clause = Vec::with_capacity(from.len());
                for table_ref in from.into_iter() {
                    self.bind_table_ref(table_ref, &mut query_tables, None)?;
                }
            }
            _ => Err(Error::AnalysisError {
                name: "query".to_string(),
                object_type: "query",
                msg: "Expected a SELECT query".to_string(),
            }),
        }
    }

    fn from_clause_to_join(&self, tables: Vec<TableRef>) -> Result<query_plan::Join>

    fn bind_table_ref(
        &self,
        table_ref: &mut TableRef,
        seen: &mut Vec<AliasedTableSchema>,
        alias: Option<String>,
    ) -> Result<()> {
        match table_ref {
            TableRef::Table { name, binding } => {
                if let Some(schema_obj) = self.catalog.find_relation(name) {
                    seen.push((alias.unwrap_or_else(|| name.clone()), schema_obj.clone()));
                    *binding = Binding::Bound(schema_obj);
                } else {
                    return Err(Error::AnalysisError {
                        name: name.clone(),
                        object_type: "relation",
                        msg: "table not found in catalog".to_string(),
                    });
                }
            }
            TableRef::AliasedTable {
                table,
                alias: tbl_alias,
            } => {
                self.bind_table_ref(table, seen, Some(tbl_alias.to_string()))?;
            }
            TableRef::JoinedTable { right, left, .. } => {
                self.bind_table_ref(right, seen, alias.clone())?;
                self.bind_table_ref(left, seen, alias)?;
            }
            _ => {
                return Err(Error::GenericError(
                    "Unsupported TableRef form for analyzer".to_string(),
                ));
            }
        };

        Ok(())
    }

    // Should the analyzer create schema objects for temporary tables?
    fn try_bind_column_ref(
        &self,
        select_expr: &mut Expr,
        seen: &Vec<AliasedTableSchema>,
    ) -> Result<()> {
        match select_expr {
            Expr::Alias(aliased_col, _) => self.try_bind_column_ref(&mut *aliased_col, seen),
            Expr::Column(column_expr) => self.bind_column(&mut *column_expr, seen),
            Expr::And(exprs) | Expr::Or(exprs) => {
                for expr in exprs.iter_mut() {
                    self.try_bind_column_ref(expr, seen)?;
                }
                Ok(())
            }
            _ => Ok(()), // Do not try to bind otherwise
        }
    }

    fn bind_column(&self, column: &mut ColumnExpr, seen: &Vec<AliasedTableSchema>) -> Result<()> {
        match column {
            ColumnExpr::ColumnRef {
                table_alias,
                column,
                binding,
            } => {
                *binding = self.resolve_column_binding(column, table_alias.as_ref(), seen)?;
            }

            ColumnExpr::AnyColumn {
                table_alias: None, ..
            } => {}
            // Should we rewrite wildcards as an enumerated list of all the columns from the tables that
            // we are selecting from? This could make it easier to reason about the result set from
            // subqueries, but it would potentially make it difficult to elide unnecessary projections from
            // subqueries and optimize selects that pass through all results unmodified.
            ColumnExpr::AnyColumn {
                table_alias,
                binding,
            } => {
                println!("Matching: ColumnExpr::AnyColumn: {:?}", table_alias.clone());
                // TODO: Unqualified *
                // We need to resolve against any table in the FROM clause and error on conflict
                if let Some(schema_obj) = self.catalog.find_relation(&table_alias.clone().unwrap())
                {
                    *binding = Binding::Bound(schema_obj);
                } else {
                    return Err(Error::AnalysisError {
                        name: format!("{}.*", table_alias.clone().unwrap()),
                        object_type: "column",
                        msg: "column not found in catalog".to_string(),
                    });
                }
            }
        }

        Ok(())
    }

    fn resolve_column_binding(
        &self,
        column: &str,
        table_alias: Option<&String>,
        seen: &Vec<AliasedTableSchema>,
    ) -> Result<Binding> {
        match table_alias {
            Some(ta) => self.qualified_column_binding(column, ta, seen),
            None => self.unqualified_column_binding(column, seen),
        }
    }

    /// Get a Binding for a column that is qualified by a table alias.
    /// This will only succeed if the referenced table exists in the selection and refers
    /// to an actual table.
    fn qualified_column_binding(
        &self,
        column: &str,
        table_alias: &str,
        seen: &Vec<AliasedTableSchema>,
    ) -> Result<Binding> {
        self.resolve_table_by_alias(table_alias, seen)
            .ok_or(Error::AnalysisError {
                name: column.to_string(),
                object_type: "column",
                msg: format!("table \"{}\" is not part of selection", table_alias),
            })
            .and_then(|table| match self.catalog.find_column(table.id, column) {
                Some(schema_obj) => Ok(Binding::Bound(schema_obj)),
                None => Err(Error::AnalysisError {
                    name: format!("{}.{}", table_alias, column),
                    object_type: "column",
                    msg: "column not found on table".to_string(),
                }),
            })
    }

    /// Get a Binding for a column that is lacking an explicit table qualification.
    /// This will succeed if and only if there is exactly 1 table in the selection that has a column
    /// with a matching name.
    fn unqualified_column_binding(
        &self,
        column: &str,
        seen: &Vec<AliasedTableSchema>,
    ) -> Result<Binding> {
        let mut table_objs: Vec<AliasedTableSchema> = Vec::new();
        for ats in seen.iter() {
            if let Some(column) = self.catalog.find_column(ats.1.id, column) {
                table_objs.push(ats.clone());
            }
        }

        match table_objs.len() {
            0 => Err(Error::AnalysisError {
                name: column.to_string(),
                object_type: "column",
                msg: "table not found in selection".to_string(),
            }),

            1 => self.qualified_column_binding(column, &table_objs.get(0).unwrap().0, seen),

            _ => {
                let table_names = table_objs
                    .into_iter()
                    .map(|ats| ats.0)
                    .collect::<Vec<String>>()
                    .join(", ");
                Err(Error::AnalysisError {
                    name: column.to_string(),
                    object_type: "column",
                    msg: format!(
                        "Ambiguous table reference: {} may exist on any of: {}",
                        column, table_names
                    ),
                })
            }
        }
    }

    fn resolve_table_by_alias(
        &self,
        alias: &str,
        seen: &Vec<AliasedTableSchema>,
    ) -> Option<SchemaObject> {
        seen.iter()
            .find(|ats| {
                println!("Checking table alias: {} == {}?", &ats.0, alias);
                ats.0 == alias
            })
            .map(|ats| ats.1.clone())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use compiler::test_helpers::*;

    #[test]
    fn creates_analyzer() {
        let catalog = get_test_catalog();
        let _ = Analyzer::new(&catalog);
    }

    #[test]
    fn converts_select_star_to_scan_without_projection() {
        let bound_query = parse_and_bind_select("SELECT id FROM \"user\"").unwrap();
        let user_table = get_test_catalog().lookup(1).unwrap();
        let user_id_col = get_test_catalog().lookup(10).unwrap();

        let SelectQuery { from, .. } = bound_query;
        if let TableRef::Table {
            name,
            binding: Binding::Bound(obj),
        } = from.get(0).unwrap()
        {
            assert_eq!(user_table, *obj);
        } else {
            panic!("Table not bound");
        }
    }

    // #[test]
    // fn binds_unaliased_table_in_from() {
    //     let bound_query = parse_and_bind_select("SELECT id FROM \"user\"").unwrap();
    //     let user_table = get_test_catalog().lookup(1).unwrap();

    //     let SelectQuery { from, .. } = bound_query;
    //     if let TableRef::Table {
    //         name,
    //         binding: Binding::Bound(obj),
    //     } = from.get(0).unwrap()
    //     {
    //         assert_eq!(user_table, *obj);
    //     } else {
    //         panic!("Table not bound");
    //     }
    // }

    // #[test]
    // fn binds_aliased_table_in_from() {
    //     let bound_query = parse_and_bind_select("SELECT * FROM \"user\" as u_tbl").unwrap();
    //     let user_table = get_test_catalog().lookup(1).unwrap();

    //     let SelectQuery { from, .. } = bound_query;
    //     if let TableRef::AliasedTable { ref table, .. } = from.get(0).unwrap() {
    //         if let TableRef::Table {
    //             binding: Binding::Bound(ref obj),
    //             ..
    //         } = **table
    //         {
    //             assert_eq!(user_table, *obj);
    //             return;
    //         }
    //     }

    //     panic!("Query does not match expected structure");
    // }

    // #[test]
    // fn binds_joined_table_in_from() {
    //     let bound_query = parse_and_bind_select(
    //         "SELECT * FROM \"user\" AS mgr JOIN \"user\" AS emp ON mgr.id = emp.manager_id",
    //     )
    //     .unwrap();
    //     let user_table = get_test_catalog().lookup(1).unwrap();

    //     let SelectQuery { from, .. } = bound_query;
    //     // Joined
    //     if let TableRef::JoinedTable {
    //         ref right,
    //         ref left,
    //         ..
    //     } = from.get(1).unwrap()
    //     {
    //         // Aliased Left
    //         if let TableRef::AliasedTable { ref table, .. } = **left {
    //             if let TableRef::Table {
    //                 binding: Binding::Bound(ref obj),
    //                 ..
    //             } = **table
    //             {
    //                 // Table
    //                 assert_eq!(user_table, *obj);
    //                 return;
    //             }
    //         }

    //         // Aliased Right
    //         if let TableRef::AliasedTable { ref table, .. } = **right {
    //             if let TableRef::Table {
    //                 binding: Binding::Bound(ref obj),
    //                 ..
    //             } = **table
    //             {
    //                 // Table
    //                 assert_eq!(user_table, *obj);
    //                 return;
    //             }
    //         }
    //     };

    //     panic!("Query does not match expected structure");
    // }

    // #[test]
    // fn ignores_non_column_expressions_in_select() {
    //     let bound_query = parse_and_bind_select("SELECT 1 FROM \"user\"").unwrap();

    //     let SelectQuery {
    //         select: SelectExpr { result_columns, .. },
    //         ..
    //     } = bound_query;
    //     if let Expr::Integer(_) = result_columns.get(0).unwrap() {
    //         // Integer was unaliased - OK
    //         return;
    //     }

    //     panic!("Query does not match expected structure");
    // }

    // #[test]
    // fn binds_unaliased_qualified_columns() {
    //     let bound_query = parse_and_bind_select("SELECT user.first_name FROM \"user\"").unwrap();
    //     let user_first_name_col = get_test_catalog().lookup(11).unwrap();

    //     let SelectQuery {
    //         select: SelectExpr { result_columns, .. },
    //         ..
    //     } = bound_query;
    //     if let Expr::Column(ColumnExpr::ColumnRef {
    //         binding: Binding::Bound(ref obj),
    //         ..
    //     }) = result_columns.get(0).unwrap()
    //     {
    //         assert_eq!(user_first_name_col, *obj);
    //         return;
    //     } else {
    //         panic!("Column was not bound");
    //     }

    //     panic!("Query does not match expected structure");
    // }

    // #[test]
    // fn binds_aliased_qualified_columns() {
    //     let bound_query =
    //         parse_and_bind_select("SELECT user.first_name AS fname FROM \"user\"").unwrap();
    //     let user_first_name_col = get_test_catalog().lookup(11).unwrap();

    //     let SelectQuery {
    //         select: SelectExpr { result_columns, .. },
    //         ..
    //     } = bound_query;
    //     if let Expr::Alias(ref aliased_col, _) = result_columns.get(0).unwrap() {
    //         if let Expr::Column(ColumnExpr::ColumnRef {
    //             binding: Binding::Bound(ref obj),
    //             ..
    //         }) = **aliased_col
    //         {
    //             assert_eq!(user_first_name_col, *obj);
    //             return;
    //         } else {
    //             panic!("Column was not bound");
    //         }
    //     }

    //     panic!("Query does not match expected structure");
    // }

    // #[test]
    // fn binds_unqualified_column_to_single_table() {
    //     let bound_query = parse_and_bind_select("SELECT id FROM \"user\"").unwrap();
    //     let user_id_col = get_test_catalog().lookup(10).unwrap();

    //     let SelectQuery {
    //         select: SelectExpr { result_columns, .. },
    //         ..
    //     } = bound_query;
    //     if let Expr::Column(ColumnExpr::ColumnRef {
    //         binding: Binding::Bound(ref obj),
    //         ..
    //     }) = result_columns.get(0).unwrap()
    //     {
    //         assert_eq!(user_id_col, *obj);
    //         return;
    //     } else {
    //         panic!("Column was not bound");
    //     }

    //     panic!("Query does not match expected structure");
    // }

    // #[test]
    // fn binds_column_refs_in_nested_expressions() {
    //     let bound_query =
    //         parse_and_bind_select("SELECT id > 0 AND first_name != 'Bob' FROM \"user\"").unwrap();
    //     let user_id_col = get_test_catalog().lookup(10).unwrap();

    //     let SelectQuery {
    //         select: SelectExpr { result_columns, .. },
    //         ..
    //     } = bound_query;
    //     if let Expr::Column(ColumnExpr::ColumnRef {
    //         binding: Binding::Bound(ref obj),
    //         ..
    //     }) = result_columns.get(0).unwrap()
    //     {
    //         assert_eq!(user_id_col, *obj);
    //         return;
    //     } else {
    //         panic!("Column was not bound");
    //     }

    //     panic!("Query does not match expected structure");
    // }

    // #[test]
    // fn bind_fails_when_column_does_not_exist() {
    //     match parse_and_bind_select("SELECT user.liver_quiver FROM user") {
    //         Ok(_) => panic!("Should not bind query selecting nonexistent column"),
    //         _ => {}
    //     };
    // }

    // #[test]
    // fn bind_fails_with_no_table_in_selection() {
    //     match parse_and_bind_select("SELECT user.id") {
    //         Ok(_) => {
    //             panic!("Should not bind query selecting column with no matching table in selection")
    //         }
    //         _ => {}
    //     };
    // }

    // #[test]
    // fn bind_fails_with_table_not_matching_alias_in_selection() {
    //     match parse_and_bind_select("SELECT user.id FROM user as u") {
    //         Ok(_) => panic!("Should not bind query selecting column whose table alias does not match an aliased table in the selection"),
    //         _ => {}
    //     };
    // }

    // #[test]
    // fn bind_fails_with_ambiguous_column_ref() {
    //     match parse_and_bind_select(
    //         "SELECT id FROM user JOIN company ON user.company_id = company.id",
    //     ) {
    //         Ok(_) => panic!(
    //             "Should not bind query selecting ambiguous column (multiple possible tables)"
    //         ),
    //         _ => {}
    //     };
    // }

    // Binds columns arbitrarily nested inside expressions (need visitor or similar)

    fn parse_and_bind_select(sql: &str) -> Result<QueryPlan> {
        let catalog = get_test_catalog();
        let query = parse_sql_string(sql);
        let analyzer = Analyzer::new(&catalog);

        analyzer.bind(&query).and_then(|query| match query {
            Query::Select(ref boxed_select) => Ok(*boxed_select.clone()),
            _ => panic!("The parsed query was not a SELECT"),
        })
    }
}
