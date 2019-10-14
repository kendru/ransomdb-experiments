use std::collections::HashMap;

use ast::query::Query;
use catalog::schema::{DbType, SchemaObject};
use catalog::Catalog;
use compiler::lexer::Lexer;
use compiler::parser::Parser;

#[derive(Debug)]
pub struct TestCatalog {
    tbl: HashMap<u64, SchemaObject>,
    pub is_debug: bool,
}

impl TestCatalog {
    pub fn new() -> TestCatalog {
        TestCatalog {
            tbl: HashMap::new(),
            is_debug: false,
        }
    }
}

impl Catalog for TestCatalog {
    fn put(&mut self, obj: SchemaObject) {
        self.tbl.insert(obj.id, obj);
    }

    fn lookup(&self, id: u64) -> Option<SchemaObject> {
        if self.is_debug {
            println!("Lookup by ID: {}", id);
        }
        self.tbl.get(&id).map(|obj| obj.clone())
    }

    fn find_relation(&self, relation_name: &str) -> Option<SchemaObject> {
        if self.is_debug {
            println!("Finding relation: {}", relation_name);
        }
        self.tbl
            .values()
            .find(|&obj| obj.db_type == DbType::Relation && obj.name == relation_name)
            .map(|obj| obj.clone())
    }

    fn find_column(&self, relation_id: u64, column_name: &str) -> Option<SchemaObject> {
        if self.is_debug {
            println!("Finding column: <{}>.{}", relation_id, column_name);
        }
        self.tbl
            .values()
            .find(|&obj| obj.parent_id == Some(relation_id) && obj.name == column_name)
            .map(|obj| obj.clone())
    }
}

pub fn get_test_catalog() -> TestCatalog {
    let mut catalog = TestCatalog::new();

    catalog.put(SchemaObject {
        id: 1,
        version: 0,
        parent_id: None,
        name: "user".to_string(),
        db_type: DbType::Relation,
    });
    catalog.put(SchemaObject {
        id: 10,
        version: 0,
        parent_id: Some(1),
        name: "id".to_string(),
        db_type: DbType::Int64,
    });
    catalog.put(SchemaObject {
        id: 11,
        version: 0,
        parent_id: Some(1),
        name: "first_name".to_string(),
        db_type: DbType::VariableString(255),
    });
    catalog.put(SchemaObject {
        id: 12,
        version: 0,
        parent_id: Some(1),
        name: "manager_id".to_string(),
        db_type: DbType::Nullable(Box::new(DbType::Int64)),
    });
    catalog.put(SchemaObject {
        id: 13,
        version: 0,
        parent_id: Some(1),
        name: "company_id".to_string(),
        db_type: DbType::Nullable(Box::new(DbType::Int64)),
    });

    catalog.put(SchemaObject {
        id: 2,
        version: 0,
        parent_id: None,
        name: "company".to_string(),
        db_type: DbType::Relation,
    });
    catalog.put(SchemaObject {
        id: 20,
        version: 0,
        parent_id: Some(2),
        name: "id".to_string(),
        db_type: DbType::Int64,
    });

    catalog
}

pub fn parse_sql_string(s: &str) -> Query {
    let lexer = Lexer::from_string(s.to_string());
    let mut parser = Parser::new(lexer);

    match parser.parse() {
        Ok(query) => query,
        Err(query_error) => {
            panic!("Error parsing query: {}", query_error);
        }
    }
}
