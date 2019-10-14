pub mod schema;

use catalog::schema::{DbType, SchemaObject};

/// This needs some *major* refinement before it can be used as a
/// fully-fleged system catalog. For now, we just need a place to
/// lookup schema objects for the analyzer and optimizer.
pub trait Catalog {
    fn put(&mut self, obj: SchemaObject);
    fn lookup(&self, id: u64) -> Option<SchemaObject>;

    fn find_relation(&self, relation_name: &str) -> Option<SchemaObject>;
    fn find_column(&self, relation_id: u64, column_name: &str) -> Option<SchemaObject>;
}
