#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DbType {
    Relation,
    Int32,
    Int64,
    Boolean,
    FixedString(usize),
    VariableString(usize),
    Array(Box<DbType>),
    Nullable(Box<DbType>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SchemaObject {
    pub id: u64,
    pub version: u64, // Version timestamp to maintain transactionality of catalog. Not sure exactly how it will be used.
    pub parent_id: Option<u64>,
    pub name: String,
    pub db_type: DbType,
}
