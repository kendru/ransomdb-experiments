# Analytics/Query

This crate implements parsing of SQL queries and defines the QUERY ASTs used in the optimizer
and executor. It is written to be consumed within the database system as well as from a
WebAssembly module for query generation, validation, syntax highlighting, etc. within the
browser.

