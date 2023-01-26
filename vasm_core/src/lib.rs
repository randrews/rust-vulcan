extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
pub mod parse_error;
mod vasm_assembler;
mod vasm_evaluator;
mod vasm_parser;
mod vasm_preprocessor;

pub use vasm_assembler::assemble_snippet;
pub use vasm_assembler::snippet_source_map;
pub use vasm_assembler::assemble_file;
