use std::collections::BTreeMap;
use std::fs::File;
use std::io::Write;
use clap::lazy_static::lazy_static;
use clap::Parser;
use regex::Regex;
use serde_json::json;

/// Vulcan Assembler
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// File to output to
    #[clap(short, long)]
    output: Option<String>,

    /// Whether to generate a JSON file containing the symbol table
    #[clap(short, long)]
    symbols: bool,

    /// Input file
    #[clap()]
    file: String,
}

impl Args {
    fn warning(&self) -> Option<String> {
        if !self.file.ends_with(".asm") {
            return Some(format!("Warning: input file {} does not end in \".asm\"", self.file))
        }
        if let Some(output) = self.output.as_ref() {
            // Warn if it doesn't match /.rom$/
            if !output.ends_with(".rom") {
                return Some(format!("Warning: output file {} does not end in \".rom\"", output))
            }
        }
        None
    }

    fn deduce(&mut self) {
        if self.output.is_none() {
            let re = Regex::new(r"^(.*)\.asm$").unwrap();
            if let Some(cap) = re.captures(self.file.as_ref()) {
                self.output = Some(format!("{}.rom", cap.get(1).unwrap().as_str()));
            } else {
                panic!("No output file specified, couldn't deduce one from input filename")
            }
        }
    }
}

fn main() {
    let mut args = Args::parse();
    if let Some(warning) = args.warning() {
        println!("{}", warning);
    }
    args.deduce();

    match vasm_core::assemble_file(args.file.as_str()) {
        Ok((bytes, scope)) => {
            let mut outfile = File::create(args.output.as_ref().unwrap()).expect("Failed to open output file");
            outfile.write(bytes.as_slice()).expect("Failed to write to output file");

            if args.symbols {
                let mut important_symbols : BTreeMap<String, i32> = BTreeMap::new();
                for (sym, addr) in scope {
                    if !sym.starts_with("__gensym") {
                        important_symbols.insert(sym, addr);
                    }
                }

                let mut symfile = File::create(format!("{}.sym", args.output.unwrap())).expect("Failed to open symbol file");
                let json = json!(important_symbols).to_string();
                symfile.write(json.as_bytes()).expect("Failed to write to symbol file");
            }
        }
        Err(error) => {
            println!("{}", error)
        }
    }
}
