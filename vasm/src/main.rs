use clap::Parser;
use std::borrow::Borrow;

/// Vulcan Assembler
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Read input from stdin
    #[clap(short, long)]
    stdin: bool,

    /// File to output to
    #[clap(short, long)]
    output: Option<String>,

    /// Print info about generated file
    #[clap(short, long)]
    info: bool,

    /// Input file
    #[clap()]
    file: Option<String>,
}

impl Args {
    fn error(&self) -> Option<String> {
        if let Some(input) = self.file.as_ref() {
            if self.stdin {
                return Some(format!(
                    "Told to read from stdin but also given input file name {}",
                    input
                ));
            }
            // Warn
        }
        None
    }

    fn warning(&self) -> Option<String> {
        if let Some(input) = self.file.as_ref() {
            // Warn if it doesn't match /.asm$/
        }
        if let Some(output) = self.output.as_ref() {
            // Warn if it doesn't match /.rom$/
        } else {
        }
        None
    }
}

fn main() {
    let args = Args::parse();
    if let Some(err) = args.error() {
        println!("{}", err);
    }
}
