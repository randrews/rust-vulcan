use std::collections::BTreeMap;
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use serde_json::json;
use vasm_core::assemble_file;

fn main() {
    env::set_current_dir("src").expect("Can't chdir into src. Am I being run as a build script?");
    // We're doing test_init because it compiles the Novaforth interpreter but not any code to actually run it,
    // 0x400 is just "stop: hlt".
    // TODO: we need a system for building ROMs and actually booting the machine.
    // This is fine for now, for use by `vweb`.
    match assemble_file("test_init.asm") {
        Ok((bytes, scope)) => {
            println!("Assembled {} bytes", bytes.len());
            let dir = env::var("OUT_DIR").expect("OUT_DIR not specified. Am I being run as a build script?");
            let filename = Path::new(dir.as_str()).join("4th.rom");
            let mut f = File::create(filename).expect("Unable to open output file");
            f.write(bytes.as_slice()).expect("Unable to write to output file");

            let mut important_symbols : BTreeMap<String, i32> = BTreeMap::new();
            for (sym, addr) in scope {
                if !sym.starts_with("__gensym") {
                    important_symbols.insert(sym, addr);
                }
            }

            let mut symfile = File::create(Path::new(dir.as_str()).join("4th.rom.sym")).expect("Unable to open symbol file");
            let json = json!(important_symbols).to_string();
            symfile.write(json.as_bytes()).expect("Unable to write to symbol file");
        }
        Err(e) => {
            eprintln!("{}", e)
        }
    }
}