use std::collections::HashMap;
use lazy_static::lazy_static;
use tinyjson::JsonValue;
use vcore::Word;

lazy_static! {
    pub static ref SYMBOLS: HashMap<String, Word> = {
        let symbols: JsonValue = novaforth::SYMBOLS.parse().unwrap();
        let mut cast = HashMap::new();
        if let Ok(JsonValue::Object(map)) = symbols.try_into() {
            for (sym, val) in map {
                if let JsonValue::Number(f) = val {
                    cast.insert(sym, Word::from(f as u32));
                }
            }
        }
        cast
    };
}

pub const TIB: u32 = 80000;
pub const SCREEN: u32 = 0x10000;