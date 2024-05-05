use std::collections::vec_deque::VecDeque;
use crate::ast::{Macro, VASMLine};
use crate::parse_error::{AssembleError, Location};
use crate::vasm_parser::parse_vasm_line;
use std::iter::Enumerate;

#[derive(Debug, PartialEq, Clone)]
pub struct Line {
    pub line: VASMLine,
    pub location: Location,
}

#[derive(Debug, Clone, PartialEq)]
enum LoopType {
    While,
    Until,
}

impl From<Macro> for LoopType {
    fn from(mac: Macro) -> Self {
        match mac {
            Macro::While => LoopType::While,
            Macro::Until => LoopType::Until,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ControlStructure {
    Target(String),
    Loop(String, LoopType),
    LoopDo(String, String),
}

pub struct LineSource<T: IntoIterator<Item = String>, F: Fn(String) -> Result<T, AssembleError>> {
    generated_lines: VecDeque<Line>,
    current_line: usize,
    filename_stack: Vec<String>,
    current_sym: usize,
    control_stack: Vec<ControlStructure>,
    iter_stack: Vec<Enumerate<<T as IntoIterator>::IntoIter>>,
    include: F,
}

impl From<VASMLine> for Line {
    fn from(other: VASMLine) -> Self {
        Line {
            line: other,
            location: Location {
                line_num: 0,
                file: "<none>".to_string(),
            },
        }
    }
}

impl<T: IntoIterator<Item = String>, F: Fn(String) -> Result<T, AssembleError>> Iterator
    for LineSource<T, F>
{
    type Item = Result<Line, AssembleError>;

    fn next(&mut self) -> Option<Self::Item> {
        // Have we any macro-generated lines?
        if let Some(line) = self.generated_lines.pop_front() {
            return Some(Ok(line));
        }

        // Are we out of lines in total?
        if self.iter_stack.is_empty() {
            return None;
        }

        // Fetch a line from the top iterator
        if let Some((line_idx, line)) = self.iter_stack.last_mut().unwrap().next() {
            self.current_line = line_idx + 1;
            // Try and parse it
            return match parse_vasm_line(line.as_str()) {
                // We failed to parse it
                Err(err) => Some(Err(AssembleError::ParseError(self.current_location(), err))),

                // It's a macro, so do it and then try again
                Ok(VASMLine::Macro(mac)) => {
                    match self.handle_macro(mac) {
                        None => {}
                        Some(err) => return Some(Err(err))
                    }
                    self.next()
                }

                // TODO: This makes line numbers on error messages wrong, but it's hard to fix. We need
                // to rip out and redo the whole error system. Parse lines initially to tuples of their
                // line and location (file and line number) and then pass one of those tuples to create
                // an error.
                Ok(VASMLine::Blank) => self.next(),

                Ok(normal_line) => Some(Ok(Line {
                    line: normal_line,
                    location: Location {
                        line_num: self.current_line,
                        file: self.filename_stack.last().unwrap().clone(),
                    },
                })),
            };
        }

        // Pop the iterator stack and try again
        self.iter_stack.pop();
        self.filename_stack.pop();
        self.next()
    }
}

impl<T: IntoIterator<Item = String>, F: Fn(String) -> Result<T, AssembleError>> LineSource<T, F> {
    // TODO: linesource should take a deref instead so it accepts either str or string.
    pub fn new(file: &str, lines: T, include: F) -> Self {
        LineSource {
            generated_lines: VecDeque::new(),
            current_line: 0,
            filename_stack: vec![file.to_string()],
            current_sym: 0,
            control_stack: vec![],
            iter_stack: vec![lines.into_iter().enumerate()],
            include,
        }
    }

    fn emit(&mut self, line: String) {
        self.generated_lines.push_back(Line {
            line: parse_vasm_line(line.as_str()).unwrap(),
            location: Location {
                line_num: self.current_line,
                file: self.filename_stack.last().unwrap().clone(),
            },
        })
    }

    fn gensym(&mut self) -> String {
        self.current_sym += 1;
        format!("__gensym_{}", self.current_sym)
    }

    fn current_location(&self) -> Location {
        Location {
            line_num: self.current_line,
            file: self.filename_stack.last().unwrap().clone(),
        }
    }

    fn handle_macro(&mut self, mac: Macro) -> Option<AssembleError> {
        match mac {
            Macro::Include(file) => {
                match (self.include)(file.clone()) {
                    Ok(it) => {
                        self.filename_stack.push(file);
                        self.iter_stack.push(it.into_iter().enumerate());
                    }
                    Err(err) => {
                        return Some(err)
                    }
                }
            }

            Macro::If => {
                let label = self.gensym();
                self.control_stack
                    .push(ControlStructure::Target(label.clone()));
                self.emit(format!("brz @{}", label));
            }

            Macro::Unless => {
                let label = self.gensym();
                self.control_stack
                    .push(ControlStructure::Target(label.clone()));
                self.emit(format!("brnz @{}", label));
            }

            Macro::Else => {
                if let Some(ControlStructure::Target(old_end)) = self.control_stack.pop() {
                    let new_end = self.gensym();
                    self.control_stack
                        .push(ControlStructure::Target(new_end.clone()));
                    self.emit(format!("jmpr @{}", new_end));
                    self.emit(format!("{}:", old_end));
                } else {
                    return Some(AssembleError::MacroError(self.current_location()));
                }
            }

            Macro::While | Macro::Until => {
                let label = self.gensym();
                self.control_stack
                    .push(ControlStructure::Loop(label.clone(), mac.into()));
                self.emit(format!("{}:", label));
            }

            Macro::Do => {
                if let Some(ControlStructure::Loop(label, loop_type)) = self.control_stack.pop() {
                    let after = self.gensym();
                    self.control_stack
                        .push(ControlStructure::LoopDo(label, after.clone()));
                    let instr = match loop_type {
                        LoopType::While => "brz",
                        LoopType::Until => "brnz",
                    };
                    self.emit(format!("{} @{}", instr, after));
                } else {
                    return Some(AssembleError::MacroError(self.current_location()));
                }
            }

            Macro::End => match self.control_stack.pop() {
                Some(ControlStructure::Target(label)) => self.emit(format!("{}:", label)),
                Some(ControlStructure::LoopDo(start, after)) => {
                    self.emit(format!("jmpr @{}", start));
                    self.emit(format!("{}:", after));
                }
                _ => return Some(AssembleError::MacroError(self.current_location())),
            },
            Macro::Break => {
                if let Some(ControlStructure::LoopDo(_, after)) = self.innermost_loop() {
                    self.emit(format!("jmpr @{}", after));
                } else {
                    return Some(AssembleError::MacroError(self.current_location()))
                }
            }
            Macro::Continue => {
                if let Some(ControlStructure::LoopDo(test, _)) = self.innermost_loop() {
                    self.emit(format!("jmpr @{}", test));
                } else {
                    return Some(AssembleError::MacroError(self.current_location()))
                }
            }
        }
        None
    }

    pub fn innermost_loop(&self) -> Option<&ControlStructure> {
        self.control_stack.iter().rev().find(|cs| {
            match cs {
                ControlStructure::LoopDo(_, _) => true,
                _ => false
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Label, Node};
    use vcore::opcodes::Opcode::*;

    fn lines_for(source: Vec<String>) -> Vec<VASMLine> {
        let include = |_name: String| panic!();
        let src = LineSource::new("<none>", source, include);
        src.map(|line| line.unwrap().line).collect()
    }

    fn error_for(source: Vec<String>) -> Option<AssembleError> {
        let include = |_name: String| panic!();
        for line in LineSource::new("<none>", source, include) {
            if line.is_err() { return line.err() }
        }
        None
    }

    fn stringify(a: Vec<&str>) -> Vec<String> {
        a.into_iter().map(String::from).collect()
    }

    #[test]
    fn test_preprocess() {
        let include = |_name: String| panic!();
        let lines = stringify(vec!["add"]);
        let mut src = LineSource::new("blah", lines, include);
        assert_eq!(
            src.next(),
            Some(Ok(Line {
                line: VASMLine::Instruction(None, Add, None),
                location: Location {
                    line_num: 1,
                    file: "blah".to_string(),
                }
            }))
        );
        assert_eq!(src.next(), None);
    }

    #[test]
    fn test_preprocess_include() {
        let include = |_name: String| Ok(stringify(vec!["sub"]));
        let lines = stringify(vec!["#include \"foo\"", "add"]);
        let src = LineSource::new("blah", lines, include);
        assert_eq!(
            src.collect::<Vec<Result<Line, AssembleError>>>(),
            vec![
                Ok(Line {
                    line: VASMLine::Instruction(None, Sub, None),
                    location: Location {
                        line_num: 1,
                        file: "foo".to_string(),
                    }
                }),
                Ok(Line {
                    line: VASMLine::Instruction(None, Add, None),
                    location: Location {
                        line_num: 2,
                        file: "blah".to_string(),
                    }
                })
            ]
        );
    }

    #[test]
    fn test_preprocess_if_end() {
        assert_eq!(
            lines_for(stringify(vec!["#if", "#end"])),
            vec![
                VASMLine::Instruction(None, Brz, Some(Node::relative_label("__gensym_1"))),
                VASMLine::LabelDef(Label::from("__gensym_1"))
            ]
        )
    }

    #[test]
    fn test_preprocess_else() {
        assert_eq!(
            lines_for(stringify(vec!["#if", "add", "#else", "sub", "#end"])),
            vec![
                VASMLine::Instruction(None, Brz, Some(Node::relative_label("__gensym_1"))),
                VASMLine::Instruction(None, Add, None),
                VASMLine::Instruction(None, Jmpr, Some(Node::relative_label("__gensym_2"))),
                VASMLine::LabelDef(Label("__gensym_1".to_string())),
                VASMLine::Instruction(None, Sub, None),
                VASMLine::LabelDef(Label("__gensym_2".to_string())),
            ]
        )
    }

    #[test]
    fn test_preprocess_do_end() {
        assert_eq!(
            lines_for(stringify(vec!["#while", "#do", "#end"])),
            vec![
                VASMLine::LabelDef(Label("__gensym_1".to_string())),
                VASMLine::Instruction(
                    None,
                    Brz,
                    Some(Node::RelativeLabel("__gensym_2".to_string()))
                ),
                VASMLine::Instruction(
                    None,
                    Jmpr,
                    Some(Node::RelativeLabel("__gensym_1".to_string()))
                ),
                VASMLine::LabelDef(Label::from("__gensym_2"))
            ]
        )
    }

    #[test]
    fn test_preprocess_break() {
        // First a working one
        assert_eq!(
            lines_for(stringify(vec!["#while", "#do", "#break", "#end"])),
            vec![
                VASMLine::LabelDef(Label("__gensym_1".to_string())),
                VASMLine::Instruction(
                    None,
                    Brz,
                    Some(Node::RelativeLabel("__gensym_2".to_string()))
                ),
                VASMLine::Instruction(
                    None,
                    Jmpr,
                    Some(Node::RelativeLabel("__gensym_2".to_string()))
                ),
                VASMLine::Instruction(
                    None,
                    Jmpr,
                    Some(Node::RelativeLabel("__gensym_1".to_string()))
                ),
                VASMLine::LabelDef(Label::from("__gensym_2"))
            ]
        );
    }

    #[test]
    fn test_malformed_break() {
        // Malformed one
        assert_eq!(
            error_for(stringify(vec!["#while", "#break"])),
            Some(AssembleError::MacroError(Location::from(2)))
        )
    }

    #[test]
    fn test_preprocess_continue() {
        // First a working one
        assert_eq!(
            lines_for(stringify(vec!["#while", "#do", "#continue", "#end"])),
            vec![
                VASMLine::LabelDef(Label("__gensym_1".to_string())),
                VASMLine::Instruction(
                    None,
                    Brz,
                    Some(Node::RelativeLabel("__gensym_2".to_string()))
                ),
                VASMLine::Instruction(
                    None,
                    Jmpr,
                    Some(Node::RelativeLabel("__gensym_1".to_string()))
                ),
                VASMLine::Instruction(
                    None,
                    Jmpr,
                    Some(Node::RelativeLabel("__gensym_1".to_string()))
                ),
                VASMLine::LabelDef(Label::from("__gensym_2"))
            ]
        );
    }

    #[test]
    fn test_malformed_continue() {
        // Malformed one
        assert_eq!(
            error_for(stringify(vec!["#while", "#continue"])),
            Some(AssembleError::MacroError(Location::from(2)))
        )
    }
}
