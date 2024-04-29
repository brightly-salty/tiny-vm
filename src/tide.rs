#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release
#![warn(clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::future_not_send)]

use eframe::egui::{Ui, WidgetText};
use egui::text_selection::CursorRange;
use egui_dock::{DockArea, DockState, NodeIndex, Style, TabViewer};
use egui_extras::{Column, TableBuilder};

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::PathBuf;
#[cfg(target_arch = "wasm32")]
use std::sync::mpsc::{Receiver, Sender};
use tiny_vm::assemble;
use tiny_vm::cpu::{Cpu, Input, Output};
use tiny_vm::types::{Address, TinyError, TinyResult};

const EXAMPLES: [(&str, &str); 3] = [
    ("Hello World", include_str!("../examples/helloWorld.tny")),
    ("Box Volume", include_str!("../examples/boxVolume.tny")),
    ("GCD", include_str!("../examples/gcd.tny")),
];

#[cfg(target_arch = "wasm32")]
enum OpenFileResult {
    Opened(Option<PathBuf>, String),
    Cancelled,
    Fail,
}

#[cfg(target_arch = "wasm32")]
enum SaveFileResult {
    Saved(Option<PathBuf>),
    UnsavedContinuing,
    UnsavedCancelled,
    Fail,
}

#[cfg(target_arch = "wasm32")]
enum ReturnAsyncFile {
    New(bool),
    Open(OpenFileResult),
    Save(SaveFileResult),
}

#[derive(Clone, Copy)]
struct UnappliedPreferences;

#[derive(Clone, Copy, Default)]
enum Focus {
    #[default]
    None,
    Errors,
    Input,
    Output,
}

fn ascii_char(c: u8) -> String {
    match c {
        0 => "NUL".to_owned(),
        1 => "SOH".to_owned(),
        2 => "STX".to_owned(),
        3 => "ETX".to_owned(),
        4 => "EOT".to_owned(),
        5 => "ENQ".to_owned(),
        6 => "ACK".to_owned(),
        7 => "BEL".to_owned(),
        8 => "BS".to_owned(),
        9 => "HT".to_owned(),
        10 => "LF".to_owned(),
        11 => "VT".to_owned(),
        12 => "FF".to_owned(),
        13 => "CR".to_owned(),
        14 => "SO".to_owned(),
        15 => "SI".to_owned(),
        16 => "DLE".to_owned(),
        17 => "DC1".to_owned(),
        18 => "DC2".to_owned(),
        19 => "DC3".to_owned(),
        20 => "DC4".to_owned(),
        21 => "NAK".to_owned(),
        22 => "SYN".to_owned(),
        23 => "ETB".to_owned(),
        24 => "CAN".to_owned(),
        25 => "EM".to_owned(),
        26 => "SUB".to_owned(),
        27 => "ESC".to_owned(),
        28 => "FS".to_owned(),
        29 => "GS".to_owned(),
        30 => "RS".to_owned(),
        31 => "US".to_owned(),
        32 => "SP".to_owned(),
        127 => "DEL".to_owned(),
        c => String::from_utf8_lossy(&[c]).into_owned(),
    }
}

// Native
#[cfg(not(target_arch = "wasm32"))]
fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions::default();

    eframe::run_native("TIDE", options, Box::new(|cc| Box::new(Tide::new(cc))))
}

// Web
#[cfg(target_arch = "wasm32")]
fn main() {
    let web_options = eframe::WebOptions::default();

    wasm_bindgen_futures::spawn_local(async {
        eframe::WebRunner::new()
            .start(
                "the_canvas_id", // hardcode it
                web_options,
                Box::new(|cc| Box::new(Tide::new(cc))),
            )
            .await
            .expect("failed to start eframe");
    });
}

struct TINYTabViewer<'a> {
    tide: &'a mut Tide,
}

fn text_editor(
    s: &mut String,
    dirty: &mut bool,
    enabled: bool,
    executing_line: Option<usize>,
    cursor_range: &mut Option<CursorRange>,
    ui: &mut Ui,
) {
    let rightmost_comment_position = (s
        .split('\n')
        .map(|line| match line.split_once(';') {
            Some((a, _)) => a.trim_end().len(),
            None => line.trim_end().len(),
        })
        .max()
        .unwrap_or(1)
        + 4)
        / 4
        * 4;

    s.replace_range(
        ..,
        &s.split_inclusive('\n')
            .map(|line| match line.split_once(';') {
                Some((asm, comment)) => {
                    let mut output = String::new();
                    let asm = asm.trim_end_matches(' ');

                    if !asm.is_empty() && !comment.is_empty() {
                        output.push_str(asm);
                        output.push_str(&" ".repeat(rightmost_comment_position - asm.len()));
                        output.push(';');
                        output.push_str(comment);
                    } else {
                        output.push_str(line);
                    }
                    output
                }
                None => line.into(),
            })
            .collect::<String>(),
    );

    // TODO: Cursor repositioning when we mess with indents
    // TODO: Toggle for automatic indent handling

    let previous = s.clone();

    ui.add_enabled(enabled, |ui: &mut Ui| {
        let output = egui::TextEdit::multiline(s)
            .code_editor()
            .desired_width(f32::INFINITY)
            .min_size(ui.available_size())
            .layouter(&mut |ui, string, _wrap_width| {
                let mut layout_job = egui::text::LayoutJob::default();

                layout_job.wrap.max_width = f32::INFINITY;

                let default_text_format = egui::text::TextFormat {
                    font_id: egui::FontId::monospace(15.0),
                    ..Default::default()
                };
                let executing_text_format = egui::text::TextFormat {
                    font_id: default_text_format.font_id.clone(),
                    color: egui::Color32::BLUE,
                    ..Default::default()
                };

                for (i, line) in string.split_inclusive('\n').enumerate() {
                    match executing_line {
                        Some(l) if l == i => {
                            layout_job.append(line, 0.0, executing_text_format.clone());
                        }
                        _ => layout_job.append(line, 0.0, default_text_format.clone()),
                    }
                }

                ui.fonts(|f| f.layout_job(layout_job))
            })
            .show(ui);

        *dirty |= &previous != s;

        *cursor_range = output.cursor_range;

        output.response
    });
}

impl<'a> TabViewer for TINYTabViewer<'a> {
    // This associated type is used to attach some data to each tab.
    type Tab = String;

    // Returns the current `tab`'s title.
    fn title(&mut self, tab: &mut Self::Tab) -> WidgetText {
        tab.as_str().into()
    }

    // Defines the contents of a given `tab`.
    #[allow(clippy::too_many_lines)]
    fn ui(&mut self, ui: &mut Ui, tab: &mut Self::Tab) {
        match tab.as_str() {
            "Source" => {
                text_editor(
                    &mut self.tide.source,
                    &mut self.tide.dirty,
                    self.tide.cpu.is_none(),
                    self.tide
                        .cpu
                        .as_ref()
                        .and_then(|cpu| self.tide.source_map.get(&cpu.cu.ip))
                        .copied(),
                    &mut self.tide.source_cursor,
                    ui,
                );
            }
            "Listing" => {
                let num_rows = 900;

                TableBuilder::new(ui)
                    .striped(true)
                    .column(Column::auto().resizable(true))
                    .column(Column::auto().resizable(true))
                    .column(Column::remainder().resizable(true))
                    .header(20.0, |mut header| {
                        header.col(|ui| {
                            ui.label("Address");
                            ui.separator();
                        });

                        header.col(|ui| {
                            ui.label("Value");
                            ui.separator();
                        });

                        header.col(|ui| {
                            ui.label("Source line");
                            ui.separator();
                        });
                    })
                    .body(|body| {
                        body.rows(15.0, num_rows, |mut row| {
                            let index = row.index();

                            let address = Address::new(u16::try_from(index).unwrap());

                            let mut source_lines = self.tide.source.split('\n');
                            let source_line = self
                                .tide
                                .source_map
                                .get(&address)
                                .and_then(|line_num| source_lines.nth(*line_num));

                            row.col(|ui| {
                                ui.monospace(format!("{index:03}"));
                            });

                            row.col(|ui| {
                                ui.monospace(self.tide.cpu.as_ref().map_or_else(
                                    || "?????".to_owned(),
                                    |cpu| format!("{:05}", cpu.memory[address].0),
                                ));
                            });

                            row.col(|ui| {
                                ui.monospace(source_line.unwrap_or("<empty>"));
                            });
                        });
                    });
            }
            "Executable" => {
                if let Some(cpu) = self.tide.cpu.as_ref() {
                    let max_address = self
                        .tide
                        .source_map
                        .iter()
                        .max_by_key(|(&address, _)| address)
                        .map_or(0, |(a, _)| a.0);

                    // Show "AAA, XXXXX" for all addresses up to the last source-mapped one
                    for i in 0..max_address {
                        ui.monospace(format!("{:03}, {:05}", i, cpu.memory[Address::new(i)].0));
                    }

                    ui.group(|ui| {
                        ui.label("Symbol table");
                        ui.separator();

                        for address in self.tide.symbols.keys() {
                            ui.monospace(format!("{:03}, {}", address, self.tide.symbols[address]));
                        }
                    });
                }
            }
            "Memory" => TableBuilder::new(ui)
                .striped(true)
                .column(Column::remainder().resizable(false))
                .body(|body| {
                    body.rows(15.0, 900, |mut row| {
                        let addr = u16::try_from(row.index()).unwrap();

                        row.col(|ui| {
                            if let Some(ref mut cpu) = self.tide.cpu.as_mut() {
                                let value = cpu.memory[Address(addr)];
                                let chr = cpu.memory[Address(addr)].read_as_char().unwrap_or(' ');

                                ui.monospace(format!(
                                    "{addr:03}  {value:05} {}  ",
                                    if chr.is_ascii() { chr } else { ' ' }
                                ))
                                .context_menu(|ui| {
                                    ui.label(format!("Address {addr:03}"));
                                    ui.separator();
                                    ui.add(
                                        egui::DragValue::new(&mut cpu.memory[Address(addr)].0)
                                            .custom_formatter(|n, _| format!("{n:05}"))
                                            .clamp_range(-99999..=99999),
                                    );
                                });
                            } else {
                                ui.add_enabled_ui(false, |ui| {
                                    ui.monospace(format!("{addr:03}  ?????    ",));
                                });
                            }
                        });
                    });
                }),
            "Assembly Errors" => {
                ui.add_sized(
                    ui.available_size(),
                    egui::TextEdit::multiline(&mut self.tide.error)
                        .code_editor()
                        .interactive(false),
                );
            }
            "Input/Output" => {
                if matches!(self.tide.focus_redirect, Focus::Input) {
                    ui.text_edit_singleline(&mut self.tide.input)
                        .request_focus();
                } else {
                    ui.text_edit_singleline(&mut self.tide.input);
                }

                ui.add_sized(
                    ui.available_size(),
                    egui::TextEdit::multiline(&mut self.tide.output)
                        .code_editor()
                        .interactive(false),
                );
                ui.input(|i| {
                    self.tide.input_ready = i.key_pressed(egui::Key::Enter);
                });
            }
            "Registers" => {
                ui.add_enabled_ui(self.tide.cpu.is_some(), |ui| {
                    egui::Grid::new(1).show(ui, |ui| {
                        if let Some(ref mut cpu) = &mut self.tide.cpu {
                            ui.checkbox(&mut self.tide.editing_registers, "Editing");
                            ui.end_row();

                            ui.label("Accumulator");

                            if self.tide.editing_registers {
                                ui.add(
                                    egui::DragValue::new(&mut cpu.alu.acc.0)
                                        .custom_formatter(|n, _| format!("{n:05}"))
                                        .clamp_range(-99999..=99999),
                                );
                            } else {
                                ui.monospace(format!("{}", cpu.alu.acc));
                            }

                            ui.end_row();

                            ui.label("Instruction Pointer");

                            if self.tide.editing_registers {
                                ui.add(
                                    egui::DragValue::new(&mut cpu.cu.ip.0)
                                        .custom_formatter(|n, _| format!("{n:03}"))
                                        .clamp_range(0..=999),
                                );
                            } else {
                                ui.monospace(format!("{}", cpu.cu.ip));
                            }

                            ui.end_row();

                            ui.label("Stack Pointer");

                            if self.tide.editing_registers {
                                ui.add(
                                    egui::DragValue::new(&mut cpu.alu.sp.0)
                                        .custom_formatter(|n, _| format!("{n:03}"))
                                        .clamp_range(0..=999),
                                );
                            } else {
                                ui.monospace(format!("{}", cpu.alu.sp));
                            }

                            ui.end_row();

                            ui.label("Base Pointer");

                            if self.tide.editing_registers {
                                ui.add(
                                    egui::DragValue::new(&mut cpu.alu.bp.0)
                                        .custom_formatter(|n, _| format!("{n:03}"))
                                        .clamp_range(0..=999),
                                );
                            } else {
                                ui.monospace(format!("{}", cpu.alu.bp));
                            }

                            ui.end_row();

                            ui.label("Instruction Register");

                            if self.tide.editing_registers {
                                ui.add(
                                    egui::DragValue::new(&mut cpu.cu.ir.opcode.0)
                                        .custom_formatter(|n, _| format!("{n:02}"))
                                        .clamp_range(0..=99),
                                );

                                ui.add(
                                    egui::DragValue::new(&mut cpu.cu.ir.operand.0)
                                        .custom_formatter(|n, _| format!("{n:03}"))
                                        .clamp_range(0..=999),
                                );
                            } else {
                                ui.monospace(format!("{}", cpu.cu.ir.as_byte()));
                            }

                            ui.end_row();
                        }
                    });
                });
            }
            "Stack" => {
                if let Some(cpu) = &self.tide.cpu {
                    for byte in cpu.get_stack() {
                        ui.monospace(byte.to_string());
                    }
                }
            }
            "Symbols" => {
                ui.add_enabled_ui(self.tide.cpu.is_some(), |ui| {
                    for address in self.tide.symbols.keys() {
                        ui.monospace(format!("{:03}   {}", address, self.tide.symbols[address]));
                    }
                });
            }

            _ => {
                ui.label("<Invalid tab>");
            }
        };
    }

    fn closeable(&mut self, _tab: &mut Self::Tab) -> bool {
        false
    }
}

const fn default_dirty() -> bool {
    cfg!(target_arch = "wasm32")
}

#[derive(Serialize, Deserialize)]
#[allow(clippy::struct_excessive_bools)]
struct Tide {
    #[cfg_attr(not(target_arch = "wasm32"), serde(skip))]
    source: String,
    #[serde(skip, default = "default_dirty")]
    dirty: bool,
    #[serde(skip)]
    save_path: Option<PathBuf>,
    #[serde(skip, default = "default_channels")]
    #[cfg(target_arch = "wasm32")]
    channels: Option<(Sender<ReturnAsyncFile>, Receiver<ReturnAsyncFile>)>,
    #[serde(skip)]
    symbols: BTreeMap<Address, String>, // Symbols
    #[serde(skip)]
    source_map: HashMap<Address, usize>,
    #[serde(skip)]
    breakpoints: HashSet<u16>, // Indices of lines
    #[serde(skip)]
    cpu: Option<Cpu>,
    #[serde(skip)]
    cpu_state: Option<Output>,
    #[serde(skip)]
    running_to_completion: bool,
    #[serde(skip)]
    stepping_over: bool,

    #[serde(skip)]
    source_cursor: Option<CursorRange>,

    #[serde(skip)]
    editing_registers: bool,

    #[serde(skip)]
    focus_redirect: Focus,

    #[serde(skip)]
    input: String,
    #[serde(skip)]
    input_ready: bool,

    #[serde(skip)]
    output: String,

    #[serde(skip)]
    error: String,

    #[serde(skip, default = "default_dock_state")]
    dock_state: DockState<String>,

    #[serde(skip)]
    about_window_open: bool,

    #[serde(skip)]
    shortcut_window_open: bool,

    #[serde(skip)]
    ascii_window_open: bool,

    #[serde(skip)]
    unapplied_preferences: Option<UnappliedPreferences>,
}

#[allow(clippy::unnecessary_wraps)]
#[cfg(target_arch = "wasm32")]
fn default_channels() -> Option<(Sender<ReturnAsyncFile>, Receiver<ReturnAsyncFile>)> {
    Some(std::sync::mpsc::channel())
}

fn default_dock_state() -> DockState<String> {
    let tabs = ["Source", "Listing", "Executable", "Memory"]
        .map(str::to_string)
        .into_iter()
        .collect();

    let mut dock_state = DockState::new(tabs);
    let root = dock_state.main_surface_mut();

    // Add bottom panel
    let [old_node, _] = root.split_below(
        NodeIndex::root(),
        0.8,
        vec!["Assembly Errors".to_string(), "Input/Output".to_string()],
    );

    // Add side panel
    let [_, side_panel] = root.split_right(old_node, 0.8, vec!["Registers".to_string()]);
    let [_, side_panel] = root.split_below(side_panel, 1.0 / 3.0, vec!["Stack".to_string()]);
    root.split_below(side_panel, 0.5, vec!["Symbols".to_string()]);

    dock_state
}

impl Tide {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        if let Some(storage) = cc.storage {
            return eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();
        }

        Self::default()
    }

    fn assemble(&mut self) -> TinyResult<()> {
        self.input.clear();
        self.input_ready = false;
        self.output.clear();
        self.error.clear();
        self.cpu = Some(Cpu::new());

        let result = assemble(&self.source)?;
        self.symbols = result.0;
        self.source_map = result.1;
        self.cpu.as_mut().unwrap().set_memory(&result.2);
        self.cpu_state = Some(Output::ReadyToCycle);

        Ok(())
    }

    fn reset(&mut self) {
        self.source.clear();
        self.dirty = true;
        self.save_path = None;
        self.symbols.clear();
        self.source_map.clear();
        self.breakpoints.clear();
        self.cpu = None;
        self.cpu_state = None;
        self.running_to_completion = false;
        self.stepping_over = false;
        self.focus_redirect = Focus::None;
        self.input.clear();
        self.input_ready = false;
        self.output.clear();
        self.error.clear();
    }

    fn focused_error(&mut self, s: &TinyError) {
        self.error.push_str(&s.to_string());
        self.focus_redirect = Focus::Errors;
    }

    fn focused_output(&mut self, s: &str) {
        self.output.push_str(s);
        self.focus_redirect = Focus::Output;
    }

    fn run(&mut self) -> TinyResult<()> {
        self.assemble()?;
        self.running_to_completion = true;

        Ok(())
    }

    fn start(&mut self) -> TinyResult<()> {
        self.assemble()?;

        Ok(())
    }

    fn stop(&mut self) {
        self.running_to_completion = false;
        self.stepping_over = false;
        self.cpu.take();
        self.cpu_state.take();
        self.source_map.clear();
        self.symbols.clear();
    }

    fn handle_input(&mut self) -> Option<Result<Output, TinyError>> {
        let Some(cpu) = self.cpu.as_mut() else {
            return None;
        };

        let Some(cpu_state) = self.cpu_state.as_mut() else {
            return None;
        };

        match cpu_state {
            Output::WaitingForString if self.input_ready => {
                let result = cpu.step(Input::String(self.input.clone()));
                self.output.push_str(&self.input);
                self.output.push('\n');
                self.input.clear();
                self.input_ready = false;
                Some(result)
            }
            Output::WaitingForChar if self.input_ready && self.input.len() == 1 => {
                let c = self.input.pop().unwrap();
                let result = cpu.step(Input::Char(c));
                self.output.push(c);
                self.output.push('\n');
                self.input_ready = false;
                Some(result)
            }
            Output::WaitingForInteger if self.input_ready => match self.input.parse() {
                Ok(i) => {
                    let result = cpu.step(Input::Integer(i));
                    self.output.push_str(&self.input);
                    self.output.push('\n');
                    self.input.clear();
                    self.input_ready = false;
                    Some(result)
                }
                Err(_) => None,
            },
            Output::JumpedToFunction if self.stepping_over => match cpu.step(Input::None) {
                Ok(
                    Output::JumpedToFunction | Output::ReturnedFromFunction | Output::ReadyToCycle,
                ) => self.handle_input(),
                v => Some(v),
            },
            Output::ReadyToCycle | Output::ReturnedFromFunction => Some(cpu.step(Input::None)),

            _ => None,
        }
    }

    fn handle_output(&mut self, result: Result<Output, TinyError>) {
        let Some(cpu_state) = self.cpu_state.as_mut() else {
            return;
        };

        match result {
            Ok(r) => match r {
                Output::Char(c) => {
                    *cpu_state = Output::ReadyToCycle;
                    self.output.push(c);
                    self.focus_redirect = Focus::Output;
                }
                Output::String(ref s) => {
                    *cpu_state = Output::ReadyToCycle;
                    self.focused_output(s);
                }
                ref out @ Output::Stopped => {
                    *cpu_state = out.clone();
                    self.running_to_completion = false;
                    self.stepping_over = false;
                    self.error.push_str("Completed");
                }
                ref out @ (Output::WaitingForChar
                | Output::WaitingForString
                | Output::WaitingForInteger) => {
                    self.focus_redirect = Focus::Input;
                    *cpu_state = out.clone();
                }
                ref out @ Output::ReturnedFromFunction => {
                    self.stepping_over = false;
                    *cpu_state = out.clone();
                }
                out => *cpu_state = out,
            },

            Err(e) => {
                *cpu_state = Output::Stopped;
                self.running_to_completion = false;
                self.stepping_over = false;
                self.focused_error(&e);
            }
        };
    }

    fn step(&mut self) {
        // Stop running on a breakpoint: skip this step() and allow future ones
        if self.running_to_completion {
            if let Some(cpu) = self.cpu.as_ref() {
                // Safety: code can't exceed 1000 lines, so won't exceed 65535
                #[allow(clippy::cast_possible_truncation)]
                if self
                    .source_map
                    .get(&cpu.cu.ip)
                    .map_or_else(|| false, |line| self.breakpoints.contains(&(*line as u16)))
                {
                    self.running_to_completion = false;
                    self.stepping_over = false;
                    return;
                }
            }
        }

        // Handle input (changes to the CPU's step() arguments based on its current state and ours)
        let Some(result) = self.handle_input() else {
            return;
        };

        // Handle output (changes to our state based on the Cpu)
        self.handle_output(result);
    }

    fn toggle_breakpoint(&mut self) {
        if let Some(cursor_range) = self.source_cursor {
            // Safety: code can't exceed 1000 lines, so won't exceed 65535
            #[allow(clippy::cast_possible_truncation)]
            let line = cursor_range.sorted_cursors()[0].pcursor.paragraph as u16;

            if self.breakpoints.contains(&line) {
                self.breakpoints.remove(&line);
            } else {
                self.breakpoints.insert(line);
            }
        }
    }
}

impl Default for Tide {
    fn default() -> Self {
        Self {
            source: String::new(),
            dirty: default_dirty(),
            save_path: None,
            #[cfg(target_arch = "wasm32")]
            channels: default_channels(),
            symbols: BTreeMap::new(),
            source_map: HashMap::new(),
            breakpoints: HashSet::new(),
            cpu: None,
            cpu_state: None,
            running_to_completion: false,
            stepping_over: false,

            source_cursor: None,

            editing_registers: false,

            focus_redirect: Focus::None,

            input: String::new(),
            input_ready: false,

            output: String::new(),

            error: String::new(),

            dock_state: default_dock_state(),

            about_window_open: false,
            shortcut_window_open: false,
            ascii_window_open: false,
            unapplied_preferences: None,
        }
    }
}

const NEW_FILE_SHORTCUT: egui::KeyboardShortcut = egui::KeyboardShortcut::new(
    egui::Modifiers {
        ctrl: true,
        shift: false,
        alt: false,
        command: true,
        mac_cmd: false,
    },
    egui::Key::N,
);

const OPEN_FILE_SHORTCUT: egui::KeyboardShortcut = egui::KeyboardShortcut::new(
    egui::Modifiers {
        ctrl: true,
        shift: false,
        alt: false,
        command: true,
        mac_cmd: false,
    },
    egui::Key::O,
);

const SAVE_FILE_SHORTCUT: egui::KeyboardShortcut = egui::KeyboardShortcut::new(
    egui::Modifiers {
        ctrl: true,
        shift: false,
        alt: false,
        command: true,
        mac_cmd: false,
    },
    egui::Key::S,
);

const SAVE_AS_FILE_SHORTCUT: egui::KeyboardShortcut = egui::KeyboardShortcut::new(
    egui::Modifiers {
        ctrl: true,
        shift: true,
        alt: false,
        command: true,
        mac_cmd: false,
    },
    egui::Key::S,
);

const ASSEMBLE_SHORTCUT: egui::KeyboardShortcut = egui::KeyboardShortcut::new(
    egui::Modifiers {
        ctrl: true,
        shift: true,
        alt: false,
        command: false,
        mac_cmd: false,
    },
    egui::Key::B,
);
const START_SHORTCUT: egui::KeyboardShortcut =
    egui::KeyboardShortcut::new(egui::Modifiers::NONE, egui::Key::F5);
const RUN_SHORTCUT: egui::KeyboardShortcut =
    egui::KeyboardShortcut::new(egui::Modifiers::CTRL, egui::Key::F5);
const STEP_OVER_SHORTCUT: egui::KeyboardShortcut =
    egui::KeyboardShortcut::new(egui::Modifiers::NONE, egui::Key::F10);
const STEP_INTO_SHORTCUT: egui::KeyboardShortcut =
    egui::KeyboardShortcut::new(egui::Modifiers::NONE, egui::Key::F11);
const STOP_SHORTCUT: egui::KeyboardShortcut =
    egui::KeyboardShortcut::new(egui::Modifiers::SHIFT, egui::Key::F5);
const BREAKPOINT_SHORTCUT: egui::KeyboardShortcut =
    egui::KeyboardShortcut::new(egui::Modifiers::NONE, egui::Key::F9);

const SHORTCUTS: [(&str, egui::KeyboardShortcut); 11] = [
    ("New File", NEW_FILE_SHORTCUT),
    ("Open File", OPEN_FILE_SHORTCUT),
    ("Save", SAVE_FILE_SHORTCUT),
    ("Save As", SAVE_AS_FILE_SHORTCUT),
    ("Assemble", ASSEMBLE_SHORTCUT),
    ("Start", START_SHORTCUT),
    ("Start Without Debugging", RUN_SHORTCUT),
    ("Step Over", STEP_OVER_SHORTCUT),
    ("Step Into", STEP_INTO_SHORTCUT),
    ("Stop", STOP_SHORTCUT),
    ("Toggle Breakpoint", BREAKPOINT_SHORTCUT),
];

#[cfg(target_arch = "wasm32")]
fn run_future<T: std::future::Future<Output = ()> + 'static>(f: T) {
    wasm_bindgen_futures::spawn_local(f);
}

impl eframe::App for Tide {
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, self);
    }

    #[allow(clippy::too_many_lines)]
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        #[cfg(not(target_arch = "wasm32"))]
        if ctx.input(|i| i.viewport().close_requested())
            && self.dirty
            && !native_io::handle_dirty(&self.save_path, &self.source)
        {
            ctx.send_viewport_cmd(egui::ViewportCommand::CancelClose);
        }

        let mut close_preferences = false;

        if matches!(self.unapplied_preferences, Some(UnappliedPreferences)) {
            egui::Window::new("Preferences")
                .auto_sized()
                .show(ctx, |ui| {
                    ui.group(|ui| {
                        ui.label("Theme");
                        egui::global_dark_light_mode_buttons(ui);
                    });

                    close_preferences = ui.button("Close").clicked();
                });
        }

        if close_preferences {
            self.unapplied_preferences = None;
        }

        egui::CentralPanel::default().show(ctx, |ui| {
            if self.shortcut_window_open {
                egui::Window::new("Shortcuts")
                    .auto_sized()
                    .open(&mut self.shortcut_window_open)
                    .show(ctx, |ui| {
                        egui::Grid::new(7).show(ui, |ui| {
                            for (action, shortcut) in SHORTCUTS {
                                // Separate sections: File actions | Build actions | Run actions |
                                //                    Debug actions
                                if [ASSEMBLE_SHORTCUT, START_SHORTCUT, STEP_OVER_SHORTCUT]
                                    .contains(&shortcut)
                                {
                                    ui.label(""); // Would use a separator instead but they only
                                                  // take up one column and two of them looks ugly
                                    ui.end_row();
                                }

                                ui.label(action);
                                ui.label(ctx.format_shortcut(&shortcut));
                                ui.end_row();
                            }
                        })
                    });
            }

            if self.about_window_open {
                egui::Window::new("About")
                    .auto_sized()
                    .open(&mut self.about_window_open)
                    .show(ctx, |ui| {
                        ui.label(
                            egui::RichText::new("Tiny Integrated Development Environment")
                                .size(24.0),
                        );

                        egui::Grid::new(6).show(ui, |ui| {
                            ui.weak(format!(
                                "{} built on {}",
                                env!("VERGEN_GIT_DESCRIBE"),
                                env!("VERGEN_BUILD_DATE"),
                            ));
                            ui.end_row();
                            ui.end_row();

                            ui.label("Designed By:");
                            ui.label("Dr. Steve Baber");
                            ui.end_row();

                            ui.label("");
                            ui.label("Dr. Tim Baird");
                            ui.end_row();
                            ui.label("");
                            ui.end_row();

                            ui.label("Implemented By:");
                            ui.label("Caden Haustein");
                            ui.end_row();

                            ui.label("");
                            ui.label("Nathaniel Kinonen");
                            ui.end_row();
                            ui.label("");
                            ui.end_row();

                            ui.label("Originally Implemented By:");
                            ui.label("Dana Steil");
                        })
                    });
            }

            if self.ascii_window_open {
                egui::Window::new("ASCII Table")
                    .auto_sized()
                    .open(&mut self.ascii_window_open)
                    .show(ctx, |ui| {
                        for row in 0..32 {
                            ui.horizontal(|ui| {
                                for column in 0..4 {
                                    let index = row + 32 * column;

                                    ui.monospace(format!("{:<4} {:<3}", index, ascii_char(index)));

                                    if column != 3 {
                                        ui.separator();
                                    }
                                }
                            });
                        }
                    });
            }

            // Ordering is important because shortcuts are *consumed*; the longest
            // shortcuts should be checked first when the same logical key is used with
            // different modifiers
            let mut new_file_pressed = ui.input_mut(|i| i.consume_shortcut(&NEW_FILE_SHORTCUT));
            let mut open_file_pressed = ui.input_mut(|i| i.consume_shortcut(&OPEN_FILE_SHORTCUT));
            let mut save_as_file_pressed =
                ui.input_mut(|i| i.consume_shortcut(&SAVE_AS_FILE_SHORTCUT));
            let mut save_file_pressed = ui.input_mut(|i| i.consume_shortcut(&SAVE_FILE_SHORTCUT));

            let mut assemble_pressed = ui.input_mut(|i| i.consume_shortcut(&ASSEMBLE_SHORTCUT));

            let mut stop_pressed = ui.input_mut(|i| i.consume_shortcut(&STOP_SHORTCUT));
            let mut run_pressed = ui.input_mut(|i| i.consume_shortcut(&RUN_SHORTCUT));
            let mut start_pressed = ui.input_mut(|i| i.consume_shortcut(&START_SHORTCUT));

            let mut step_over_pressed = ui.input_mut(|i| i.consume_shortcut(&STEP_OVER_SHORTCUT));
            let mut step_into_pressed = ui.input_mut(|i| i.consume_shortcut(&STEP_INTO_SHORTCUT));
            let mut breakpoint_pressed = ui.input_mut(|i| i.consume_shortcut(&BREAKPOINT_SHORTCUT));

            // Handle file IO state updates
            #[cfg(target_arch = "wasm32")]
            let results = self
                .channels
                .as_mut()
                .unwrap()
                .1
                .try_iter()
                .collect::<Vec<ReturnAsyncFile>>();

            #[cfg(target_arch = "wasm32")]
            for result in results {
                match result {
                    ReturnAsyncFile::New(true) => self.reset(),
                    ReturnAsyncFile::Open(OpenFileResult::Opened(path, source)) => {
                        self.save_path = path;
                        self.source = source;
                    }
                    ReturnAsyncFile::Save(SaveFileResult::Saved(path)) => {
                        self.dirty = false;
                        self.save_path = path;
                    }
                    _ => {}
                }
            }

            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    new_file_pressed |= ui.button("New").clicked();
                    open_file_pressed |= ui.button("Open").clicked();

                    ui.separator();

                    save_file_pressed |= ui.button("Save").clicked();
                    save_as_file_pressed |= ui.button("Save As").clicked();

                    if new_file_pressed
                        || open_file_pressed
                        || save_file_pressed
                        || save_as_file_pressed
                    {
                        // This closes the menu when we click an option, but it comes with a side effect:
                        // if we open the menu and select nothing, then press any keybind in that
                        // menu, it will close
                        ui.close_menu();
                    }

                    // TODO: Submenu "Recent Files"

                    //ui.separator();

                    /*if ui.button("Exit").clicked() {
                        todo!();
                    }*/
                });

                ui.menu_button("Edit", |ui| {
                    if ui.button("Preferences").clicked() {
                        self.unapplied_preferences = Some(UnappliedPreferences);

                        ui.close_menu();
                    }

                    /*if ui.button("Cut").clicked() {
                        todo!();
                    }

                    if ui.button("Copy").clicked() {
                        todo!();
                    }

                    if ui.button("Paste").clicked() {
                        todo!();
                    }*/
                });

                ui.menu_button("View", |ui| {
                    egui::gui_zoom::zoom_menu_buttons(ui);
                });

                ui.menu_button("Build", |ui| {
                    assemble_pressed |= ui.button("Assemble").clicked();

                    if assemble_pressed {
                        ui.close_menu();
                    }
                });

                ui.menu_button("Debug", |ui| {
                    // Often we want to press multiple buttons in this menu,
                    // so we don't close it automatically when the user clicks one
                    start_pressed |= ui.button("Start").clicked()
                        || ui.input_mut(|i| i.consume_shortcut(&START_SHORTCUT));
                    run_pressed |= ui.button("Start Without Debugging").clicked()
                        || ui.input_mut(|i| i.consume_shortcut(&RUN_SHORTCUT));
                    stop_pressed |= ui.button("Stop").clicked()
                        || ui.input_mut(|i| i.consume_shortcut(&STOP_SHORTCUT));
                    ui.separator();
                    step_over_pressed |= ui.button("Step Over").clicked()
                        || ui.input_mut(|i| i.consume_shortcut(&STEP_OVER_SHORTCUT));
                    step_into_pressed |= ui.button("Step Into").clicked()
                        || ui.input_mut(|i| i.consume_shortcut(&STEP_INTO_SHORTCUT));
                    breakpoint_pressed |= ui.button("Toggle Breakpoint").clicked()
                        || ui.input_mut(|i| i.consume_shortcut(&BREAKPOINT_SHORTCUT));
                });

                ui.menu_button("Help", |ui| {
                    /*if ui.button("TINY Overview").clicked() {
                        todo!();
                    }*/

                    if ui.button("Shortcuts").clicked() {
                        self.shortcut_window_open = true;
                        ui.close_menu();
                    }

                    ui.menu_button("Examples", |ui| {
                        for (index, &example_name) in
                            EXAMPLES.iter().map(|(name, _)| name).enumerate()
                        {
                            if ui.button(example_name).clicked() {
                                let dirty = self.dirty;
                                #[cfg(target_arch = "wasm32")]
                                let tx = self.channels.as_mut().unwrap().0.clone();
                                #[cfg(target_arch = "wasm32")]
                                run_future(async move {
                                    tx.send(ReturnAsyncFile::Open(
                                        web_io::open_example(dirty, index).await,
                                    ))
                                    .expect("Couldn't send Open Example result");
                                });

                                #[cfg(not(target_arch = "wasm32"))]
                                if let Some(source) = native_io::open_example(
                                    &self.save_path.clone(),
                                    &self.source.clone(),
                                    dirty,
                                    index,
                                ) {
                                    self.save_path = None;
                                    self.source = source;
                                }

                                ui.close_menu();
                                break;
                            };
                        }
                    });

                    if ui.button("ASCII Table").clicked() {
                        self.ascii_window_open = true;
                        ui.close_menu();
                    }

                    ui.separator();

                    if ui.button("About").clicked() {
                        self.about_window_open = true;
                        ui.close_menu();
                    }
                });
            });

            if new_file_pressed {
                let dirty = self.dirty;
                #[cfg(target_arch = "wasm32")]
                let tx = self.channels.as_mut().unwrap().0.clone();
                #[cfg(target_arch = "wasm32")]
                run_future(async move {
                    tx.send(ReturnAsyncFile::New(web_io::new_file(dirty).await))
                        .expect("Couldn't send New File result");
                });

                #[cfg(not(target_arch = "wasm32"))]
                if native_io::new_file(&self.save_path.clone(), &self.source.clone(), dirty) {
                    self.reset();
                }
            }

            if open_file_pressed {
                let dirty = self.dirty;
                #[cfg(target_arch = "wasm32")]
                let tx = self.channels.as_mut().unwrap().0.clone();
                #[cfg(target_arch = "wasm32")]
                run_future(async move {
                    tx.send(ReturnAsyncFile::Open(web_io::open_file(dirty).await))
                        .expect("Couldn't send Open File result");
                });

                #[cfg(not(target_arch = "wasm32"))]
                if let Some((path, source)) =
                    native_io::open_file(&self.save_path.clone(), &self.source.clone(), dirty)
                {
                    self.save_path = Some(path);
                    self.source = source;
                }
            }

            if save_file_pressed {
                let save_path = self.save_path.clone();
                let source = self.source.clone();

                #[cfg(target_arch = "wasm32")]
                let tx = self.channels.as_mut().unwrap().0.clone();

                #[cfg(target_arch = "wasm32")]
                run_future(async move {
                    tx.send(ReturnAsyncFile::Save(
                        web_io::save_file(save_path, source).await,
                    ))
                    .expect("Couldn't send Save File result");
                });

                #[cfg(not(target_arch = "wasm32"))]
                if let Some(path) = native_io::save_file(save_path, &source) {
                    self.dirty = false;
                    self.save_path = Some(path);
                }

                ui.close_menu();
            }

            if save_as_file_pressed {
                let source = self.source.clone();

                #[cfg(target_arch = "wasm32")]
                let tx = self.channels.as_mut().unwrap().0.clone();

                #[cfg(target_arch = "wasm32")]
                run_future(async move {
                    tx.send(ReturnAsyncFile::Save(web_io::save_file_as(source).await))
                        .expect("Couldn't send Save As File result");
                });
                #[cfg(not(target_arch = "wasm32"))]
                if let Some(path) = native_io::save_file_as(&source) {
                    self.dirty = false;
                    self.save_path = Some(path);
                }
            }

            if assemble_pressed {
                self.stop();
                self.assemble()
                    .map_err(|err| self.focused_error(&err))
                    .unwrap_or_default();
            }

            if start_pressed {
                self.start()
                    .map_err(|err| self.focused_error(&err))
                    .unwrap_or_default();
            }

            if run_pressed {
                self.run()
                    .map_err(|err| self.focused_error(&err))
                    .unwrap_or_default();
            }

            if stop_pressed {
                self.stop();
            }

            if step_over_pressed {
                self.stepping_over = true;
                self.step();
            }

            if step_into_pressed {
                self.step();
            }

            if breakpoint_pressed {
                self.toggle_breakpoint();
            }

            match self.focus_redirect {
                Focus::None => {}
                Focus::Errors => {
                    let tab = self
                        .dock_state
                        .find_tab(&String::from("Assembly Errors"))
                        .unwrap();

                    self.dock_state.set_active_tab(tab);
                    self.focus_redirect = Focus::None;
                }
                Focus::Input | Focus::Output => {
                    let tab = self
                        .dock_state
                        .find_tab(&String::from("Input/Output"))
                        .unwrap();

                    self.dock_state.set_active_tab(tab);
                    self.focus_redirect = Focus::None;
                }
            };

            let mut dock_state = self.dock_state.clone();

            DockArea::new(&mut dock_state)
                .style(Style::from_egui(ui.style().as_ref()))
                .show_inside(ui, &mut TINYTabViewer { tide: self });

            self.dock_state = dock_state;

            if self.running_to_completion | self.stepping_over {
                self.step();
            } else if self.input_ready {
                match (&self.cpu_state, &self.input) {
                    (Some(Output::WaitingForInteger), s) if s.parse::<i32>().is_ok() => self.step(),
                    (Some(Output::WaitingForChar), s) if s.len() == 1 => self.step(),
                    (Some(Output::WaitingForString), _) => self.step(),
                    _ => {}
                }
            }
        });

        ctx.request_repaint();
    }
}

#[cfg(not(target_arch = "wasm32"))]
mod native_io {
    use crate::EXAMPLES;
    use rfd::FileDialog;
    use rfd::MessageButtons;
    use rfd::MessageDialog;
    use rfd::MessageDialogResult;
    use rfd::MessageLevel;
    use std::fs;
    use std::path::PathBuf;

    fn save_failed_message() {
        MessageDialog::new()
            .set_level(MessageLevel::Error)
            .set_title("Unable to save file")
            .set_description("Could not save file")
            .set_buttons(MessageButtons::Ok)
            .show();
    }

    // Returns false if it was cancelled or failed
    pub fn handle_dirty(save_path: &Option<PathBuf>, source: &str) -> bool {
        match MessageDialog::new()
            .set_level(MessageLevel::Warning)
            .set_title("Save changes?")
            .set_description("Unsaved changes will be lost. Save changes?")
            .set_buttons(MessageButtons::YesNoCancel)
            .show()
        {
            MessageDialogResult::Yes => {
                save_path.as_ref().map_or_else(
                    || {
                        FileDialog::new()
                            .set_title("Save file")
                            .add_filter("tiny", &["tny"])
                            .save_file()
                            .map_or(false, |path| {
                                if std::fs::write(path, source.as_bytes()).is_ok() {
                                    true // succeeded
                                } else {
                                    save_failed_message();
                                    false // failed
                                }
                            })
                    },
                    |path| {
                        let successful = fs::write(path, source.as_bytes()).is_ok();
                        if !successful {
                            save_failed_message();
                        }
                        successful
                    },
                )
            }
            MessageDialogResult::No => true, // chose not to save
            _ => false,                      // failed
        }
    }

    pub fn new_file(save_path: &Option<PathBuf>, source: &str, dirty: bool) -> bool {
        !dirty || handle_dirty(save_path, source) // DeMorgan's Law
    }

    pub fn open_example(
        save_path: &Option<PathBuf>,
        source: &str,
        dirty: bool,
        example_index: usize,
    ) -> Option<String> {
        if dirty && !handle_dirty(save_path, source) {
            return None;
        }

        Some(EXAMPLES[example_index].1.to_string())
    }

    pub fn open_file(
        save_path: &Option<PathBuf>,
        source: &str,
        dirty: bool,
    ) -> Option<(PathBuf, String)> {
        if dirty && !handle_dirty(save_path, source) {
            return None;
        }

        FileDialog::new()
            .set_title("Open file")
            .add_filter("tiny", &["tny"])
            .pick_file()
            .and_then(|path| match std::fs::read_to_string(path.clone()) {
                Ok(s) => Some((path, s)),
                Err(e) => {
                    MessageDialog::new()
                        .set_level(MessageLevel::Error)
                        .set_title("Unable to open file")
                        .set_description(anyhow::Error::from(e).to_string())
                        .set_buttons(MessageButtons::Ok)
                        .show();
                    None
                }
            })
    }

    pub fn save_file(save_path: Option<PathBuf>, source: &str) -> Option<PathBuf> {
        save_path.map_or_else(
            || save_file_as(source),
            |path| {
                if fs::write(path.clone(), source.as_bytes()).is_ok() {
                    Some(path)
                } else {
                    save_failed_message();
                    None
                }
            },
        )
    }

    pub fn save_file_as(source: &str) -> Option<PathBuf> {
        FileDialog::new()
            .set_title("Save file")
            .add_filter("tiny", &["tny"])
            .save_file()
            .and_then(|path| {
                if std::fs::write(path.clone(), source.as_bytes()).is_ok() {
                    Some(path)
                } else {
                    save_failed_message();
                    None
                }
            })
    }
}

#[cfg(target_arch = "wasm32")]
mod web_io {
    use crate::OpenFileResult;
    use crate::SaveFileResult;
    use crate::EXAMPLES;
    use anyhow::Error;
    use rfd::AsyncFileDialog;
    use rfd::AsyncMessageDialog;
    use rfd::MessageButtons;
    use rfd::MessageDialogResult;
    use rfd::MessageLevel;
    use std::fs;
    use std::path::PathBuf;

    async fn handle_dirty() -> SaveFileResult {
        match AsyncMessageDialog::new()
            .set_level(MessageLevel::Warning)
            .set_title("Some changes unsaved")
            .set_description("Unsaved changes will be lost. Continue?")
            .set_buttons(MessageButtons::OkCancel)
            .show()
            .await
        {
            MessageDialogResult::Ok => SaveFileResult::UnsavedContinuing,
            _ => SaveFileResult::UnsavedCancelled,
        }
    }

    async fn save_failed_message() {
        AsyncMessageDialog::new()
            .set_level(MessageLevel::Error)
            .set_title("Unable to save file")
            .set_description("Could not save file")
            .set_buttons(MessageButtons::Ok)
            .show()
            .await;
    }

    pub async fn new_file(dirty: bool) -> bool {
        if dirty {
            match handle_dirty().await {
                SaveFileResult::UnsavedCancelled | SaveFileResult::Fail => return false,
                _ => {}
            }
        }

        true
    }

    pub async fn open_example(dirty: bool, example_index: usize) -> OpenFileResult {
        if dirty {
            match handle_dirty().await {
                SaveFileResult::UnsavedCancelled | SaveFileResult::Fail => {
                    return OpenFileResult::Cancelled
                }
                _ => {}
            }
        }

        OpenFileResult::Opened(None, EXAMPLES[example_index].1.to_string())
    }

    pub async fn open_file(dirty: bool) -> OpenFileResult {
        if dirty {
            match handle_dirty().await {
                SaveFileResult::UnsavedCancelled | SaveFileResult::Fail => {
                    return OpenFileResult::Cancelled
                }
                _ => {}
            }
        }

        match AsyncFileDialog::new()
            .set_title("Open file")
            .add_filter("tiny", &["tny"])
            .pick_file()
            .await
        {
            Some(handle) => match String::from_utf8(handle.read().await) {
                Ok(s) => OpenFileResult::Opened(None, s),
                Err(e) => {
                    AsyncMessageDialog::new()
                        .set_level(MessageLevel::Error)
                        .set_title("Unable to open file")
                        .set_description(Error::from(e).to_string())
                        .set_buttons(MessageButtons::Ok)
                        .show()
                        .await;
                    OpenFileResult::Fail
                }
            },
            None => OpenFileResult::Cancelled,
        }
    }

    pub async fn save_file(save_path: Option<PathBuf>, source: String) -> SaveFileResult {
        if let Some(path) = save_path.as_ref() {
            if fs::write(path, source.as_bytes()).is_ok() {
                SaveFileResult::Saved(save_path)
            } else {
                save_failed_message().await;
                SaveFileResult::Fail
            }
        } else {
            save_file_as(source).await
        }
    }

    pub async fn save_file_as(source: String) -> SaveFileResult {
        match &rfd::AsyncFileDialog::new()
            .set_title("Save file")
            .add_filter("tiny", &["tny"])
            .save_file()
            .await
        {
            Some(handle) => {
                if handle.write(source.as_bytes()).await.is_ok() {
                    SaveFileResult::Saved(None)
                } else {
                    save_failed_message().await;
                    SaveFileResult::Fail
                }
            }
            None => SaveFileResult::UnsavedCancelled,
        }
    }
}
