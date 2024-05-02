#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release
#![warn(clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::future_not_send)]

use eframe::egui::{Ui, WidgetText};
use egui::{text_selection::CursorRange, TextBuffer};
use egui_dock::{DockArea, DockState, NodeIndex, Style, TabViewer};
use egui_extras::{Column, TableBuilder};

use serde::{Deserialize, Serialize};
#[cfg(target_arch = "wasm32")]
use std::sync::mpsc::{Receiver, Sender};
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    num::NonZeroU32,
    time::Instant,
};
use std::{path::PathBuf, time::Duration};
use tiny_vm::assemble;
use tiny_vm::cpu::{Cpu, Input, Output};
use tiny_vm::types::{Address, TinyError, TinyResult};

const EXAMPLES: [(&str, &str); 3] = [
    ("Hello World", include_str!("../examples/helloWorld.tny")),
    ("Box Volume", include_str!("../examples/boxVolume.tny")),
    ("GCD", include_str!("../examples/gcd.tny")),
];

const DEFAULT_MAX_CPU_BLOCK_DURATION: Duration = Duration::from_millis(16); // About 60 FPS
const MIN_CPU_BLOCK_DURATION: Duration = Duration::from_millis(3); // 334 FPS
const MAX_CPU_BLOCK_DURATION: Duration = Duration::from_millis(50); // 20 FPS

const DEFAULT_STOP_ON_ERROR: bool = false;

#[derive(Clone)]
struct CpuBundle {
    cpu: Cpu,
    last_output: Output,
}

impl Default for CpuBundle {
    fn default() -> Self {
        Self {
            cpu: Cpu::new(),
            last_output: Output::ReadyToCycle,
        }
    }
}

#[derive(Default, Clone, Copy)]
enum RunMode {
    #[default]
    Pause,
    Run,
    StepOver(NonZeroU32),
}

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
struct UnappliedPreferences {
    max_cpu_block_duration: Duration,
    stop_on_error: bool,
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

struct TIDETabViewer<'a> {
    tide: &'a mut Tide,
}

fn code_editor(
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

    let output = egui::TextEdit::multiline(s)
        .code_editor()
        .interactive(enabled)
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
}

#[derive(Clone, PartialEq)]
enum TideTab {
    Source,
    Listing,
    Executable,
    Memory,
    AssemblyErrors,
    InputOutput,
    Registers,
    Stack,
    Symbols,
}

impl TideTab {
    const fn name(&self) -> &'static str {
        match self {
            Self::Source => "Source",
            Self::Listing => "Listing",
            Self::Executable => "Executable",
            Self::Memory => "Memory",
            Self::AssemblyErrors => "Assembly Errors",
            Self::InputOutput => "Input/Output",
            Self::Registers => "Registers",
            Self::Stack => "Stack",
            Self::Symbols => "Symbols",
        }
    }
}

type Tab = TideTab;

impl<'a> TabViewer for TIDETabViewer<'a> {
    // This associated type is used to attach some data to each tab.
    type Tab = Tab;

    // Returns the current `tab`'s title.
    fn title(&mut self, tab: &mut Self::Tab) -> WidgetText {
        tab.name().into()
    }

    // Defines the contents of a given `tab`.
    #[allow(clippy::too_many_lines)]
    fn ui(&mut self, ui: &mut Ui, tab: &mut Self::Tab) {
        match tab {
            Tab::Source => {
                code_editor(
                    &mut self.tide.source,
                    &mut self.tide.dirty,
                    self.tide.cpu_bundle.is_none(),
                    self.tide
                        .cpu_bundle
                        .as_ref()
                        .and_then(|bundle| self.tide.source_map.get(&bundle.cpu.cu.ip))
                        .copied(),
                    &mut self.tide.source_cursor,
                    ui,
                );
            }

            Tab::Listing => {
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
                                ui.monospace(self.tide.cpu_bundle.as_ref().map_or_else(
                                    || "?????".to_owned(),
                                    |bundle| format!("{:05}", bundle.cpu.memory[address].0),
                                ));
                            });

                            row.col(|ui| {
                                ui.monospace(source_line.unwrap_or("<empty>"));
                            });
                        });
                    });
            }

            Tab::Executable => {
                if let Some(CpuBundle { cpu, .. }) = self.tide.cpu_bundle.as_ref() {
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

            Tab::Memory => TableBuilder::new(ui)
                .striped(true)
                .column(Column::remainder().resizable(false))
                .body(|body| {
                    body.rows(15.0, 900, |mut row| {
                        let addr = u16::try_from(row.index()).unwrap();

                        row.col(|ui| {
                            if let Some(CpuBundle { ref mut cpu, .. }) =
                                self.tide.cpu_bundle.as_mut()
                            {
                                let value = cpu.memory[Address(addr)];
                                let chr = cpu.memory[Address(addr)].read_as_char().unwrap_or(' ');

                                ui.monospace(format!(
                                    "{addr:03}  {}{:05} {}  ",
                                    if value.0.is_negative() { "-" } else { " " },
                                    value.0.abs(),
                                    if chr.is_ascii() { chr } else { ' ' },
                                ))
                                .context_menu(|ui| {
                                    ui.label(format!("Address {addr:03}"));
                                    ui.separator();
                                    ui.add(
                                        egui::DragValue::new(&mut cpu.memory[Address(addr)].0)
                                            .custom_formatter(|n, _| {
                                                format!(
                                                    "{}{:05}",
                                                    if n.is_sign_positive() { " " } else { "-" },
                                                    n.abs(),
                                                )
                                            })
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

            Tab::AssemblyErrors => {
                enforce_scrollback(&mut self.tide.error);

                let slices = &self.tide.error.as_slices();
                let mut error = format!("{}\n{}", slices.0.join("\n"), slices.1.join("\n"));

                ui.add_sized(
                    ui.available_size(),
                    egui::TextEdit::multiline(&mut self.tide.error)
                        .code_editor()
                        .interactive(false),
                );
            }

            Tab::InputOutput => {
                let response = ui.text_edit_singleline(&mut self.tide.input);

                if !matches!(self.tide.run_mode, RunMode::Pause) {
                    response.request_focus();
                }

                ui.add_sized(
                    ui.available_size(),
                    egui::TextEdit::multiline(&mut self.tide.output)
                        .code_editor()
                        .interactive(false),
                );
                ui.input(|i| {
                    if i.key_pressed(egui::Key::Enter) {
                        // If the Cpu is waiting for Input matching what we have, step it
                        if let Some(CpuBundle {
                            ref last_output, ..
                        }) = self.tide.cpu_bundle
                        {
                            match last_output {
                                Output::WaitingForInteger => {
                                    if let Ok(i) = self.tide.input.parse() {
                                        _ = self
                                            .tide
                                            .step(Input::Integer(i))
                                            .map_err(|err| self.tide.focused_error(&err));
                                        self.tide.input.clear();
                                    }
                                }

                                Output::WaitingForChar if self.tide.input.len() == 1 => {
                                    if let Some(c) = self.tide.input.pop() {
                                        _ = self
                                            .tide
                                            .step(Input::Char(c))
                                            .map_err(|err| self.tide.focused_error(&err));
                                    }
                                }

                                Output::WaitingForString => {
                                    if !self.tide.input.is_empty() {
                                        let s = self.tide.input.take();
                                        _ = self
                                            .tide
                                            .step(Input::String(s))
                                            .map_err(|err| self.tide.focused_error(&err));
                                    }
                                }

                                _ => {}
                            }
                        };
                    }
                });
            }

            Tab::Registers => {
                ui.add_enabled_ui(self.tide.cpu_bundle.is_some(), |ui| {
                    egui::Grid::new("RegistersGrid").show(ui, |ui| {
                        if let Some(CpuBundle { ref mut cpu, .. }) = &mut self.tide.cpu_bundle {
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

            Tab::Stack => {
                if let Some(CpuBundle { cpu, .. }) = &self.tide.cpu_bundle {
                    for byte in cpu.get_stack() {
                        ui.monospace(byte.to_string());
                    }
                }
            }

            Tab::Symbols => {
                ui.add_enabled_ui(self.tide.cpu_bundle.is_some(), |ui| {
                    for address in self.tide.symbols.keys() {
                        ui.monospace(format!("{:03}   {}", address, self.tide.symbols[address]));
                    }
                });
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
    cpu_bundle: Option<CpuBundle>,

    #[serde(skip)]
    run_mode: RunMode,

    #[serde(skip)]
    source_cursor: Option<CursorRange>,

    #[serde(skip)]
    editing_registers: bool,

    #[serde(skip)]
    input: String,

    #[serde(skip)]
    output: String,

    #[serde(skip)]
    error: String,

    max_cpu_block_duration: Duration,
    stop_on_error: bool,

    #[serde(skip, default = "default_dock_state")]
    dock_state: DockState<Tab>,

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

fn default_dock_state() -> DockState<Tab> {
    let mut dock_state = DockState::new(vec![
        Tab::Source,
        Tab::Listing,
        Tab::Executable,
        Tab::Memory,
    ]);
    let root = dock_state.main_surface_mut();

    // Add bottom panel
    let [old_node, _] = root.split_below(
        NodeIndex::root(),
        0.8,
        vec![Tab::AssemblyErrors, Tab::InputOutput],
    );

    // Add side panel
    let [_, side_panel] = root.split_right(old_node, 0.8, vec![Tab::Registers]);
    let [_, side_panel] = root.split_below(side_panel, 1.0 / 3.0, vec![Tab::Stack]);
    root.split_below(side_panel, 0.5, vec![Tab::Symbols]);

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
        self.output.clear();
        self.error.clear();
        self.cpu_bundle = Some(CpuBundle::default());

        let result = assemble(&self.source)?;
        self.symbols = result.0;
        self.source_map = result.1;
        self.cpu_bundle.as_mut().unwrap().cpu.set_memory(&result.2);

        Ok(())
    }

    fn reset(&mut self) {
        self.source.clear();
        self.dirty = false;
        self.save_path = None;
        self.symbols.clear();
        self.source_map.clear();
        self.breakpoints.clear();
        self.cpu_bundle = None;
        self.run_mode = RunMode::Pause;
        self.input.clear();
        self.output.clear();
        self.error.clear();
    }

    fn focus_io(dock_state: &mut DockState<Tab>) {
        let tab = dock_state.find_tab(&Tab::InputOutput).unwrap();
        dock_state.set_active_tab(tab);
    }

    fn focused_error(&mut self, s: &TinyError) {
        self.error.push_str(&s.to_string());
    }

    fn run(&mut self) -> TinyResult<()> {
        self.assemble()?;
        self.run_mode = RunMode::Run;

        Ok(())
    }

    fn start(&mut self) -> TinyResult<()> {
        self.assemble()?;

        Ok(())
    }

    fn stop(&mut self) {
        self.run_mode = RunMode::Pause;
        self.cpu_bundle.take();
        self.source_map.clear();
        self.symbols.clear();
    }

    fn step(&mut self, input: Input) -> TinyResult<()> {
        if let Some(ref mut bundle) = self.cpu_bundle {
            // Forward our input to our output
            match input {
                Input::Integer(i) => self.output.push_str(&format!("{i}\n")),
                Input::Char(c) => self.output.push_str(&format!("{c}\n")),
                Input::String(ref s) => self.output.push_str(&format!("{s}\n")),
                Input::None => {}
            }

            // Step the Cpu, pausing the RunMode if we encounter an error
            bundle.last_output = bundle.cpu.step(input).map_err(|err| {
                if self.stop_on_error {
                    self.run_mode = RunMode::Pause;
                }
                err
            })?;

            // Forward Cpu output to our output
            match &bundle.last_output {
                Output::Stopped => self.error.push_str("Completed"),
                Output::Char(c) => self.output.push(*c),
                Output::String(s) => self.output.push_str(s),

                _ => {}
            };

            // NOTE: Output::Stopped is not focused because it's likely the user would
            // prefer to see the Input/Output tab when their program runs to completion
            match &bundle.last_output {
                // Focus IO if the Cpu wants input or had output
                Output::WaitingForInteger
                | Output::WaitingForChar
                | Output::WaitingForString
                | Output::Char(_)
                | Output::String(_) => {
                    Self::focus_io(&mut self.dock_state);
                }

                _ => {}
            }

            // If stepping over, keep track of the call depth
            match (&bundle.last_output, self.run_mode) {
                // NOTE: If the relative call depth from a step over exceeds the max for a u32, we
                // will stop stepping over. The user should know that this is their fault
                (Output::JumpedToFunction, RunMode::StepOver(depth)) => {
                    self.run_mode = depth
                        .checked_add(1)
                        .map_or(RunMode::Pause, RunMode::StepOver);
                }
                (Output::ReturnedFromFunction, RunMode::StepOver(depth)) => {
                    self.run_mode = NonZeroU32::try_from(depth.get() - 1)
                        .ok()
                        .map_or(RunMode::Pause, RunMode::StepOver);
                }
                _ => {}
            };

            // If stopped, update the run mode
            if matches!(&bundle.last_output, Output::Stopped) {
                self.run_mode = RunMode::Pause;
            }
        } else {
            // TODO: Actually error here somehow
            /*return anyhow::Error::from(
                "Attempt to step with no Cpu (is the program assembled?)".into(),
            );*/
        }

        Ok(())
    }

    fn step_over(&mut self) -> TinyResult<()> {
        self.run_mode = RunMode::StepOver(NonZeroU32::MIN);
        self.step(Input::None)?;
        Ok(())
    }

    fn step_into(&mut self) -> TinyResult<()> {
        self.run_mode = RunMode::Pause;
        self.step(Input::None)?;
        Ok(())
    }

    fn should_autostep(&self) -> bool {
        matches!((&self.run_mode, &self.cpu_bundle), (
            RunMode::Run | RunMode::StepOver(_),
            Some(CpuBundle {
                cpu,
                last_output:
                    Output::ReadyToCycle
                    | Output::JumpedToFunction
                    | Output::ReturnedFromFunction
                    | Output::Char(_)
                    | Output::String(_),
                ..
            }),
        ) if !self
            .source_map
            .get(&cpu.cu.ip)
            .map_or_else(|| false, |line| self.breakpoints.contains(&(*line as u16))) && {
            true
        })
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

    fn new_file(&mut self) {
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

    fn open_file(&mut self) {
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

    fn save_file_as(&mut self) {
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

    fn save_file(&mut self) {
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
            cpu_bundle: None,
            run_mode: RunMode::Pause,

            source_cursor: None,

            editing_registers: false,

            input: String::new(),

            output: String::new(),

            error: String::new(),

            max_cpu_block_duration: DEFAULT_MAX_CPU_BLOCK_DURATION,
            stop_on_error: DEFAULT_STOP_ON_ERROR,

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

        match &mut self.unapplied_preferences {
            Some(UnappliedPreferences {
                ref mut max_cpu_block_duration,
                ref mut stop_on_error,
            }) => {
                egui::Window::new("Preferences")
                    .auto_sized()
                    .show(ctx, |ui| {
                        ui.group(|ui| {
                            ui.label("Theme");
                            egui::global_dark_light_mode_buttons(ui);
                        });

                        ui.group(|ui| {
                            ui.label("CPU");

                            let mut v = max_cpu_block_duration.as_secs_f32();
                            let min: f32 = MIN_CPU_BLOCK_DURATION.as_secs_f32();
                            let max: f32 = MAX_CPU_BLOCK_DURATION.as_secs_f32();

                            ui.add(
                                egui::Slider::new(&mut v, min..=max).text("Max CPU time per frame"),
                            );

                            *max_cpu_block_duration = Duration::from_secs_f32(v);

                            ui.checkbox(stop_on_error, "Pause on error");
                        });

                        close_preferences = ui.button("Done").clicked();

                        if close_preferences {
                            self.max_cpu_block_duration = *max_cpu_block_duration;
                            self.stop_on_error = *stop_on_error;
                        }
                    });
            }

            None => {}
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
                        egui::Grid::new("ShortcutsGrid").show(ui, |ui| {
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

                        egui::Grid::new("VersionGrid").show(ui, |ui| {
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
            if ui.input_mut(|i| i.consume_shortcut(&NEW_FILE_SHORTCUT)) {
                self.new_file();
            }

            if ui.input_mut(|i| i.consume_shortcut(&OPEN_FILE_SHORTCUT)) {
                self.open_file();
            }

            if ui.input_mut(|i| i.consume_shortcut(&SAVE_AS_FILE_SHORTCUT)) {
                self.save_file_as();
            }

            if ui.input_mut(|i| i.consume_shortcut(&SAVE_FILE_SHORTCUT)) {
                self.save_file();
            }

            if ui.input_mut(|i| i.consume_shortcut(&ASSEMBLE_SHORTCUT)) {
                self.stop();
                _ = self.assemble().map_err(|err| self.focused_error(&err));
            }

            if ui.input_mut(|i| i.consume_shortcut(&STOP_SHORTCUT)) {
                self.stop();
            }

            if ui.input_mut(|i| i.consume_shortcut(&RUN_SHORTCUT)) {
                _ = self.run().map_err(|err| self.focused_error(&err));
            }

            if ui.input_mut(|i| i.consume_shortcut(&START_SHORTCUT)) {
                _ = self.start().map_err(|err| self.focused_error(&err));
            }

            if ui.input_mut(|i| i.consume_shortcut(&STEP_OVER_SHORTCUT)) {
                _ = self.step_over().map_err(|err| self.focused_error(&err));
            }

            if ui.input_mut(|i| i.consume_shortcut(&STEP_INTO_SHORTCUT)) {
                _ = self.step_into().map_err(|err| self.focused_error(&err));
            }

            if ui.input_mut(|i| i.consume_shortcut(&BREAKPOINT_SHORTCUT)) {
                self.toggle_breakpoint();
            }

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
                    let mut any = false;

                    if ui.button("New").clicked() {
                        self.new_file();
                        any = true;
                    }

                    if ui.button("Open").clicked() {
                        self.open_file();
                        any = true;
                    }

                    ui.separator();

                    if ui.button("Save As").clicked() {
                        self.save_file_as();
                        any = true;
                    }

                    if ui.button("Save").clicked() {
                        self.save_file();
                        any = true;
                    }

                    if any {
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
                        self.unapplied_preferences = Some(UnappliedPreferences {
                            max_cpu_block_duration: self.max_cpu_block_duration,
                            stop_on_error: self.stop_on_error,
                        });

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
                    if ui.button("Assemble").clicked() {
                        self.stop();
                        self.assemble()
                            .map_err(|err| self.focused_error(&err))
                            .unwrap_or_default();
                        ui.close_menu();
                    }
                });

                ui.menu_button("Debug", |ui| {
                    // Often we want to press multiple buttons in this menu,
                    // so we don't close it automatically when the user clicks one
                    if ui.button("Start").clicked() {
                        _ = self.start().map_err(|err| self.focused_error(&err));
                    }

                    if ui.button("Start Without Debugging").clicked() {
                        _ = self.run().map_err(|err| self.focused_error(&err));
                    }

                    if ui.button("Stop").clicked() {
                        self.stop();
                    }

                    ui.separator();

                    if ui.button("Step Over").clicked() {
                        _ = self.step_over().map_err(|err| self.focused_error(&err));
                    }

                    if ui.button("Step Into").clicked() {
                        _ = self.step_into().map_err(|err| self.focused_error(&err));
                    }

                    if ui.button("Toggle Breakpoint").clicked() {
                        self.toggle_breakpoint();
                    }
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
                                    self.reset();
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

            let mut dock_state = self.dock_state.clone();

            DockArea::new(&mut dock_state)
                .style(Style::from_egui(ui.style().as_ref()))
                .show_inside(ui, &mut TIDETabViewer { tide: self });

            self.dock_state = dock_state;

            let cpu_block_time = Instant::now();

            while cpu_block_time.elapsed() < self.max_cpu_block_duration && self.should_autostep() {
                if let Err(e) = self.step(Input::None) {
                    self.focused_error(&e);
                    break;
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
