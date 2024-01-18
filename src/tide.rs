#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use eframe::egui;
use eframe::egui::{Ui, WidgetText};
use egui_dock::{DockArea, DockState, NodeIndex, Style, TabViewer};
use egui_extras::{Column, TableBuilder};
use std::collections::HashMap;
use tiny_vm::assemble;
use tiny_vm::cpu::{Cpu, Input, Output};
use tiny_vm::types::{Address, TinyError, TinyResult};

#[derive(Clone, Copy)]
enum Focus {
    None,
    Errors,
    Input,
    Output,
}

fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([320.0, 240.0]),
        ..Default::default()
    };

    eframe::run_native("TIDE", options, Box::new(|cc| Box::<TIDE>::default()))
}

struct TINYTabViewer<'a> {
    tide: &'a mut TIDE,
}

impl<'a> TabViewer for TINYTabViewer<'a> {
    // This associated type is used to attach some data to each tab.
    type Tab = String;

    // Returns the current `tab`'s title.
    fn title(&mut self, tab: &mut Self::Tab) -> WidgetText {
        tab.as_str().into()
    }

    // Defines the contents of a given `tab`.
    fn ui(&mut self, ui: &mut Ui, tab: &mut Self::Tab) {
        match tab.as_str() {
            "Source" => {
                ui.add_sized(
                    ui.available_size(),
                    egui::TextEdit::multiline(&mut self.tide.source).code_editor(),
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
                    .body(|mut body| {
                        body.rows(15.0, num_rows, |mut row| {
                            let index = row.index();

                            let mut source_lines = self.tide.source.split('\n');
                            let source_line = self
                                .tide
                                .source_map
                                .get(&Address(index as u16))
                                .and_then(|line_num| source_lines.nth(*line_num));

                            row.col(|ui| {
                                ui.label(format!("{:03}", index));
                            });

                            row.col(|ui| {
                                ui.label(format!(
                                    "{:05}",
                                    self.tide.cpu.memory[Address::new(index as u16)].0
                                ));
                            });

                            row.col(|ui| {
                                ui.label(source_line.unwrap_or("<empty>"));
                            });
                        });
                    })
            }
            "Executable" => {
                ui.label("exec");
            }
            "Memory" => {
                ui.with_layout(
                    egui::Layout::top_down(egui::Align::Min)
                        .with_main_wrap(true)
                        .with_cross_justify(false),
                    |ui| {
                        for (addr, value, chr) in (0..900).map(|a| {
                            (
                                a,
                                self.tide.cpu.memory[Address(a)],
                                self.tide.cpu.memory[Address(a)]
                                    .read_as_char()
                                    .unwrap_or(' '),
                            )
                        }) {
                            ui.monospace(format!(
                                "{:03}  {:05} {}  ",
                                addr,
                                value,
                                if chr.is_ascii() { chr } else { ' ' }
                            ));

                            ui.end_row();
                        }
                    },
                );
            }

            "Assembly Errors" => {
                ui.add_sized(
                    ui.available_size(),
                    egui::TextEdit::multiline(&mut self.tide.error)
                        .code_editor()
                        .interactive(false),
                );
            }
            "Input/Output" => {
                if let Focus::Input = self.tide.focus_redirect {
                    ui.text_edit_singleline(&mut self.tide.input)
                        .request_focus();
                    self.tide.focus_redirect = Focus::None;
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
                    // Don't let a running Cpu eat our input on a different frame
                    // Let stepping Cpus eat it or discard it the step after we press enter,
                    // regardless of how many frames removed it is
                    self.tide.input_ready = i.key_pressed(egui::Key::Enter)
                        || (!self.tide.running_to_completion && self.tide.input_ready);
                });
            }
            "Registers" => {
                egui::Grid::new(1).show(ui, |ui| {
                    ui.label("Accumulator");
                    ui.monospace(format!("{}", self.tide.cpu.alu.acc));
                    ui.end_row();

                    ui.label("Instruction Pointer");
                    ui.monospace(format!("{}", self.tide.cpu.cu.ip));
                    ui.end_row();

                    ui.label("Stack Pointer");
                    ui.monospace(format!("{}", self.tide.cpu.alu.sp));
                    ui.end_row();

                    ui.label("Base Pointer");
                    ui.monospace(format!("{}", self.tide.cpu.alu.bp));
                    ui.end_row();

                    ui.label("Instruction Register");
                    ui.monospace(format!("{}", self.tide.cpu.cu.ir.as_byte()));

                    ui.end_row();
                });
            }
            "Stack" => {
                for byte in self.tide.cpu.get_stack() {
                    ui.monospace(byte.to_string());
                }
            }
            "Symbols" => {
                // FIXME: Sorting is not yet guaranteed
                for address in self.tide.symbols.keys() {
                    ui.monospace(format!("{:03}   {}", address, self.tide.symbols[address]));
                }
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

#[derive(Clone)]
struct TIDE {
    source: String,
    symbols: HashMap<Address, String>, // Symbols
    source_map: HashMap<Address, usize>,
    breakpoints: Vec<u16>, // Indices of lines
    cpu: Cpu,
    cpu_state: Output,
    running_to_completion: bool,

    focus_redirect: Focus,

    input: String,
    input_ready: bool,

    output: String,

    error: String,

    dock_state: DockState<String>,
}

impl TIDE {
    fn assemble(&mut self) -> TinyResult<()> {
        self.input.clear();
        self.input_ready = false;
        self.output.clear();
        self.error.clear();
        self.cpu = Cpu::new();

        let result = assemble(&self.source)?;
        self.symbols = result.0;
        self.source_map = result.1;
        self.cpu.set_memory(&result.2);
        self.cpu_state = Output::ReadyToCycle;

        Ok(())
    }

    fn new_file(&mut self) -> Result<(), String> {
        // TODO: Err if we need to save this file first
        todo!();
    }

    fn open_file(&mut self) -> Result<(), String> {
        // TODO: Err if we need to save this file first
        todo!();
    }

    fn save_file(&mut self) -> Result<(), String> {
        todo!();
    }

    fn save_file_as(&mut self) -> Result<(), String> {
        todo!();
    }

    fn manage_file_result(result: Result<(), String>) {
        match result {
            Ok(_) => {} // Happy case; file should be open and ready to edit
            Err(s) if s == "Close without saving?" => {
                todo!(); // TODO: Show dialog for this
            }
            _ => {
                todo!(); // TODO: Show separate dialog for file creation errors, etc.
            }
        }
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

    fn stop(&mut self) {}

    fn step(&mut self) {
        let result = match self.cpu_state {
            Output::WaitingForString => {
                if self.input_ready {
                    let result = self.cpu.step(Input::String(self.input.clone()));
                    self.output.push_str(&self.input);
                    self.output.push('\n');
                    self.input.clear();
                    self.input_ready = false;
                    result
                } else {
                    // FIXME: Can't select Errors tab while waiting for input
                    self.focus_redirect = Focus::Input;
                    return;
                }
            }
            Output::WaitingForChar => {
                if self.input_ready && self.input.len() == 1 {
                    let c = self.input.pop().unwrap();
                    let result = self.cpu.step(Input::Char(c));
                    self.output.push(c);
                    self.output.push('\n');
                    self.input_ready = false;
                    result
                } else {
                    self.focus_redirect = Focus::Input;
                    return;
                }
            }
            Output::WaitingForInteger => {
                if self.input_ready {
                    match self.input.parse() {
                        Ok(i) => {
                            let result = self.cpu.step(Input::Integer(i));
                            self.output.push_str(&self.input);
                            self.output.push('\n');
                            self.input.clear();
                            self.input_ready = false;
                            result
                        }
                        Err(_) => return,
                    }
                } else {
                    self.focus_redirect = Focus::Input;
                    return;
                }
            }
            Output::ReadyToCycle => self.cpu.step(Input::None),

            _ => {
                // TODO
                return;
            }
        };

        match result {
            Ok(Output::Char(c)) => {
                self.cpu_state = Output::ReadyToCycle;
                self.output.push(c);
                self.focus_redirect = Focus::Output;
            }
            Ok(Output::String(ref s)) => {
                self.cpu_state = Output::ReadyToCycle;
                self.focused_output(s);
            }
            Ok(ref out @ Output::Stopped) => {
                self.cpu_state = out.clone();
                self.running_to_completion = false;
                self.error.push_str("Completed");
            }
            Ok(out) => self.cpu_state = out,

            Err(e) => {
                self.cpu_state = Output::Stopped;
                self.running_to_completion = false;
                self.focused_error(&e);
            }
        };
    }

    fn step_over(&mut self) {
        todo!();
        //self.cpu.step_over();
    }

    fn step_into(&mut self) {
        todo!();
        //self.cpu.step_into();
    }

    fn toggle_breakpoint(&mut self) {
        todo!();
    }
}

impl Default for TIDE {
    fn default() -> Self {
        let tabs = ["Source", "Listing", "Executable", "Memory"]
            .map(str::to_string)
            .into_iter()
            .collect();

        let mut dock_state = DockState::new(tabs);
        let mut root = dock_state.main_surface_mut();

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

        Self {
            source: String::new(),
            symbols: HashMap::new(),
            source_map: HashMap::new(),
            breakpoints: vec![],
            cpu: Cpu::new(),
            cpu_state: Output::Stopped,
            running_to_completion: false,

            focus_redirect: Focus::None,

            input: String::new(),
            input_ready: false,

            output: String::new(),

            error: String::new(),

            dock_state,
        }
    }
}

const ASSEMBLE_SHORTCUT: egui::KeyboardShortcut = egui::KeyboardShortcut::new(
    egui::Modifiers {
        alt: false,
        ctrl: true,
        shift: true,
        mac_cmd: false,
        command: false,
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

impl eframe::App for TIDE {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            // Ordering is important for stop/run/start because they *consume* shortcuts; longest
            // shortcuts should be checked first when the same logical key is used with
            // different modifiers
            let mut stop_pressed = ui.input_mut(|i| i.consume_shortcut(&STOP_SHORTCUT));
            let mut run_pressed = ui.input_mut(|i| i.consume_shortcut(&RUN_SHORTCUT));
            let mut start_pressed = ui.input_mut(|i| i.consume_shortcut(&START_SHORTCUT));

            let mut step_over_pressed = ui.input_mut(|i| i.consume_shortcut(&STEP_OVER_SHORTCUT));
            let mut step_into_pressed = ui.input_mut(|i| i.consume_shortcut(&STEP_INTO_SHORTCUT));
            let mut breakpoint_pressed = ui.input_mut(|i| i.consume_shortcut(&BREAKPOINT_SHORTCUT));

            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("New").clicked() {
                        TIDE::manage_file_result(self.new_file());
                    }

                    if ui.button("Open").clicked() {
                        TIDE::manage_file_result(self.open_file());
                    }

                    ui.separator();

                    if ui.button("Save").clicked() {
                        TIDE::manage_file_result(self.save_file());
                    }

                    if ui.button("Save As").clicked() {
                        TIDE::manage_file_result(self.save_file_as());
                    }

                    ui.separator();

                    // TODO: Submenu "Recent Files"

                    ui.separator();

                    if ui.button("Exit").clicked() {
                        todo!();
                    }
                });

                ui.menu_button("Edit", |ui| {
                    if ui.button("Cut").clicked() {
                        todo!();
                    }

                    if ui.button("Copy").clicked() {
                        todo!();
                    }

                    if ui.button("Paste").clicked() {
                        todo!();
                    }
                });

                ui.menu_button("Build", |ui| {
                    if ui.button("Assemble").clicked()
                        || ui.input_mut(|i| i.consume_shortcut(&ASSEMBLE_SHORTCUT))
                    {
                        self.stop();
                        self.assemble()
                            .map_err(|err| self.focused_error(&err))
                            .unwrap_or_default();
                    }
                });

                ui.menu_button("Debug", |ui| {
                    start_pressed |= ui.button("Start").clicked();
                    run_pressed |= ui.button("Start Without Debugging").clicked();
                    stop_pressed |= ui.button("Stop").clicked();
                    ui.separator();
                    step_over_pressed |= ui.button("Step Over").clicked();
                    step_into_pressed |= ui.button("Step Into").clicked();
                    breakpoint_pressed |= ui.button("Toggle Breakpoint").clicked();

                    ui.separator();
                });

                ui.menu_button("Help", |ui| {
                    if ui.button("TINY Overview").clicked() {
                        todo!();
                    }
                });
            });

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
                self.step();
                //self.step_over();
            }

            if step_into_pressed {
                self.step_into();
            }

            if breakpoint_pressed {
                self.toggle_breakpoint();
            }

            let mut cloned = self.clone();

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

            DockArea::new(&mut self.dock_state)
                .style(Style::from_egui(ui.style().as_ref()))
                .show_inside(ui, &mut TINYTabViewer { tide: &mut cloned });

            self.cpu = cloned.cpu;
            self.source = cloned.source;
            self.symbols = cloned.symbols;
            self.breakpoints = cloned.breakpoints;
            self.input = cloned.input;
            self.input_ready = cloned.input_ready;
            self.cpu_state = cloned.cpu_state;
            self.focus_redirect = cloned.focus_redirect;

            if self.running_to_completion {
                self.step();
            }
        });

        ctx.request_repaint();
    }
}
