#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use eframe::egui;
use eframe::egui::{Ui, WidgetText};
use egui_dock::{DockArea, DockState, NodeIndex, Style, TabViewer};
use egui_extras::{Column, TableBuilder};
use std::collections::HashMap;
use tiny_vm::assemble;
use tiny_vm::cpu::{Cpu, Input, Output};
use tiny_vm::types::Address;

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
                ui.text_edit_singleline(&mut self.tide.input_buffer);
                ui.add_sized(
                    ui.available_size(),
                    egui::TextEdit::multiline(&mut self.tide.output)
                        .code_editor()
                        .interactive(false),
                );
                ui.input(|i| {
                    if i.key_pressed(egui::Key::Enter) && !self.tide.input_buffer.is_empty() {
                        self.tide.input.clear();
                        self.tide.input.push_str(&self.tide.input_buffer);
                        self.tide.input_buffer.clear();
                    }
                });
            }
            "Registers" => {
                egui::Grid::new(1).show(ui, |ui| {
                    ui.label("Accumulator");
                    ui.label(format!("{:05}", self.tide.cpu.alu.acc));
                    ui.end_row();

                    ui.label("Instruction Pointer");
                    ui.label(format!("{:03}", self.tide.cpu.cu.ip));
                    ui.end_row();

                    ui.label("Stack Pointer");
                    ui.label(format!("{:03}", self.tide.cpu.alu.sp));
                    ui.end_row();

                    ui.label("Base Pointer");
                    ui.label(format!("{:03}", self.tide.cpu.alu.bp));
                    ui.end_row();

                    ui.label("Instruction Register");
                    //ui.label(format!("{:05}", self.tide.cpu.cu.ir));
                    ui.end_row();
                });
            }
            "Stack" => {
                ui.label("pancake");
            }
            "Symbols" => {
                ui.label("@#$%");
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

    input_buffer: String,
    input: String,

    output: String,

    error: String,

    dock_state: DockState<String>,
}

impl TIDE {
    fn assemble(&mut self) -> Result<(), String> {
        self.input_buffer.clear();
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

    fn run(&mut self) -> Result<(), String> {
        self.assemble()?;
        self.running_to_completion = true;

        Ok(())
    }

    fn start(&mut self) -> Result<(), String> {
        self.assemble()?;

        Ok(())
    }

    fn stop(&mut self) {}

    fn step(&mut self) {
        let result = match self.cpu_state {
            Output::ReadyToCycle => {
                if !self.input.is_empty() {
                    let result = self.cpu.step(Input::String(self.input.clone()));
                    self.input.clear();
                    result
                } else {
                    self.cpu.step(Input::None)
                }
            }

            _ => {
                // TODO
                return;
            }
        };

        let result = if !self.input.is_empty() {
            let result = self.cpu.step(Input::String(self.input.clone()));
            self.input.clear();
            result
        } else {
            self.cpu.step(Input::None)
        };

        match result {
            Ok(ref out @ Output::Char(c)) => {
                self.cpu_state = out.clone();
                self.output.push(c);
            }
            Ok(ref out @ Output::String(ref s)) => {
                self.cpu_state = out.clone();
                self.output.push_str(&s);
            }
            Ok(ref out @ Output::Stopped) => {
                self.cpu_state = out.clone();
                self.running_to_completion = false;
            }
            Ok(out) => self.cpu_state = out,

            Err(s) => {
                self.cpu_state = Output::Stopped;
                self.running_to_completion = false;
                self.error.push_str(&s);
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

            input_buffer: String::new(),
            input: String::new(),

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
                        todo!();
                    }

                    if ui.button("Open").clicked() {
                        todo!();
                    }

                    ui.separator();

                    if ui.button("Save").clicked() {
                        todo!();
                    }

                    if ui.button("Save As").clicked() {
                        todo!();
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
                            .map_err(|err| self.error.push_str(&err))
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
                    .map_err(|err| self.error.push_str(&err))
                    .unwrap_or_default();
            }
            if run_pressed {
                self.run()
                    .map_err(|err| self.error.push_str(&err))
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

            DockArea::new(&mut self.dock_state)
                .style(Style::from_egui(ui.style().as_ref()))
                .show_inside(ui, &mut TINYTabViewer { tide: &mut cloned });

            self.cpu = cloned.cpu;
            self.source = cloned.source;
            self.symbols = cloned.symbols;
            self.breakpoints = cloned.breakpoints;
            self.input_buffer = cloned.input_buffer;
            self.input = cloned.input;
            self.cpu_state = cloned.cpu_state;

            if self.running_to_completion {
                self.step();
            }
        });
    }
}
