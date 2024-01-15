#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use eframe::egui;
use eframe::egui::{Ui, WidgetText};
use egui_dock::{DockArea, DockState, NodeIndex, Style, TabViewer};
use egui_extras::{Column, TableBuilder};
use std::collections::HashMap;
use tiny_vm::assemble;
use tiny_vm::cpu::Cpu;

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
                    egui::TextEdit::multiline(&mut self.tide.source),
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
                        body.rows(20.0, num_rows, |mut row| {
                            let mut source_lines = self.tide.source.split('\n');
                            let index = row.index();
                            let source_line = &source_lines.nth(index);

                            row.col(|ui| {
                                let result = ui.selectable_label(false, format!("{:03}", index));

                                if result.clicked() {
                                    todo!();
                                }

                                result.on_hover_text("Click to go to address in memory");
                            });

                            row.col(|ui| {
                                todo!();
                                // TODO
                                //let _ = ui.selectable_label(false, self.tide.cpu.memory);
                            });

                            row.col(|ui| {
                                let result =
                                    ui.selectable_label(false, source_line.unwrap_or("<empty>"));

                                if result.clicked() {
                                    todo!();
                                }

                                result.on_hover_text("Click to go to source line");
                            });
                        });
                    })
            }
            "Executable" => {
                ui.label("exec");
            }
            "Memory" => {
                ui.label("rember");
            }

            "Assembly Errors" => {
                ui.label("asm");
            }
            "Input/Output" => {
                ui.label("IO");
            }
            "Registers" => {
                egui::Grid::new(1).show(ui, |ui| {
                    ui.label("Accumulator");
                    ui.label("XXXXX");
                    ui.end_row();

                    ui.label("Instruction Pointer");
                    ui.label("XXX");
                    ui.end_row();

                    ui.label("Stack Pointer");
                    ui.label("XXX");
                    ui.end_row();

                    ui.label("Base Pointer");
                    ui.label("XXX");
                    ui.end_row();

                    ui.label("Instruction Register");
                    ui.label("XXXXX");
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
    symbols: HashMap<String, String>, // Symbols
    source_map: HashMap<u16, usize>,
    breakpoints: Vec<u16>, // Indices of lines
    cpu: Cpu,

    dock_state: DockState<String>,
}

impl TIDE {
    fn assemble(&mut self) {
        let result = assemble(&self.source).unwrap();
        self.symbols = result.0;
        self.source_map = result.1;
        self.cpu.parse_machine_code(&result.2);
    }

    fn run(&mut self) {
        self.assemble();
        self.cpu.run().unwrap();
    }

    fn start(&mut self) {
        self.assemble();
    }

    fn stop(&mut self) {
        todo!();
    }

    fn step(&mut self) {
        self.cpu.step().unwrap();
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

            dock_state,
        }
    }
}

impl eframe::App for TIDE {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
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
                    if ui.button("Assemble").clicked() {
                        self.assemble();
                    }
                });

                ui.menu_button("Debug", |ui| {
                    if ui.button("Start").clicked() {
                        self.start();
                    }

                    if ui.button("Start Without Debugging").clicked() {
                        self.run();
                    }

                    if ui.button("Stop").clicked() {
                        self.stop();
                    }

                    ui.separator();

                    if ui.button("Step Over").clicked() {
                        self.step_over();
                    }

                    if ui.button("Step Into").clicked() {
                        self.step_into();
                    }

                    if ui.button("Toggle Breakpoint").clicked() {
                        self.toggle_breakpoint();
                    }

                    ui.separator();
                });

                ui.menu_button("Help", |ui| {
                    if ui.button("TINY Overview").clicked() {
                        todo!();
                    }
                });
            });

            let mut cloned = self.clone();

            DockArea::new(&mut self.dock_state)
                .style(Style::from_egui(ui.style().as_ref()))
                .show_inside(ui, &mut TINYTabViewer { tide: &mut cloned });

            self.cpu = cloned.cpu;
            self.source = cloned.source;
            self.symbols = cloned.symbols;
            self.breakpoints = cloned.breakpoints;
        });
    }
}