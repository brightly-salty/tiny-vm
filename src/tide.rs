#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use anyhow::{anyhow, Result};
use eframe::egui;
use eframe::egui::{Ui, WidgetText};
use egui_dock::{DockArea, DockState, NodeIndex, Style, TabViewer};
use egui_extras::{Column, TableBuilder};
use pollster::block_on;
use rfd::{
    AsyncFileDialog, AsyncMessageDialog, FileHandle, MessageButtons, MessageDialogResult,
    MessageLevel,
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::future::Future;
use std::path::PathBuf;
use std::rc::Rc;
use tiny_vm::assemble;
use tiny_vm::cpu::{Cpu, Input, Output};
use tiny_vm::types::{Address, TinyError, TinyResult};

enum OpenFileResult {
    Opened(Option<PathBuf>, String),
    Cancelled,
    Fail,
}

enum SaveFileResult {
    Saved(Option<PathBuf>),
    UnsavedContinuing,
    UnsavedCancelled,
    Fail,
}

enum AsyncFnReturn {
    NewFile(bool),
    OpenFile(OpenFileResult),
    SaveFile(SaveFileResult),
}

#[derive(Clone, Copy, Default)]
enum Focus {
    #[default]
    None,
    Errors,
    Input,
    Output,
}

// Native
#[cfg(not(target_arch = "wasm32"))]
fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        ..Default::default()
    };

    eframe::run_native("TIDE", options, Box::new(|cc| Box::new(TIDE::new(cc))))
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
                Box::new(|cc| Box::new(TIDE::new(cc))),
            )
            .await
            .expect("failed to start eframe");
    });
}

struct TINYTabViewer<'a> {
    tide: &'a mut TIDE,
}

fn text_editor(s: &mut String, enabled: bool, executing_line: Option<usize>, ui: &mut Ui) -> bool {
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

                    if asm.len() != 0 && comment.len() != 0 {
                        output.push_str(asm);
                        output.push_str(&" ".repeat(rightmost_comment_position - asm.len()));
                        output.push(';');
                        output.push_str(comment);
                        output
                    } else {
                        output.push_str(line);
                        output
                    }
                }
                None => line.into(),
            })
            .collect::<String>(),
    );

    // TODO: Cursor repositioning when we mess with indents
    // TODO: Toggle for automatic indent handling

    let mut changed = false;

    ui.add_enabled(enabled, |ui: &mut Ui| {
        let output = egui::TextEdit::multiline(s)
            .code_editor()
            .desired_width(f32::INFINITY)
            .min_size(ui.available_size())
            .layouter(&mut |ui, string, wrap_width| {
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
                            layout_job.append(line, 0.0, executing_text_format.clone())
                        }
                        _ => layout_job.append(line, 0.0, default_text_format.clone()),
                    }
                }

                ui.fonts(|f| f.layout_job(layout_job))
            })
            .show(ui);

        changed = output.response.changed();

        output.response
    });

    return changed;
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
                if text_editor(
                    &mut self.tide.source,
                    self.tide.cpu.is_none(),
                    self.tide
                        .cpu
                        .as_ref()
                        .and_then(|cpu| self.tide.source_map.get(&cpu.cu.ip))
                        .copied(),
                    ui,
                ) {
                    self.tide.unsaved = true;
                };
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

                            let mut source_lines = self.tide.source.split('\n');
                            let source_line = self
                                .tide
                                .source_map
                                .get(&Address(index as u16))
                                .and_then(|line_num| source_lines.nth(*line_num));

                            row.col(|ui| {
                                ui.label(format!("{:03}", index));
                            });

                            if let Some(cpu) = self.tide.cpu.as_ref() {
                                row.col(|ui| {
                                    ui.label(format!(
                                        "{:05}",
                                        cpu.memory[Address::new(index as u16)].0
                                    ));
                                });
                            } else {
                                row.col(|ui| {
                                    ui.label("?????");
                                });
                            }

                            row.col(|ui| {
                                ui.label(source_line.unwrap_or("<empty>"));
                            });
                        });
                    })
            }
            "Executable" => {
                ui.label("TODO");
            }
            "Memory" => {
                if let Some(cpu) = self.tide.cpu.as_ref() {
                    for (addr, value, chr) in (0..900).map(|a| {
                        (
                            a,
                            cpu.memory[Address(a)],
                            cpu.memory[Address(a)].read_as_char().unwrap_or(' '),
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
                } else {
                    ui.add_enabled_ui(false, |ui| {
                        for addr in 0..900 {
                            ui.monospace(format!("{:03}  ?????    ", addr,));
                            ui.end_row();
                        }
                    });
                }
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
                    egui::Grid::new(1).show(ui, |ui| match &self.tide.cpu {
                        Some(cpu) => {
                            ui.label("Accumulator");
                            ui.monospace(format!("{}", cpu.alu.acc));

                            if ui.button("Edit").clicked() {
                                //todo!();
                            }

                            ui.end_row();

                            ui.label("Instruction Pointer");
                            ui.monospace(format!("{}", cpu.cu.ip));
                            ui.end_row();

                            ui.label("Stack Pointer");
                            ui.monospace(format!("{}", cpu.alu.sp));
                            ui.end_row();

                            ui.label("Base Pointer");
                            ui.monospace(format!("{}", cpu.alu.bp));
                            ui.end_row();

                            ui.label("Instruction Register");
                            ui.monospace(format!("{}", cpu.cu.ir.as_byte()));

                            ui.end_row();
                        }
                        None => {
                            ui.label("Accumulator");
                            ui.monospace("?????");
                            _ = ui.button("Edit");
                            ui.end_row();

                            ui.label("Instruction Pointer");
                            ui.monospace("???");
                            ui.end_row();

                            ui.label("Stack Pointer");
                            ui.monospace("???");
                            ui.end_row();

                            ui.label("Base Pointer");
                            ui.monospace("???");
                            ui.end_row();

                            ui.label("Instruction Register");
                            ui.monospace("?????");

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

#[derive(Clone, Serialize, Deserialize)]
struct TIDE {
    source: String,
    #[serde(skip)]
    unsaved: bool,
    #[serde(skip)]
    save_path: Option<PathBuf>,
    #[serde(skip)]
    symbols: BTreeMap<Address, String>, // Symbols
    #[serde(skip)]
    source_map: HashMap<Address, usize>,
    #[serde(skip)]
    breakpoints: Vec<u16>, // Indices of lines (TODO)
    #[serde(skip)]
    cpu: Option<Cpu>,
    #[serde(skip)]
    cpu_state: Option<Output>,
    #[serde(skip)]
    running_to_completion: bool,

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

#[cfg(not(target_arch = "wasm32"))]
fn maybe_file_path(handle: &FileHandle) -> Option<PathBuf> {
    Some(handle.path().to_owned())
}

#[cfg(target_arch = "wasm32")]
fn maybe_file_path(handle: &FileHandle) -> Option<PathBuf> {
    None
}

impl TIDE {
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

    async fn handle_unsaved(save_path: Option<PathBuf>, source: String) -> SaveFileResult {
        match AsyncMessageDialog::new()
            .set_level(MessageLevel::Warning)
            .set_title("Save changes before closing?")
            .set_description("Unsaved changes will be lost")
            .set_buttons(MessageButtons::YesNoCancel)
            .show()
            .await
        {
            MessageDialogResult::Yes => match TIDE::save_file(save_path, source).await {
                AsyncFnReturn::SaveFile(SaveFileResult::Saved(s)) => SaveFileResult::Saved(s),
                _ => SaveFileResult::Fail,
            },
            MessageDialogResult::No => SaveFileResult::UnsavedContinuing,
            _ => SaveFileResult::UnsavedCancelled,
        }
    }

    fn reset(&mut self) {
        self.source.clear();
        self.unsaved = true;
        self.save_path = None;
        self.symbols.clear();
        self.source_map.clear();
        self.breakpoints.clear();
        self.cpu = None;
        self.cpu_state = None;
        self.running_to_completion = false;
        self.focus_redirect = Focus::None;
        self.input.clear();
        self.input_ready = false;
        self.output.clear();
        self.error.clear();
    }

    async fn new_file(save_path: Option<PathBuf>, source: String, unsaved: bool) -> bool {
        if unsaved {
            match TIDE::handle_unsaved(save_path, source).await {
                SaveFileResult::UnsavedCancelled | SaveFileResult::Fail => return false,
                _ => {}
            }
        }

        return true;
    }

    async fn open_file(save_path: Option<PathBuf>, source: String, unsaved: bool) -> AsyncFnReturn {
        if unsaved {
            match TIDE::handle_unsaved(save_path, source).await {
                SaveFileResult::UnsavedCancelled | SaveFileResult::Fail => {
                    return AsyncFnReturn::OpenFile(OpenFileResult::Cancelled)
                }
                _ => {}
            }
        }

        let mut path = None;

        let result = match AsyncFileDialog::new()
            .set_title("Open file")
            .add_filter("tiny", &["tny"])
            .pick_file()
            .await
        {
            Some(handle) => {
                path = maybe_file_path(&handle);
                let bytes = handle.read().await;
                std::str::from_utf8(&bytes)
                    .map(|s| s.to_owned())
                    .map_err(|e| e.into())
            }
            None => Err(anyhow!("Unable to open file")),
        };

        match result {
            Ok(s) => AsyncFnReturn::OpenFile(OpenFileResult::Opened(path, s)),
            Err(e) => {
                AsyncMessageDialog::new()
                    .set_level(MessageLevel::Error)
                    .set_title("Unable to open file")
                    .set_description(e.to_string())
                    .set_buttons(MessageButtons::Ok)
                    .show()
                    .await;
                AsyncFnReturn::OpenFile(OpenFileResult::Fail)
            }
        }
    }

    async fn save_file(save_path: Option<PathBuf>, source: String) -> AsyncFnReturn {
        let result = match save_path.as_ref() {
            Some(path) => fs::write(path, source.as_bytes()),
            None => {
                return TIDE::save_file_as(source).await;
            }
        };

        match result {
            Ok(_) => AsyncFnReturn::SaveFile(SaveFileResult::Saved(save_path)),
            Err(_) => {
                AsyncMessageDialog::new()
                    .set_level(MessageLevel::Error)
                    .set_title("Unable to save file")
                    .set_description("Could not save file")
                    .set_buttons(MessageButtons::Ok)
                    .show()
                    .await;
                AsyncFnReturn::SaveFile(SaveFileResult::Fail)
            }
        }
    }

    async fn save_file_as(source: String) -> AsyncFnReturn {
        let save_path;

        let result = match AsyncFileDialog::new()
            .set_title("Save file")
            .add_filter("tiny", &["tny"])
            .save_file()
            .await
        {
            Some(handle) => {
                save_path = maybe_file_path(&handle);
                handle.write(source.as_bytes()).await.map_err(|e| e)
            }
            None => {
                return AsyncFnReturn::SaveFile(SaveFileResult::UnsavedCancelled);
            }
        };

        match result {
            Ok(_) => AsyncFnReturn::SaveFile(SaveFileResult::Saved(save_path)),
            Err(_) => {
                AsyncMessageDialog::new()
                    .set_level(MessageLevel::Error)
                    .set_title("Unable to save file")
                    .set_description("Could not save file")
                    .set_buttons(MessageButtons::Ok)
                    .show()
                    .await;
                AsyncFnReturn::SaveFile(SaveFileResult::Fail)
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

    fn stop(&mut self) {
        self.running_to_completion = false;
        self.cpu.take();
        self.cpu_state.take();
        self.source_map.clear();
        self.symbols.clear();
    }

    fn step(&mut self) {
        let cpu = if let Some(cpu) = self.cpu.as_mut() {
            cpu
        } else {
            return;
        };

        let cpu_state = if let Some(cpu_state) = self.cpu_state.as_mut() {
            cpu_state
        } else {
            return;
        };

        let result = match cpu_state {
            Output::WaitingForString => {
                if self.input_ready {
                    let result = cpu.step(Input::String(self.input.clone()));
                    self.output.push_str(&self.input);
                    self.output.push('\n');
                    self.input.clear();
                    self.input_ready = false;
                    result
                } else {
                    return;
                }
            }
            Output::WaitingForChar => {
                if self.input_ready && self.input.len() == 1 {
                    let c = self.input.pop().unwrap();
                    let result = cpu.step(Input::Char(c));
                    self.output.push(c);
                    self.output.push('\n');
                    self.input_ready = false;
                    result
                } else {
                    return;
                }
            }
            Output::WaitingForInteger => {
                if self.input_ready {
                    match self.input.parse() {
                        Ok(i) => {
                            let result = cpu.step(Input::Integer(i));
                            self.output.push_str(&self.input);
                            self.output.push('\n');
                            self.input.clear();
                            self.input_ready = false;
                            result
                        }
                        Err(_) => return,
                    }
                } else {
                    return;
                }
            }
            Output::ReadyToCycle => cpu.step(Input::None),

            _ => {
                // TODO
                return;
            }
        };

        match result {
            Ok(Output::Char(c)) => {
                *cpu_state = Output::ReadyToCycle;
                self.output.push(c);
                self.focus_redirect = Focus::Output;
            }
            Ok(Output::String(ref s)) => {
                *cpu_state = Output::ReadyToCycle;
                self.focused_output(s);
            }
            Ok(ref out @ Output::Stopped) => {
                *cpu_state = out.clone();
                self.running_to_completion = false;
                self.error.push_str("Completed");
            }
            Ok(
                ref out @ Output::WaitingForChar
                | ref out @ Output::WaitingForString
                | ref out @ Output::WaitingForInteger,
            ) => {
                self.focus_redirect = Focus::Input;
                *cpu_state = out.clone();
            }
            Ok(out) => *cpu_state = out,

            Err(e) => {
                *cpu_state = Output::Stopped;
                self.running_to_completion = false;
                self.focused_error(&e);
            }
        };
    }

    fn step_over(&mut self) {
        //todo!();
        //self.cpu.step_over();
    }

    fn step_into(&mut self) {
        //todo!();
        //self.cpu.step_into();
    }

    fn toggle_breakpoint(&mut self) {
        //todo!();
    }

    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        if let Some(storage) = cc.storage {
            return eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();
        }

        Default::default()
    }
}

impl Default for TIDE {
    fn default() -> Self {
        let dock_state = default_dock_state();

        Self {
            source: String::new(),
            unsaved: false,
            save_path: None,
            symbols: BTreeMap::new(),
            source_map: HashMap::new(),
            breakpoints: vec![],
            cpu: None,
            cpu_state: None,
            running_to_completion: false,

            focus_redirect: Focus::None,

            input: String::new(),
            input_ready: false,

            output: String::new(),

            error: String::new(),

            dock_state,

            about_window_open: false,
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
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, self);
    }

    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            if self.about_window_open {
                egui::Window::new("About")
                    .auto_sized()
                    .open(&mut self.about_window_open)
                    .show(ctx, |ui| {
                        ui.label(
                            egui::RichText::new("Tiny Integrated Development Environment")
                                .size(30.0),
                        );
                        ui.label(""); // For spacing

                        egui::Grid::new(6).show(ui, |ui| {
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
                        let made_new = block_on(TIDE::new_file(
                            self.save_path.clone(),
                            self.source.clone(),
                            self.unsaved,
                        ));

                        if made_new {
                            self.reset();
                        }
                    }

                    if ui.button("Open").clicked() {
                        if let AsyncFnReturn::OpenFile(OpenFileResult::Opened(path, source)) =
                            block_on(TIDE::open_file(
                                self.save_path.clone(),
                                self.source.clone(),
                                self.unsaved,
                            ))
                        {
                            self.save_path = path;
                            self.source = source;
                        }
                    }

                    ui.separator();

                    if ui.button("Save").clicked() {
                        let result =
                            block_on(TIDE::save_file(self.save_path.clone(), self.source.clone()));

                        match result {
                            AsyncFnReturn::SaveFile(SaveFileResult::Saved(path)) => {
                                self.unsaved = false;
                                self.save_path = path;
                            }
                            _ => {}
                        }
                    }

                    if ui.button("Save As").clicked() {
                        let result = block_on(TIDE::save_file_as(self.source.clone()));

                        match result {
                            AsyncFnReturn::SaveFile(SaveFileResult::Saved(path)) => {
                                self.unsaved = false;
                                self.save_path = path;
                            }
                            _ => {}
                        }
                    }

                    //ui.separator();

                    // TODO: Submenu "Recent Files"

                    //ui.separator();

                    /*if ui.button("Exit").clicked() {
                        todo!();
                    }*/
                });

                /*
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
                */

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
                });

                ui.menu_button("Help", |ui| {
                    /*if ui.button("TINY Overview").clicked() {
                        todo!();
                    }*/

                    if ui.button("About").clicked() {
                        self.about_window_open = true;
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
            self.focus_redirect = Focus::None;

            if self.running_to_completion {
                self.step();
            } else {
                if self.input_ready {
                    match (&self.cpu_state, &self.input) {
                        (Some(Output::WaitingForInteger), s) if s.parse::<i32>().is_ok() => {
                            self.step()
                        }
                        (Some(Output::WaitingForChar), s) if s.len() == 1 => self.step(),
                        (Some(Output::WaitingForString), _) => self.step(),
                        _ => {}
                    }
                }
            }
        });

        ctx.request_repaint();
    }
}
