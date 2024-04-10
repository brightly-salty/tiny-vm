#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use anyhow::Result;
use eframe::egui::{Ui, WidgetText};
use egui_dock::{DockArea, DockState, NodeIndex, Style, TabViewer};
use egui_extras::{Column, TableBuilder};

#[cfg(not(target_os = "macos"))]
use rfd::FileHandle;
use rfd::{AsyncMessageDialog, MessageButtons, MessageDialogResult, MessageLevel};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::future::Future;
use std::path::PathBuf;
use std::sync::mpsc::{Receiver, Sender};
use tiny_vm::assemble;
use tiny_vm::cpu::{Cpu, Input, Output};
use tiny_vm::types::{Address, TinyError, TinyResult};

const EXAMPLES: [(&str, &str); 3] = [
    ("Hello World", include_str!("../examples/helloWorld.tny")),
    ("Box Volume", include_str!("../examples/boxVolume.tny")),
    ("GCD", include_str!("../examples/gcd.tny")),
];

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

                    if !asm.is_empty() && !comment.is_empty() {
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

    changed
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
                if let Some(cpu) = self.tide.cpu.as_ref() {
                    let max_address = self
                        .tide
                        .source_map
                        .iter()
                        .max_by_key(|(&address, _)| address)
                        .map(|(a, _)| a.0)
                        .unwrap_or(0);

                    // Show "AAA, XXXXX" for all addresses up to the last source-mapped one
                    for i in 0..max_address {
                        ui.monospace(format!("{:03}, {:05}", i, cpu.memory[Address::new(i)].0));
                    }

                    ui.group(|ui| {
                        ui.label("Symbol table");
                        ui.separator();

                        // Symbol table here
                        for address in self.tide.symbols.keys() {
                            ui.monospace(format!("{:03}, {}", address, self.tide.symbols[address]));
                        }
                    });
                }
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

                            if ui.add_enabled(false, egui::Button::new("Edit")).clicked() {
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

fn default_unsaved() -> bool {
    true
}

#[derive(Serialize, Deserialize)]
struct TIDE {
    source: String,
    #[serde(skip, default = "default_unsaved")]
    unsaved: bool,
    #[serde(skip)]
    save_path: Option<PathBuf>,
    #[serde(skip, default = "default_channels")]
    channels: Option<(Sender<AsyncFnReturn>, Receiver<AsyncFnReturn>)>,
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
    stepping_over: bool,

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

fn default_channels() -> Option<(Sender<AsyncFnReturn>, Receiver<AsyncFnReturn>)> {
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

#[cfg(all(not(target_arch = "wasm32"), not(target_os = "macos")))]
fn maybe_file_path(handle: &FileHandle) -> Option<PathBuf> {
    Some(handle.path().to_owned())
}

#[cfg(target_arch = "wasm32")]
fn maybe_file_path(_handle: &FileHandle) -> Option<PathBuf> {
    None
}

impl TIDE {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        if let Some(storage) = cc.storage {
            return eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();
        }

        Default::default()
    }

    fn clone(&self) -> TIDE {
        TIDE {
            source: self.source.clone(),
            unsaved: self.unsaved,
            save_path: self.save_path.clone(),
            channels: None,
            symbols: self.symbols.clone(),
            source_map: self.source_map.clone(),
            breakpoints: self.breakpoints.clone(),
            cpu: self.cpu.clone(),
            cpu_state: self.cpu_state.clone(),
            running_to_completion: self.running_to_completion,
            stepping_over: self.stepping_over,
            focus_redirect: self.focus_redirect,
            input: self.input.clone(),
            input_ready: self.input_ready,
            output: self.output.clone(),
            error: self.error.clone(),
            dock_state: self.dock_state.clone(),
            about_window_open: self.about_window_open,
        }
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

    async fn save_failed_message() {
        AsyncMessageDialog::new()
            .set_level(MessageLevel::Error)
            .set_title("Unable to save file")
            .set_description("Could not save file")
            .set_buttons(MessageButtons::Ok)
            .show()
            .await;
    }

    #[cfg(not(target_arch = "wasm32"))]
    async fn handle_unsaved(save_path: Option<PathBuf>, source: String) -> SaveFileResult {
        match AsyncMessageDialog::new()
            .set_level(MessageLevel::Warning)
            .set_title("Save changes?")
            .set_description("Unsaved changes will be lost. Save changes?")
            .set_buttons(MessageButtons::YesNoCancel)
            .show()
            .await
        {
            MessageDialogResult::Yes => match TIDE::save_file(save_path, source).await {
                AsyncFnReturn::SaveFile(r @ SaveFileResult::Saved(_)) => r,
                AsyncFnReturn::SaveFile(r @ SaveFileResult::UnsavedCancelled) => r,
                _ => {
                    TIDE::save_failed_message().await;
                    SaveFileResult::Fail
                }
            },
            MessageDialogResult::No => SaveFileResult::UnsavedContinuing,
            _ => SaveFileResult::UnsavedCancelled,
        }
    }

    #[cfg(target_arch = "wasm32")]
    async fn handle_unsaved(_save_path: Option<PathBuf>, _source: String) -> SaveFileResult {
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
        self.stepping_over = false;
        self.focus_redirect = Focus::None;
        self.input.clear();
        self.input_ready = false;
        self.output.clear();
        self.error.clear();
    }

    async fn new_file(save_path: Option<PathBuf>, source: String, unsaved: bool) -> AsyncFnReturn {
        if unsaved {
            match TIDE::handle_unsaved(save_path, source).await {
                SaveFileResult::UnsavedCancelled | SaveFileResult::Fail => {
                    return AsyncFnReturn::NewFile(false)
                }
                _ => {}
            }
        }

        AsyncFnReturn::NewFile(true)
    }

    async fn open_example(
        save_path: Option<PathBuf>,
        source: String,
        unsaved: bool,
        example_index: usize,
    ) -> AsyncFnReturn {
        if unsaved {
            match TIDE::handle_unsaved(save_path.clone(), source).await {
                SaveFileResult::UnsavedCancelled | SaveFileResult::Fail => {
                    return AsyncFnReturn::OpenFile(OpenFileResult::Cancelled)
                }
                _ => {}
            }
        }

        AsyncFnReturn::OpenFile(OpenFileResult::Opened(
            None,
            EXAMPLES[example_index].1.to_string(),
        ))
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

        match create_pick_file_dialog().await {
            Some(Ok((path, s))) => AsyncFnReturn::OpenFile(OpenFileResult::Opened(path, s)),
            Some(Err(e)) => {
                AsyncMessageDialog::new()
                    .set_level(MessageLevel::Error)
                    .set_title("Unable to open file")
                    .set_description(e.to_string())
                    .set_buttons(MessageButtons::Ok)
                    .show()
                    .await;
                AsyncFnReturn::OpenFile(OpenFileResult::Fail)
            }
            None => AsyncFnReturn::OpenFile(OpenFileResult::Cancelled),
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
                TIDE::save_failed_message().await;
                AsyncFnReturn::SaveFile(SaveFileResult::Fail)
            }
        }
    }

    async fn save_file_as(source: String) -> AsyncFnReturn {
        let result = match create_save_file_dialog(source).await {
            Some(result) => result,
            None => {
                return AsyncFnReturn::SaveFile(SaveFileResult::UnsavedCancelled);
            }
        };

        match result {
            Ok(save_path) => AsyncFnReturn::SaveFile(SaveFileResult::Saved(save_path)),
            Err(_) => {
                TIDE::save_failed_message().await;
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
        self.stepping_over = false;
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
            Output::JumpedToFunction | Output::ReadyToCycle | Output::ReturnedFromFunction => {
                cpu.step(Input::None)
            }

            _ => {
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
                self.stepping_over = false;
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
            Ok(ref out @ Output::JumpedToFunction | ref out @ Output::ReturnedFromFunction) => {
                self.stepping_over = false;
                *cpu_state = out.clone();
            }
            Ok(out) => *cpu_state = out,

            Err(e) => {
                *cpu_state = Output::Stopped;
                self.running_to_completion = false;
                self.stepping_over = false;
                self.focused_error(&e);
            }
        };
    }

    fn toggle_breakpoint(&mut self) {
        //todo!();
    }
}

impl Default for TIDE {
    fn default() -> Self {
        Self {
            source: String::new(),
            unsaved: default_unsaved(),
            save_path: None,
            channels: default_channels(),
            symbols: BTreeMap::new(),
            source_map: HashMap::new(),
            breakpoints: vec![],
            cpu: None,
            cpu_state: None,
            running_to_completion: false,
            stepping_over: false,

            focus_redirect: Focus::None,

            input: String::new(),
            input_ready: false,

            output: String::new(),

            error: String::new(),

            dock_state: default_dock_state(),

            about_window_open: false,
        }
    }
}

#[cfg(target_os = "macos")]
async fn create_pick_file_dialog() -> Option<Result<(Option<PathBuf>, String)>> {
    match rfd::FileDialog::new()
        .set_title("Open file")
        .add_filter("tiny", &["tny"])
        .pick_file()
    {
        Some(path) => Some(
            std::fs::read_to_string(path.clone())
                .map(|s| (Some(path.to_owned()), s))
                .map_err(|e| e.into()),
        ),
        None => None,
    }
}

#[cfg(not(target_os = "macos"))]
async fn create_pick_file_dialog() -> Option<Result<(Option<PathBuf>, String)>> {
    match rfd::AsyncFileDialog::new()
        .set_title("Open file")
        .add_filter("tiny", &["tny"])
        .pick_file()
        .await
    {
        Some(handle) => Some(
            String::from_utf8(handle.read().await)
                .map(|s| (maybe_file_path(&handle), s))
                .map_err(|e| e.into()),
        ),
        None => None,
    }
}

#[cfg(target_os = "macos")]
async fn create_save_file_dialog(source: String) -> Option<Result<Option<PathBuf>>> {
    rfd::FileDialog::new()
        .set_title("Save file")
        .add_filter("tiny", &["tny"])
        .save_file()
        .map(|path| {
            std::fs::write(path.clone(), source.as_bytes())
                .map(|_| Some(path))
                .map_err(|e| e.into())
        })
}

#[cfg(not(target_os = "macos"))]
async fn create_save_file_dialog(source: String) -> Option<Result<Option<PathBuf>>> {
    match &rfd::AsyncFileDialog::new()
        .set_title("Save file")
        .add_filter("tiny", &["tny"])
        .save_file()
        .await
    {
        Some(handle) => {
            let path = maybe_file_path(handle);
            Some(
                handle
                    .write(source.as_bytes())
                    .await
                    .map(|_| path)
                    .map_err(|e| e.into()),
            )
        }
        None => None,
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

#[cfg(not(target_arch = "wasm32"))]
fn run_future<T: Future<Output = ()> + 'static>(f: T) {
    block_on(f);
}

#[cfg(target_arch = "wasm32")]
fn run_future<T: Future<Output = ()> + 'static>(f: T) {
    wasm_bindgen_futures::spawn_local(f);
}

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

            // Handle file IO state updates
            let results = self
                .channels
                .as_mut()
                .unwrap()
                .1
                .try_iter()
                .collect::<Vec<AsyncFnReturn>>();

            for result in results {
                match result {
                    AsyncFnReturn::NewFile(true) => self.reset(),
                    AsyncFnReturn::OpenFile(OpenFileResult::Opened(path, source)) => {
                        self.save_path = path;
                        self.source = source;
                    }
                    AsyncFnReturn::SaveFile(SaveFileResult::Saved(path)) => {
                        self.unsaved = false;
                        self.save_path = path;
                    }
                    _ => {}
                }
            }

            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("New").clicked() {
                        let tx = self.channels.as_mut().unwrap().0.clone();
                        let save_path = self.save_path.clone();
                        let source = self.source.clone();
                        let unsaved = self.unsaved;

                        run_future(async move {
                            tx.send(TIDE::new_file(save_path, source, unsaved).await)
                                .expect("Couldn't send New File result");
                        });

                        ui.close_menu();
                    }

                    if ui.button("Open").clicked() {
                        let tx = self.channels.as_mut().unwrap().0.clone();
                        let save_path = self.save_path.clone();
                        let source = self.source.clone();
                        let unsaved = self.unsaved;

                        run_future(async move {
                            tx.send(TIDE::open_file(save_path, source, unsaved).await)
                                .expect("Couldn't send Open File result");
                        });

                        ui.close_menu();
                    }

                    ui.separator();

                    if ui.button("Save").clicked() {
                        let tx = self.channels.as_mut().unwrap().0.clone();
                        let save_path = self.save_path.clone();
                        let source = self.source.clone();

                        run_future(async move {
                            tx.send(TIDE::save_file(save_path, source).await)
                                .expect("Couldn't send Save File result");
                        });

                        ui.close_menu();
                    }

                    if ui.button("Save As").clicked() {
                        let tx = self.channels.as_mut().unwrap().0.clone();
                        let source = self.source.clone();

                        run_future(async move {
                            tx.send(TIDE::save_file_as(source).await)
                                .expect("Couldn't send Save As File result");
                        });

                        ui.close_menu();
                    }

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

                        ui.close_menu();
                    }
                });

                ui.menu_button("Debug", |ui| {
                    // Often we want to press multiple buttons in this menu,
                    // so we don't close it automatically when the user clicks one
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

                    ui.menu_button("Examples", |ui| {
                        for (index, &example_name) in
                            EXAMPLES.iter().map(|(name, _)| name).enumerate()
                        {
                            if ui.button(example_name).clicked() {
                                let tx = self.channels.as_mut().unwrap().0.clone();
                                let save_path = self.save_path.clone();
                                let source = self.source.clone();
                                let unsaved = self.unsaved;

                                run_future(async move {
                                    tx.send(
                                        TIDE::open_example(save_path, source, unsaved, index).await,
                                    )
                                    .expect("Couldn't send Open Example result");
                                });

                                ui.close_menu();
                                break;
                            };
                        }
                    });

                    ui.separator();

                    if ui.button("About").clicked() {
                        self.about_window_open = true;
                        ui.close_menu();
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
                self.stepping_over = true;
                self.step();
            }

            if step_into_pressed {
                self.step();
            }

            if breakpoint_pressed {
                self.toggle_breakpoint();
            }

            let mut cloned = TIDE::clone(self);

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
