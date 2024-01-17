# Roadmap

## Phase 1: Feature parity with TIDE.exe

- file opening, saving, creation
- breakpoints
- symbol view
- stack view
- executable tab
- add shortcuts to full menubar
- help menu option
- correct outputting of newlines
- add step over/step into

## Phase 2: Internal refactoring for performance and clarity

- custom error types, better error handling
- use enums or constants for tab names
- do only two passes in assembly step
- step should be run continuously instead of in every frame redraw

## Phase 3: New features

- add line numbers in source 
- consider merging source lines in listing
- syntax highlighting
- show breakpoints in source tab
- nice handling of indents in source tab
- add way to see the TIDE Cpu state, or at least focus the input box when awaiting input
- show keyboard shortcuts in menu
