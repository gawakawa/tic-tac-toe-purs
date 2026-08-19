# Developer Guide

## Commands

- `nix fmt` - Format code
- `nix flake check` - Run checks (format, lint)
- `nix build` - Build `src/Main.purs` into a single JS file (`result`) via purs-nix's `bundle {}`
- `purs-nix compile` - Generate `output/` for editor/LSP use
- `npm install` - Install npm dependencies
- `npm run serve` - Start the dev server at http://localhost:5173
