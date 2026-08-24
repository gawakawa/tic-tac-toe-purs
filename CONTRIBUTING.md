# Developer Guide

## Commands

- `nix fmt` - Format code
- `nix flake check` - Run checks (format, lint)
- `nix build` - Compile `src/Main.purs` into `output/` (`result`)
- `purs-nix compile` - Generate `output/` for editor/LSP use
- `npm install` - Install npm dependencies
- `npm run serve` - Start the dev server at http://localhost:5173
- `npm run build` - Build for production into `dist/` (requires `purs-nix compile` first)
