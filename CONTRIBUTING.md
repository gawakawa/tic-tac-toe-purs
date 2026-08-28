# Developer Guide

## Commands

- `nix fmt` - Format code
- `nix flake check` - Run checks (format, lint)
- `nix build` - Compile the `useMemo`-transformed `src/Main.purs` into `output/` (`result`)
- `purs-nix compile` - Generate `output/` for editor/LSP use (compiles the untransformed working tree)
- `purs-memo src && purs-nix compile; git restore src/` - Reproduce what `nix build` compiles, locally
- `npm install` - Install npm dependencies
- `npm run serve` - Start the dev server at http://localhost:5173
- `npm run build` - Build for production into `dist/` (requires `purs-nix compile` first)
- `npm run preview` - Preview the build on the Cloudflare Workers runtime (requires `purs-nix compile` first)
- `npm run deploy` - Build and deploy to Cloudflare Workers
