import { defineConfig } from "vite";

// Every value here is the Vite 8 default, written out explicitly so the build
// is declared rather than inherited.
export default defineConfig({
  root: ".",
  base: "/",
  publicDir: "public",
  cacheDir: "node_modules/.vite",
  appType: "spa",

  resolve: {
    // PureScript FFI resolves `react` through output/*/node_modules, a symlink
    // into the Nix store. Resolving to the real path keeps React a single copy.
    preserveSymlinks: false,
  },

  build: {
    target: "baseline-widely-available",
    outDir: "dist",
    assetsDir: "assets",
    assetsInlineLimit: 4096,
    cssCodeSplit: true,
    cssMinify: "lightningcss",
    minify: "oxc",
    sourcemap: false,
    emptyOutDir: true,
    copyPublicDir: true,
    modulePreload: { polyfill: true },
    reportCompressedSize: true,
    chunkSizeWarningLimit: 500,
  },

  server: {
    host: "localhost",
    port: 5173,
    strictPort: false,
  },

  preview: {
    port: 4173,
  },
});
