dev:
    nix develop --command watchexec -w Main.hs -o restart cabal run -O0 logjuicer-github-io -- watch
