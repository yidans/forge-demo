#!/bin/bash
# Publish the static demo (demo/ client files + README) to the gh-pages branch,
# which GitHub Pages serves at https://yidans.github.io/forge-demo/.
# Usage: bash scripts/publish_pages.sh        (run from the repository root, clean tree)
set -e
ROOT="$(git rev-parse --show-toplevel)"; cd "$ROOT"
TMP="$(mktemp -d)"
git worktree add -q "$TMP" gh-pages
cp demo/index.html demo/app.js demo/live.js demo/styles.css README.md LICENSE "$TMP/"
( cd "$TMP" && git add -A && git commit -q -m "Publish static demo from demo/ at $(git -C "$ROOT" rev-parse --short HEAD)" && git push -q origin gh-pages ) || echo "nothing to publish"
git worktree remove --force "$TMP"
