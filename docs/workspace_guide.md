# FORGE Repository Guide

Run all commands from the repository root; the R scripts source one another
with root-relative paths.

## Layout

- `demo/`: the browser client (`index.html`, `app.js`, `styles.css`, `live.js`)
  and the local live server (`live/server.py`, `live/run_stage.R`).
  The static walkthrough at <https://yidans.github.io/forge-demo/> is published
  from this directory with `scripts/publish_pages.sh`.
- `consolidated_guardrails.R`, `stage1_candidate_library.R`: the deterministic
  specification checks and the valid-term (L*) builder used by both the live
  server and the offline scripts.
- `evaluation/`: the data behind the numbers reported in the paper, and the
  recorded example runs that the static walkthrough replays
  (`evaluation/demo_examples/`).
- `stage0_*.R` … `stage4_*.R`, `benchmark_datasets.R`: the offline experiment
  scripts (see the README section "Offline experiment scripts").
- `prompts/`: saved Stage 1 and Stage 4 prompts from the offline experiments.
- `data/raw/external/`: two external benchmark source files used by the
  offline scripts.
- `docs/input_format.md`: the custom-network JSON format.
- `scripts/`: dependency installation, data export, and Pages publishing.
- `results/`: locally generated outputs; ignored by Git.

The public release omits videos, build archives, cached network objects, and
development results; none are needed to run FORGE.

## Conventions

- Lowercase `snake_case` for directories and ordinary files; `README.md` and
  `LICENSE` stay uppercase.
- No `final`, `fixed`, or numbered suffixes on canonical files: replace the
  canonical file after verification instead.
- Put generated experiment artifacts in `results/` and disposable work in
  `tmp/` (both ignored).
