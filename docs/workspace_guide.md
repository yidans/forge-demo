# FORGE Workspace Guide

Run all commands from the repository root. Active R scripts source one another
with root-relative paths.

## Current layout

- `demo/`: cached browser interface and optional local live server.
- `data/raw/external/`: the two external benchmark source files.
- `evaluation/`: paper-locked benchmark summary and figure generator.
- `paper/`: the current EMNLP manuscript source and figures.
- `scripts/`: installation and maintenance utilities.
- `results/`: locally generated Stage 0-4 outputs; ignored by Git.

The public release intentionally omits local build archives, videos, cached
network objects, and development results. They are not required to run FORGE.

## Naming rules

- Use lowercase `snake_case` for directories and ordinary files.
- Keep standard repository names uppercase: `README.md` and `LICENSE`.
- Do not add `final`, `fixed`, `improved`, or numbered version suffixes to the
  canonical source. Replace the canonical file after verification instead.
- Put generated experiment artifacts in `results/`, prompts in `prompts/`, and
  public deliverables in `dist/`.
- Use `tmp/` only for disposable local work; it is ignored and may be deleted.

## Canonical Stage 3 files

- `stage3_pipeline.R`: finalist selection and MCMLE fitting.
- `stage3_refinement_pipeline.R`: the single supported refinement pipeline.
- `results/stage3_refinement_history.{json,rds}`: refinement provenance.
- `results/stage3_refinement_summary.csv`: refinement summary.
