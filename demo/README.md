# FORGE EMNLP Demo

Browser demo for the FORGE pipeline. It runs in two modes:

- **Cached mode** (any static host, e.g. GitHub Pages): replays pre-recorded stage
  records for three illustrative networks — School friendship, Research
  collaboration, Neighborhood mutual aid.
- **Live mode** (local server): runs the real pipeline end-to-end on a chosen
  network — R computes diagnostics and the valid term library, an LLM proposes
  candidate formulas, R screens them with fast MPLE and a quick GOF simulation,
  the LLM proposes one checked edit that is kept only if the evidence improves,
  and the LLM writes the final model-grounded interpretation. A full run takes
  roughly 30–60 seconds; every Stage Input/Output pane shows the real prompt
  and response.

Live demo (cached mode): https://yidans.github.io/forge-demo/

## Run (cached mode)

Open `index.html` directly in a browser, or serve the folder statically:

```bash
python3 -m http.server 8765 --directory demo
```

## Run (live mode)

Requirements: `Rscript` on PATH with the `ergm`/`network` packages (same setup
as the main FORGE pipeline), and `OPENROUTER_API_KEY` in the repo-root `.env`
or the environment.

```bash
python3 demo/live/server.py --port 8765
# open http://127.0.0.1:8765
```

A "Live pipeline run" bar appears under the header (it stays hidden when the
API is absent, so static deployments are unaffected). Pick a scenario network
or paste a custom node/edge JSON, edit the three-line system brief, choose a
model, and hit **Run live pipeline**. Stages light up in the rail as each real
stage completes; a "▶ Live" network joins the picker so you can compare it
with the cached walkthroughs.

Pieces:

- `live/server.py` — stdlib-only HTTP server: serves the demo, proxies the LLM
  calls to OpenRouter (never exposes the key to the browser), and shells out to
  R for fitting. Binds 127.0.0.1 only.
- `live/run_stage.R` — sources `consolidated_guardrails.R` and
  `stage1_candidate_library.R` from the repo root; modes `intake`
  (diagnostics + valid term library) and `screen` (guardrail check + MPLE fits
  + pseudo-BIC ranking + GOF for the winner).
- `live.js` — builds scenario-shaped stage records from the API responses and
  reuses the cached-mode renderer unchanged.

Small-network note: the live library builder relaxes the categorical
`min_expected_cell` guardrail to 3 (the benchmark default of 5 would exclude
every attribute on a 12-node demo network); the per-candidate guardrail panel
still reports the strict check honestly as a warning.

## Demo Flow

- Stage 0: selected network and diagnostics
- Stage 1a: valid ERGM term library
- Stage 1b: LLM JSON specification proposal
- Stage 2: MPLE model screening
- Stage 3: one-edit refinement loop
- Stage 4: human-understandable interpretation theory

Cached mode uses fixed illustrative data so it is reliable for live EMNLP
presentation; live mode produces real outputs for the same six stages.
