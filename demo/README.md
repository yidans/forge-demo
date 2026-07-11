# FORGE Local Live Interface

This directory contains the browser client and local server for the runnable
FORGE workflow. The server performs real LLM calls and invokes R for network
diagnostics, valid-term construction, formula checks, MPLE fitting,
pseudo-BIC screening, one checked revision, and final interpretation.

## Run

Run from the repository root:

```bash
Rscript scripts/install_dependencies.R
cp .env.example .env
# Add OPENROUTER_API_KEY to .env
python3 demo/live/server.py --port 8765
```

Open <http://127.0.0.1:8765/>. Choose one of the three included example
networks or paste a custom network, edit the short system brief, choose an
LLM, and click **Run live pipeline**. Each Stage Input/Output panel displays
the prompt, response, fit evidence, and accept/reject decision produced during
that run.

The API key is read by the local server from the environment or repository
root `.env`; it is not exposed to the browser. The server binds to
`127.0.0.1` by default.

## Components

- `live/server.py` — serves the browser, calls OpenRouter, and invokes R
- `live/run_stage.R` — computes diagnostics and the valid term library, checks
  formulas, fits candidates, and returns structured results
- `live.js` — runs the browser workflow and renders the live stage records
- `index.html`, `styles.css`, `app.js` — interface layout, styling, and the
  included example-network definitions

## Live workflow

- Stage 0: network intake and diagnostics
- Stage 1a: graph-specific valid ERGM term library
- Stage 1b: structured LLM formula proposals
- Stage 2: guardrails, MPLE fits, and pseudo-BIC screening
- Stage 3: one checked model revision
- Stage 4: model-grounded, non-causal interpretation

The small-network interface caps custom inputs at 60 nodes and 400 edges. See
`../docs/input_format.md` for the JSON format. A typical run takes roughly
30--60 seconds, depending on the LLM and local R setup.
