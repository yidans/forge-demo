# FORGE

FORGE is a runnable local system for guarded LLM-assisted Exponential Random
Graph Model (ERGM) specification and interpretation. Given a binary network
and a short domain description, it builds a graph-specific menu of valid ERGM
terms, asks an LLM for structured candidate formulas, screens and fits those
candidates, permits one checked revision, and produces a non-causal,
model-grounded explanation.

Paper: *FORGE: An LLM-Assisted System for Proposing and Testing Explanations
of Network Tie Formation* (EMNLP 2026 System Demonstrations).

## What is included

The repository contains the complete local workflow:

1. **Network intake** — validate the network and compute diagnostics.
2. **Valid-term construction** — build the ERGM terms supported by the graph
   direction and available attributes.
3. **LLM proposal** — request JSON candidate formulas using only valid terms.
4. **Model screening** — apply formula guardrails and rank successful MPLE
   fits with an MPLE-based pseudo-BIC.
5. **Checked revision** — request one small edit and keep it only if it passes
   the checks and improves the recorded diagnostics.
6. **Interpretation** — generate a term-linked explanation that separates
   supported associations from limitations and avoids causal claims.

The browser shows the network, prompts, LLM responses, fit results, checks,
revision decision, and final interpretation as the run proceeds. Three small
example networks are included, and users may provide a custom network in the
documented JSON format.

## Requirements

- Python 3.9 or newer
- R 4.3 or newer with the packages installed by
  `scripts/install_dependencies.R`
- An OpenRouter API key for the live LLM stages

The API key stays on the local server and is never sent to the browser or
included in the repository.

## Quick start

Clone or download this repository, then run the following commands from its
root directory:

```bash
Rscript scripts/install_dependencies.R
cp .env.example .env
```

Open `.env` and set:

```text
OPENROUTER_API_KEY=your_key_here
```

Start the local system:

```bash
python3 demo/live/server.py --port 8765
```

Then open <http://127.0.0.1:8765/>. Choose an included example or paste a
custom network, edit the short domain description, select an LLM, and click
**Run live pipeline**. A typical small-network run takes roughly 30--60
seconds, depending on the selected model and local R setup.

The live interface currently supports:

- `anthropic/claude-haiku-4.5`
- `openai/gpt-4o-mini`
- `google/gemini-2.5-flash`
- `anthropic/claude-sonnet-4.5`

## Custom networks

The accepted JSON format is documented in `docs/input_format.md`. The current
live interface handles binary directed or undirected networks with at least
four nodes and caps inputs at 60 nodes and 400 edges.

## Repository layout

- `demo/` — browser client and local live server
- `demo/live/run_stage.R` — R bridge for diagnostics, term construction, and
  candidate fitting
- `benchmark_datasets.R`, `stage*.R` — offline Stage 0--4 experiment pipeline
- `consolidated_guardrails.R` — deterministic specification checks
- `docs/input_format.md` — custom-network format
- `evaluation/` — locked paper benchmark summary and figure reproduction
- `prompts/` — saved Stage 1 specification and Stage 4 interpretation prompts
- `scripts/` — dependency installation and data-export helpers

## Offline benchmark pipeline

The paper evaluation uses the offline pipeline below. Run commands from the
repository root because the scripts use root-relative paths:

```bash
Rscript stage0_load_all_datasets.R
Rscript stage0_run_full_diagnostic.R
Rscript stage1_llm_library_evaluation.R
Rscript stage1_specification_generation.R
Rscript stage1_specification_metrics.R
Rscript stage2_pipeline.R
Rscript stage3_pipeline.R
Rscript stage3_refinement_pipeline.R
Rscript stage4_interpretation_pipeline.R
```

Stages that call an LLM require `OPENROUTER_API_KEY`. Full MCMLE fits can take
substantial time on the largest networks. Stage 4 can be checked without an
external call using:

```bash
STAGE4_SKIP_LLM=1 Rscript stage4_interpretation_pipeline.R
```

Reproduce the aggregate paper figure with:

```bash
python3 evaluation/make_baseline_improvement.py
```

## Interpretation limits

FORGE compares statistical explanations expressible by its current ERGM term
library. A selected model is not proof of a causal or uniquely correct
mechanism. Users should inspect convergence, goodness-of-fit, data provenance,
and domain constraints before drawing substantive conclusions.

## License

MIT License. See `LICENSE`.
