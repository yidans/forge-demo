# FORGE

FORGE is an interactive system for guarded LLM-assisted Exponential Random
Graph Model (ERGM) specification and interpretation. It turns a binary network
and a short domain description into candidate explanations of tie formation,
checks those candidates with deterministic rules and statistical fits, permits
one evidence-checked revision, and produces a model-grounded explanation.

- Hosted cached interface: https://yidans.github.io/forge-demo/
- Paper: *FORGE: An LLM-Assisted System for Proposing and Testing
  Explanations of Network Tie Formation* (EMNLP 2026 System Demonstrations)

## Scope of this release

The hosted GitHub Pages interface replays three completed scenario records for
reliable presentation: school friendship, research collaboration, and
neighborhood mutual aid. It does not make live LLM calls or fit ERGMs.

The same repository contains the runnable local system:

1. Stage 0 loads a network and computes diagnostics.
2. Stage 1 builds the graph-specific valid term list and requests structured
   LLM candidate formulas.
3. Stage 2 screens candidates with fast fits and model checks.
4. Stage 3 performs full fitting and one guarded refinement.
5. Stage 4 produces a non-causal, model-grounded interpretation.

The benchmark loader supports the twelve networks used by the paper:
`faux_mesa`, `faux_dixon`, `faux_magnolia`, `kapferer`, `lazega`,
`krackhardt`, `glasgow_s50`, `manufacturing_emails`, `enron_emails`,
`florentine`, `noordin_top`, and `caltech_36`.

The paper reports non-null convergence for 10 of 12 networks under fixed
controls. The locked values used by the aggregate paper figure are recorded in
`evaluation/paper_benchmark_summary.csv`; they are kept separate from local
development outputs in `results/`.

## Repository layout

- `demo/`: cached browser interface plus the optional local live server.
- `benchmark_datasets.R`, `stage*.R`: offline Stage 0-4 pipeline.
- `data/raw/external/`: external source files needed for Caltech and Noordin.
- `evaluation/`: paper-locked benchmark summary and figure reproduction.
- `paper/`: current v3 manuscript source and figures.
- `docs/input_format.md`: custom-network JSON format.
- `results/`: generated locally and intentionally not committed.

The site files are also mirrored at repository root so GitHub Pages can serve
the interface without a separate web build.

## Install

R 4.3 or newer is recommended. Install the required packages with:

```bash
Rscript scripts/install_dependencies.R
```

Copy `.env.example` to `.env` and add an OpenRouter key only if you want live
LLM calls. `.env` is ignored and must never be committed.

## Run the cached interface

From the repository root:

```bash
python3 -m http.server 8765 --directory demo
```

Open http://127.0.0.1:8765/.

## Run the local live system

```bash
python3 demo/live/server.py --port 8765
```

The live control bar appears after `/api/health` confirms that the local Python
and R backend is available. GitHub Pages remains cached-only because it is a
static host.

The live interface accepts the three included scenarios or a custom binary
network using the format in `docs/input_format.md`.

## Run the offline benchmark pipeline

Run commands from the repository root because the scripts use root-relative
paths:

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

Stages that call an LLM require `OPENROUTER_API_KEY`. Full MCMLE runs can take
substantial time, especially on the largest networks.

To validate Stage 4 without an external call:

```bash
STAGE4_SKIP_LLM=1 Rscript stage4_interpretation_pipeline.R
```

## Reproduce the paper aggregate figure

```bash
python3 evaluation/make_baseline_improvement.py
```

This command reads the locked CSV and writes
`evaluation/baseline_improvement.pdf`.

## Interpretation limits

FORGE compares plausible explanations expressible by its current ERGM term
list. A selected model is not proof of a causal or uniquely true mechanism.
Users should inspect convergence, goodness-of-fit, data provenance, and domain
constraints before drawing substantive conclusions.

## License

MIT License. See `LICENSE`.
