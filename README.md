# FORGE EMNLP Demo

Self-contained browser demo for the FORGE pipeline. The interface supports three selectable toy networks:

- School friendship
- Research collaboration
- Neighborhood mutual aid

## Run

Open `index.html` directly in a browser, or serve the folder locally:

```bash
python3 -m http.server 8765 --directory demo
```

Then open:

```text
http://127.0.0.1:8765
```

## Demo Flow

- Stage 0: selected toy network and diagnostics
- Stage 1a: admissible ERGM term library
- Stage 1b: LLM JSON specification proposal
- Stage 2: MPLE-style model screening
- Stage 3: one-edit refinement loop
- Stage 4: human-understandable interpretation theory

The demo uses fixed toy data so it is reliable for live EMNLP presentation. Each network has its own graph, admissible term library, candidate specs, fit evidence, refinement step, and Stage 4 human-readable theory. It is meant to illustrate the interface and pipeline semantics, not to replace the full R fitting workflow.
