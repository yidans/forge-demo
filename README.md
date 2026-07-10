# FORGE Demo

Browser demo for **FORGE: An Interactive System for Guarded LLM-Assisted ERGM
Specification and Interpretation** (EMNLP 2026 System Demonstrations
submission).

Live site: https://yidans.github.io/forge-demo/

The interface walks through the six FORGE stages on three illustrative
networks — School friendship, Research collaboration, Neighborhood mutual aid:

- Stage 0: selected network and diagnostics (computed live in the browser)
- Stage 1a: valid ERGM term library
- Stage 1b: LLM JSON specification proposal
- Stage 2: MPLE model screening
- Stage 3: one-edit refinement loop
- Stage 4: human-understandable interpretation theory

This hosted version replays cached stage records so it is reliable for
presentation and review; the intake diagnostics are computed from the network
data at load time.

## Live pipeline mode

The demo also ships with a live mode (`live.js`) that runs the real pipeline
end-to-end — R builds the valid term library and screens candidates with fast
MPLE fits, an LLM proposes formulas and one checked edit, and the LLM writes
the final model-grounded interpretation, with every Stage Input/Output pane
showing the real prompts and responses. Live mode needs the full FORGE
codebase (R with the `ergm`/`network` packages, a local pipeline server, and
an OpenRouter API key), so it is not active on this static site: the run bar
appears automatically when the demo is served by the FORGE live server. See
the FORGE system distribution for setup instructions.

## Run locally (cached mode)

```bash
python3 -m http.server 8765
# open http://127.0.0.1:8765
```
